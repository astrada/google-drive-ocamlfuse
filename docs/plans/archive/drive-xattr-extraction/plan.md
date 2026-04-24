# Extract Drive Xattrs

## Goal

Move the xattr entrypoints out of `src/drive.ml` into a small testable module
that follows the existing functorized runtime pattern used by
`DriveViews`, `DriveFileMutations`, `DriveUploadDispatch`, and
`DriveMutations`.

The extraction target is:

- `Drive.get_xattr`
- `Drive.list_xattr`
- `Drive.set_xattr`
- `Drive.remove_xattr`

The production behavior should stay unchanged. `src/drive.ml` should retain
only thin wrappers that build the runtime, call the extracted operations, and
run them through `do_request`.

## Current Problem

The xattr logic is currently embedded directly in `src/drive.ml` and depends on
global production concerns:

- `Context.get_ctx`
- `get_path_in_cache`
- `get_resource`
- `update_remote_resource`
- `FilesResource.update`
- `with_retry_default`
- `build_resource_keys_header_from_resource`

That makes useful unit tests hard to write because tests have to cross the
global context and Google Drive API boundary just to exercise local xattr
rules.

The behavior worth testing is mostly local:

- path normalization is invoked consistently
- cached xattrs are parsed before reads and writes
- missing xattrs raise `No_attribute`
- `Fuse.CREATE`, `Fuse.REPLACE`, and `Fuse.AUTO` are enforced
- JSON-escaped attribute length is checked before remote updates
- Drive `appProperties` patches use the expected `x-` keys

## Proposed Shape

Add:

- `src/driveXattrs.ml`
- `src/driveXattrs.mli`
- `test/testDriveXattrs.ml`

Expose a functor:

```ocaml
type runtime = {
  cache : CacheData.t;
  config : Config.t;
}

module type PORTS = sig
  val max_attribute_length : int
  val json_length : string -> int
  val get_path_in_cache : string -> Config.t -> string * bool
  val get_resource : string -> bool -> CacheData.Resource.t GapiMonad.SessionM.m

  val build_resource_keys_header_from_resource :
    CacheData.Resource.t -> GapiCore.Header.t list

  val remote_update :
    custom_headers:GapiCore.Header.t list ->
    fileId:string ->
    GapiDriveV3Model.File.t ->
    GapiDriveV3Model.File.t GapiMonad.SessionM.m

  val update_remote_resource :
    runtime ->
    string ->
    (CacheData.Resource.t ->
    GapiDriveV3Model.File.t option GapiMonad.SessionM.m) ->
    unit GapiMonad.SessionM.m
end
```

The xattr module should own only the xattr-specific behavior. Generic update
semantics should remain delegated through `P.update_remote_resource`, which can
be backed by `DriveMutations.update_remote_resource` in production.

## Production Wiring

In `src/drive.ml`, add a ports module:

```ocaml
module DriveXattrPorts = struct
  let max_attribute_length = max_attribute_length
  let json_length = json_length
  let get_path_in_cache = get_path_in_cache
  let get_resource = get_resource
  let build_resource_keys_header_from_resource =
    build_resource_keys_header_from_resource

  let remote_update ~custom_headers ~fileId file_patch =
    with_retry_default
      (FilesResource.update ~enforceSingleParent:true ~supportsAllDrives:true
         ~std_params:file_std_params ~custom_headers ~fileId file_patch)

  let update_remote_resource runtime path do_remote_update =
    MutationOps.update_remote_resource
      {
        DriveMutations.cache = runtime.DriveXattrs.cache;
        config = runtime.config;
        mountpoint_path = "";
        skip_trash = false;
      }
      path do_remote_update
end
```

Then instantiate:

```ocaml
module XattrOps = DriveXattrs.Make (DriveXattrPorts)
```

Add:

```ocaml
let drive_xattr_runtime () =
  let context = Context.get_ctx () in
  {
    DriveXattrs.cache = context.Context.cache;
    config = context |. Context.config_lens;
  }
```

Replace the existing `get_xattr`, `list_xattr`, `set_xattr`, and
`remove_xattr` bodies with `do_request` calls into `XattrOps`.

If the runtime adapter through `DriveMutations.runtime` feels too awkward,
prefer implementing `DriveXattrPorts.update_remote_resource` as a direct
production function in `drive.ml` rather than adapting through `MutationOps`.
Do not duplicate the generic update logic in the new module.

## Implementation Steps

1. Create `driveXattrs.mli` with exceptions, runtime, `PORTS`, and the four
   public operations.
2. Create `driveXattrs.ml` by moving the xattr-specific logic from
   `src/drive.ml`.
3. Keep exception names aligned with `DriveMutations` and `Drive`:
   `Existing_attribute`, `Invalid_operation`, `No_attribute`, and
   `Permission_denied` where needed by the update wrapper.
4. Wire `DriveXattrs.Make` into `src/drive.ml` with a production ports module
   and `drive_xattr_runtime`.
5. Replace the old xattr bodies in `src/drive.ml` with thin wrappers.
6. Add the new source files to `src/dune` if the stanza does not pick them up
   automatically.
7. Add `test/testDriveXattrs.ml`.
8. Register the suite in `test/testSuite.ml`.
9. Run `dune build @install` and `dune runtest`.
10. Run `make format` or `tools/format_ocaml` if formatting changes are needed.

## Unit Test Plan

Use fake ports, following the style in `test/testDriveViews.ml`,
`test/testDriveFileMutations.ml`, and `test/testDriveMutations.ml`.

Cover read behavior:

- `get_xattr` returns an existing value.
- `get_xattr` raises `No_attribute` for a missing name.
- `list_xattr` returns only xattr names.
- path normalization receives the runtime config and uses the normalized path
  for resource lookup.

Cover set behavior:

- `set_xattr` with `Fuse.AUTO` sends one app property for create-or-replace.
- `set_xattr` with `Fuse.CREATE` raises `Existing_attribute` when present.
- `set_xattr` with `Fuse.CREATE` succeeds when absent.
- `set_xattr` with `Fuse.REPLACE` raises `No_attribute` when absent.
- `set_xattr` with `Fuse.REPLACE` succeeds when present.
- over-limit escaped name/value length raises `Invalid_operation`.
- a value with JSON escaping overhead is counted using `json_length`, not raw
  byte length.

Cover remove behavior:

- `remove_xattr` sends a no-value app property for an existing name.
- `remove_xattr` raises `No_attribute` for a missing name.

Cover update behavior through fakes:

- successful mutations call `update_remote_resource`.
- successful mutations call `remote_update` with the resource remote id.
- custom headers are requested from the resource.
- failed local validation does not call `remote_update`.

## Acceptance Criteria

- `src/drive.ml` no longer contains xattr parsing, xflag validation, length
  checks, or xattr app-property patch construction.
- The public `Drive` xattr entrypoints keep the same signatures and observable
  behavior.
- Xattr branch logic can be tested without `Context.get_ctx`, real cache files,
  or Google Drive requests.
- `dune runtest` passes.

## Notes

`docs/agent-docs/drive-xattr.md` documents the current behavior and should be
updated only if the extraction changes terminology or control flow. The
intended refactor is structural, so user-facing behavior should not change.
