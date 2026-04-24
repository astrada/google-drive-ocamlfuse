# Extract Drive Metadata Mutations

## Goal

Move the existing `chmod`, `chown`, and `utime` metadata-mutation behavior out
of `src/drive.ml` into a small testable module that follows the current
functorized runtime pattern used by `DriveViews`, `DriveFileMutations`,
`DriveUploadDispatch`, `DriveMutations`, and `DriveXattrs`.

The extraction target is:

- `Drive.utime`
- `Drive.chmod`
- `Drive.chown`

The production behavior should stay unchanged. `src/drive.ml` should retain
only thin wrappers that build the runtime, call the extracted operations, and
run them through `do_request`.

## Current Problem

The three metadata mutations still build their Drive patches directly in
`src/drive.ml`.

The behavior worth testing is local and deterministic:

- `utime` sends only `mtime` to Drive as `File.modifiedTime`.
- `utime` mirrors both `atime` and `mtime` to an existing synchronized cache
  file through `update_file_in_cache`.
- `chmod` stores the requested mode through
  `CacheData.Resource.mode_to_app_property`.
- `chown` converts POSIX "leave unchanged" sentinels to omitted app properties.
- `chown` includes uid and gid app properties only when each side is provided.
- all three mutations use resource-key headers and patch the resolved remote
  id.

Today, those rules are mixed with production concerns:

- `Context.get_ctx`
- `do_request`
- `update_remote_resource`
- `FilesResource.update`
- `with_retry_default`
- `Unix.utimes`

That makes the patch-building and sentinel behavior harder to unit test
without crossing the global context and Google Drive API boundary.

## Proposed Shape

Add:

- `src/driveMetadataMutations.ml`
- `src/driveMetadataMutations.mli`
- `test/testDriveMetadataMutations.ml`

Expose a functor:

```ocaml
type runtime = {
  cache : CacheData.t;
  config : Config.t;
}

module type PORTS = sig
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
    ?update_file_in_cache:(string -> unit) ->
    (CacheData.Resource.t ->
    GapiDriveV3Model.File.t option GapiMonad.SessionM.m) ->
    unit GapiMonad.SessionM.m

  val update_file_times : string -> float -> float -> unit
end
```

The module should expose:

```ocaml
val utime : runtime -> string -> float -> float -> unit GapiMonad.SessionM.m
val chmod : runtime -> string -> int -> unit GapiMonad.SessionM.m
val chown : runtime -> string -> int -> int -> unit GapiMonad.SessionM.m
```

The metadata module should own only the metadata-specific behavior. Generic
path normalization, read-only checks, resource lookup, and cache reconciliation
should continue to flow through the `update_remote_resource` port.

## Production Wiring

In `src/drive.ml`, add a ports module near the existing mutation/xattr wiring:

```ocaml
module DriveMetadataMutationPorts = struct
  let build_resource_keys_header_from_resource =
    build_resource_keys_header_from_resource

  let remote_update ~custom_headers ~fileId file_patch =
    with_retry_default
      (FilesResource.update ~enforceSingleParent:true ~supportsAllDrives:true
         ~std_params:file_std_params ~custom_headers ~fileId file_patch)

  let update_remote_resource runtime path ?update_file_in_cache do_remote_update =
    update_remote_resource path ?update_file_in_cache do_remote_update

  let update_file_times = Unix.utimes
end
```

Then instantiate:

```ocaml
module MetadataMutationOps =
  DriveMetadataMutations.Make (DriveMetadataMutationPorts)
```

Add:

```ocaml
let drive_metadata_mutation_runtime () =
  let context = Context.get_ctx () in
  {
    DriveMetadataMutations.cache = context.Context.cache;
    config = context |. Context.config_lens;
  }
```

Replace the existing `utime`, `chmod`, and `chown` bodies with `do_request`
calls into `MetadataMutationOps`.

Keep the existing `Drive.update_remote_resource` helper in `drive.ml` as the
production implementation of the update-wrapper port. Do not duplicate that
generic wrapper inside `DriveMetadataMutations`.

## Implementation Steps

1. Create `driveMetadataMutations.mli` with `runtime`, `PORTS`, and the three
   public operations.
2. Create `driveMetadataMutations.ml` by moving the metadata-specific callback
   construction out of `src/drive.ml`.
3. Keep a small helper inside the new module for converting chown ids:
   `-1` and unsigned 32-bit all-ones (`4294967295`) mean "omit this side".
4. Use `P.remote_update` for the common `FilesResource.update` operation.
5. Use `P.update_remote_resource` for shared read-only enforcement, path
   normalization, resource lookup, and cache refresh.
6. For `utime`, pass
   `~update_file_in_cache:(fun content_path ->
     P.update_file_times content_path atime mtime)`.
7. Wire the new module into `src/drive.ml`.
8. Replace the old `utime`, `chmod`, and `chown` bodies in `src/drive.ml` with
   thin wrappers.
9. Add `test/testDriveMetadataMutations.ml`.
10. Register the suite in `test/testSuite.ml`.
11. Run `ocamlformat` on touched OCaml files.
12. Run `dune build @install` and `dune runtest`.

## Unit Test Plan

Use fake ports, following the style in `test/testDriveXattrs.ml` and
`test/testDriveFileMutations.ml`.

Cover common behavior:

- each operation calls the update-wrapper port with the visible path
- each successful operation calls the remote-update port with the resource
  remote id
- each successful operation asks for resource-key headers from the resource
- each operation returns `Some patched_file` to the update wrapper

Cover `utime`:

- remote patch contains `modifiedTime = Netdate.create mtime`
- remote patch does not encode `atime`
- the update wrapper receives an `update_file_in_cache` hook
- invoking that hook calls the `update_file_times` port with both `atime` and
  `mtime`

Cover `chmod`:

- patch app properties equal
  `[ CacheData.Resource.mode_to_app_property mode ]`
- modes are passed through without masking

Cover `chown`:

- normal uid/gid produce both app properties
- `uid = -1` omits uid and keeps gid
- `gid = -1` omits gid and keeps uid
- unsigned 32-bit all-ones (`4294967295`) omits that side on 64-bit OCaml
- both sides omitted results in an empty app-property patch
- property order matches the current implementation: uid property before gid
  when both are present

## Acceptance Criteria

- `src/drive.ml` no longer contains the metadata patch construction for
  `utime`, `chmod`, or `chown`.
- `src/drive.ml` no longer contains the chown id-sentinel conversion helper.
- The public `Drive.utime`, `Drive.chmod`, and `Drive.chown` signatures and
  observable behavior remain unchanged.
- The `utime` local-cache timestamp hook keeps its current narrow behavior
  through `update_remote_resource`.
- The metadata mutation branch logic can be tested without `Context.get_ctx`,
  real cache files, or Google Drive requests.
- `dune build @install` and `dune runtest` pass.

## Agent Docs Follow-Up

After implementation, update the agent docs to describe the current state
directly:

- `docs/agent-docs/drive-chmod-chown-utime.md`
- `docs/agent-docs/architecture.md`
- `docs/agent-docs/repo-map.md`
- `docs/agent-docs/drive-update-remote-resource.md`
- `docs/agent-docs/README.md`

Avoid before/after language. The docs should describe `DriveMetadataMutations`
as the current implementation boundary.
