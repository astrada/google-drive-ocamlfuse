# Extract Drive Update Remote Resource

## Goal

Move the `Drive.update_remote_resource` wrapper out of `src/drive.ml` into a
small testable module that follows the current functorized runtime pattern used
by `DriveOpens`, `DriveDownloads`, `DriveReads`, `DriveFileMutations`,
`DriveMetadataMutations`, `DriveUploadDispatch`, `DriveMutations`, and
`DriveXattrs`.

The extraction target is:

- `Drive.update_remote_resource`
- the default save-to-db behavior used by that wrapper

The production behavior should stay unchanged. `src/drive.ml` should retain a
thin compatibility helper with the same optional arguments, but the wrapper
policy should live in the new module.

## Current Problem

`Drive.update_remote_resource` is still embedded directly in `src/drive.ml`.
It is small, but it is an important shared mutation boundary for metadata
updates.

The current helper owns several behaviors that are worth testing directly:

- visible path normalization
- trash-path handling
- filesystem read-only rejection
- resource lookup before remote mutation
- the `Some file` success branch
- the `None` purge branch
- default cache reconciliation through `update_resource_from_file`
- optional `save_to_db` override
- optional `purge_cache` override
- optional `update_file_in_cache` hook
- the narrow conditions for touching a local cache file

Today those behaviors can only be exercised through production `Context`, cache
access, and Drive API adapters, or indirectly through higher-level mutation
modules.

## Scope

This plan targets the `Drive`-side wrapper used by metadata mutations:

- `Drive.update_remote_resource`
- `DriveMetadataMutationPorts.update_remote_resource`

It should not change the existing `DriveMutations.Make.update_remote_resource`
implementation in this pass. That wrapper is already inside the extracted
mutation core and has different responsibilities for rename/delete paths.

After this extraction, both wrappers may still exist:

- `DriveRemoteUpdates` for the metadata-side wrapper with
  `update_file_in_cache`
- `DriveMutations.Make.update_remote_resource` for create/delete/rename
  mutation-core callers

## Proposed Shape

Add:

- `src/driveRemoteUpdates.ml`
- `src/driveRemoteUpdates.mli`
- `test/testDriveRemoteUpdates.ml`

Expose:

```ocaml
exception Permission_denied

type runtime = {
  cache : CacheData.t;
  config : Config.t;
}

module type PORTS = sig
  val get_path_in_cache : string -> Config.t -> string * bool
  val get_resource : string -> bool -> CacheData.Resource.t GapiMonad.SessionM.m
  val get_content_path : CacheData.t -> CacheData.Resource.t -> string
  val file_exists : string -> bool

  val update_resource_from_file :
    CacheData.Resource.t ->
    GapiDriveV3Model.File.t ->
    CacheData.Resource.t

  val update_cached_resource : CacheData.t -> CacheData.Resource.t -> unit
end

module Make (P : PORTS) : sig
  val default_save_resource_to_db :
    CacheData.t ->
    CacheData.Resource.t ->
    GapiDriveV3Model.File.t ->
    unit

  val update_remote_resource :
    runtime ->
    string ->
    ?update_file_in_cache:(string -> unit) ->
    ?save_to_db:
      (CacheData.t -> CacheData.Resource.t -> GapiDriveV3Model.File.t -> unit) ->
    ?purge_cache:(CacheData.t -> CacheData.Resource.t -> unit) ->
    (CacheData.Resource.t ->
    GapiDriveV3Model.File.t option GapiMonad.SessionM.m) ->
    unit GapiMonad.SessionM.m
end
```

The new module should alias the existing exception identity:

```ocaml
exception Permission_denied = DriveMutations.Permission_denied
```

## Production Wiring

In `src/drive.ml`, add a ports module near the current update-operation
section:

```ocaml
module DriveRemoteUpdatePorts = struct
  let get_path_in_cache = get_path_in_cache
  let get_resource = get_resource
  let get_content_path = Cache.get_content_path
  let file_exists = Sys.file_exists

  let update_resource_from_file resource file =
    update_resource_from_file resource file

  let update_cached_resource = update_cached_resource
end

module RemoteUpdateOps = DriveRemoteUpdates.Make (DriveRemoteUpdatePorts)

let drive_remote_update_runtime () =
  let context = Context.get_ctx () in
  {
    DriveRemoteUpdates.cache = context.Context.cache;
    config = context |. Context.config_lens;
  }
```

Replace the current helper body with a compatibility wrapper:

```ocaml
let update_remote_resource path ?update_file_in_cache ?save_to_db ?purge_cache
    do_remote_update =
  RemoteUpdateOps.update_remote_resource
    (drive_remote_update_runtime ())
    path ?update_file_in_cache ?save_to_db ?purge_cache do_remote_update
```

`DriveMetadataMutationPorts.update_remote_resource` should keep delegating to
this `Drive.update_remote_resource` helper, so the public shape seen by
`DriveMetadataMutations` remains unchanged.

## Extracted Behavior

The extracted implementation should preserve the current flow.

### Path Normalization

Normalize the visible path before the read-only guard:

```ocaml
let path_in_cache, trashed = P.get_path_in_cache path runtime.config
```

This preserves the current ordering in `src/drive.ml`.

### Read-Only Guard

Reject with `Permission_denied` when:

```ocaml
runtime.config.Config.read_only = true
```

This should happen after path normalization but before `P.get_resource` or the
remote mutation callback.

### Resource Lookup And Remote Mutation

When not read-only:

1. resolve `P.get_resource path_in_cache trashed`
2. pass the resolved resource to `do_remote_update`
3. branch on the returned `GapiDriveV3Model.File.t option`

Exceptions from `P.get_resource` or `do_remote_update` should propagate without
running `save_to_db`, `purge_cache`, or `update_file_in_cache`.

### `Some file` Branch

When `do_remote_update resource` returns `Some file`:

1. optionally run `update_file_in_cache`
2. run `save_to_db runtime.cache resource file`
3. return `SessionM.return ()`

The default `save_to_db` should:

1. call `P.update_resource_from_file resource file`
2. call `P.update_cached_resource runtime.cache updated_resource`

The optional `save_to_db` override should completely replace that default.

### Local File Hook

`update_file_in_cache` should run only when all of these hold:

- the hook is provided
- `resource.state = CacheData.Resource.State.Synchronized`
- `P.file_exists content_path = true`

The content path should come from:

```ocaml
P.get_content_path runtime.cache resource
```

The hook should not run for `ToDownload`, `Downloading`, `ToUpload`,
`Uploading`, or `NotFound` resources, and should not create a file when the
content path is missing.

### `None` Branch

When `do_remote_update resource` returns `None`:

- run `purge_cache runtime.cache resource`
- do not run `save_to_db`
- do not run `update_file_in_cache`

The default `purge_cache` should remain a no-op.

## Unit Test Plan

Use fake ports with a trace log, following the style in
`test/testDriveDownloads.ml` and `test/testDriveOpens.ml`.

Cover path and read-only behavior:

- visible paths are normalized with runtime config
- trash paths pass normalized path and `trashed = true`
- read-only config raises `Permission_denied`
- read-only rejection happens after path normalization but before resource
  lookup

Cover resource and remote mutation behavior:

- `do_remote_update` receives the resolved resource
- exceptions from `get_resource` propagate
- exceptions from `do_remote_update` propagate
- failed lookup or remote mutation does not save, purge, or update local file

Cover `Some file` default behavior:

- default save calls `update_resource_from_file`
- default save calls `update_cached_resource` with the updated resource
- the remote returned file id is visible to the update port

Cover `save_to_db` override:

- custom `save_to_db` runs instead of the default save
- default update ports are not called when the override is supplied

Cover `update_file_in_cache`:

- synchronized resource with existing local content runs the hook before save
- synchronized resource with missing local content does not run the hook
- dirty or transitional states do not run the hook
- no hook means no content-path lookup is needed

Cover `None` branch:

- custom `purge_cache` runs for `None`
- default `purge_cache` is a no-op
- `save_to_db` does not run for `None`
- `update_file_in_cache` does not run for `None`

## Acceptance Criteria

- `src/drive.ml` no longer contains the `update_remote_resource` branch logic.
- `Drive.update_remote_resource` retains its current optional arguments and
  observable behavior for metadata callers.
- `DriveMetadataMutations.utime`, `chmod`, and `chown` continue to use the same
  production port shape.
- The read-only guard, path normalization, `Some file` branch, `None` branch,
  default save behavior, custom hooks, and local file update conditions are unit
  tested without real `Context`, Drive API requests, cache files, or filesystem
  checks.
- `dune build @install` and `dune runtest` pass.

## Agent Docs Follow-Up

After implementation, update the agent docs to describe the current state
directly:

- `docs/agent-docs/drive-update-remote-resource.md`
- `docs/agent-docs/architecture.md`
- `docs/agent-docs/repo-map.md`
- `docs/agent-docs/README.md`

Avoid before/after language. The docs should describe `DriveRemoteUpdates` as
the current implementation boundary for the `Drive`-side remote update wrapper,
while still documenting that `DriveMutations.Make.update_remote_resource`
remains the mutation-core wrapper for rename/delete paths.
