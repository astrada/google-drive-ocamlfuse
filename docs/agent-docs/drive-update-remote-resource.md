# `update_remote_resource` Wrapper Pattern

## Purpose

The repository has two `update_remote_resource` wrappers with similar control
flow and different call-site responsibilities:

- `DriveRemoteUpdates.Make.update_remote_resource` for the `Drive`-side
  metadata-update wrapper
- `DriveMutations.Make.update_remote_resource` for the mutation core used by
  delete/trash, rename, and xattr mutation paths

`Drive.update_remote_resource` is the production compatibility helper in
`src/drive.ml`. It builds a `DriveRemoteUpdates.runtime` from `Context` and
delegates to `RemoteUpdateOps`.

Use this document when maintaining code that needs to:

- resolve the current resource for a visible path
- perform one remote mutation against that resource
- reconcile local cache state afterwards

Current caller groups are:

- `DriveMetadataMutations.utime`
- `DriveMetadataMutations.chmod`
- `DriveMetadataMutations.chown`
- `DriveXattrs.set_xattr`
- `DriveXattrs.remove_xattr`
- `DriveMutations.trash_resource`
- `DriveMutations.delete_resource`
- `DriveMutations.rename`

Content uploads do not use this wrapper pattern. They go through the upload
path centered on `queue_upload` and `upload_resource_with_retry`; see
`docs/agent-docs/drive-upload-path.md`.

See `docs/agent-docs/drive-chmod-chown-utime.md` for concrete metadata
mutations, `docs/agent-docs/drive-xattr.md` for xattr mutations,
`docs/agent-docs/drive-rename.md` for rename-specific cache surgery, and
`docs/agent-docs/drive-delete-remote-resource.md` for delete-vs-trash policy
selection.

## Implementation Boundaries

### `DriveRemoteUpdates`

`src/driveRemoteUpdates.ml` owns the metadata-side wrapper policy:

- visible-path normalization through the injected `get_path_in_cache` port
- read-only rejection from `runtime.config.Config.read_only`
- resource resolution through the injected `get_resource` port
- `Some file` cache-save behavior
- `None` purge behavior
- the optional `update_file_in_cache` hook used by `utime`

Production wiring lives in `DriveRemoteUpdatePorts` in `src/drive.ml`.

`DriveMetadataMutationPorts.update_remote_resource` delegates through
`Drive.update_remote_resource`, so `utime`, `chmod`, and `chown` use this
wrapper.

### `DriveMutations`

`src/driveMutations.ml` owns the mutation-core wrapper used inside create,
delete/trash, and rename logic.

Its `update_remote_resource` has the same `Some file` / `None` protocol and
the same default save/purge shape, but it does not expose
`update_file_in_cache`. It also receives read-only policy through the
`is_filesystem_read_only` port.

`DriveXattrPorts.update_remote_resource` adapts xattr mutations onto this
mutation-core wrapper.

## Signatures

```ocaml
val DriveRemoteUpdates.Make.update_remote_resource :
  DriveRemoteUpdates.runtime ->
  string ->
  ?update_file_in_cache:(string -> unit) ->
  ?save_to_db:
    (CacheData.t -> CacheData.Resource.t -> GapiDriveV3Model.File.t -> unit) ->
  ?purge_cache:(CacheData.t -> CacheData.Resource.t -> unit) ->
  (CacheData.Resource.t -> GapiDriveV3Model.File.t option GapiMonad.SessionM.m) ->
  unit GapiMonad.SessionM.m

val Drive.update_remote_resource :
  string ->
  ?update_file_in_cache:(string -> unit) ->
  ?save_to_db:
    (CacheData.t -> CacheData.Resource.t -> GapiDriveV3Model.File.t -> unit) ->
  ?purge_cache:(CacheData.t -> CacheData.Resource.t -> unit) ->
  (CacheData.Resource.t -> GapiDriveV3Model.File.t option GapiMonad.SessionM.m) ->
  unit GapiMonad.SessionM.m

val DriveMutations.Make.update_remote_resource :
  DriveMutations.runtime ->
  string ->
  ?save_to_db:
    (CacheData.t -> CacheData.Resource.t -> GapiDriveV3Model.File.t -> unit) ->
  ?purge_cache:(CacheData.t -> CacheData.Resource.t -> unit) ->
  (CacheData.Resource.t -> GapiDriveV3Model.File.t option GapiMonad.SessionM.m) ->
  unit GapiMonad.SessionM.m
```

The common contract is:

- take a visible `path`
- normalize it into cache coordinates
- resolve the current `CacheData.Resource.t`
- run one caller-supplied remote mutation
- reconcile cache state based on whether that mutation returns `Some file` or
  `None`

## High-Level Flow

The shared shape is:

```ocaml
let path_in_cache, trashed = get_path_in_cache path config in
if read_only then raise Permission_denied
else
  get_resource path_in_cache trashed >>= fun resource ->
  do_remote_update resource >>= fun file_option ->
  (match file_option with
  | None -> purge_cache cache resource
  | Some file ->
      maybe_update_local_file resource;
      save_to_db cache resource file);
  SessionM.return ()
```

The `maybe_update_local_file` step exists only in `DriveRemoteUpdates`.

Because both wrappers resolve by path before the remote mutation runs, they
inherit normal resource lookup semantics: trash-view mapping, metadata refresh,
and stale-cache behavior come from `get_path_in_cache` and `get_resource`.
See `docs/agent-docs/drive-get-resource.md`.

## Read-Only Enforcement

The read-only guard sits at the wrapper boundary.

For `DriveRemoteUpdates`, the guard uses:

```ocaml
runtime.config.Config.read_only
```

For `DriveMutations`, the guard uses the injected:

```ocaml
P.is_filesystem_read_only ()
```

Path normalization happens before this guard in both wrappers. Resource lookup
and the remote mutation callback happen only when the filesystem is writable.

## The `do_remote_update` Contract

`do_remote_update` receives the resolved `CacheData.Resource.t` and performs the
remote side effect. Its return value controls local reconciliation:

- `Some file`: the remote resource still exists; refresh or otherwise reconcile
  the cache row from the returned Drive metadata
- `None`: the remote resource is considered gone; call `purge_cache`

Exceptions from resource lookup or `do_remote_update` propagate. The wrappers
do not run save, purge, or local-file hooks after those failures.

## `Some file`

Most metadata-patch operations return `Some patched_file` after
`FilesResource.update`.

That includes:

- `DriveMetadataMutations.utime`
- `DriveMetadataMutations.chmod`
- `DriveMetadataMutations.chown`
- `DriveXattrs.set_xattr`
- `DriveXattrs.remove_xattr`
- `DriveMutations.trash_resource`
- `DriveMutations.rename`

The default save behavior is:

```ocaml
let default_save_resource_to_db cache resource file =
  let updated_resource = P.update_resource_from_file resource file in
  P.update_cached_resource cache updated_resource
```

Callers can replace this with `save_to_db` when the returned Drive metadata is
not enough to preserve local invariants.

Examples:

- `DriveMutations.trash_resource` marks the row trashed, invalidates the trash
  root cache, and trashes cached descendants for folders
- `DriveMutations.rename` rewrites path and parent fields, handles target
  replacement cases, clears destination `NotFound` rows, and removes stale
  folder subtree entries

## `None`

`DriveMutations.delete_resource` returns `None` after `FilesResource.delete`.

That selects the purge branch:

```ocaml
purge_cache cache resource
```

The default purge is a no-op:

```ocaml
fun _cache _resource -> ()
```

So any caller that returns `None` and needs local cleanup must supply
`purge_cache`.

## Local File Hook

`DriveRemoteUpdates` exposes:

```ocaml
?update_file_in_cache:(string -> unit)
```

`DriveMetadataMutations.utime` uses this hook to mirror timestamps onto an
existing local cache file after a successful remote metadata patch:

```ocaml
~update_file_in_cache:(fun content_path ->
  Unix.utimes content_path atime mtime)
```

The hook runs only when all of these are true:

- the hook is provided
- `resource.state = CacheData.Resource.State.Synchronized`
- `P.file_exists content_path = true`

The content path comes from:

```ocaml
P.get_content_path runtime.cache resource
```

The hook does not create cache files and does not run for dirty or transitional
states such as `ToDownload`, `Downloading`, `ToUpload`, `Uploading`, or
`NotFound`.

## Caller Categories

### Metadata Patches

`DriveMetadataMutations.chmod`, `DriveMetadataMutations.chown`, and
`DriveMetadataMutations.utime` use `DriveRemoteUpdates` through
`DriveMetadataMutationPorts.update_remote_resource`.

`utime` additionally passes `update_file_in_cache`; `chmod` and `chown` rely on
the default save path.

### Xattr Patches

`DriveXattrs.set_xattr` and `DriveXattrs.remove_xattr` use the
`DriveXattrPorts.update_remote_resource` port. In production that port delegates
to `MutationOps.update_remote_resource`.

### Soft Delete

`DriveMutations.trash_resource` returns `Some trashed_file`, but supplies custom
cache reconciliation so local rows move into the trash namespace correctly.

### Hard Delete

`DriveMutations.delete_resource` returns `None` and supplies `purge_cache` to
remove the cached row and, for folders, cached descendants.

### Move/Rename

`DriveMutations.rename` uses the mutation-core wrapper with custom
`save_to_db`. The custom save hook handles move-vs-rename composition,
replacement behavior, path rewrites, parent rewrites, and folder subtree cache
cleanup.

## Test Coverage

`test/testDriveRemoteUpdates.ml` covers the `DriveRemoteUpdates` wrapper with
fake ports:

- path normalization and trash-path lookup
- read-only rejection ordering
- resource lookup and remote mutation exception propagation
- default save behavior
- `save_to_db` override behavior
- `update_file_in_cache` conditions and ordering
- custom and default `purge_cache` behavior

`test/testDriveMutations.ml` covers the mutation-core wrapper indirectly
through create/delete/rename behavior.

## Maintenance Notes

### Path Resolution Happens Before Mutation

The wrappers resolve the current resource by path before the remote mutation
runs. A caller that needs to operate on a different object identity needs a
different helper or must first make that object discoverable at the input path.

### Returning `None` Is Only Branch Selection

`None` does not define cleanup by itself. It only selects the purge branch.
Callers that delete or otherwise remove remote resources must supply meaningful
`purge_cache` behavior.

### Local File Updates Are Deliberately Narrow

`update_file_in_cache` is intentionally conservative. It touches only existing
cache files for synchronized resources. Broader local file repair belongs in a
separate operation-specific path.

## Source Pointers

- `src/driveRemoteUpdates.ml`: metadata-side wrapper
- `src/drive.ml`: `DriveRemoteUpdatePorts`, `RemoteUpdateOps`, and
  `Drive.update_remote_resource`
- `src/driveMetadataMutations.ml`: metadata callers using the
  `update_remote_resource` port
- `src/driveMutations.ml`: mutation-core wrapper for delete/trash and rename
- `src/driveXattrs.ml`: xattr callers using the `update_remote_resource` port
- `test/testDriveRemoteUpdates.ml`
- `test/testDriveMutations.ml`
