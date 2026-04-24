# `update_remote_resource` Wrapper Pattern

## Purpose

The repository has two closely related `update_remote_resource` wrappers:

- `Drive.update_remote_resource` in `src/drive.ml` for metadata-only mutations
- `DriveMutations.Make.update_remote_resource` in `src/driveMutations.ml` for
  the rename/delete mutation paths

`DriveMetadataMutations` and `DriveXattrs` also receive
`update_remote_resource` ports. In production, the metadata-mutation port
delegates to `Drive.update_remote_resource`, and the xattr port delegates to
`DriveMutations.Make.update_remote_resource`.

This document describes the shared wrapper pattern these paths use.

It is used when a mutation path needs to:

- resolve the current resource for a visible path
- perform one remote mutation against that resource
- reconcile the local cache afterwards

Current callers are:

- `DriveMetadataMutations.utime`
- `DriveMetadataMutations.chmod`
- `DriveMetadataMutations.chown`
- `DriveXattrs.set_xattr`
- `DriveXattrs.remove_xattr`
- `DriveMutations.trash_resource`
- `DriveMutations.delete_resource`
- `DriveMutations.rename`

It is not used for content uploads. Those go through the upload path centered on
`queue_upload` and `upload_resource_with_retry`.

See `docs/agent-docs/drive-upload-path.md` for content uploads and
`docs/agent-docs/drive-rename.md` for the rename-specific cache logic built on
top of the mutation-core wrapper.

See `docs/agent-docs/drive-delete-remote-resource.md` for the higher-level
policy wrapper that decides when deletion requests reach `trash_resource`
versus `delete_resource`.

See `docs/agent-docs/drive-chmod-chown-utime.md` for the concrete metadata
mutation paths.

See `docs/agent-docs/drive-xattr.md` for the concrete xattr read/mutate paths.

## Signatures

```ocaml
val Drive.update_remote_resource :
  string ->
  ?update_file_in_cache:(string -> unit) ->
  ?save_to_db:
    (CacheData.t -> CacheData.Resource.t -> GapiDriveV3Model.File.t -> unit) ->
  ?purge_cache:(CacheData.t -> CacheData.Resource.t -> unit) ->
  (CacheData.Resource.t -> GapiDriveV3Model.File.t option GapiMonad.SessionM.m) ->
  unit GapiMonad.SessionM.m

val DriveMutations.update_remote_resource :
  DriveMutations.runtime ->
  string ->
  ?save_to_db:
    (CacheData.t -> CacheData.Resource.t -> GapiDriveV3Model.File.t -> unit) ->
  ?purge_cache:(CacheData.t -> CacheData.Resource.t -> unit) ->
  (CacheData.Resource.t -> GapiDriveV3Model.File.t option GapiMonad.SessionM.m) ->
  unit GapiMonad.SessionM.m

val DriveMetadataMutationPorts.update_remote_resource :
  DriveMetadataMutations.runtime ->
  string ->
  ?update_file_in_cache:(string -> unit) ->
  (CacheData.Resource.t -> GapiDriveV3Model.File.t option GapiMonad.SessionM.m) ->
  unit GapiMonad.SessionM.m

val DriveXattrPorts.update_remote_resource :
  DriveXattrs.runtime ->
  string ->
  (CacheData.Resource.t -> GapiDriveV3Model.File.t option GapiMonad.SessionM.m) ->
  unit GapiMonad.SessionM.m
```

The common contract is:

- take a visible `path`
- optionally override cache-reconciliation hooks
- run one `do_remote_update` callback that performs the remote mutation

The `Drive`-side wrapper and metadata-mutation port have one extra
`update_file_in_cache` hook because `DriveMetadataMutations.utime` may need to
touch an existing local cache file after a successful remote metadata patch.
The mutation-core wrapper and xattr port do not expose that hook.

## High-Level Flow

The wrapper pattern does four things:

1. reject the operation if the filesystem is read-only
2. normalize the visible path with `get_path_in_cache`
3. resolve the current resource with `get_resource`
4. run the caller-supplied remote mutation and reconcile local cache state

In outline, both implementations look like:

```ocaml
if is_filesystem_read_only () then raise Permission_denied;
get_resource path_in_cache trashed >>= fun resource ->
do_remote_update resource >>= fun file_option ->
match file_option with
| None -> purge_cache cache resource
| Some file ->
    maybe_update_local_file resource;
    save_to_db cache resource file
```

The `maybe_update_local_file` step exists only in the `Drive`-side wrapper.

Because the wrappers always go through `get_resource`, they inherit the normal
resource lookup rules:

- metadata may be refreshed first through `get_metadata`
- virtual namespaces like trash are already mapped into cache coordinates
- stale cache rows may be refreshed before the mutation runs

See `docs/agent-docs/drive-get-resource.md` for the resolution semantics these
wrappers depend on.

## Read-Only Enforcement

The read-only guard sits at the wrapper boundary:

```ocaml
if is_filesystem_read_only () then raise Permission_denied else update_file
```

That means all current callers inherit the same policy automatically. Callers
do not need to re-check filesystem mutability themselves before constructing
the remote request.

## The `do_remote_update` Contract

`do_remote_update` receives the resolved `CacheData.Resource.t` and is
responsible for the actual Drive side effect.

Its return value controls which post-processing branch runs:

- `Some file`: the remote resource still exists, and the returned Drive file
  metadata should drive local cache reconciliation
- `None`: the remote resource is considered gone, so the wrapper calls
  `purge_cache`

This is the central protocol of the helper.

### `Some file`

Most metadata-patch operations return `Some patched_file` after a
`FilesResource.update` call.

That includes:

- `DriveMetadataMutations.utime`
- `DriveMetadataMutations.chmod`
- `DriveMetadataMutations.chown`
- `DriveXattrs.set_xattr`
- `DriveXattrs.remove_xattr`
- `DriveMutations.trash_resource`
- `DriveMutations.rename`

### `None`

`DriveMutations.delete_resource` returns `None` after `FilesResource.delete`.

That tells the wrapper there is no replacement metadata to save, and the local
cache should instead be cleaned up through `purge_cache`.

## Post-Update Hooks

The metadata-side wrapper in `Drive` exposes three extension points. The
mutation-core wrapper exposes the same `save_to_db` and `purge_cache` hooks,
but not `update_file_in_cache`.

### `save_to_db`

This is the main post-success hook.

The default implementation is:

```ocaml
let default_save_resource_to_db cache resource file =
  let updated_resource = update_resource_from_file resource file in
  update_cached_resource cache updated_resource
```

So by default, a successful remote mutation:

1. rebuilds the cache row from the returned Drive file
2. writes that updated row back into the cache

This default is enough for simple in-place metadata changes such as
`DriveMetadataMutations.chmod`, `DriveMetadataMutations.chown`,
`DriveXattrs.set_xattr`, and `DriveXattrs.remove_xattr`.

More complex callers override it:

- `DriveMutations.trash_resource` marks the row trashed, invalidates the
  trash-bin cache, and trashes cached descendants for folders
- `DriveMutations.rename` rewrites path and parent fields, handles replacement
  cases, clears destination `NotFound` rows, and removes stale folder subtree
  entries

The key design point is that the wrapper owns the control flow, while the
caller owns any operation-specific cache invariants.

### `purge_cache`

This hook runs only when `do_remote_update` returns `None`.

Its default value is a no-op:

```ocaml
fun cache resource -> ()
```

So returning `None` without overriding `purge_cache` will leave local cache
state untouched.

`DriveMutations.delete_resource` supplies the meaningful purge behavior:

- remove the resource row itself
- if deleting a folder, also remove cached descendants under the old path

That makes `None` effectively the "resource disappeared" branch.

### `update_file_in_cache`

This optional hook exists only on `Drive.update_remote_resource` and the
`DriveMetadataMutationPorts.update_remote_resource` port.

It lets a caller mutate the local cached content file after a successful remote
metadata update.

The wrapper only invokes it when both conditions hold:

- `resource.state = Synchronized`
- the content file already exists on disk

So the hook does not create a cache file, and it does not run for dirty or
transitional resource states.

`DriveMetadataMutations.utime` uses it to mirror the new timestamps onto the
local cache file:

```ocaml
~update_file_in_cache:(fun content_path ->
  Unix.utimes content_path atime mtime)
```

That keeps on-disk cache metadata aligned with the successful remote patch when
the file already exists locally.

## Caller Categories

Although the wrapper pattern is generic, its current callers fall into a small
number of patterns.

### In-Place Metadata Patches

These callers all:

- patch remote file metadata with `FilesResource.update`
- return `Some patched_file`
- rely on the default `save_to_db`

They are:

- `DriveMetadataMutations.chmod`
- `DriveMetadataMutations.chown`
- `DriveXattrs.set_xattr`
- `DriveXattrs.remove_xattr`

`DriveMetadataMutations.utime` belongs to the same category, but also uses
`update_file_in_cache` to touch the local cache file.

### Soft Delete

`DriveMutations.trash_resource` also returns `Some trashed_file`, but it cannot
use the default save path because local cache state needs extra bookkeeping:

- mark the row trashed locally
- invalidate the special trash-bin listing
- trash cached descendants when the target is a folder

This is a good example of a caller that still has a normal remote return value
but needs custom cache reconciliation.

### Hard Delete

`DriveMutations.delete_resource` is the pure `None` case:

- remote side-effect is `FilesResource.delete`
- no replacement file metadata exists afterwards
- local cleanup happens through `purge_cache`

### Move/Rename

`DriveMutations.rename` is the most complex caller.

It uses the same shared wrapper, but almost all of its correctness depends on
the custom `save_to_db` hook rather than the wrapper itself. That hook handles:

- move-versus-rename composition
- `mv_keep_target` replacement cases
- path and parent rewrites
- folder subtree cache cleanup

See `docs/agent-docs/drive-rename.md` for the rename-specific details.

## Maintenance Notes

There are a few non-obvious rules worth preserving if this helper pattern
changes.

### Path Resolution Happens Before Mutation

The wrapper resolves the current resource by path before the remote mutation
runs. So a caller that wants to operate on some other object identity cannot use
this helper directly without first making that object discoverable at the input
path.

### Returning `None` Is Not Enough By Itself

`None` only selects the purge branch. It does not define the purge behavior.

If a new caller returns `None` but forgets to override `purge_cache`, local
metadata will remain behind.

### Local File Updates Are Deliberately Narrow

`update_file_in_cache` is intentionally conservative:

- only synchronized resources qualify
- only existing cache files are touched

If a future operation needs broader local file repair, it will need custom logic
outside this hook or a change to the wrapper contract.

## Source Pointers

- `src/drive.ml`: `update_remote_resource`
- `src/driveMetadataMutations.ml`: metadata callers using the
  `update_remote_resource` port
- `src/driveMutations.ml`: `update_remote_resource`
- `src/driveXattrs.ml`: xattr callers using the `update_remote_resource` port
- `docs/agent-docs/drive-rename.md`
- `docs/agent-docs/drive-delete-remote-resource.md`
