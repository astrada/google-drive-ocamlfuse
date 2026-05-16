# `Drive.rename`

## Purpose

`Drive.rename` implements both rename and move semantics for the mounted
filesystem.

The public `Drive.rename` function in `src/drive.ml` is a thin adapter.
It builds the mutation runtime from `Context`, then runs the real rename logic
in `DriveMutations.Make.rename` through `do_request`.

In this codebase, that one function has to cover:

- same-directory rename
- move to a different parent
- optional overwrite/trash behavior
- optional preserve-target-history behavior
- folder moves
- document-export name handling
- cache cleanup after path changes

This is one of the densest mutation paths in the repository because it
mixes remote Drive operations with local cache surgery. That is why the core
logic is in `src/driveMutations.ml` instead of directly inside
`src/drive.ml`.

## High-Level Shape

At a high level, `rename path new_path` does this:

1. normalize source and target paths into cache coordinates
2. reject namespace transitions the filesystem does not permit
3. decide whether a target path should be trashed first
4. choose among:
   - rename-only
   - move-only
   - move then rename
   - replace-target-and-upload
5. persist the resulting state back into the cache with custom path handling
6. clean up tombstones and old folder cache state

The core implementation lives in `src/driveMutations.ml`. `src/drive.ml`
provides the public wrapper and production ports.

## Early Namespace Guards

The function immediately rejects two classes of move:

### 1. Crossing Trash Boundaries

```ocaml
if trashed <> target_trashed then raise Permission_denied
```

So a rename cannot move an item:

- from the normal namespace into `/.Trash`
- or from `/.Trash` back into the normal namespace

Trashing and restoring are handled elsewhere, not through `rename`.

### 2. Lost+Found Restrictions

```ocaml
if
  is_lost_and_found_root path trashed config
  || is_lost_and_found new_path target_trashed config
then raise Permission_denied
```

This means:

- the synthetic `/lost+found` root itself cannot be renamed
- targets inside `/lost+found` are not allowed
- the namespace predicates are provided by `DrivePathNamespace` through the
  production `Drive` ports

The source may come from under `/lost+found`; the function has a special
remote-parent handling path for that case later.

## Path Breakdown

Once the rename is accepted, the function derives:

- `old_parent_path`
- `new_parent_path`
- `old_name`
- `new_name`

Those four values drive the rest of the branching:

- same parent or different parent
- same basename or different basename

## Target Deletion Policy: `keep_duplicates`

The local helper `delete_path` defines whether an existing target path is trashed
before the rename or move proceeds.

Its policy is:

- if the target is in the normal namespace and `keep_duplicates = false`, try to
  trash the target path first
- if the target does not exist, ignore `File_not_found`
- otherwise do nothing

So `keep_duplicates` only affects the pre-delete step. It does not change the
rest of the remote rename/move logic directly.

This matches the user-facing config description:

- `keep_duplicates=false`: behave like standard overwrite-by-trashing-target
- `keep_duplicates=true`: keep the target instead of removing it first

## Two Main Remote Operations

Inside the main update block, the function defines two primitive remote actions:

- rename the file name
- move the file between parents

Everything else is composition around those primitives.

### Rename Primitive

`trash_target_and_rename_file resource` does:

1. `delete_target_path`
2. compute a cleaned new name
3. call `FilesResource.update ~fileId:remote_id` with `File.name = clean_new_name`

The name cleaning step is important for Google Docs export semantics:

```ocaml
let clean_new_name = clean_document_extension new_name resource config
```

`clean_document_extension` is provided by the production `Drive` port and uses
`DriveResourceMapping` for document extension rules.

So local visible names and stored Drive names are not always identical for
document-like resources.

### Move Primitive

`trash_target_and_move resource` does:

1. `delete_target_path`
2. resolve the new parent resource
3. compute `addParents = new_parent_id`
4. compute `removeParents = old_parent_id`
5. call `FilesResource.update` with parent modifications

This is the real move operation on Drive.

## Special Case: Moving Out Of `/lost+found`

The move primitive has one non-obvious branch:

```ocaml
if is_lost_and_found_root old_parent_path trashed config then
  SessionM.return ("", custom_headers)
```

That means when the source currently lives under the synthetic `lost+found`
view, the move request uses:

- `removeParents = ""`

instead of resolving an old parent id.

That matches the model for unorganized files: they have no real parent folder to
remove, even though they were visible through the synthetic `/lost+found`
directory.

## `mv_keep_target`: Preserve Target History / Metadata

The most unusual branch is `replace_target`.

This exists to support `mv_keep_target = true`, whose intended behavior is:

- keep the target file's history and metadata
- replace its content with the source file's content

### When It Is Used

The function uses `replace_target` in two places:

- rename-with-same-parent when `mv_keep_target = true`
- move-between-parents when `mv_keep_target = true`

In both cases it wraps the normal rename/move primitive with a fallback:

- if the target path does not exist, fall back to the normal rename/move
- if the target exists, preserve the target remote object

### What `replace_target` Actually Does

If the target exists, `replace_target`:

1. resolves the target resource at `new_path`
2. flushes source memory buffers
3. downloads the source content if needed
4. patches the target remote file metadata with the source MIME type
5. copies the local source cache file onto the target cache file
6. updates cache-size metadata
7. marks the target resource `ToUpload`
8. calls `DriveUploadDispatch.queue_upload target_resource`
9. deletes the source path

So the preserved object is the target, not the source.

That is why the later save-to-cache logic checks whether the returned file id is
different from the original source resource id.

### Important Consequence

`replace_target` is not a single remote "replace this file" API call.

Instead it is:

- a target metadata patch
- a local cache content copy
- a queued content upload for the target afterwards
- deletion/trashing of the source path

That is the main place where `rename` intersects with the upload pipeline.

## Composition Order: Move Then Rename

The helper `move resource` applies operations in this order:

1. if parent changed, do the move path first
2. then attempt the rename path
3. prefer the rename result if both happened

So a cross-directory rename is modeled as:

- parent update first
- basename update second

If neither parent nor basename changes, the remote side-effect portion becomes a
no-op.

## Shared Wrapper: `update_remote_resource`

`rename` runs its remote work through the mutation-core
`update_remote_resource` helper.

See `docs/agent-docs/drive-update-remote-resource.md` for the full wrapper
contract and the other call-site patterns that reuse it.

That shared helper already provides:

- source-path resolution through `get_resource`
- read-only filesystem enforcement
- a custom `save_to_db` hook after the remote operation returns

For rename, the custom `save_to_db` logic is where most of the local cache
correctness work happens.

## Custom Cache Save Logic

The custom `save_to_db` starts with:

```ocaml
let is_file_replaced =
  resource.CacheData.Resource.remote_id <> Some file.File.id
```

This is the key discriminator for `mv_keep_target` behavior.

### Case 1: Normal Rename/Move

If the returned file id matches the source resource remote id:

- the source remote object survived
- the cached source row is updated in place

### Case 2: `mv_keep_target` Replaced The Target

If the returned file id differs from the source resource remote id:

- the returned file is the target remote object
- the code reloads the cached row owning that remote id
- then updates that row from the returned Drive metadata

So the cache update intentionally switches allegiance from source row to target
row when history-preserving replacement happened.

## Path And State Rewriting

After the row is updated from Drive metadata, rename rewrites local cache fields
to the new logical location:

- `path = new_path_in_cache`
- `parent_path = new_parent_path`
- `trashed = Some target_trashed`

It also assigns a new state:

- `ToDownload` for folders
- `ToDownload` for document-like resources
- `Synchronized` for ordinary files

That distinction matters because folders and docs often need their derived local
view rebuilt after a move or rename.

## Recompute Path On Parent Move

There is one more subtle branch:

```ocaml
if
  new_parent_path <> old_parent_path
  && new_name = old_name
  && not is_file_replaced
then
  let path = recompute_path resource_with_new_path new_name in
  ...
```

This handles the case where a resource is moved into a new parent without an
explicit basename change, but the new parent already contains another item that
would collide with the same visible filename.

So after a pure move, the cache path may need to be recomputed locally for
duplicate disambiguation.

This is a local-filesystem naming concern, not a change to the underlying Drive
remote name.

## Tombstone Cleanup

After updating the main resource row, `rename` deletes any cached `NotFound`
tombstone at the destination path:

```ocaml
Cache.Resource.delete_not_found_resource_with_path cache new_path_in_cache
```

That is required because path-resolution misses can be negatively cached by
`get_resource`. A successful rename into the destination path must clear that
negative cache immediately.

## Folder Cache Cleanup

If the renamed resource is a folder, `rename` also purges cached old content
through:

```ocaml
Cache.Resource.delete_all_with_parent_path cache path_in_cache trashed
```

This is cache surgery for the old folder location. After a folder move or rename,
cached children under the old path are no longer trustworthy and should be
rebuilt lazily from the new location.

The exact scope of that purge is delegated to the cache backend helper, but the
intent is clear: invalidate cached folder content rooted at the old location.

## Config Knobs That Matter

`rename` is mainly shaped by these config fields:

- `keep_duplicates`
- `mv_keep_target`
- `read_only`

Their roles are:

- `keep_duplicates`: whether an existing target path is trashed preemptively
- `mv_keep_target`: whether an existing target should keep its own history and
  metadata while receiving source content
- `read_only`: enforced through the shared `update_remote_resource` wrapper

## What This Function Does Not Do

`rename` does not:

- move files into or out of trash
- rename the synthetic `/lost+found` root
- treat all rename cases as metadata-only changes

In particular, the `mv_keep_target` path is not a pure metadata rename. It can
turn into a content-copy plus queued upload workflow.

## Related Docs

- `docs/agent-docs/drive-upload-path.md`
- `docs/agent-docs/drive-create-remote-resource.md`
- `docs/agent-docs/drive-get-resource.md`

## Maintenance Notes

When changing this area, watch these invariants:

- source and target trash namespaces must match
- `lost+found` remains a synthetic namespace with special move semantics
- path namespace predicates must stay aligned with `DrivePathNamespace`
- `keep_duplicates` and `mv_keep_target` are separate policy levers
- `mv_keep_target` can switch the surviving remote id from source to target
- destination `NotFound` tombstones must be cleared after success
- moved/renamed folders need cache cleanup at the old location
- pure parent moves may need local path recomputation for duplicate
  disambiguation
- the production `recompute_path` port uses `DriveResourceMapping` for the
  final unique filename calculation

## Source Pointers

- `src/drive.ml`: `rename`
- `src/driveMutations.ml`: `rename`
- `src/driveMutations.ml`: `update_remote_resource`
- `src/driveUploadDispatch.ml`: `queue_upload`
- `src/drive.ml`: `recompute_path`
- `src/driveResourceMapping.ml`: `clean_document_extension` and unique path
  calculation
- `test/testDriveMutations.ml`
