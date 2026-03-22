# `Drive.read_dir`

## Purpose

`Drive.read_dir` implements directory listing for the mounted filesystem.

Its open-time sibling is `Drive.opendir`, which only validates that the path
resolves and does not itself list children; see
`docs/agent-docs/drive-opendir.md`.

At the FUSE boundary, `bin/gdfuseFuse.ml` calls it from the `readdir`
callback, then prepends `"."` and `".."` to the returned list. So
`Drive.read_dir` itself returns only the directory's child names.

This function also doubles as a background prefetch path:
`BackgroundFolderFetching` calls it indirectly from
`Drive.init_filesystem`. Any behavior change here affects both foreground
`readdir` requests and background folder warming.

## High-Level Behavior

`Drive.read_dir` does three things:

1. normalize the requested path into cache coordinates
2. decide whether the cached directory snapshot is still valid
3. either return cached children or rebuild the directory snapshot from Drive

The implementation is in `src/drive.ml`.

## Path Normalization

The function starts by converting the FUSE path into `(path_in_cache, trashed)`
using `get_path_in_cache`.

Important mappings:

- `"/"` becomes `("/", false)`
- `"/.Trash"` becomes `("/", true)`
- `"/.Trash/foo"` becomes `("/foo", true)`
- ordinary paths stay unchanged with `trashed = false`

This is how the code models the trash namespace: the same cached path can exist
in two parallel views, distinguished by the `trashed` flag.

Two other virtual roots are not remapped by `get_path_in_cache`:

- `"/lost+found"`
- `"/.shared"`

Those stay as normal paths and are handled later by special-case branches.

## Cache Validity Gate

Before doing any remote list call, `read_dir` checks:

```ocaml
check_resource_in_cache cache path_in_cache trashed
```

For directories, that is only true when both conditions hold:

- the cached resource is still valid relative to global metadata freshness
- the directory resource state is `Synchronized`

The validity rule comes from `CacheData.Resource.is_valid`:

- `ToUpload` and `Uploading` resources are always considered valid
- otherwise `resource.last_update` must be at least the global metadata
  `last_update`

For `read_dir`, the extra `Synchronized` requirement matters most. A folder in
`ToDownload` is treated as stale even if it exists locally.

If the cache is valid, `read_dir` does not talk to Drive. It just loads all
child resources with:

```ocaml
Cache.Resource.select_resources_with_parent_path cache path_in_cache trashed
```

## Remote Fetch Path

If the directory snapshot is missing or stale, `read_dir` rebuilds it.

The remote path begins with:

```ocaml
get_resource path_in_cache trashed
```

That call ensures the directory resource itself exists in cache and is fresh
enough to use. For well-known roots, `get_resource` synthesizes cache entries:

- root
- trash-root view of root
- `lost+found`
- `.shared`

See `docs/agent-docs/drive-get-resource.md` for the full path-resolution
contract.

After that, `read_dir` chooses one of three query strategies.

### 1. `lost+found`

For `"/lost+found"`, it lists all files owned by the current user:

```ocaml
'me' in owners
```

and then filters them down to files with no parents:

```ocaml
file.File.parents = []
```

So `lost+found` is not a real Drive folder. It is a synthetic view over
unorganized files.

### 2. `.shared`

For `"/.shared"`, it queries:

```ocaml
sharedWithMe = true
```

Again, this is a synthetic directory, not a real parent-child folder in Drive.

### 3. Ordinary folders, including `/.Trash`

For normal folders, `read_dir` first resolves the folder's remote id with
`get_folder_id path_in_cache trashed`, then lists children via:

```ocaml
'<folder_id>' in parents and trashed = <bool>
```

The trash root is a special case layered on top of that behavior.

Because `"/.Trash"` maps to `("/", true)`, the ordinary query only returns
trashed items whose parent is the root folder. The function then issues an
additional query:

```ocaml
not '<folder_id>' in parents and trashed = true
```

and keeps only `explicitlyTrashed` items. Those results are appended to the
directory snapshot so the trash view includes items that are trashed but do not
appear under the normal root-parent query.

## Snapshot Rebuild And Reconciliation

When the remote path is taken, `read_dir` does not update child rows
individually. It rebuilds the whole directory snapshot.

The reconciliation flow is:

1. fetch remote files
2. build tables from existing cached children with `build_resource_tables`
3. match fetched files against cached rows by remote id
4. create/update `CacheData.Resource.t` values for the full child set
5. replace the cached children for that parent
6. mark the folder resource itself as `Synchronized`

### Matching Existing Children

`build_resource_tables path_in_cache trashed` returns:

- a `filename_table` used for conflict detection
- a `remote_id_table` used to find existing cached rows by Drive file id

If a fetched file already exists in `remote_id_table`, `read_dir` updates that
cached row with `update_resource_from_file`.

That preserves useful local state where possible:

- stable cache row identity
- stable duplicate-name disambiguation when the remote item is the same file
- path recomputation only when the remote name actually changed

### Creating New Children

If no cached row matches a fetched file, `read_dir` creates a fresh resource.

The filename is not just `file.File.name`. It goes through
`get_unique_filename_from_file`, which applies:

- filename cleaning
- document export extension rules
- collision disambiguation using a fingerprint of the Drive remote id

That matters because Drive permits duplicate sibling names, while the mounted
filesystem cannot expose duplicate basenames in one directory.

### Replacing The Cached Child Set

After building the child resource list, `read_dir` calls:

```ocaml
Cache.Resource.insert_resources cache resources path_in_cache trashed
```

Both cache backends treat this as a replacement of the directory snapshot for
that `(parent_path, trashed)` pair:

- existing cached children under that parent are deleted
- the newly built resource list is inserted

So stale child rows disappear automatically when they are no longer returned by
Drive.

### Marking The Folder As Synchronized

After the children are replaced, the folder resource fetched earlier is updated
to:

- `state = Synchronized`
- `last_update = Unix.gettimeofday ()`

and saved back through `update_cached_resource`.

That state transition is what makes later `check_resource_in_cache` calls treat
the directory snapshot as reusable.

## Return Value

The function returns:

- `Filename.basename resource.path` for each child resource

It does not return full paths or metadata.

On top of the fetched/cached child list, it may add synthetic root entries:

- `".Trash"` at `"/"` when trash is enabled
- `".shared"` at `"/"` when not in the trash view
- `"lost+found"` at `"/"` when `config.lost_and_found = true`

These additions happen only when the requested `path` is the visible root path
`"/"`.

`"."` and `".."` are added later by the FUSE adapter, not here.

## Ordering

`Drive.read_dir` does not explicitly sort the returned names.

The observed order depends on:

- cache backend result order on cache hits
- Drive API pagination/result order on refreshes
- the extra synthetic root entries being prepended after the child list is built

If stable ordering becomes important, it needs to be added explicitly here.

## Why Background Prefetch Reuses This

`BackgroundFolderFetching` selects folder resources in `ToDownload` state and
calls `Drive.read_dir` on their paths.

That works because `read_dir` already encapsulates the full folder-refresh
contract:

- fetch the remote children when needed
- rebuild the child snapshot
- mark the folder `Synchronized`

So the background thread is not implementing separate synchronization logic. It
is just running the same code path ahead of demand.

See `docs/agent-docs/background-folder-fetching-start-thread.md` for the
startup boundary that installs the callback and starts that polling thread.

## Maintenance Notes

When changing `read_dir`, watch these invariants:

- `get_path_in_cache` and special-root branching must stay aligned
- `Cache.Resource.insert_resources` is snapshot replacement, not incremental
  merge
- filename disambiguation must stay deterministic enough to avoid path churn
  for unchanged remote ids
- background folder fetching depends on `read_dir` marking folders
  `Synchronized`
- FUSE-facing entry lists need to remain basename-only because `gdfuseFuse`
  prepends `"."` and `".."` separately

## Source Pointers

- `src/drive.ml`: `read_dir`
- `src/drive.ml`: `get_path_in_cache`
- `src/drive.ml`: `get_resource`
- `src/drive.ml`: `build_resource_tables`
- `src/drive.ml`: `get_unique_filename_from_file`
- `src/cacheData.ml`: `CacheData.Resource.is_valid`
- `src/cache.ml`: `Cache.Resource.insert_resources`
- `bin/gdfuseFuse.ml`: FUSE `readdir` adapter
