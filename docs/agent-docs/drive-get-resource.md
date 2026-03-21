# `Drive.get_resource`

## Purpose

`Drive.get_resource` is the core path-to-resource resolver for the filesystem
implementation.

Most higher-level Drive operations rely on it after translating the visible FUSE
path into cache coordinates. It is the function that answers:

- does this path currently exist?
- if it exists, which `CacheData.Resource.t` describes it?
- is the cached row fresh enough to reuse, or must it be refreshed from Drive?

Unlike `Drive.read_dir`, this function resolves one path, not a directory
snapshot.

## Signature And Calling Convention

```ocaml
val get_resource : string -> bool -> CacheData.Resource.t GapiMonad.SessionM.m
```

The parameters are:

- `path`: the cache path, not necessarily the raw visible FUSE path
- `trashed`: whether the lookup is happening in the trash namespace

That distinction matters. Most FUSE-facing callers first do:

```ocaml
let path_in_cache, trashed = get_path_in_cache path config
```

and then call `get_resource path_in_cache trashed`.

So `get_resource` should be understood as a low-level resolver over the
internal `(path, trashed)` namespace.

On failure it raises `File_not_found`.

## High-Level Algorithm

At a high level, `get_resource` does this:

1. refresh global metadata if needed
2. short-circuit well-known virtual roots
3. look up the path in the cache
4. if missing, try to resolve it from the server via parent-plus-name lookup
5. if present but stale, refresh it from the server via remote id
6. reject cached `NotFound` tombstones as `File_not_found`

The important point is step 1: resource lookup is coupled to metadata refresh.
The function does not just trust the local row timestamp in isolation.

## Step 1: Metadata Refresh First

The first thing `get_resource` does is:

```ocaml
let metadata_last_update =
  get_metadata () |. CacheData.Metadata.last_update
```

`get_metadata` is more than a getter:

- it loads metadata from context or cache if present
- it checks `Config.metadata_cache_time`
- if metadata is stale, it refreshes account metadata and Drive change tokens
- it applies change-feed updates to cached resources
- if the change set is too large, it invalidates all resources

See `docs/agent-docs/drive-get-metadata.md` for the full metadata-refresh and
change-reconciliation flow.

So by the time `get_resource` performs its path lookup, the resource cache may
already have been updated, invalidated, or had paths removed.

This is why `metadata_last_update` is the freshness boundary for individual
resources.

## Step 2: Well-Known Virtual Roots

Before ordinary cache lookup, `get_resource` special-cases:

- `"/"` with either trash view
- `"/lost+found"`
- `"/.shared"`

Those are handled by `get_well_known_resource`, which inserts a synthetic cache
row on first access and reuses it afterwards.

Important details:

- the visible root and the trash-root view both use `path = "/"`, but differ by
  the `trashed` flag
- `lost+found` and `.shared` are synthetic folders with no real Drive parent id
- ordinary path refresh logic does not run for these roots

## Step 3: Cache Lookup

For normal paths, the function reads:

```ocaml
match lookup_resource path trashed with
```

There are three cases:

- no cached row
- cached row present and valid
- cached row present but stale

Validity is based on `CacheData.Resource.is_valid`:

- `ToUpload` is always valid
- `Uploading` is always valid
- otherwise `resource.last_update >= metadata_last_update`

That differs from `read_dir`, which adds an extra `Synchronized` requirement for
folders before reusing a cached directory snapshot.

Here, `get_resource` only needs the row itself. It does not require that a
folder's children already be synchronized.

## Cache Miss Path

If `lookup_resource` returns `None`, `get_resource` runs `get_new_resource`.

That logic first checks the parent directory:

```ocaml
if check_resource_in_cache cache parent_path trashed then
  File_not_found
else
  ...
```

This is a key optimization and correctness rule.

If the parent directory is already cached and synchronized, then all of its
children are assumed to be known. A missing child path can therefore be treated
as absent immediately, with no extra server lookup.

If the parent is not known-good, `get_resource` does a server lookup:

1. build a provisional `new_resource = create_resource path`
2. resolve the parent folder id with `get_folder_id parent_path trashed`
3. query Drive by parent folder id plus basename
4. insert the result into cache, or insert a `NotFound` tombstone

The server-side lookup helper is `get_resource_from_server`.

## `get_resource_from_server`

`get_resource_from_server parent_folder_id name new_resource trashed cache`
performs a name-based lookup under one parent.

It uses `get_file_from_server`, which queries either:

- `name='<name>' and '<parent_id>' in parents and trashed=<bool>`
- or `name='<name>' and sharedWithMe = true` for the special shared view

Then:

- if no file is returned, a cache row is inserted with
  `state = CacheData.Resource.State.NotFound`
- if a file is returned, a normal resource row is inserted from that file

That `NotFound` row is a negative cache entry, not just a temporary local
exception.

## Negative Cache Semantics

`NotFound` rows matter for performance and behavior:

- they make repeated misses cheap
- they prevent repeated server lookups for paths already known absent
- `get_resource` converts them back into `File_not_found` at the end

The final check is:

```ocaml
match resource.state with
| NotFound -> File_not_found
| _ -> return resource
```

Write paths must account for that negative cache. The codebase already does this
in places like create and rename by explicitly deleting `NotFound` rows for the
new path before inserting the real resource.

## Stale Cache Row Path

If a cached row exists but is stale relative to `metadata_last_update`,
`get_resource` refreshes it through `refresh_resource`.

The refresh strategy is by remote id, not by path:

- if `resource.remote_id` exists, fetch the file directly with
  `FilesResource.get`
- then rebuild the cached row with `update_resource_from_file`
- then save it back with `update_cached_resource`

This is the authoritative refresh path for existing resources because remote id
is more stable than parent-plus-name lookup.

If the direct `FilesResource.get` fails with a not-found response, the wrapped
default exception handling raises `File_not_found` immediately. That path does
not first rewrite the cached row into a `NotFound` tombstone.

### Reload By Remote Id First

During refresh, the code does:

```ocaml
let reloaded_resource =
  Cache.Resource.select_first_resource_with_remote_id cache remote_id
```

before updating from the fetched file.

That means refresh prefers whichever cached row currently owns the same remote
id, instead of blindly updating the originally found row. This helps converge on
the canonical row if paths or duplicate-name resolution changed elsewhere.

## What Happens If The Cached Row Has No `remote_id`

There is a fallback branch:

```ocaml
| None ->
    delete_cached_resource resource;
    get_new_resource cache
```

That path only applies when a stale cached row has no `remote_id`.

In that situation, the row cannot be refreshed by id, so the code deletes the
cached row and falls back to the miss path, which uses parent-plus-name lookup.

For ordinary established Drive resources this is uncommon. It is mainly a safety
path for incomplete or synthetic cache entries that should not survive as normal
stale rows.

## Parent Resolution Is Recursive

The miss path depends on `get_folder_id`, which is recursive:

- root resolves from cached/configured root-folder state
- any other path resolves its parent by calling `get_resource`
- then extracts that parent's `remote_id`

So path resolution naturally walks upward until it reaches a known root.

This also means path lookup can cascade through parent refreshes before the
final child lookup is attempted.

## Returned Guarantees

If `get_resource` succeeds, the caller gets a `CacheData.Resource.t` that is:

- derived from resolution in the requested `(path, trashed)` namespace
- not a `NotFound` tombstone
- fresh enough relative to the current metadata refresh boundary, or explicitly
  preserved because it is `ToUpload` / `Uploading`

What it does not guarantee:

- file content is downloaded locally
- a folder's children are synchronized
- the caller is looking at a sorted or fully enumerated directory view

Those are handled elsewhere.

## Important Distinction From `read_dir`

`get_resource` and `read_dir` do not use exactly the same reuse criteria.

`get_resource` may return a cached folder row even if that folder is still in
`ToDownload`, as long as the row itself is considered valid.

`read_dir`, by contrast, requires a folder to be `Synchronized` before reusing
its child snapshot.

So:

- `get_resource` answers "what is this path?"
- `read_dir` answers "is this directory listing reusable?"

They are related, but not interchangeable.

## Limitation: Uncached Lookup Is Name-Based

By inspection of `get_file_from_server`, uncached lookup is based on exact
server-side name plus parent, and it only asks Drive for one result
(`pageSize:1`).

This implies a maintenance constraint:

- local path disambiguation for duplicate sibling names lives primarily in the
  cache built from directory listings
- the uncached miss path is not the authoritative mechanism for reconstructing a
  locally disambiguated path from scratch

That is an inference from the current source, not an explicitly documented API
contract, but it is important when refactoring path-resolution behavior.

## Maintenance Notes

When changing `get_resource`, watch these invariants:

- callers are expected to pass normalized `(path_in_cache, trashed)` values
- metadata refresh and resource freshness are coupled
- `NotFound` is a negative-cache state, not just an exception mapping
- stale existing resources refresh by remote id; missing resources resolve by
  parent-plus-name lookup
- write paths that create or rename into a path may need to delete cached
  `NotFound` rows first
- any change to `CacheData.Resource.is_valid` changes the reuse semantics here

## Related Docs

- `docs/agent-docs/drive-read-dir.md`
- `docs/agent-docs/drive-init-filesystem.md`

## Source Pointers

- `src/drive.ml`: `get_resource`
- `src/drive.ml`: `get_folder_id`
- `src/drive.ml`: `get_resource_from_server`
- `src/drive.ml`: `get_file_from_server`
- `src/drive.ml`: `get_metadata`
- `src/drive.ml`: `get_well_known_resource`
- `src/cacheData.ml`: `CacheData.Resource.is_valid`
