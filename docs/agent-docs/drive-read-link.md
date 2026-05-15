# `Drive.read_link`

## Purpose

`Drive.read_link` is the FUSE read-side path that returns the target string for
symlink-like resources.

In this codebase, that includes two different underlying representations:

- stored symlinks, where the target string lives in cached app properties
- Drive shortcuts, where the visible link target may need to be reconstructed
  from `target_id`

So `read_link` is not just a trivial field read. For shortcuts it can perform a
target-resource lookup and update the cached source row with the reconstructed
target path.

The public `Drive.read_link` function in `src/drive.ml` is a thin
adapter. It builds a small runtime from `Context`, then runs the real
link-target resolution logic in `DriveViews.Make.read_link` through
`do_request`.

## Signature And FUSE Boundary

```ocaml
val read_link : string -> string
```

The production FUSE adapter wires:

```ocaml
let readlink path = drive_path_op ~name:"readlink" path Drive.read_link
```

So repository exceptions are translated at the boundary into Unix/FUSE errors.

The most important one here is:

- `Invalid_operation` -> `EINVAL`

That is how non-link-like resources fail when passed to `readlink`.

## High-Level Flow

`read_link` itself is intentionally small:

1. normalize the visible path into `(path_in_cache, trashed)`
2. grab the cache handle from `Context`
3. call `fetch_link_target path_in_cache trashed cache`
4. run that request through `do_request`
5. return the resulting string

So almost all of the real behavior lives in `DriveViews.fetch_link_target`.

## Path Normalization

Like the other FUSE-facing entrypoints, `read_link` starts with:

```ocaml
let path_in_cache, trashed = get_path_in_cache path config
```

That means it inherits the normal path-mapping rules for trash and other
special namespaces before link-target resolution starts.

There is no separate symlink-only namespace or special root short-circuit in
this function.

## The Shared Helper: `fetch_link_target`

`fetch_link_target` inside `src/driveViews.ml` is the real readlink engine.

Its first step is always:

```ocaml
get_resource path_in_cache trashed
```

So `read_link` first resolves the source resource through the normal cache and
metadata-refresh path.

After that, the helper branches on two cached fields:

- `resource.link_target`
- `resource.target_id`

## Case 1: Cached `link_target` Already Present

If `resource.link_target = Some target`, the helper simply returns that string.

This is the common path for:

- stored symlinks created with a literal target string in app properties
- shortcuts whose target string was preserved at creation time
- shortcuts whose target string was reconstructed and cached by an earlier
  `readlink` or `getattr`

In this branch, no target-resource lookup is needed.

## Case 2: Reconstruct Through `target_id`

If `resource.link_target = None`, the helper checks `resource.target_id`.

### Missing `target_id`

If `target_id = None`, it raises:

```ocaml
Invalid_operation
```

This is the "not actually link-like" failure path.

At the FUSE boundary, that becomes `EINVAL`.

### Present `target_id`

If `target_id = Some tid`, the helper resolves the target resource:

```ocaml
get_resource_with_id tid cache
```

That means the target may come from:

- the local cache, if already known by remote id
- the server, if it is not currently cached

The cache-first lookup and server-side path reconstruction live in
`DriveResourceById`, reached through the `Drive.get_resource_with_id` wrapper
that `DriveViews` receives in its ports.

So `readlink` on a shortcut can trigger extra metadata lookups for the target
resource even though it does not touch file content.

## Mountpoint-Relative Reconstruction For Shortcuts

When reconstructing a target from `target_id`, the helper builds:

```ocaml
let link_target =
  normalized_mountpoint_path ^ link_resource.path
```

where `normalized_mountpoint_path` is the mountpoint path with any trailing
slash removed first.

So reconstructed shortcut targets are always absolute paths inside the current
mount.

This is a key semantic difference from stored symlinks:

- stored symlinks return the target string that was stored
- shortcuts return a path synthesized from the current target resource path in
  the mount namespace

Because the reconstruction is by remote id, it naturally tracks target rename
or move operations. The visible link target is based on the target resource's
current cached path, not only on the path it had when the shortcut was created.

## Cache Update Side Effect

After reconstructing the target string, `fetch_link_target` writes it back into
the source resource row:

```ocaml
let updated_resource =
  resource |> CacheData.Resource.link_target ^= Some link_target
in
update_cached_resource cache updated_resource
```

So both `read_link` and other callers of `fetch_link_target` act as lazy cache
repair for missing shortcut targets.

This is why a later `readlink` on the same resource can become a cheap cached
string return instead of repeating the target lookup.

## Stored Symlinks vs Drive Shortcuts

The most important user-visible distinction is:

### Stored Symlink

`read_link` returns the stored target string directly.

That string may be:

- relative
- absolute inside the mountpoint
- absolute outside the mountpoint

because it is just the preserved symlink payload.

### Drive Shortcut

`read_link` returns a mountpoint-prefixed absolute path derived from the target
resource.

That means shortcut readlink results are always tied to the current mounted
view, not just to an opaque stored string.

This is why shortcut targets can track target rename/move behavior better than
plain stored symlinks.

## Relationship To `Drive.get_attr`

`Drive.get_attr` uses the same `fetch_link_target` helper when it needs the
target string to compute `st_size` for shortcuts or symlink-like resources.

So `read_link` and `get_attr` share the same lazy reconstruction and cache-fill
behavior for missing shortcut targets.

See `docs/agent-docs/drive-get-attr.md` for the stat-side consequences of that
shared helper.

## Relationship To Creation

The behavior of `read_link` is largely determined earlier by
`Drive.create_remote_resource`:

- stored symlinks keep a literal target in app properties
- shortcuts keep `target_id` and may also preserve an initial `link_target`

See `docs/agent-docs/drive-create-remote-resource.md` for the creation-side
decision tree that determines which representation a later `readlink` sees.

## What `read_link` Does Not Do

`read_link` does not:

- download file content
- inspect local cache files on disk
- perform permission masking
- verify that the source resource is a shortcut or stored symlink before
  calling `fetch_link_target`

Its validation model is indirect: if the resource does not have a cached
`link_target` and does not have a `target_id`, the helper raises
`Invalid_operation`.

## Maintenance Notes

### Shortcut Reads Can Trigger Target Lookup

Do not treat `read_link` as a pure field accessor. A cache miss on
`link_target` can force lookup of the target resource by remote id.

### Returned Targets Depend On Representation

Stored symlinks and shortcuts are intentionally not equivalent here. Their
`readlink` results come from different data sources and may evolve differently
over time.

### `fetch_link_target` Is Shared

Changes to `fetch_link_target` affect at least:

- `Drive.read_link`
- `Drive.get_attr`

so link-target caching and reconstruction changes must be reviewed in both
contexts.

## Source Pointers

- `src/drive.ml`: `read_link`
- `src/drive.ml`: `DriveViewPorts`
- `src/drive.ml`: `DriveResourceByIdPorts`
- `src/driveViews.ml`: `read_link`
- `src/driveViews.ml`: `fetch_link_target`
- `src/driveResourceById.ml`: remote-id lookup and path reconstruction
- `test/testDriveResourceById.ml`
- `test/testDriveViews.ml`
