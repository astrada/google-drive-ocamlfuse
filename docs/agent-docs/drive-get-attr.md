# `Drive.get_attr`

## Purpose

`Drive.get_attr` is the path-level stat synthesis function used by the FUSE
`getattr` callback.

It is the main place where `Drive` turns cached resource metadata, optional
local cache-file state, and a few virtual-root rules into a
`Unix.LargeFile.stats` record.

This means `get_attr` defines much of the visible filesystem behavior for:

- file type (`st_kind`)
- permissions (`st_perm`)
- ownership (`st_uid`, `st_gid`)
- size (`st_size`)
- timestamps (`st_atime`, `st_mtime`, `st_ctime`)

It also contains one important side effect: for some documents it may
materialize local content first so stat data can come from the exported file.

## Signature And FUSE Boundary

```ocaml
val get_attr : string -> Unix.LargeFile.stats
```

The FUSE adapter wires:

```ocaml
let getattr path =
  drive_path_op ~name:"getattr" ~label:"stat" path Drive.get_attr
```

So repository exceptions such as `File_not_found` and `Permission_denied` are
converted at the boundary into Unix/FUSE errors.

## High-Level Flow

At a high level, `get_attr` does this:

1. normalize the visible path into `(path_in_cache, trashed)`
2. short-circuit a few virtual roots to mountpoint-derived stats
3. otherwise resolve the resource with `get_resource`
4. for some documents, optionally materialize local content first
5. optionally load a local `stat(2)` result from the cache file
6. synthesize the visible stat fields from a mix of resource metadata, local
   file state, and mountpoint defaults

The function returns one fully built `Unix.LargeFile.stats` value.

## Virtual-Root Shortcuts

Before doing any ordinary resource lookup, `get_attr` handles three special
root cases directly from `context.mountpoint_stats`.

### Root

For `"/"`, it returns `context.mountpoint_stats` unchanged.

### Trash Root And Shared Root

For:

- `"/.Trash"` when trash is enabled
- `"/.shared"`

it returns `context.mountpoint_stats` with permissions masked to read-only:

```ocaml
stats.st_perm = stats.st_perm land 0o555
```

So these roots present the same basic stat shape as the mountpoint, but are
visibly non-writable.

### `lost+found`

For `"/lost+found"` when that feature is enabled, it returns
`context.mountpoint_stats` unchanged.

Important consequence: these virtual-root branches do not call `get_resource`
at all.

## Ordinary Resource Path

For every non-short-circuited path, `get_attr` builds:

```ocaml
let request_resource =
  get_resource path_in_cache trashed >>= fun resource ->
  ...
  SessionM.return (resource, content_path)
```

So an ordinary `getattr` request still performs the usual path-resolution and
metadata-refresh logic first.

See `docs/agent-docs/drive-get-resource.md` for that lookup contract.

## Document Materialization Side Effect

After resolving the resource, `get_attr` has one special branch:

```ocaml
if CacheData.Resource.is_document resource && config.Config.download_docs then
  ...
else SessionM.return ""
```

When this branch is taken, it does:

1. `flush_memory_buffers resource`
2. `with_retry download_resource resource`

and returns the resulting `content_path`.

So `getattr` can trigger document export or local representation generation just
to compute stat data.

This is narrower than "all files download on stat":

- it applies only to Google documents
- it applies only when `config.download_docs = true`

### Suppressed `File_not_found` During Materialization

The document-materialization branch catches:

```ocaml
| File_not_found -> SessionM.return ""
```

So if the resource itself resolved successfully but the local document download
or export step raises `File_not_found`, `get_attr` falls back to metadata-only
stat synthesis instead of failing the whole `getattr` call.

Other exceptions still propagate normally.

## Optional Local `stat(2)` Source

After `request_resource` completes, `get_attr` computes:

```ocaml
let stat =
  if content_path <> "" && Sys.file_exists content_path then
    Some (Unix.LargeFile.stat content_path)
  else None
```

This local `stat` result is optional and only exists when `get_attr` has a
materialized local file path and the file is actually present.

The rest of the function then mixes:

- fields from this local `stat`
- fields from `CacheData.Resource.t`
- defaults from `context.mountpoint_stats`

Which source wins depends on the individual field.

## `st_kind`

The visible file type is chosen like this:

- folders are forced to `Unix.S_DIR`
- shortcuts are forced to `Unix.S_LNK`
- everything else uses
  `CacheData.Resource.file_mode_bits_to_kind resource.file_mode_bits`
  with `Unix.S_REG` as the fallback

One subtle point follows from that split:

- Drive shortcuts are always presented as link-like objects
- stored symlinks depend on the saved mode bits to come back as symlinks

So link-like behavior is represented through two slightly different internal
paths that converge at attribute time.

## `st_perm`

Permission synthesis starts from a default mode:

- folders default to `0o777`
- non-folders default to `0o666`

If the resource has saved mode bits, those override the default permission
portion through `file_mode_bits_to_perm`.

After that, `get_attr` applies a mask.

### Symlinks And Shortcuts

For symlinks and shortcuts, the mask is simply:

```ocaml
0o777
```

So their visible permissions do not get reduced by `umask` or by the
`is_file_read_only` rule at this stage.

### Other Resources

For everything else, the mask is:

```ocaml
(lnot config.umask) land
  (if is_file_read_only resource then 0o555 else 0o777)
```

So visible write bits are removed when the resource is effectively read-only.

This makes `get_attr` the stat-side mirror of the access policy documented in
`Drive.fopen`.

See `docs/agent-docs/drive-fopen.md` for the open-time enforcement rule behind
`is_file_read_only`.

## `st_nlink`

`get_attr` hard-codes:

```ocaml
let st_nlink = 1
```

for every ordinary resource.

The inline comment explains why: the implementation deliberately avoids trying
to count subdirectories for folder link counts, both for performance and for
compatibility with software that accepts `1` as "subdirectory count unknown".

So `st_nlink` here is a compatibility placeholder, not a literal POSIX link
count.

## `st_uid` And `st_gid`

Ownership comes from cached resource metadata when present:

- `resource.uid`
- `resource.gid`

If either is absent, `get_attr` falls back to the corresponding field from
`context.mountpoint_stats`.

So mountpoint ownership acts as the default owner/group view for resources that
do not carry explicit uid/gid app properties.

## `st_size`

Size synthesis is the most branch-heavy field.

### Symlinks And Shortcuts

For symlinks and shortcuts, `st_size` is:

- the cached `link_target` length, if already present
- otherwise the length of a target reconstructed through `fetch_link_target`

That reconstructed path may require:

- resolving `target_id`
- prefixing the mountpoint path
- updating the cached row with the resolved `link_target`

So asking for `st_size` on a shortcut can have the side effect of filling in
missing cached link-target data.

### Other Resources With A Local `stat`

If a local `stat` exists, `st_size` comes from the on-disk file size.

This is how downloaded or exported content can expose a local representation
size instead of only the remote metadata size.

### Folders Without A Local `stat`

If there is no local file stat and the resource is a folder, `st_size` is the
constant filesystem block size:

```ocaml
f_bsize = 4096L
```

So directories do not use remote Drive size metadata here.

### Other Resources Without A Local `stat`

For ordinary non-folder resources without a local file stat, `st_size` falls
back to:

```ocaml
Option.default 0L resource.size
```

## Timestamp Rules

`get_attr` does not take all timestamps from the same source.

### `st_atime`

If a local `stat` exists, `st_atime` comes from the local file.

Otherwise it falls back to:

```ocaml
resource.viewed_by_me_time
```

So the no-local-file case uses a Drive-side "viewed by me" concept rather than
a true local access timestamp.

### `st_mtime`

For `st_mtime`, the local file only wins in one case:

- a local `stat` exists
- and `resource.state = ToUpload`

Then `st_mtime = local_stat.st_mtime`.

Otherwise, `st_mtime` comes from:

```ocaml
resource.modified_time
```

This is the key "read your own local mutation" rule for dirty resources: once a
file is `ToUpload`, `getattr` reports the local file mtime instead of the older
remote modified time.

### `st_ctime`

`st_ctime` follows the same dirty-resource rule:

- local `stat.st_ctime` when a local stat exists and the resource is `ToUpload`
- otherwise it is set equal to `st_mtime`

So for non-dirty resources, visible ctime is not a separate creation/change
timestamp. It simply mirrors the chosen mtime value.

## Final Record Construction

The final result is built by taking `context.mountpoint_stats` as a template and
overriding:

- `st_kind`
- `st_perm`
- `st_nlink`
- `st_uid`
- `st_gid`
- `st_size`
- `st_atime`
- `st_mtime`
- `st_ctime`

So any other fields not explicitly rewritten continue to come from the cached
mountpoint stats baseline.

## Relationship To Symlink And Shortcut Creation

`get_attr` is one of the read-side consumers of the symlink-versus-shortcut
decisions made during creation.

That is why:

- plain stored symlinks rely on cached app properties and mode bits
- Drive shortcuts may need `target_id` resolution through `fetch_link_target`
- both ultimately surface as link-like objects with link-target-sized `st_size`

See `docs/agent-docs/drive-create-remote-resource.md` for how those resource
shapes are created in the first place.

## Maintenance Notes

### `getattr` Can Materialize Content

This is easy to miss. For some documents, a stat call can export or recreate a
local representation before building the result.

### Local File State Only Partially Overrides Remote Metadata

Even when a local `stat` exists, not every field comes from it.

- `st_size` and `st_atime` do
- `st_mtime` and `st_ctime` only do when the resource is `ToUpload`

### Shortcut Size Lookup Can Update Cache

If `link_target` is missing, `fetch_link_target` resolves it and stores it back
into the cached resource row while answering the stat request.
