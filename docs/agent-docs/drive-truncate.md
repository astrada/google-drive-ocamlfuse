# `Drive.truncate`

## Purpose

`Drive.truncate` is the top-level local-size-mutation path used by the FUSE
`truncate` callback.

Like `Drive.write`, it is a local mutation step, not an immediate remote API
call. Its job is to:

- resolve the target resource
- flush any dirty in-memory write buffers first
- ensure there is a usable local cache file
- change the local file length with `Unix.LargeFile.truncate`
- update cached size metadata
- mark the resource `ToUpload`

The later upload back to Drive is handled by the same upload pipeline used for
ordinary writes.

See `docs/agent-docs/drive-upload-path.md` for the later scheduling and network
upload stages.

## Signature

```ocaml
val truncate : string -> int64 -> unit
```

The arguments are:

- `path`: visible FUSE path
- `size`: target file length in bytes

Unlike `Drive.write`, there is no ignored file-handle parameter here. The
operation is purely path-based.

## High-Level Flow

At a high level, `truncate` does this:

1. normalize the visible path into `(path_in_cache, trashed)`
2. resolve the resource with `get_resource`
3. flush dirty write buffers for that resource
4. ensure usable local content exists via `with_retry download_resource`
5. update the cached resource row to `size = Some size` and `state = ToUpload`
6. update cache-size accounting through `shrink_cache ~file_size:(size - old_size)`
7. truncate the local cache file on disk
8. return `unit`

That is the whole lifecycle of this function. It stops after local mutation and
does not schedule upload work itself.

## Path Normalization And Resolution

Like the other FUSE-facing operations, `truncate` starts with:

```ocaml
let path_in_cache, trashed = get_path_in_cache path config
```

and resolves the current resource through:

```ocaml
get_resource path_in_cache trashed
```

So the same path-resolution and metadata-refresh rules apply here as elsewhere.

See `docs/agent-docs/drive-get-resource.md` for the lookup contract and
`docs/agent-docs/drive-download-resource.md` for the local-content
materialization contract used later in this path.

## Flush Memory Buffers First

The first operation after resource lookup is:

```ocaml
flush_memory_buffers resource
```

This matters because a previous `Drive.write` may have deposited dirty bytes in
`Buffering.MemoryBuffers` instead of the on-disk cache file.

Without this flush, truncating the local file could race with or discard
buffered local writes that had not yet reached disk.

So `truncate` deliberately forces the buffered-write path back onto disk before
it touches file length.

## Ensure Local Content Exists

After flushing write buffers, `truncate` runs:

```ocaml
with_retry download_resource resource
```

This gives it a valid `content_path` to truncate.

That step may:

- reuse an existing synchronized local file
- reuse an already dirty local file
- download or export content
- synthesize a local representation for some document formats

So `truncate` shares the same local-content setup as `Drive.write`, but then
applies a size change instead of writing a byte range.

## Cached Metadata Is Updated Before The Filesystem Call

Once `content_path` exists, the implementation immediately constructs:

```ocaml
let updated_resource =
  resource
  |> CacheData.Resource.size ^= Some size
  |> CacheData.Resource.state ^= CacheData.Resource.State.ToUpload
```

and writes it back into the cache before calling `Unix.LargeFile.truncate`.

Two consequences follow from that ordering:

- the resource is marked dirty as soon as truncate begins its local mutation
- the cached size changes before the local `truncate(2)` call has definitely
  succeeded

That ordering is worth remembering when debugging partial failures or races.

## Cache-Size Accounting Uses A Signed Delta

After updating the resource row, `truncate` computes:

```ocaml
let file_size = size - old_size
```

and passes that to:

```ocaml
shrink_cache ~file_size ()
```

The important detail is that this delta is signed.

### Growing The File

If `size > old_size`, the delta is positive.

That means truncate is increasing the expected local cache usage, so
`shrink_cache` may:

- update metadata cache size upward directly, or
- evict other cached files and mark them `ToDownload` if the cache would exceed
  `max_cache_size_mb`

### Shrinking The File

If `size < old_size`, the delta is negative.

In that case `shrink_cache` effectively reduces the tracked cache usage. No
eviction work is needed because the target size is already moving downward.

So unlike `Drive.write`, which only ever increases size during one call,
`truncate` can move cache accounting in either direction.

## Local Filesystem Mutation

After the metadata update and cache-size accounting, `truncate` performs:

```ocaml
Unix.LargeFile.truncate content_path size
```

when the local file exists.

That single system call handles both directions:

- shrinking the file drops trailing bytes
- growing the file extends it, with the usual filesystem semantics for the new
  sparse/zero-filled region

So there is no separate "grow" and "shrink" branch in the local filesystem
operation itself.

## Defensive Missing-File Branch

Even after `download_resource`, the implementation still checks:

```ocaml
if Sys.file_exists content_path then
  Unix.LargeFile.truncate content_path size
else
  log warning
```

So a missing local file at that point is handled as a warning, not as an
exception raised by `truncate` itself.

That branch should normally be rare, since `download_resource` is supposed to
materialize or validate the local content path first. It is best understood as
a defensive fallback for races or unexpected local cache deletion.

## No Upload Scheduling Here

`truncate` ends after local mutation and does not call:

- `queue_upload`
- `upload_if_dirty`
- `flush`
- `fsync`
- `release`

So a successful truncate only guarantees:

- local cache state changed or was at least attempted
- cached metadata now says `state = ToUpload`

The later upload trigger still comes from the close/sync path, just like for
ordinary writes.

## Relationship To `Drive.write`

`truncate` shares much of its setup with `Drive.write`:

- path-based resource resolution
- `download_resource` as the local-content bridge
- `ToUpload` as the dirty state
- later upload through the same pipeline

But the mutation itself is different:

- `write` changes byte ranges and only increases size when the write extends the
  file
- `truncate` changes only file length and can either grow or shrink the cached
  size in one call

`truncate` also flushes memory buffers unconditionally before its content step,
because it needs the on-disk file to reflect all buffered writes before the
length change is applied.

See `docs/agent-docs/drive-write.md` for the ordinary write path.

## Permission Model

Unlike `Drive.update_remote_resource`, `truncate` does not contain an explicit
`is_filesystem_read_only ()` guard.

Unlike `Drive.write`, it also does not rely on an open-file handle path inside
this function itself.

So the implementation currently assumes permission checks have already happened
outside this function, typically through the filesystem/open layer.

That is an implementation fact worth remembering if write-access rules are ever
tightened or refactored.

See `docs/agent-docs/drive-fopen.md` for the open-time access gate this path
currently relies on.

## Maintenance Notes

### Dirty Buffered Writes Must Reach Disk First

The `flush_memory_buffers` call is essential. Removing or bypassing it would
risk truncating a stale on-disk file while newer local bytes still live only in
memory buffers.

### Metadata Is Optimistically Updated

The cache row is rewritten before the local `truncate(2)` call. If the local
filesystem step fails after that point, cached metadata may already reflect the
requested size/state.

### This Path Handles Both Shrink And Extend

Do not read the function as "only shrink". Passing a larger `size` extends the
local file and still routes through the normal upload lifecycle afterwards.
