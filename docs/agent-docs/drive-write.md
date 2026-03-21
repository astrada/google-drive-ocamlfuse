# `Drive.write`

## Purpose

`Drive.write` is the top-level local-mutation path used by the FUSE `write`
callback.

It does not upload data to Drive directly. Its job is to:

- resolve the target resource
- ensure there is a usable local cache file
- write the incoming bytes either to disk or to in-memory write buffers
- mark the resource dirty with `state = ToUpload`
- update cached size/accounting when the write extends the file

The later upload step is handled separately by `flush`, `fsync`, `release`,
`upload_if_dirty`, and the upload pipeline.

See `docs/agent-docs/drive-upload-path.md` for the later scheduling and network
upload stages.

## Signature

```ocaml
val write :
  string ->
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  int64 ->
  'a ->
  int
```

The arguments are:

- `path`: visible FUSE path
- `buf`: source bytes to write
- `offset`: target file offset
- `file_descr`: ignored by the implementation

Like `Drive.read`, this path is path-based rather than file-handle-based. The
ignored `file_descr` means the function re-resolves the resource for each call.

## Preconditions And Permission Model

`write` itself does not contain an explicit read-only or can-edit guard.

Those checks happen earlier in `Drive.fopen` for non-read-only opens:

- filesystem-wide read-only mode rejects the open
- file-level non-editable resources reject the open

So `write` should be understood as the data path that runs after the open path
has already accepted the write-capable access mode.

## High-Level Flow

At a high level, `write` does this:

1. normalize the visible path into `(path_in_cache, trashed)`
2. resolve the resource with `get_resource`
3. ensure usable local content exists via `with_retry download_resource`
4. write the incoming bytes either to memory buffers or directly to disk
5. mark the resource `ToUpload`
6. if the write grew the file, update cached size and cache accounting
7. return the number of bytes written

The function does not queue or trigger an upload on its own.

## Path Normalization And Resource Resolution

Like the other FUSE entrypoints, `write` starts with:

```ocaml
let path_in_cache, trashed = get_path_in_cache path config
```

and then resolves the resource through:

```ocaml
get_resource path_in_cache trashed
```

So the usual lookup behavior applies before any local mutation:

- metadata may be refreshed first
- the cache row may be refreshed by remote id
- virtual namespaces are already mapped into cache coordinates

See `docs/agent-docs/drive-get-resource.md` for the resolution semantics.

## Ensuring Local Content Exists First

Before touching the local file or write buffers, `write` always runs:

```ocaml
with_retry download_resource resource
```

That is the bridge from the metadata row to the local content path.

It matters for several cases:

- synchronized files may already have a usable cache file
- dirty files may already have the authoritative local file
- documents may need export or local representation generation
- absent local files may need download or recreation

So `write` is not "create a local file if missing and start from empty". It
first synchronizes itself with the current local-content contract for that
resource.

See `docs/agent-docs/drive-download-resource.md` for the full materialization
state machine.

## Two Write Sinks

Once `download_resource` returns `content_path`, `write` chooses one of two
local sinks based on `config.write_buffers`.

### Branch 1: In-Memory Write Buffers

If `config.write_buffers = true`, `write` calls:

```ocaml
Buffering.MemoryBuffers.write_to_block
  remote_id content_path buf offset memory_buffers
```

This path does not immediately write the bytes into the cache file on disk.

Instead it:

- locates or allocates one or two memory blocks covering the write range
- copies the incoming bytes into those blocks
- marks the touched blocks `Dirty`
- records `content_path` in the block metadata for later flushes

The return value is the number of bytes copied into the memory-buffer blocks.

### Important Consequence

After a buffered write, the authoritative local content may live partly in
memory until a later flush step pushes dirty blocks to disk.

That later flush can happen through:

- `Drive.read`, before a non-streaming local-file read
- `Drive.truncate`, before local truncation
- `Drive.queue_upload` / `upload_resource_with_retry`, before upload
- explicit buffer-pressure eviction inside `Buffering.MemoryBuffers`

So `write_buffers` trades immediate disk writes for deferred flushing, but the
rest of the pipeline is already written to flush those dirty blocks before
reading or uploading the cache file.

### Branch 2: Direct Local File Write

If `config.write_buffers = false`, `write` writes straight to `content_path`:

```ocaml
Utils.with_out_channel content_path (fun ch ->
  let file_descr = Unix.descr_of_out_channel ch in
  Unix.LargeFile.lseek file_descr offset Unix.SEEK_SET |> ignore;
  Fuse.Unix_util.write file_descr buf)
```

Important details:

- the file is opened for writing without truncation
- the code seeks to `offset` before writing
- partial overwrite and append-like writes are both handled by the same path

This is the simpler branch: on successful return, the cache file on disk
already contains the new bytes.

## Dirty-State Transition

After the local write completes, `write` always marks the resource dirty.

There are two cases.

### Write Extended The File

The function computes:

```ocaml
let top_offset = offset + bytes_written
```

and compares it to the cached `resource.size`.

If `top_offset > file_size`, the write extended the file. In that case it:

1. updates `size = Some top_offset`
2. updates `state = ToUpload`
3. saves the updated resource row
4. calls `shrink_cache ~file_size:(top_offset - old_size) ()`

The `shrink_cache` call accounts for the extra local cache usage caused by the
growth.

### Write Stayed Within The Existing Size

If `top_offset <= file_size`, the write does not change the cached file length.

In that case `write` only updates:

- `state = ToUpload`

through `update_cached_resource_state`.

So overwrite-in-place writes still become dirty, but they do not rewrite the
cached size field.

## Return Value

`write` returns the number of bytes accepted by the local write sink.

That comes from:

- `Buffering.MemoryBuffers.write_to_block` in buffered mode
- `Fuse.Unix_util.write` in direct-file mode

The result is then used to compute `top_offset` for the size update logic.

## No Upload Scheduling Here

One important boundary is that `write` stops at local mutation.

It does not call:

- `queue_upload`
- `upload_if_dirty`
- `flush`
- `fsync`
- `release`

So a successful `write` only guarantees:

- local cache state changed
- the resource is marked `ToUpload`

The later upload trigger is normally one of the close/sync-related callbacks.

See `docs/agent-docs/drive-upload-path.md` for that second phase.

## Interaction With `Drive.read`

The write path is designed to preserve read-your-own-write behavior.

Two parts matter:

- dirty resources are marked `ToUpload`, which prevents `Drive.read` from
  choosing the large-file streaming path
- non-streaming reads flush memory buffers before opening the local cache file

So once a write succeeds, later reads are directed back to local content rather
than fresh remote bytes, even when large-file streaming is enabled.

See `docs/agent-docs/drive-read.md` for the read-side policy.

## Maintenance Notes

### `write` Depends On Earlier Open Checks

If write access rules change, check `Drive.fopen` as well as `Drive.write`.
The write path itself does not re-enforce those permissions.

### Buffered Writes Are Not Immediately On Disk

This is the most important operational distinction in the function.

When `config.write_buffers = true`, success means the bytes are in local write
buffers, not necessarily already present in the cache file.

### Size Accounting Is Monotonic Per Call

`write` only adjusts size upward when the current call extends the file.

It does not shrink the file, and it does not recompute size from disk. Shrinks
are handled by `truncate`, not by overwrite writes.

See `docs/agent-docs/drive-truncate.md` for the dedicated size-mutation path.
