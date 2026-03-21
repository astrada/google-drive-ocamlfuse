# `Drive.read`

## Purpose

`Drive.read` is the top-level file-read path used by the FUSE `read` callback.

Its main job is to decide which of three strategies should satisfy the read:

- stream bytes directly into the caller buffer
- stream into `Buffering.MemoryBuffers` and copy the requested slice out
- ensure a local cache file exists and read from that file

So `Drive.read` is the policy layer above helpers like:

- `stream_resource`
- `stream_resource_to_memory_buffer`
- `stream_resource_to_read_ahead_buffers`
- `download_resource`

## Signature

```ocaml
val read :
  string ->
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  int64 ->
  'a ->
  int
```

The arguments are:

- `path`: visible FUSE path
- `buf`: destination buffer to fill
- `offset`: file offset
- `file_descr`: ignored by the implementation

The ignored `file_descr` is worth noting: the function resolves the path again
for each read instead of depending on an already opened file handle.

## High-Level Flow

At a high level, `read` does this:

1. normalize the visible path into `(path_in_cache, trashed)`
2. resolve the current resource with `get_resource`
3. decide whether that resource should be streamed or read through the local
   cache file
4. perform the foreground read
5. optionally schedule asynchronous read-ahead work
6. return the byte count

The implementation splits this into two internal request builders:

- `request_resource`: performs the foreground read path and returns either a
  local `content_path` or `""` for streaming
- `build_read_ahead_requests`: returns zero or more background read-ahead
  requests to launch after the main read

## Path Normalization And Resource Resolution

Like most FUSE-facing operations, `read` starts with:

```ocaml
let path_in_cache, trashed = get_path_in_cache path config
```

and then resolves the resource through:

```ocaml
get_resource path_in_cache trashed
```

That means all of the usual lookup behavior happens before any read strategy is
chosen:

- metadata may be refreshed first
- cache rows may be refreshed by remote id
- trash and other virtual namespaces are already translated into cache
  coordinates

See `docs/agent-docs/drive-get-resource.md` for the path-resolution contract.

## Streaming Decision: `CacheData.Resource.to_stream`

The central branch point is:

```ocaml
let to_stream, to_memory_buffer =
  CacheData.Resource.to_stream config resource
```

That helper currently returns `to_stream = true` only when all of these hold:

- `config.stream_large_files = true`
- the resource is not a Google document
- `resource.state = ToDownload`
- the file is larger than `config.large_file_threshold_mb`

Then it sets:

- `to_memory_buffer = true` when `config.memory_buffer_size > 0`
- `to_memory_buffer = false` otherwise

So the policy is intentionally narrow:

- only large ordinary files stream
- documents never stream through this path
- dirty resources do not stream, because `ToUpload` and `Uploading` fail the
  `ToDownload` check

That last rule is important. If a file has local modifications in flight,
`read` falls back to the local cache file so the caller sees local content, not
fresh remote bytes.

## Branch 1: Direct Range Streaming

If `to_stream = true` and `to_memory_buffer = false`, `read` runs:

```ocaml
with_retry (stream_resource offset buf) resource
```

`stream_resource`:

1. computes the inclusive byte range `[offset, offset + len - 1]`
2. wraps the destination as `GapiMediaResource.ArrayBuffer buf`
3. calls `download_media` to fetch that range directly into the caller buffer

No local cache file is touched in this branch.

After the request completes, `request_resource` returns `""` to signal that the
buffer is already filled and there is no local file to read from.

## Branch 2: Memory-Buffered Streaming

If `to_stream = true` and `to_memory_buffer = true`, `read` runs:

```ocaml
with_retry (stream_resource_to_memory_buffer offset buf) resource
```

This still streams from the remote side, but through `Buffering.MemoryBuffers`
instead of directly into the FUSE buffer.

The helper does three important things:

1. lazily starts the buffer-eviction thread if needed
2. fills one or more memory blocks through `stream_resource`
3. copies the requested slice into `buf`

So the current read may trigger remote fetches aligned to memory-buffer block
boundaries, not just the exact visible read range.

As in direct streaming, `request_resource` returns `""` afterwards because the
foreground read already populated `buf`.

## Branch 3: Local Cache File Read

If `to_stream = false`, `read` switches to the local-file path:

```ocaml
flush_memory_buffers resource;
with_retry download_resource resource
```

Two pieces matter here:

### Flush Write Buffers First

`flush_memory_buffers resource` runs before the local read.

This is what makes buffered local writes visible to subsequent reads. If the
resource has dirty blocks in `Buffering.MemoryBuffers`, they are pushed to disk
before `read` opens the cache file.

### Ensure The Local File Exists

`with_retry download_resource resource` then ensures the local cache file is
usable and returns its path.

That may mean:

- reusing an existing synchronized file
- reusing a dirty local file
- downloading or exporting content
- synthesizing a local representation for some document formats

See `docs/agent-docs/drive-download-resource.md` for the full state machine and
materialization logic behind this step.

Once `content_path` comes back, `read` opens the file, seeks to `offset`, and
uses `Fuse.Unix_util.read` to copy bytes into `buf`.

## Read-Ahead Behavior

Read-ahead is built separately from the foreground read.

After `request_resource` completes, `read` optionally runs:

```ocaml
do_request build_read_ahead_requests
```

This path only creates work when all of these hold:

- `config.read_ahead_buffers > 0`
- a fresh `get_resource` call still resolves the file
- `CacheData.Resource.to_stream config resource` still says to stream
- `to_memory_buffer = true`

If those conditions hold, `stream_resource_to_read_ahead_buffers` returns a
list of session monads, each representing one future block fill.

`read` then launches them fire-and-forget with:

```ocaml
List.iter (fun m -> async_do_request m |> ignore) read_ahead_requests
```

Important consequences:

- read-ahead is only used for memory-buffer streaming
- it does not run for direct streaming
- it does not run for local-file reads
- the foreground read is completed before these background requests start

## Return Value Semantics

`read` returns the number of bytes read, but the source of that value depends on
the chosen branch.

### Local-File Branch

When `content_path <> ""`, the function returns the result of:

```ocaml
Fuse.Unix_util.read file_descr buf
```

So the byte count comes from the actual local file read.

### Streaming Branches

When `content_path = ""`, the function returns:

```ocaml
Bigarray.Array1.dim buf
```

because the foreground streaming helper has already written directly into the
destination buffer.

So the empty-string sentinel is the implementation's way of saying "the buffer
is already populated; skip the local-file read step".

## Why Dirty Files Do Not Stream

One of the more important policy details is that `to_stream` requires:

```ocaml
resource.state = ToDownload
```

That excludes:

- `ToUpload`
- `Uploading`

So once a file has local modifications pending upload, reads go through the
local cache file path instead of remote range streaming.

This preserves read-your-own-write behavior even when
`config.stream_large_files = true`.

See `docs/agent-docs/drive-write.md` for the producer-side path that marks
resources dirty and feeds this rule.

## Maintenance Notes

### `read` Resolves The Resource Twice When Read-Ahead Is Enabled

The foreground path and the read-ahead builder each call `get_resource`.

That means the resource lookup and `to_stream` decision are recomputed for
read-ahead scheduling instead of reusing the first result.

### `download_resource` Is Only The Non-Streaming Branch

`download_resource` is not the general read implementation. It is only the
helper used when `read` chooses the local-file path.

### Memory-Buffered Streaming Owns The Eviction Thread

Only the memory-buffer path starts the buffer-eviction thread. Direct
range streaming never touches that infrastructure.
