# `Drive.read`

## Purpose

`Drive.read` is the top-level regular-file read path used by the FUSE `read`
callback.

The production entrypoint in `src/drive.ml` is a thin adapter over
`DriveReads`:

```ocaml
let read path buf offset file_descr =
  do_request (ReadOps.read (drive_read_runtime ()) path buf offset) |> fst
```

The `file_descr` argument is ignored. Each read resolves the visible path again
instead of depending on an already opened file handle.

`DriveReads` owns the read-strategy policy. `DriveReadPorts` connects that
policy to the production helpers for path normalization, resource lookup,
streaming, local cache-file materialization, local file reads, and asynchronous
read-ahead scheduling.

## Strategy

The core chooses one of three foreground read strategies:

- stream bytes directly into the caller buffer
- stream through `Buffering.MemoryBuffers` and copy the requested slice into the
  caller buffer
- ensure a local cache file exists and read from that file

The foreground strategy is selected with:

```ocaml
CacheData.Resource.to_stream config resource
```

That helper returns `to_stream = true` only when all of these hold:

- `config.stream_large_files = true`
- the resource is not a Google document
- `resource.state = ToDownload`
- the file is larger than `config.large_file_threshold_mb`

It sets `to_memory_buffer = true` when `config.memory_buffer_size > 0`.

Because streaming requires `ToDownload`, local dirty states such as `ToUpload`
and `Uploading` use the local cache-file branch. That preserves
read-your-own-write behavior when local content has not been uploaded yet.

## High-Level Flow

`DriveReads.read` runs this sequence:

1. Normalize the visible FUSE path with `get_path_in_cache`, producing
   `(path_in_cache, trashed)`.
2. Resolve the current resource with `get_resource path_in_cache trashed`.
3. Run the foreground read strategy.
4. Build read-ahead requests when configured.
5. Enqueue any read-ahead requests asynchronously.
6. Return the foreground byte count.

Path normalization and resource resolution follow the same cache / remote lookup
contract as other FUSE-facing operations. See
`docs/agent-docs/drive-get-resource.md` for that contract.

## Direct Range Streaming

When `to_stream = true` and `to_memory_buffer = false`, `DriveReadPorts` calls:

```ocaml
with_retry (stream_resource offset buf) resource
```

`stream_resource` computes the requested byte range, wraps the destination as a
`GapiMediaResource.ArrayBuffer`, and downloads that range directly into the FUSE
buffer.

No local cache file is touched in this branch. On success, `DriveReads.read`
returns:

```ocaml
Bigarray.Array1.dim buf
```

because the streaming helper has already populated the caller buffer.

## Memory-Buffered Streaming

When `to_stream = true` and `to_memory_buffer = true`, `DriveReadPorts` calls:

```ocaml
with_retry (stream_resource_to_memory_buffer offset buf) resource
```

This branch still streams from Drive, but it goes through
`Buffering.MemoryBuffers`.

The memory-buffer helper:

1. starts the buffer-eviction thread when needed
2. fetches remote ranges into memory-buffer blocks
3. copies the requested slice into the caller buffer

The foreground return value is also `Bigarray.Array1.dim buf`, because the
destination buffer has been filled before `DriveReads.read` returns.

## Local Cache File Read

When `to_stream = false`, the read path uses local cached content:

```ocaml
flush_memory_buffers resource;
with_retry download_resource resource
```

`flush_memory_buffers` runs first so any dirty in-memory blocks are written to
the cache file before the read opens it.

`download_resource` then ensures usable local content exists and returns its
path. That may mean reusing an existing synchronized file, reusing dirty local
content, downloading media, exporting a Google document, or synthesizing local
content for supported document formats.

See `docs/agent-docs/drive-download-resource.md` for the materialization state
machine behind this step.

After the content path is available, `DriveReadPorts.read_local_file` opens the
file, seeks to `offset`, and calls `Fuse.Unix_util.read`. The return value is
the actual byte count from that local file read.

## Read-Ahead Behavior

Read-ahead is built after the foreground read has completed.

`DriveReads` creates read-ahead work only when all of these hold:

- `config.read_ahead_buffers > 0`
- a fresh `get_resource path_in_cache trashed` call resolves the file
- `CacheData.Resource.to_stream config resource` still says to stream
- `to_memory_buffer = true`

The fresh lookup is intentional behavior: read-ahead uses the current resource
state instead of assuming the foreground lookup is still valid. If the second
lookup no longer qualifies for memory-buffer streaming, no read-ahead work is
started.

When read-ahead qualifies,
`stream_resource_to_read_ahead_buffers offset resource` returns one session
request per future block. `DriveReadPorts.enqueue_async_request` launches each
request through `async_do_request`.

Consequences:

- direct range streaming does not enqueue read-ahead work
- local cache-file reads do not enqueue read-ahead work
- memory-buffered streaming can enqueue read-ahead work
- background read-ahead starts after the foreground read

## Test Boundary

`test/testDriveReads.ml` exercises the read policy through fake ports. The tests
cover direct streaming, memory-buffer streaming, read-ahead scheduling, disabled
read-ahead, local cache-file reads, trash-path normalization, and the second
lookup that can suppress read-ahead when the resource no longer qualifies.
