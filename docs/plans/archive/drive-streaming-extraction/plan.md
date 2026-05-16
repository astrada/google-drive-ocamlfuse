# Extract Drive Streaming

## Goal

Move Drive media streaming and memory-buffer orchestration out of `src/drive.ml`
and into a focused, port-driven `DriveStreaming` module.

The extraction target is:

- `download_media`
- `stream_resource`
- `start_buffer_eviction_thread`
- `stream_resource_to_memory_buffer`
- `stream_resource_to_read_ahead_buffers`

The production behavior should stay unchanged. `src/drive.ml` should keep the
same local helper names used by download and read ports, but their behavior
should be delegated to the extracted module.

## Current Problem

The streaming path currently mixes several concerns inside `Drive`:

- Drive media download request construction
- resource-key request headers
- abusive-file retry handling
- byte-range streaming into bigarray buffers
- memory-buffer read-through callbacks
- read-ahead request creation
- buffer-eviction thread startup against global `Context`
- per-resource retry wrapping for deferred read-ahead requests

This makes the read path difficult to test directly because unit tests must
either work above the `DriveReads` boundary or rely on production global state.

## Proposed Shape

Add:

- `src/driveStreaming.ml`
- `src/driveStreaming.mli`
- `test/testDriveStreaming.ml`

Expose:

```ocaml
type io_buffer =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type runtime = {
  config : Config.t;
  memory_buffers : Buffering.MemoryBuffers.t;
  buffer_eviction_thread : Thread.t option;
}

module type PORTS = sig
  val build_resource_keys_header_from_resource :
    CacheData.Resource.t -> GapiCore.Header.t list

  val get_media :
    acknowledge_abuse:bool ->
    media_download:GapiMediaResource.download ->
    custom_headers:GapiCore.Header.t list ->
    file_id:string ->
    GapiDriveV3Model.File.t GapiMonad.SessionM.m

  val match_service_error : string -> exn -> bool
  val handle_default_exceptions : exn -> 'a GapiMonad.SessionM.m
  val with_retry_default : 'a GapiMonad.SessionM.m -> 'a GapiMonad.SessionM.m

  val create_eviction_thread : Buffering.MemoryBuffers.t -> Thread.t
  val set_buffer_eviction_thread : Thread.t -> unit

  val read_block :
    string ->
    int64 ->
    int64 ->
    (int64 -> io_buffer -> unit GapiMonad.SessionM.m) ->
    ?dest_arr:io_buffer ->
    Buffering.MemoryBuffers.t ->
    unit GapiMonad.SessionM.m

  val read_ahead :
    int ->
    string ->
    int64 ->
    int64 ->
    (int64 -> io_buffer -> unit GapiMonad.SessionM.m) ->
    Buffering.MemoryBuffers.t ->
    unit GapiMonad.SessionM.m list GapiMonad.SessionM.t

  val with_resource_retry :
    CacheData.Resource.t ->
    unit GapiMonad.SessionM.m ->
    unit GapiMonad.SessionM.m
end

module Make (P : PORTS) : sig
  val download_media :
    runtime ->
    GapiMediaResource.download ->
    CacheData.Resource.t ->
    GapiDriveV3Model.File.t GapiMonad.SessionM.m

  val stream_resource :
    runtime ->
    int64 ->
    io_buffer ->
    CacheData.Resource.t ->
    unit GapiMonad.SessionM.m

  val start_buffer_eviction_thread : runtime -> unit

  val stream_resource_to_memory_buffer :
    runtime ->
    int64 ->
    io_buffer ->
    CacheData.Resource.t ->
    unit GapiMonad.SessionM.m

  val stream_resource_to_read_ahead_buffers :
    runtime ->
    int64 ->
    CacheData.Resource.t ->
    unit GapiMonad.SessionM.m list GapiMonad.SessionM.m
end
```

The port layer should keep real Drive API calls, global context writes, memory
buffer operations, and retry policy outside the pure orchestration module.

## Extracted Behavior

`download_media` should preserve current behavior:

1. take the file ID from `resource.remote_id`
2. build resource-key headers from the resource
3. request media without abuse acknowledgement first
4. when the request fails with `cannotDownloadAbusiveFile` and
   `runtime.config.acknowledge_abuse = true`, log the warning and retry through
   `P.with_retry_default` with abuse acknowledgement enabled
5. otherwise delegate to `P.handle_default_exceptions`

`stream_resource` should preserve current byte-range behavior:

- calculate `finish = offset + buffer_length - 1`
- use `GapiMediaResource.ArrayBuffer buffer`
- use `GapiMediaResource.generate_range_spec`
- call `download_media`

`stream_resource_to_memory_buffer` should:

- start the buffer eviction thread when needed
- call `P.read_block` with the resource remote ID and size
- pass a callback that streams each requested block through `stream_resource`

`stream_resource_to_read_ahead_buffers` should:

- start the buffer eviction thread when needed
- call `P.read_ahead` with `runtime.config.read_ahead_buffers`
- wrap each returned deferred request with `P.with_resource_retry resource`

## Production Wiring

In `src/drive.ml`, add production ports for:

- `DriveResourceKeys.build_resource_keys_header_from_resource`
- `FilesResource.get` with `file_download_std_params`
- `match_service_error`
- `handle_default_exceptions`
- `with_retry_default`
- `Buffering.MemoryBuffers.create_eviction_thread`
- `Context.update_ctx (Context.buffer_eviction_thread ^= Some thread)`
- `Buffering.MemoryBuffers.read_block`
- `Buffering.MemoryBuffers.read_ahead`
- `with_retry`

Add a `drive_streaming_runtime ()` helper that reads `config`,
`memory_buffers`, and `buffer_eviction_thread` from `Context.get_ctx ()`.

Keep the existing local `download_media`, `stream_resource`,
`stream_resource_to_memory_buffer`, and `stream_resource_to_read_ahead_buffers`
names as wrappers around `DriveStreaming.Make`.

## Implementation Steps

1. Create `driveStreaming.mli` with runtime, port, and functor signatures.
2. Create `driveStreaming.ml`.
3. Move streaming orchestration from `src/drive.ml` into the new module.
4. Wire `DriveStreaming.Make` into `src/drive.ml`.
5. Keep existing callers in `DriveDownloadPorts` and `DriveReadPorts` stable.
6. Add `test/testDriveStreaming.ml`.
7. Register the suite in `test/testSuite.ml`.
8. Run `tools/format_ocaml` or `ocamlformat` on touched OCaml files.
9. Run `dune build @install` and `dune runtest` sequentially.

## Unit Test Plan

Use fake ports with trace lists and a small `Buffering.MemoryBuffers.t` value.

Cover:

- normal media download sends the resource-key headers and file ID
- abusive-file retry is attempted only when `acknowledge_abuse = true`
- non-retryable media errors delegate to the default exception handler
- byte-range streaming builds the expected range spec for the destination buffer
- eviction startup is skipped when large-file streaming is disabled
- eviction startup is skipped when a thread is already registered
- eviction startup creates and stores a thread when streaming is enabled and no
  thread is registered
- memory-buffer streaming calls `read_block` and streams through the callback
- read-ahead streaming calls `read_ahead` and wraps deferred requests with
  per-resource retry

## Acceptance Criteria

- `src/drive.ml` no longer owns media streaming and buffer orchestration logic.
- `DriveStreaming` owns download, byte-range, memory-buffer, and read-ahead
  streaming behavior behind testable ports.
- Existing read and download public behavior remains unchanged.
- Focused unit tests cover retry policy, range construction, buffer thread
  startup, memory-buffer callbacks, and read-ahead retry wrapping.
- `dune build @install` and `dune runtest` pass.

## Agent Docs Follow-Up

After implementation, update agent docs to describe the current state directly:

- `docs/agent-docs/architecture.md`
- `docs/agent-docs/repo-map.md`
