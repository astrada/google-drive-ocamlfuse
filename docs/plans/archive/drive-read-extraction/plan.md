# Extract Drive Read

## Goal

Move the top-level read policy out of `src/drive.ml` into a small testable
module that follows the current functorized runtime pattern used by
`DriveViews`, `DriveDirectoryReads`, `DriveFileMutations`,
`DriveMetadataMutations`, `DriveUploadDispatch`, `DriveMutations`, and
`DriveXattrs`.

The extraction target is:

- `Drive.read`

The production behavior should stay unchanged. `src/drive.ml` should retain a
thin wrapper that builds the runtime, calls the extracted operation, and runs
it through `do_request`.

## Current Problem

`Drive.read` still contains read-strategy policy directly in `src/drive.ml`.
The branch logic is small enough to extract, but important enough to test:

- direct range streaming
- memory-buffer streaming
- local cache-file materialization and read
- optional read-ahead request creation
- fire-and-forget read-ahead dispatch

Today that policy is mixed with production concerns:

- `Context.get_ctx`
- `do_request`
- `async_do_request`
- `with_retry`
- `stream_resource`
- `stream_resource_to_memory_buffer`
- `stream_resource_to_read_ahead_buffers`
- `download_resource`
- local file open/seek/read through Unix/FUSE helpers

That makes it hard to unit test read strategy without touching global context,
remote Drive calls, memory buffers, or local files.

## Proposed Shape

Add:

- `src/driveReads.ml`
- `src/driveReads.mli`
- `test/testDriveReads.ml`

Expose a functor:

```ocaml
type runtime = {
  cache : CacheData.t;
  config : Config.t;
}

type io_buffer =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module type PORTS = sig
  val get_path_in_cache : string -> Config.t -> string * bool
  val get_resource : string -> bool -> CacheData.Resource.t GapiMonad.SessionM.m

  val stream_resource :
    int64 -> io_buffer -> CacheData.Resource.t -> unit GapiMonad.SessionM.m

  val stream_resource_to_memory_buffer :
    int64 -> io_buffer -> CacheData.Resource.t -> unit GapiMonad.SessionM.m

  val stream_resource_to_read_ahead_buffers :
    int64 ->
    CacheData.Resource.t ->
    unit GapiMonad.SessionM.m list GapiMonad.SessionM.m

  val flush_memory_buffers : CacheData.Resource.t -> unit
  val ensure_local_content : CacheData.Resource.t -> string GapiMonad.SessionM.m
  val read_local_file : string -> io_buffer -> int64 -> int
  val enqueue_async_request : unit GapiMonad.SessionM.m -> unit
end
```

Expose:

```ocaml
val read : runtime -> string -> io_buffer -> int64 -> int GapiMonad.SessionM.m
```

The extracted module should own only the read-strategy behavior. Production
ports should own remote streaming, local materialization, local file I/O,
retry wrapping, and async request dispatch.

## Production Wiring

In `src/drive.ml`, add a ports module near the existing read/file-mutation
wiring:

```ocaml
module DriveReadPorts = struct
  let get_path_in_cache = get_path_in_cache
  let get_resource = get_resource

  let stream_resource offset buf resource =
    with_retry (stream_resource offset buf) resource

  let stream_resource_to_memory_buffer offset buf resource =
    with_retry (stream_resource_to_memory_buffer offset buf) resource

  let stream_resource_to_read_ahead_buffers offset resource =
    stream_resource_to_read_ahead_buffers offset resource

  let flush_memory_buffers = flush_memory_buffers

  let ensure_local_content resource =
    with_retry download_resource resource

  let read_local_file content_path buf offset =
    Utils.with_in_channel content_path (fun ch ->
        let file_descr = Unix.descr_of_in_channel ch in
        Unix.LargeFile.lseek file_descr offset Unix.SEEK_SET |> ignore;
        Fuse.Unix_util.read file_descr buf)

  let enqueue_async_request request =
    async_do_request request |> ignore
end
```

Instantiate:

```ocaml
module ReadOps = DriveReads.Make (DriveReadPorts)
```

Add:

```ocaml
let drive_read_runtime () =
  let context = Context.get_ctx () in
  {
    DriveReads.cache = context.Context.cache;
    config = context |. Context.config_lens;
  }
```

Replace `Drive.read` with:

```ocaml
let read path buf offset file_descr =
  do_request (ReadOps.read (drive_read_runtime ()) path buf offset) |> fst
```

Keep `file_descr` ignored, preserving the current public callback behavior.

## Implementation Steps

1. Create `driveReads.mli` with `runtime`, `io_buffer`, `PORTS`, and `read`.
2. Create `driveReads.ml` by moving the policy from `Drive.read`.
3. Replace the empty-string sentinel with an internal variant if useful:
   `Streamed` vs `LocalFile of string`. This is acceptable if behavior remains
   identical.
4. Keep `CacheData.Resource.to_stream` as the policy branch point.
5. Keep the second resource lookup for read-ahead separate from the foreground
   read.
6. Call `P.enqueue_async_request` only after the foreground read request has
   completed.
7. Wire `DriveReads.Make` into `src/drive.ml`.
8. Replace the old `Drive.read` body with the thin wrapper.
9. Add `test/testDriveReads.ml`.
10. Register the suite in `test/testSuite.ml`.
11. Run `ocamlformat` on touched OCaml files.
12. Run `dune build @install` and `dune runtest`.

## Unit Test Plan

Use fake ports, following the style in `test/testDriveFileMutations.ml` and
`test/testDriveXattrs.ml`.

Cover path setup:

- visible path is normalized with the runtime config
- foreground read uses the normalized path and `trashed` flag

Cover direct streaming:

- a resource where `CacheData.Resource.to_stream config resource` returns
  `(true, false)` calls the direct streaming port
- direct streaming returns `Bigarray.Array1.dim buf`
- direct streaming does not flush memory buffers
- direct streaming does not ensure local content
- direct streaming does not enqueue read-ahead work

Cover memory-buffer streaming:

- a resource where `to_stream` returns `(true, true)` calls the
  memory-buffer streaming port
- memory-buffer streaming returns `Bigarray.Array1.dim buf`
- when `config.read_ahead_buffers > 0`, read-ahead performs a second
  `get_resource` lookup
- read-ahead requests returned by the port are enqueued after the foreground
  read completes
- when `config.read_ahead_buffers = 0`, no second lookup or enqueue happens

Cover local-file reads:

- a non-streaming resource flushes memory buffers before materialization
- local content is ensured through the `ensure_local_content` port
- local file bytes are read through the `read_local_file` port
- the returned byte count comes from `read_local_file`
- local-file reads do not enqueue read-ahead work

Cover mixed/read-ahead edge cases:

- if the foreground resource streams but the second read-ahead lookup no longer
  streams, no read-ahead work is enqueued
- if the second read-ahead lookup streams directly without memory buffers, no
  read-ahead work is enqueued

## Acceptance Criteria

- `src/drive.ml` no longer contains read-strategy branch logic.
- The public `Drive.read` signature and observable behavior remain unchanged.
- Direct streaming, memory-buffer streaming, local-file reads, and read-ahead
  scheduling are unit tested without real `Context`, Drive API requests,
  memory buffers, or local files.
- `dune build @install` and `dune runtest` pass.

## Agent Docs Follow-Up

After implementation, update the agent docs to describe the current state
directly:

- `docs/agent-docs/drive-read.md`
- `docs/agent-docs/architecture.md`
- `docs/agent-docs/repo-map.md`
- `docs/agent-docs/README.md`

Avoid before/after language. The docs should describe `DriveReads` as the
current implementation boundary.
