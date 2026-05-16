# Extract Drive Runtime Services

## Goal

Move FUSE-init runtime service startup out of `src/drive.ml` and into a
focused, port-driven `DriveRuntimeServices` module.

The extraction target is:

- `init_filesystem`
- memory-cache flush-thread startup
- async upload queue startup
- background folder fetching startup
- callbacks from those services back into Drive upload and directory-read paths

The production behavior should stay unchanged. `src/drive.ml` should keep the
public `init_filesystem : unit -> unit` wrapper because the FUSE adapter calls
`Drive.init_filesystem` directly.

## Current Problem

`Drive.init_filesystem` is small, but it coordinates long-running service
startup and currently hard-wires production side effects:

- reads cache/config from global `Context`
- always asks `MemoryCache` to start its flush-db thread
- conditionally starts `UploadQueue` when `config.async_upload_queue = true`
- conditionally starts `BackgroundFolderFetching` when
  `config.background_folder_fetching = true`
- passes `upload_resource_by_id` to upload workers
- passes `read_dir |> ignore` to folder prefetching

This behavior is FUSE-startup critical and should have direct unit coverage
without starting real threads, mutating global service state, or relying on
production `Context`.

## Proposed Shape

Add:

- `src/driveRuntimeServices.ml`
- `src/driveRuntimeServices.mli`
- `test/testDriveRuntimeServices.ml`

Expose:

```ocaml
type runtime = {
  cache : CacheData.t;
  config : Config.t;
}

module type PORTS = sig
  val start_flush_db_thread : CacheData.t -> unit

  val start_async_upload_thread :
    CacheData.t -> int -> (int64 -> unit) -> unit

  val start_folder_fetching_thread :
    CacheData.t -> (string -> unit) -> unit

  val upload_resource_by_id : int64 -> unit
  val read_dir : string -> string list
end

module Make (P : PORTS) : sig
  val init_filesystem : runtime -> unit
end
```

The extracted module should own startup branch policy only. Production ports
should own the real thread/service startup functions and callbacks into Drive.

## Extracted Behavior

`init_filesystem runtime` should preserve the current startup flow:

1. call `P.start_flush_db_thread runtime.cache`
2. if `runtime.config.async_upload_queue = true`, call
   `P.start_async_upload_thread runtime.cache
   runtime.config.async_upload_threads P.upload_resource_by_id`
3. if `runtime.config.background_folder_fetching = true`, call
   `P.start_folder_fetching_thread runtime.cache (fun path ->
   P.read_dir path |> ignore)`

Preserve the current ordering:

- flush-db startup first
- async upload startup second when enabled
- background folder fetching startup third when enabled

Do not add idempotency guards in this extraction. The current public contract
still assumes FUSE calls `init_filesystem` once per mounted instance.

Do not broaden shutdown ownership. `DriveRuntimeServices` starts services only;
shutdown remains in `GdfuseFlow.shutdown`.

## Production Wiring

In `src/drive.ml`, add:

```ocaml
module DriveRuntimeServicePorts = struct
  let start_flush_db_thread = MemoryCache.start_flush_db_thread
  let start_async_upload_thread = UploadQueue.start_async_upload_thread

  let start_folder_fetching_thread =
    BackgroundFolderFetching.start_folder_fetching_thread

  let upload_resource_by_id = upload_resource_by_id
  let read_dir = read_dir
end

module RuntimeServiceOps =
  DriveRuntimeServices.Make (DriveRuntimeServicePorts)

let drive_runtime_services_runtime () =
  let context = Context.get_ctx () in
  {
    DriveRuntimeServices.cache = context.Context.cache;
    config = context |. Context.config_lens;
  }

let init_filesystem () =
  RuntimeServiceOps.init_filesystem (drive_runtime_services_runtime ())
```

The exact module name can follow local naming style. Keep
`src/drive.mli` stable for this pass.

## Implementation Steps

1. Create `driveRuntimeServices.mli` with `runtime`, `PORTS`, and functor
   signatures.
2. Create `driveRuntimeServices.ml`.
3. Move the `init_filesystem` startup branch policy into the new module.
4. Wire `DriveRuntimeServices.Make` into `src/drive.ml`.
5. Keep `Drive.init_filesystem` as the public no-argument wrapper.
6. Add `test/testDriveRuntimeServices.ml`.
7. Register the suite in `test/testSuite.ml`.
8. Run `tools/format_ocaml` or `ocamlformat` on touched OCaml files.
9. Run `dune build @install` and `dune runtest` sequentially.

## Unit Test Plan

Use fake ports with trace lists and captured callbacks.

Cover startup policy:

- flush-db startup is always requested
- async upload startup is skipped when `async_upload_queue = false`
- async upload startup uses `config.async_upload_threads` when enabled
- async upload startup receives the `upload_resource_by_id` callback
- background folder fetching startup is skipped when
  `background_folder_fetching = false`
- background folder fetching startup receives a callback that calls
  `read_dir path` and ignores its result
- when both optional services are enabled, calls occur in flush, async,
  background order

Existing FUSE boundary tests should continue to call the public
`Drive.init_filesystem` wrapper shape.

## Acceptance Criteria

- `src/drive.ml` no longer contains runtime service startup branch policy.
- `DriveRuntimeServices` owns startup orchestration for flush-db, async upload,
  and background folder fetching services.
- Existing `Drive.init_filesystem` public call shape remains available.
- Focused unit tests cover enabled/disabled startup branches and callback
  wiring without starting real threads.
- `dune build @install` and `dune runtest` pass.

## Agent Docs Follow-Up

After implementation, update agent docs to describe the current state directly:

- `docs/agent-docs/architecture.md`
- `docs/agent-docs/repo-map.md`
- `docs/agent-docs/drive-init-filesystem.md`
- `docs/agent-docs/background-folder-fetching-start-thread.md`
- `docs/agent-docs/upload-queue-start-async-upload-thread.md`
- `docs/agent-docs/drive-upload-resource-by-id.md`

Avoid before/after language. The docs should describe `DriveRuntimeServices`
as the current implementation boundary for Drive-specific runtime service
startup at FUSE init time.
