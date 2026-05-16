# Extract Drive Upload Worker Bridge

## Goal

Move the upload worker bridge helpers out of `src/drive.ml` and into a focused,
port-driven `DriveUploadWorkerBridge` module.

The extraction target is:

- `upload_resource_with_retry`
- `upload_resource_by_id`

The production behavior should stay unchanged. `src/drive.ml` should keep the
same local helper names because runtime-service startup and upload dispatch
ports already refer to them.

## Current Problem

The concrete upload attempt already lives in `DriveUploads`, and path-level
upload dispatch already lives in `DriveUploadDispatch`. The remaining
Drive-level worker bridge still mixes several concerns:

- flushing memory buffers before an upload attempt
- wrapping the concrete upload in default exception translation
- applying the per-resource retry loop
- reloading queued resources by cache row id
- executing a session request from the async worker callback
- logging missing queued resource rows

This behavior is small, but it is the point where async upload workers re-enter
the normal authenticated Drive request path. It should have focused tests
without requiring global `Context`, real upload queue state, or real Drive
requests.

## Proposed Shape

Add:

- `src/driveUploadWorkerBridge.ml`
- `src/driveUploadWorkerBridge.mli`
- `test/testDriveUploadWorkerBridge.ml`

Expose:

```ocaml
type runtime = { cache : CacheData.t }

module type PORTS = sig
  val flush_memory_buffers : CacheData.Resource.t -> unit

  val upload :
    CacheData.Resource.t -> unit GapiMonad.SessionM.m

  val try_with_default :
    unit GapiMonad.SessionM.m -> unit GapiMonad.SessionM.m

  val with_resource_retry :
    (CacheData.Resource.t -> unit GapiMonad.SessionM.m) ->
    CacheData.Resource.t ->
    unit GapiMonad.SessionM.m

  val select_resource_with_id :
    CacheData.t -> int64 -> CacheData.Resource.t option

  val run_request : unit GapiMonad.SessionM.m -> unit
  val log_missing_resource : int64 -> unit
end

module Make (P : PORTS) : sig
  val upload_resource_with_retry :
    CacheData.Resource.t -> unit GapiMonad.SessionM.m

  val upload_resource_by_id : runtime -> int64 -> unit
end
```

The module should own bridge control flow only. Production ports should own
global context lookup, `Oauth2.do_request`, cache access, the concrete upload
attempt, retry implementation, and logging side effects.

## Extracted Behavior

`upload_resource_with_retry resource` should preserve the current flow:

1. call `P.flush_memory_buffers resource`
2. call `P.with_resource_retry` with a callback that runs
   `P.try_with_default (P.upload refreshed_resource)`

The retry callback must use the resource supplied by the retry layer. That
preserves the existing behavior where `with_retry` can refresh metadata and
retry with an updated resource row.

`upload_resource_by_id runtime resource_id` should preserve the current flow:

1. call `P.select_resource_with_id runtime.cache resource_id`
2. if a resource row exists, run
   `P.run_request (upload_resource_with_retry resource)`
3. if no row exists, call `P.log_missing_resource resource_id` and return

Missing rows should still be non-fatal. Upload failures should still propagate
out of `run_request` so the upload queue can requeue the entry.

## Production Wiring

In `src/drive.ml`, add production ports for:

- `flush_memory_buffers`
- `upload`
- `try_with_default`
- `with_retry`
- `Cache.Resource.select_resource_with_id`
- `do_request request |> ignore`
- the existing missing-resource log message

Add a `drive_upload_worker_bridge_runtime ()` helper that reads `cache` from
`Context.get_ctx ()`.

Keep the existing local names:

```ocaml
let upload_resource_with_retry resource = ...
let upload_resource_by_id resource_id = ...
```

`DriveRuntimeServices` and `DriveUploadDispatch` should continue to refer to
those local helpers.

## Implementation Steps

1. Create `driveUploadWorkerBridge.mli`.
2. Create `driveUploadWorkerBridge.ml`.
3. Move worker-bridge orchestration from `src/drive.ml` into the new module.
4. Wire `DriveUploadWorkerBridge.Make` into `src/drive.ml`.
5. Keep existing callers in `DriveRuntimeServicePorts` and
   `DriveUploadDispatchPorts` stable.
6. Add `test/testDriveUploadWorkerBridge.ml`.
7. Register the suite in `test/testSuite.ml`.
8. Run `tools/format_ocaml` or `ocamlformat` on touched OCaml files.
9. Run `dune build @install` and `dune runtest` sequentially.

## Unit Test Plan

Use fake ports with trace lists and an in-memory fake resource table.

Cover:

- `upload_resource_with_retry` flushes memory buffers before entering retry
- `upload_resource_with_retry` wraps the concrete upload in
  `try_with_default`
- the retry callback uploads the resource supplied by the retry layer
- `upload_resource_by_id` reloads the current resource by cache id
- `upload_resource_by_id` executes the session request when the row exists
- `upload_resource_by_id` logs and returns when the row is missing
- upload failures propagate from `upload_resource_by_id`

## Acceptance Criteria

- `src/drive.ml` no longer owns upload worker bridge control flow.
- `DriveUploadWorkerBridge` owns flush/retry wrapping and queued-resource
  reload/execution behavior behind testable ports.
- Existing direct and async upload call shapes remain unchanged.
- Focused unit tests cover retry callback wiring, request execution, missing
  resource behavior, and failure propagation.
- `dune build @install` and `dune runtest` pass.

## Agent Docs Follow-Up

After implementation, update agent docs to describe the current state directly:

- `docs/agent-docs/architecture.md`
- `docs/agent-docs/repo-map.md`
- `docs/agent-docs/drive-upload-resource-with-retry.md`
- `docs/agent-docs/drive-upload-resource-by-id.md`
- upload path docs that mention the Drive-level worker helpers
