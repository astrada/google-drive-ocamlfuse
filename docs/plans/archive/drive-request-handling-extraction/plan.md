# Extract Drive Request Handling

## Goal

Move Drive request exception translation and default retry wrapping out of
`src/drive.ml` and into a focused `DriveRequestHandling` module.

The extraction target is:

- `match_service_error`
- `handle_default_exceptions`
- `try_with_default`
- `with_retry_default`

The production behavior should keep the public `Drive` helper names and call
shapes because Drive API adapters, download handling, metadata refresh,
resource resolution, upload paths, and mutation ports already consume these
helpers from `Drive`.

## Current Problem

Request handling is central policy, but it currently sits in the top of
`Drive` next to unrelated constants and resource helpers. The block mixes:

- Google service-error reason matching
- Google request exception mapping into repository Drive exceptions
- translation of retryable request failures into `Utils.Temporary_error`
- retry-limit and exponential-backoff behavior
- monadic `try_with_m` wiring

This behavior is high-impact because it is used around most Google Drive API
requests. It should have direct unit coverage without invoking OAuth, real
HTTP, cache state, or global `Context`.

## Proposed Shape

Add:

- `src/driveRequestHandling.ml`
- `src/driveRequestHandling.mli`
- `test/testDriveRequestHandling.ml`

Expose:

```ocaml
exception File_not_found
exception IO_error
exception Invalid_operation
exception Permission_denied

module type PORTS = sig
  val max_retries : unit -> int
  val wait_exponential_backoff : int -> unit
end

module Make (P : PORTS) : sig
  val match_service_error : string -> exn -> bool
  val handle_default_exceptions : exn -> 'a GapiMonad.SessionM.m
  val try_with_default : 'a GapiMonad.SessionM.m -> 'a GapiMonad.SessionM.m
  val with_retry_default : 'a GapiMonad.SessionM.m -> 'a GapiMonad.SessionM.m
end

val match_service_error : string -> exn -> bool
val handle_default_exceptions : exn -> 'a GapiMonad.SessionM.m
val try_with_default : 'a GapiMonad.SessionM.m -> 'a GapiMonad.SessionM.m
val with_retry_default : 'a GapiMonad.SessionM.m -> 'a GapiMonad.SessionM.m
```

The top-level values should use production ports:

- `max_retries () = !Utils.max_retries`
- `wait_exponential_backoff = GapiUtils.wait_exponential_backoff`

The functor exists only to make retry behavior testable without sleeping or
mutating the global retry limit.

## Extracted Behavior

### Service-Error Matching

`match_service_error reason exn` should preserve the current behavior:

- return `true` when `exn` is `GapiService.ServiceError` and the first
  `GapiError.SingleError.reason` equals `reason`
- return `false` when the service error has no errors
- return `false` when the first error reason differs
- return `false` for non-service exceptions

Only the first error is considered. Do not broaden matching to later errors in
this extraction.

### Default Exception Translation

`handle_default_exceptions exn` should preserve the current mapping:

- service-error reasons `userRateLimitExceeded`, `rateLimitExceeded`,
  `backendError`, and `downloadQuotaExceeded` become `Utils.Temporary_error`
- service-error reasons `insufficientFilePermissions` and
  `insufficientPermissions` become `Permission_denied`
- service errors with no first error become `IO_error`
- other service-error reasons become `IO_error`
- `GapiRequest.PermissionDenied` becomes `Permission_denied`
- `GapiRequest.RequestTimeout` becomes `Utils.Temporary_error`
- `GapiRequest.PreconditionFailed` and `GapiRequest.Conflict` become
  `Utils.Temporary_error`
- `GapiRequest.Forbidden` becomes `IO_error`
- `GapiRequest.Gone` becomes `IO_error`
- `GapiRequest.BadRequest` becomes `Utils.Temporary_error`
- `Buffering.Invalid_block` becomes `Invalid_operation`
- `GapiRequest.NotFound` becomes `File_not_found`
- all other exceptions are re-raised unchanged

Keep the existing log messages and service-error JSON logging.

### Retry Wrappers

`try_with_default request` should remain:

- `Utils.try_with_m request handle_default_exceptions`

`with_retry_default request` should keep the intended retry policy:

- execute `request`
- normalize failures through `handle_default_exceptions`
- retry only when the normalized exception is `Utils.Temporary_error`
- wait with exponential backoff before each retry
- log `Retrying (n/max).` after incrementing the retry count
- when retries are exhausted, raise `IO_error`
- let non-temporary translated exceptions propagate

Do not change the global `with_retry` helper used by upload materialization in
this pass. That helper has cache-refresh behavior and belongs to a later upload
worker extraction.

## Production Wiring

In `src/drive.ml`, replace the helper bodies with aliases:

```ocaml
let match_service_error = DriveRequestHandling.match_service_error
let handle_default_exceptions = DriveRequestHandling.handle_default_exceptions
let try_with_default = DriveRequestHandling.try_with_default
let with_retry_default = DriveRequestHandling.with_retry_default
```

Keep `src/drive.mli` stable for this pass. These helpers are internal to
production wiring except where existing tests reach them through `Drive` call
paths.

## Implementation Steps

1. Create `driveRequestHandling.mli` with exception aliases, `PORTS`, functor,
   and top-level helper signatures.
2. Create `driveRequestHandling.ml` with the current translation behavior and
   testable retry ports.
3. Replace the request-handling helper bodies in `Drive` with aliases to
   `DriveRequestHandling`.
4. Keep existing production port signatures unchanged.
5. Add `test/testDriveRequestHandling.ml`.
6. Register the suite in `test/testSuite.ml`.
7. Run `tools/format_ocaml` or `ocamlformat` on touched OCaml files.
8. Run `dune build @install` and `dune runtest` sequentially.

## Unit Test Plan

Use synthetic `GapiService.ServiceError` values, direct GAPI request
exceptions, and a fake retry-port module.

Cover service-error matching:

- matching first reason returns true
- different first reason returns false
- service error with no errors returns false
- non-service exception returns false

Cover exception translation:

- retryable service-error reasons map to `Utils.Temporary_error`
- permission service-error reasons map to `Permission_denied`
- unknown and empty service errors map to `IO_error`
- `GapiRequest.PermissionDenied` maps to `Permission_denied`
- timeout, precondition, conflict, and bad-request exceptions map to
  `Utils.Temporary_error`
- forbidden and gone map to `IO_error`
- invalid buffer blocks map to `Invalid_operation`
- not found maps to `File_not_found`
- unknown exceptions propagate unchanged

Cover retry behavior with fake ports:

- successful requests return without waiting
- temporary failures are retried until success
- the wait port receives retry indexes in order
- exhausting the retry limit raises `IO_error`
- non-temporary translated exceptions are not retried

Existing tests for metadata refresh, resource resolution, downloads, uploads,
mutations, xattrs, and Drive views should continue to pass through the `Drive`
wrappers.

## Acceptance Criteria

- `src/drive.ml` no longer contains request exception translation or default
  retry-wrapper bodies.
- `DriveRequestHandling` owns service-error matching, default exception
  translation, and default retry behavior.
- Existing `Drive` helper names and production call shapes remain available.
- Focused unit tests cover service-error matching, exception mapping, retry
  success, retry exhaustion, and non-temporary no-retry behavior without real
  network calls or sleeping.
- `dune build @install` and `dune runtest` pass.

## Agent Docs Follow-Up

After implementation, update agent docs to describe the current state directly:

- `docs/agent-docs/architecture.md`
- `docs/agent-docs/repo-map.md`
- `docs/agent-docs/drive-upload-resource-with-retry.md`
- `docs/agent-docs/drive-upload-path.md`
- `docs/agent-docs/drive-download-resource.md`

Avoid before/after language. The docs should describe `DriveRequestHandling`
as the current implementation boundary for Drive request exception translation
and default retry behavior.
