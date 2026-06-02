# Drive Test Support Refactor

## Goal

Reduce duplicated setup code in the Drive unit tests without hiding each
module's dependency boundary.

The target is shared test scaffolding that is repeated across many
`test/testDrive*.ml` modules:

- session construction and `run_session`
- dummy cache records
- common runtime constructors
- common resource and file builders
- simple event-trace assertions

The refactor should keep the tests explicit about each module's fake `PORTS`
implementation. It should not introduce a broad fake Drive environment or make
test behavior harder to inspect locally.

## Current Duplication

Many Drive tests define the same basic fixtures:

- a no-auth `GapiConversation.Session.t`
- `run_session m = fst (m session)`
- an in-memory `dummy_cache`
- `default_runtime` helpers for shared Drive runtime shapes
- `make_resource` wrappers around `Drive.create_resource`
- `make_file` wrappers around `GapiDriveV3Model.File.empty`
- event traces using `trace : string list ref`
- ordering assertions such as `assert_before`
- negative event assertions such as `assert_no_event`

The fake ports themselves are also similar, but they encode module-specific
dependencies and should remain local unless a smaller helper clearly improves
readability.

## Design Principles

- Keep fake `PORTS` modules local to each test file.
- Extract only boring, repeated test infrastructure.
- Prefer small helpers over a central fake Drive runtime.
- Keep resource builders flexible enough for existing tests, but do not try to
  encode every possible resource field up front.
- Preserve current test names and behavioral assertions.
- Migrate incrementally so each batch remains easy to validate with
  `dune runtest`.

## Proposed Test Support Module

Add `test/driveTestSupport.ml`.

Suggested initial contents:

```ocaml
val session : GapiConversation.Session.t
val run_session : 'a GapiMonad.SessionM.m -> 'a
val dummy_cache : CacheData.t

val base_runtime : ?config:Config.t -> unit -> DriveRuntime.base
val cache_only_runtime : unit -> DriveRuntime.cache_only
val config_only_runtime : ?config:Config.t -> unit -> DriveRuntime.config_only

val make_resource :
  ?id:int64 ->
  ?remote_id:string option ->
  ?name:string ->
  ?mime_type:string ->
  ?size:int64 ->
  ?trashed:bool ->
  ?state:CacheData.Resource.State.t ->
  string ->
  CacheData.Resource.t

val make_file :
  ?id:string ->
  ?name:string ->
  ?mime_type:string ->
  ?size:int64 ->
  ?version:int64 ->
  unit ->
  GapiDriveV3Model.File.t

module Trace : sig
  type t

  val create : unit -> t
  val reset : t -> unit
  val record : t -> string -> unit
  val events : t -> string list
  val assert_before : string -> string -> string list -> unit
  val assert_no_event : string -> string list -> unit
end
```

Use the support module from tests as `DriveTestSupport`, following Dune's
normal test executable module discovery.

## Implementation Phases

### 1. Add Core Support Helpers

Create `test/driveTestSupport.ml` with:

- `session`
- `run_session`
- `dummy_cache`
- runtime constructors for `DriveRuntime.base`, `cache_only`, and
  `config_only`
- `Trace` helpers

Acceptance criteria:

- No production code changes.
- Existing tests still pass before any large migration.
- `dune runtest` passes.

### 2. Migrate Low-Risk Runtime Setup

Replace duplicated session/cache/runtime helpers in tests that already use
`DriveRuntime` aliases directly.

Good first targets:

- `test/testDriveReads.ml`
- `test/testDriveOpens.ml`
- `test/testDriveRemoteUpdates.ml`
- `test/testDriveUploadDispatch.ml`
- `test/testDriveXattrs.ml`

Acceptance criteria:

- Fake `PORTS` modules remain local.
- Test assertions and test names do not change.
- `dune runtest` passes.

### 3. Add And Migrate Common Builders

Add `make_resource` and `make_file` only after the first migration shows the
support module is useful.

Good targets:

- `test/testDriveReads.ml`
- `test/testDriveRemoteUpdates.ml`
- `test/testDriveUploads.ml`
- `test/testDriveMetadataMutations.ml`
- `test/testDriveXattrs.ml`

Acceptance criteria:

- Builders reduce repeated default fields without obscuring the fields that
  matter to each test.
- Tests can still override important fields explicitly.
- `dune runtest` passes.

### 4. Migrate Trace Assertions

Move repeated trace ordering and absence assertions into
`DriveTestSupport.Trace`.

Good targets:

- `test/testDriveReads.ml`
- `test/testDriveRemoteUpdates.ml`
- `test/testDriveUploads.ml`
- `test/testDriveFileMutations.ml`
- `test/testDriveMutations.ml`

Acceptance criteria:

- Trace event strings remain local to the fake ports.
- Shared helpers cover only generic list assertions and trace storage.
- `dune runtest` passes.

### 5. Review Remaining Duplication

After the safe helpers are in place, review the remaining duplication and decide
whether additional helpers are worthwhile.

Likely deferred helpers:

- fake resource stores backed by `Hashtbl`
- queued response helpers for `get_resource`
- common path-normalization tracing

Acceptance criteria:

- No helper is added unless it makes at least two test modules clearer.
- Tests remain easier to read than the duplicated version.

## Non-Goals

- Do not extract whole fake `PORTS` modules.
- Do not introduce a shared fake Drive API.
- Do not change production modules.
- Do not change behavior under test.
- Do not collapse module-specific test setup into one opaque fixture.

## Test Strategy

Run after each phase:

```sh
dune runtest
```

For larger migrations, also run:

```sh
dune build @install
```

## Completion Criteria

- Common Drive test setup lives in `test/driveTestSupport.ml`.
- Repeated session/cache/runtime setup is removed from migrated tests.
- Repeated resource/file builder defaults are centralized where useful.
- Trace assertion helpers are centralized where useful.
- Fake `PORTS` modules remain module-specific.
- `dune runtest` passes.
