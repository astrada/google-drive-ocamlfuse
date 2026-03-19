# Plan: Make `gdfuse` Application Flow Unit-Testable

## Summary

Refactor the `gdfuse` startup/shutdown orchestration so it can be unit tested
from `test/` without invoking real filesystem side effects, auth flows, CURL,
FUSE, browser launches, or process exit.

The key architectural change is to move the testable flow out of `bin/` and
into `src/`, because the existing test executable links only the library
(`google_drive_ocamlfuse`) and cannot directly exercise executable-private
modules under `bin/`.

Use one coarse functorized dependency boundary for orchestration, not a large
number of tiny functors around leaf modules. Keep `AppDir`, `ConfigStore`,
`Context.StateFileStore`, and `Context` real in tests; inject only the truly
external or process-wide effects.

## Implementation Changes

- Move the shared application-flow types and helpers currently needed by startup
  orchestration out of `bin/` and into library-visible modules under `src/`.
  At minimum this includes `application_params` and any shared request-id /
  config-state helpers currently living in `GdfuseCommon`.
- Introduce a new library module pair:
  - `src/gdfuseFlow.ml`
  - `src/gdfuseFlow.mli`
- In `src/gdfuseFlow.mli`, define:
  - `module type DEPS`
  - `module Make (D : DEPS) : sig ... end`
- Keep the functor surface coarse. `DEPS` should be organized as a few semantic
  submodules, not one function per existing concrete module:
  - `System`: browser launch, logging channel setup, process exit, `at_exit`,
    and CURL init/cleanup
  - `Auth`: OAuth and GAE-proxy auth operations
  - `Cache`: cache creation, cleanup, DB setup, flush, and clean-shutdown flags
  - `Fuse`: start-filesystem hook for the mounted path
- In `src/gdfuseFlow.Make`, implement the behavior currently spread across:
  - `bin/gdfuse.ml`
  - `bin/gdfuseSetup.ml`
  - `bin/gdfuseShutdown.ml`
- The public flow API should expose only the orchestration entrypoints needed by
  production and tests:
  - `setup_application`
  - `run_bootstrap_only`
  - `run_mount_mode`
  - `shutdown`
- Keep `bin/gdfuseFuse.ml` as the concrete FUSE adapter. It does not need to be
  unit tested through full FUSE execution for this refactor; production wiring
  can call it via the `Fuse.start_filesystem` dependency.
- Keep `bin/gdfuse.ml` thin. It should:
  - parse CLI
  - instantiate `GdfuseFlow.Make(RealDeps)`
  - dispatch to `run_bootstrap_only` or `run_mount_mode`
- Add a concrete production dependency module, either:
  - `bin/gdfuseRuntimeDeps.ml`, or
  - `src/gdfuseRealDeps.ml`
  Choose the location that keeps executable-only concerns out of tests while
  still letting `bin/gdfuse.ml` instantiate the functor cleanly.
- Use real `AppDir`, `ConfigStore`, `Context.StateFileStore`, and
  `Context.get_ctx` / `set_ctx` / `clear_ctx` inside the flow implementation.
- Do not functorize `AppDir`, `ConfigStore`, `Context`, `Oauth2`, `GaeProxy`,
  or `Cache` individually. Wrap only the external/process-wide orchestration
  effects behind the coarse `DEPS` boundary.
- Keep real filesystem behavior for deterministic path/config/state helpers
  when a temp directory is enough. Fake only the dangerous or hard-to-control
  boundaries: browser, network/auth, CURL, FUSE, `exit`, and `at_exit`.
- Add `GdfuseCli.parse_argv : string array -> parsed` and redefine `parse ()`
  as `parse_argv Sys.argv`. This is a small, independent improvement that makes
  parser tests possible without relying on global process argv.

## Public Interfaces / Behavioral Constraints

- Preserve all current startup, shutdown, auth, cache, and FUSE behavior.
- Preserve the current intentional foreground-only behavior in CLI parsing:
  `fuse_args` starts with `-f` and `-obig_writes`.
- Preserve the current branch structure:
  - no mountpoint -> setup only, no FUSE loop
  - mountpoint present -> setup, register shutdown, start FUSE
- Preserve current error behavior and messages closely enough that existing
  operator-facing flow remains recognizable.
- Keep `Context` semantics unchanged in production and in tests.
- Replace hard process termination with an injectable `exit` dependency inside
  the flow module. Production should still terminate; tests should raise a
  dedicated exception such as `Exit_called of int`.
- Replace direct `at_exit` registration with an injectable `register_exit`
  dependency so tests can assert the callback was registered without mutating
  global process state.
- Tests must clear the real global `Context` before and after each case to keep
  isolation.

## Test Plan

- Add `test/testGdfuseFlow.ml` and register it from
  `test/testSuite.ml`.
- Build unit tests around a fake dependency module that records an ordered event
  trace in a mutable list. Assert both branch selection and effect ordering.
- Use temp directories for config/state paths where using real
  `ConfigStore`/`AppDir`/`Context.StateFileStore` is simpler than mocking
  filesystem behavior.
- Use fake implementations for:
  - browser launch
  - OAuth token acquisition / refresh
  - GAE proxy polling / refresh
  - CURL init / cleanup
  - FUSE startup
  - `exit`
  - `at_exit`
  - cache maintenance hooks
- Add a test helper that resets process-global runtime state before and after
  each case:
  - `Context.clear_ctx ()`
  - `Utils.verbose := false`
  - `Utils.debug_buffers := false`
  - `Utils.max_retries := Config.default.Config.max_retries`
  - restore any overridden log-channel state as needed
- Keep these tests serial; they should not run concurrently because `Context`
  and several `Utils` refs are process-global.
- Add unit tests for these scenarios:
  - bootstrap-only mode does not call FUSE startup
  - mount mode registers shutdown and starts FUSE
  - existing refresh token skips interactive auth
  - service-account mode skips user OAuth flow
  - GAE-proxy mode launches browser and polls server
  - missing client id/secret raises the current failure path
  - docs-mode change requests cache cleanup
  - version mismatch rewrites saved version and clears cache
  - dirty shutdown triggers cache recovery path
  - refresh-token validation failure exits with the expected code
  - shutdown stops background work, flushes cache, marks clean shutdown, and
    clears context in the current order
- If `parse_argv` is added, add `test/testGdfuseCli.ml` for:
  - `-debug`
  - `-d`
  - `-s`
  - `-o gdfroot=...`
  - mountpoint vs no mountpoint
  - preservation of forced `-f`

## Assumptions

- This refactor targets application-flow unit testing, not full FUSE or network
  integration testing.
- The preferred testing style remains OUnit, consistent with the current test
  suite.
- The dependency boundary should be coarse and semantic; a traceable fake
  orchestration environment is more valuable here than a highly abstract
  fine-grained mock framework.
- Real `Context` usage in tests is acceptable because it is an in-memory global
  store; isolation will be enforced with explicit cleanup around each test.
