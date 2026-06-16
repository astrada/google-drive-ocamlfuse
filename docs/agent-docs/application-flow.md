# Application Flow (`gdfuse`)

## Purpose

The `gdfuse` executable is split between library-visible orchestration code
in `src/` and thin production wiring in `bin/`.

The important modules are:

- `src/gdfuseCommon.ml`: shared startup types and config/state helpers
- `src/gdfuseCli.ml`: CLI parsing and explicit parse outcomes
- `src/gdfuseApp.ml`: top-level CLI runner over parse outcomes and flow
  dispatch
- `src/gdfuseFlow.ml`: startup, auth bootstrap, shutdown, and mount-mode
  orchestration behind a functorized dependency boundary
- `bin/gdfuseRuntimeDeps.ml`: production dependency implementation for browser,
  auth, cache, CURL, `at_exit`, and FUSE startup
- `bin/gdfuseFuse.ml`: FUSE callback table and exception mapping
- `bin/gdfuse.ml`: top-level entrypoint that instantiates the real flow module

Filesystem semantics are implemented in `Drive`. The application-flow
code only prepares runtime state and hands off to FUSE.

## Flow At A Glance

1. Parse CLI options and mount options in `GdfuseCli.parse`.
2. `GdfuseApp` handles parse outcomes for version, help, and CLI errors.
3. Instantiate `GdfuseFlow.Make(GdfuseRuntimeDeps)` in the executable.
4. If no mountpoint was passed, run bootstrap-only mode with `"."`.
5. Otherwise run full mount mode.
6. In mount mode, register shutdown via injected `register_exit`.
7. Start the filesystem through injected FUSE startup.
8. Let FUSE callbacks forward requests to `Drive`.

The "no mountpoint" branch is important: it performs setup and authorization
without starting the FUSE loop. That is effectively a bootstrap/auth-only mode.

## Module Responsibilities

### Entry Point

[gdfuse.ml](/home/alex/src/google-drive-ocamlfuse/bin/gdfuse.ml) is very
thin. It:

- instantiates the real flow module
- instantiates the real CLI runner
- calls the CLI runner

### Top-Level CLI Runner

[gdfuseApp.ml](/home/alex/src/google-drive-ocamlfuse/src/gdfuseApp.ml) owns
the top-level user-visible CLI behavior above the flow module.

It:

- matches on `GdfuseCli.parse_result`
- prints version/help output
- prints parse errors without surfacing uncaught `Arg` exceptions
- dispatches successful parses to bootstrap-only or mount mode
- keeps runtime `Failure` handling on the `Error: ...` stderr path

### CLI

[gdfuseCli.ml](/home/alex/src/google-drive-ocamlfuse/src/gdfuseCli.ml) owns all
command-line parsing.

Important behavior:

- `fuse_args` starts with `-f`
- foreground mode is forced unconditionally
- this is intentional: on OCaml 5+, calling `fork()` after `caml_main()` is not
  safe, so the old daemonizing path should not be relied on
- there is no separate `-f` CLI option anymore
- `-debug` enables verbose logging
- default mount mode is multithreaded and sets app-level `multi_threading`
- `-m` is accepted as an explicit, idempotent multithreaded-mode request
- `-s` prepends the FUSE `-s` option and clears app-level `multi_threading`
- `-o` is split by commas
- `gdfroot=...` is handled specially and stored in `base_dir`
- all remaining mount options are forwarded into the FUSE argv

`parse_argv` returns an explicit outcome:

- `Parsed parsed` for successful flow execution
- `Show_version` for the version-only path
- `Help text` for help output
- `Error text` for CLI parse failures

That keeps help/version/error handling testable without relying on uncaught
`Arg.Help` or `Arg.Bad` exceptions.

### Library Flow

[gdfuseFlow.ml](/home/alex/src/google-drive-ocamlfuse/src/gdfuseFlow.ml) owns
the orchestration logic.

Its public entrypoints are:

- `setup_application`
- `run_bootstrap_only`
- `run_mount_mode`
- `shutdown`

It is functorized over `DEPS`, but the boundary is intentionally coarse:

- `System`: browser launch, log-channel opening, CURL init/cleanup, `exit`,
  and `at_exit`
- `Auth`: OAuth and GAE-proxy operations
- `Cache`: cache lifecycle hooks and clean-shutdown flags
- `Fuse`: start-filesystem handoff

`AppDir`, `ConfigStore`, `Context.StateFileStore`, and `Context` are
directly. Tests rely on the real in-memory `Context` and clean it up between
cases.

## `setup_application`

`GdfuseFlow.Make(...).setup_application` performs the runtime bootstrap before
FUSE starts.

### 1. Validate mountpoint

It first checks that `params.mountpoint` exists and is a directory.

### 2. Resolve config path and logging

The flow then:

1. resolves the config path with `AppDir.get_config_path`
2. loads or creates config with `ConfigStore.load_or_create`
3. derives `AppDir.t`
4. creates the application directories
5. opens the configured log channel through injected system deps

### 3. Resolve runtime config

`ConfigRuntime.resolve` merges persisted config with CLI overrides and runtime
mode flags, producing:

- `runtime_config`
- `persisted_config`
- `should_persist`
- `clear_cache`

If persistence is required, the config store is saved back to disk.

### 4. Build GAPI auth config

`Config.create_gapi_config` constructs the base Google API config.

In GAE-proxy mode, the OAuth refresh callback is rewritten so refresh goes
through the injected GAE-proxy dependency path.

### 5. Load state and initialize cache

The flow then:

1. loads or creates the state file via `Context.StateFileStore`
2. creates the cache handle through injected cache deps
3. optionally clears cache for docs-mode changes or explicit `-cc`
4. clears cache again on saved-version mismatch and updates state
5. initializes the cache DB
6. initializes CURL through injected system deps
7. creates memory buffers

### 6. Install global `Context`

A `Context.t` is built and installed with `Context.set_ctx`.

That global runtime state contains:

- app-dir paths
- runtime config store
- state store
- cache handle
- gapi/curl state
- mountpoint metadata
- thread slots
- memory buffers
- per-file lock table

### 7. Recover from dirty shutdown

The flow checks the cache clean-shutdown flag through injected cache deps.

- if the previous run shut down cleanly, the flag is reset for the new session
- otherwise the cache is rebuilt unless it was already being cleared

### 8. Acquire credentials

Credential bootstrap has three branches:

1. service-account mode
2. user OAuth with an existing refresh token
3. user OAuth without a refresh token

The no-refresh-token branch selects among:

- GAE-proxy flow
- standard OAuth flow
- failure for missing client id/secret

### 9. Validate access token once

After credential acquisition, the flow immediately forces one refresh attempt:

- GAE-proxy mode uses injected GAE refresh
- regular OAuth mode uses injected OAuth refresh
- service-account mode is a no-op

Failure aborts startup.

## Mount And Shutdown

`run_mount_mode` does three things in order:

1. `setup_application`
2. register `shutdown` through injected `register_exit`
3. call injected FUSE startup

`shutdown`:

1. reads the current global context
2. stops and joins any background threads present in the context
3. flushes cache through injected cache deps
4. stores the clean-shutdown flag through injected cache deps
5. runs injected CURL cleanup
6. clears the global `Context`

## FUSE Boundary

[gdfuseFuse.ml](/home/alex/src/google-drive-ocamlfuse/bin/gdfuseFuse.ml)
is the production FUSE adapter layer.

It:

- maps `Drive` exceptions into Unix/FUSE errors
- logs each request
- forwards operations to `Drive`
- prepends `"."` and `".."` in `readdir`
- builds the final `Fuse.main` argv from executable name, `fuse_args`, and
  mountpoint
- converts native FUSE 3 callback details such as `file_info`, `utimens`
  timestamps, rename flags, and directory entries before calling `Drive`

## Testability

The main reason for the `GdfuseFlow` split is unit testing.

Tests can now:

- instantiate `GdfuseFlow.Make(FakeDeps)` from `test/`
- use real `AppDir`, `ConfigStore`, `Context.StateFileStore`, and `Context`
- fake browser/auth/CURL/FUSE/`exit`/`at_exit`
- assert event ordering for bootstrap, mount, auth, and shutdown behavior

Because `Context` and several `Utils` refs are process-global, flow tests must
reset runtime globals before and after each case.

## Practical Reading Guide

When changing startup behavior, read these modules in order:

1. [gdfuse.ml](/home/alex/src/google-drive-ocamlfuse/bin/gdfuse.ml)
2. [gdfuseCli.ml](/home/alex/src/google-drive-ocamlfuse/src/gdfuseCli.ml)
3. [gdfuseFlow.ml](/home/alex/src/google-drive-ocamlfuse/src/gdfuseFlow.ml)
4. [gdfuseRuntimeDeps.ml](/home/alex/src/google-drive-ocamlfuse/bin/gdfuseRuntimeDeps.ml)
5. [gdfuseFuse.ml](/home/alex/src/google-drive-ocamlfuse/bin/gdfuseFuse.ml)
6. [drive.ml](/home/alex/src/google-drive-ocamlfuse/src/drive.ml)

When debugging auth issues, focus on:

- `GdfuseCommon.get_authorization_url`
- `get_auth_tokens_from_server` inside `GdfuseFlow`
- the credential branch inside `setup_application`
- [oauth2.ml](/home/alex/src/google-drive-ocamlfuse/src/oauth2.ml)
- [gaeProxy.ml](/home/alex/src/google-drive-ocamlfuse/src/gaeProxy.ml)

When debugging mount/runtime issues, focus on:

- context creation in `GdfuseFlow`
- dirty-shutdown recovery
- shutdown ordering
- `GdfuseFuse.handle_exception`
  see `docs/agent-docs/gdfuse-fuse-boundary.md`
- the specific `Drive.*` callback being exercised
