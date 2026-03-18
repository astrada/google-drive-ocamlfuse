# Plan: Refactor `gdfuse.ml` For Readability

## Summary

Refactor `bin/gdfuse.ml` as a behavior-preserving readability pass. Keep
`gdfuse.ml` as the executable entrypoint, but split its current responsibilities
into a small set of focused helpers so the top-level flow reads as: parse CLI,
build params, run setup, optionally mount, register shutdown.

## Implementation Changes

- Keep `bin/gdfuse.ml` as the only executable entrypoint and preserve all CLI
  flags, startup branches, auth branches, logging, cache behavior, FUSE
  callbacks, and shutdown behavior exactly as today.
- Split `setup_application` into named helpers with a linear orchestration
  function:
  - `validate_mountpoint`
  - `resolve_paths_and_logging`
  - `resolve_runtime_config`
  - `build_gapi_config`
  - `load_state_and_cache`
  - `build_context`
  - `recover_from_dirty_shutdown`
  - `ensure_credentials`
  - `validate_initial_access_token`
- Replace the nested auth helper with a top-level helper that makes the
  request-id lifecycle explicit and keeps the GAE-proxy branch isolated from the
  rest of startup.
- Replace the large `let () =` body with small helpers:
  - `parse_mount_options`
  - `build_arg_specs`
  - `parse_cli`
  - `build_application_params`
  - `run_bootstrap_only`
  - `run_mount_mode`
  - `quit_with_error`
- Extract shutdown logic into a helper module or a dedicated section with:
  - `stop_background_threads`
  - `flush_and_mark_clean_shutdown`
  - `cleanup_curl_and_context`
  - one public `shutdown ()` function used by `at_exit`
- Reduce repetitive FUSE wrappers by introducing a small helper layer for
  "log + call `Drive.*` + map exceptions", while keeping special cases separate:
  - generic path operations
  - path-plus-flags operations
  - path-plus-handle operations
  - custom wrappers for `read`, `write`, and `readdir`
- If helper modules are created, keep the split small and local:
  - `GdfuseCli` for CLI parsing and params construction
  - `GdfuseSetup` for startup/config/auth/context assembly
  - `GdfuseFuse` for callback registration and exception mapping
  - `GdfuseShutdown` for exit cleanup
- Prefer internal modules under `bin/` if Dune supports them cleanly for this
  executable; use `src/` only if the repository already expects these helpers
  to be reusable outside the executable.

## Public Interfaces / Behavior Constraints

- Do not change any CLI flag names, defaults, parsing behavior, or `Fuse.main`
  argv construction.
- Do not change `application_params` field meanings; renaming is acceptable
  only if all call sites remain local and semantics stay identical.
- Do not change logging messages unless needed to keep extracted helpers
  readable; if changed, keep wording and timing effectively equivalent.
- Do not change any exception-to-`Unix_error` mappings in the FUSE boundary.
- Do not change auth mode precedence among service account, refresh-token reuse,
  GAE-proxy flow, device flow, headless flow, or loopback/browser flow.
- Do not change shutdown ordering for thread stop, cache flush, clean-shutdown
  flag, CURL cleanup, and context clear.

## Test Plan

- Build the executable and confirm the refactor compiles without interface
  drift.
- Run the existing test suite if available, plus at least one executable smoke
  check for `-version`.
- Verify CLI parsing parity for:
  - no mountpoint
  - mountpoint present
  - `-debug`
  - `-s` and `-m`
  - `-o gdfroot=...,...`
  - `-docsmode`
- Verify startup parity for:
  - existing config/state
  - missing state file
  - version mismatch triggering cache cleanup
  - dirty-shutdown recovery path
  - refresh-token-present path
  - missing client-id/client-secret failure path
  - service-account path
- Verify FUSE wrapper parity by confirming the same `Drive` functions are still
  registered and the same exception mappings are preserved for `ENOENT`,
  `EACCES`, `ENOTEMPTY`, `EIO`, `EEXIST`, and `EINVAL`.
- Verify shutdown parity by checking that all optional threads are still stopped
  and joined before cache flush and cleanup.

## Assumptions

- Default chosen: small split, not a broad architectural rewrite.
- Default chosen: no behavior changes; this is a pure readability refactor.
- The follow-up implementation should also update
  `docs/agent-docs/application-flow.md` if the refactor materially changes where
  startup logic lives.
