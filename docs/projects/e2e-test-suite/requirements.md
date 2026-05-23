# End-To-End Test Suite Requirements

This document captures the requirements for a future end-to-end test suite for
`google-drive-ocamlfuse`. The suite is intended to validate the installed
filesystem behavior through the executable and FUSE boundary, not internal module
behavior that is already covered by unit tests.

## Goals

- Provide a separate executable for end-to-end tests.
- Keep end-to-end tests out of the existing `dune runtest` unit-test flow.
- Exercise real mount lifecycle behavior: configure, mount, operate on files,
  unmount, and inspect results.
- Validate user-visible filesystem semantics through ordinary filesystem
  operations where possible.
- Keep test state isolated from a user's normal `~/.gdfuse` configuration,
  cache, tokens, and Drive content.
- Make failures diagnosable with logs, command output, mountpoint state, and the
  test case name.

## Non-Goals

- Replacing the existing OUnit unit tests.
- Running automatically in package builds, opam test runs, or default CI jobs.
- Testing every internal `Drive` helper directly.
- Depending on a developer's personal Google Drive files or default
  google-drive-ocamlfuse profile.
- Leaving mounted filesystems, temporary directories, credentials, or remote
  test files behind after normal failures.

## Execution Model

- The suite should be built as a separate OUnit2 executable under `test/e2e/`,
  for example `e2eSuite.exe`.
- The executable must not be registered under the existing `runtest` alias.
- A developer should run it explicitly, for example with `dune exec` or a
  dedicated `@e2e` alias.
- The executable should fail fast when required environment variables,
  credentials, FUSE support, or mount helpers are unavailable.
- The test runner should support selecting one test, a small group of tests, or
  the whole suite with a substring filter over OUnit case labels and per-case
  directory names.
- The runner should enforce per-test timeouts and a global timeout.
- Build and test commands that invoke dune should remain sequential because dune
  uses a lock file.

## Environment Requirements

- The host must support FUSE mounts for the current user.
- The google-drive-ocamlfuse executable under test must be built from the working
  tree, not discovered from the user's `PATH`.
- Each run must use a fresh temporary mountpoint and a fresh temporary
  application/config/cache directory.
- The test label, config path, log path, and cache location must be generated for
  the run and must not overlap with normal user profiles.
- The suite expects standard user-account OAuth credentials with the OAuth flow
  already completed before the run starts.
- The runner should seed the temporary e2e profile from a configured client id,
  client secret, and refresh token.
- Secrets and safe-area configuration should be supplied through
  `test/e2e/config.json`, a local JSON file ignored by git.
- A checked-in `test/e2e/config.template.json` file should document the required
  JSON shape.
- The suite must not print token values, OAuth client secrets, or full
  credential files to stdout, stderr, or logs.

## Configuration File

The default local configuration file should be `test/e2e/config.json`. It must
be ignored by git because it contains OAuth secrets.

The checked-in template should live at `test/e2e/config.template.json` and use
the same keys as the real config file:

```json
{
  "client_id": "replace-with-oauth-client-id",
  "client_secret": "replace-with-oauth-client-secret",
  "refresh_token": "replace-with-refresh-token",
  "test_folder_path": "/google-drive-ocamlfuse-e2e"
}
```

- `client_id`: OAuth client id for the standard user account.
- `client_secret`: OAuth client secret for that client id.
- `refresh_token`: refresh token produced by an already completed OAuth flow.
- `test_folder_path`: Drive folder path for the safe parent area that contains
  only disposable e2e run folders.

The runner supports `GDFUSE_E2E_CONFIG` as an environment variable override for
the local configuration path. The default path should be enough for normal local
developer use.

## Authentication Strategy

The initial authentication path is a standard Google user account. The OAuth
authorization flow is out of scope for the e2e executable: before the suite runs,
the account must already have a refresh token available through the e2e
configuration or environment.

The preferred account is a dedicated test user with access only to a disposable
Drive area. A developer account can be used only when the configured safe Drive
area is isolated from personal or project-maintainer files.

- Service accounts are not part of the initial authentication path.
- Device or browser OAuth can be supported later for manual setup, but should not
  be required during the test run.

The runner should use `gapi-ocaml` to refresh the access token and validate
credentials with a small Drive API preflight operation before attempting to
mount. If preflight fails, the suite should report an environment failure rather
than a filesystem regression.

## Direct Drive API Use

The runner should call the Drive API directly through `gapi-ocaml` for harness
responsibilities, not for ordinary filesystem behavior assertions.

Allowed direct API responsibilities:

- Refresh the access token and verify account access during preflight.
- Locate or create the configured safe parent folder.
- Create a unique remote root folder for each test run.
- Configure the temporary e2e profile to mount that run root through
  `root_folder`.
- Clean up the run root after successful and failed runs.
- Report remote ids and metadata needed for manual cleanup when automatic cleanup
  fails.
- Make targeted remote-side assertions only when the behavior cannot be observed
  reliably through the mounted filesystem.

Filesystem behavior tests should otherwise assert through the mounted
filesystem. This keeps the end-to-end suite focused on the executable, FUSE
boundary, cache, upload, and remount behavior users actually exercise.

## Remote Test Data Isolation

- The suite should require a safe Drive area: a dedicated parent folder that the
  e2e suite is allowed to create files in and recursively delete from.
- The safe area must not be the account's Drive root, a folder containing
  personal files, or a folder shared with unrelated workflows.
- Every run should create a unique remote root folder inside the safe Drive area.
- Test files and folders should include a generated run identifier in their
  names or app properties.
- Tests should operate only inside the run's remote root.
- Cleanup should trash the run's remote root after successful and failed runs.
  Trashing keeps failed-run state available for manual debugging while still
  removing it from the mounted test area.
- If cleanup cannot complete, the runner should report the remote folder id and
  enough metadata for manual cleanup.
- The suite should include a dry-run or preflight mode that checks access without
  mutating Drive content beyond an optional disposable probe.

## Local Lifecycle Requirements

For each test or test group, the harness should:

- Create a temporary config/cache area.
- Create a temporary mountpoint.
- Start the executable under test with explicit flags.
- Wait until the mount is usable before running filesystem assertions.
- Capture stdout, stderr, and configured logs.
- Unmount during teardown, even when assertions fail.
- Detect and clean up stale mountpoints from interrupted runs where feasible.
- Fail clearly if unmount leaves the mountpoint busy or the process still alive.

## Initial Test Coverage

The first milestone should favor small, high-signal smoke tests:

- Mount an empty remote root and verify directory listing works.
- Create a file, write bytes, close it, remount, and read the same bytes.
- Create and remove a directory.
- Rename a file within the same directory.
- Move a file between directories.
- Truncate an existing file and verify size and contents after remount.
- Delete a file and verify it disappears from the mounted filesystem.
- List a directory after remote or local mutations that should invalidate cache.
- Verify a clean unmount flushes pending metadata and content changes.

Additional coverage can be added after the harness is stable:

- Async upload behavior.
- Trash versus permanent delete behavior.
- Google Docs export/editability modes.
- Shared folders and shared drives.
- Symlink or shortcut behavior where supported.
- Extended attributes.
- Quota/statfs reporting.
- Recovery after interrupted uploads or unclean shutdown.

## Assertions

- Prefer assertions through the mounted filesystem for user-visible behavior.
- Use the Drive API only for setup, teardown, preflight checks, cleanup
  diagnostics, and assertions that cannot be observed reliably through FUSE.
- When checking eventual consistency, use bounded polling with clear timeout
  messages instead of fixed sleeps.
- Verify both immediate mounted behavior and behavior after remount for changes
  that must persist remotely.
- Treat infrastructure failures separately from product failures where possible.

## Dune And Dependency Requirements

- The end-to-end suite should have its own dune stanza and entrypoint under
  `test/e2e/`.
- The existing `test/testSuite.exe` and `dune runtest` behavior must remain
  unchanged.
- Add a dedicated `@e2e` dune alias for explicit end-to-end runs.
- The runner should use OUnit2 for consistency with the existing unit tests.
- Any new test-only dependencies should be scoped to the end-to-end executable
  when possible.
- If a new opam dependency is required for the end-to-end suite, it should not
  make normal builds or unit tests harder to run unless there is a clear
  benefit.
- Dedicated Makefile targets should invoke the separate end-to-end commands
  explicitly for the full suite, preflight, and case listing.

## Reporting Requirements

- Print the test run id, mountpoint, config directory, and log file path at the
  start of the run.
- Redact credential values from all output.
- On failure, report the failed test name, command or operation, expected result,
  actual result, relevant logs, and cleanup status.
- Use nonzero exit codes for failed assertions, failed environment preflight, and
  cleanup failures.
- Keep normal successful output concise enough for CI logs.

## Resolved Decisions

- Use OUnit2 as the test runner.
- Use direct `gapi-ocaml` Drive API access for preflight, remote run-root
  lifecycle, cleanup, cleanup diagnostics, and rare remote-only assertions.
- Require a standard user account with OAuth already completed and a refresh
  token available to the e2e runner.
- Put the suite under `test/e2e/`.
- Add a dedicated `@e2e` dune alias.
- Treat the safe Drive area as a dedicated parent folder that contains only
  disposable e2e run folders.
- Use a git-ignored JSON file at `test/e2e/config.json` for client id, client
  secret, refresh token, and test folder path.
- Track `test/e2e/config.template.json` as the user-facing configuration guide.
- Trash run roots during cleanup instead of deleting them permanently.
- Support `GDFUSE_E2E_CONFIG` for overriding the local e2e config path.
- Support `GDFUSE_E2E_GDFUSE_EXE` for overriding the executable under test.
- Support `GDFUSE_E2E_ONLY` and `make e2e CASE=...` for filtered case runs.
- Support `make e2e-list` and `@e2e-list` for listing available cases without
  mounting Drive.
