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

- The suite should be built as a separate executable, for example an
  `e2eSuite.exe` target under a dedicated `e2e/` directory.
- The executable must not be registered under the existing `runtest` alias.
- A developer should run it explicitly, for example with `dune exec` or a
  dedicated alias such as `@e2e`.
- The executable should fail fast when required environment variables,
  credentials, FUSE support, or mount helpers are unavailable.
- The test runner should support selecting one test, a small group of tests, or
  the whole suite.
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
- The suite must document which authentication mode it expects before it is run.
- Secrets must be supplied through local files or environment variables that are
  ignored by git.
- The suite must not print token values, OAuth client secrets, service-account
  private keys, or full credential JSON to stdout, stderr, or logs.

## Authentication Strategy

The preferred authentication mode for unattended runs is a dedicated test Google
account or service account with access only to a disposable Drive area.

The initial suite should support one explicit authentication path before adding
more:

- Service-account credentials are suitable for repeatable automation when the
  target Drive area can be shared with the service account.
- Device or browser OAuth can be supported later for manual developer runs, but
  should not be required for unattended execution.

The runner should validate credentials with a small preflight operation before
attempting to mount. If preflight fails, the suite should report an environment
failure rather than a filesystem regression.

## Remote Test Data Isolation

- Every run should create or use a unique remote root folder for that run.
- Test files and folders should include a generated run identifier in their
  names or app properties.
- Tests should operate only inside the run's remote root.
- Cleanup should remove the run's remote root after successful and failed runs.
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
- Use the Drive API only for setup, teardown, preflight checks, and assertions
  that cannot be observed reliably through FUSE.
- When checking eventual consistency, use bounded polling with clear timeout
  messages instead of fixed sleeps.
- Verify both immediate mounted behavior and behavior after remount for changes
  that must persist remotely.
- Treat infrastructure failures separately from product failures where possible.

## Dune And Dependency Requirements

- The end-to-end suite should have its own dune stanza and entrypoint.
- The existing `test/testSuite.exe` and `dune runtest` behavior must remain
  unchanged.
- Any new test-only dependencies should be scoped to the end-to-end executable
  when possible.
- If a new opam dependency is required for the end-to-end suite, it should not
  make normal builds or unit tests harder to run unless there is a clear
  benefit.
- A dedicated Makefile target may be added later, but it should invoke the
  separate end-to-end command explicitly.

## Reporting Requirements

- Print the test run id, mountpoint, config directory, and log file path at the
  start of the run.
- Redact credential values from all output.
- On failure, report the failed test name, command or operation, expected result,
  actual result, relevant logs, and cleanup status.
- Use nonzero exit codes for failed assertions, failed environment preflight, and
  cleanup failures.
- Keep normal successful output concise enough for CI logs.

## Open Decisions

- Whether the first implementation should use OUnit2, Alcotest, or a small
  custom runner.
- Whether the runner should call the Drive API directly for setup and teardown or
  shell out to external tooling.
- Whether the first authentication path should be service-account-only.
- Whether the suite should live in `e2e/` or under `test/e2e/`.
- Whether `@e2e` should be added as a dune alias, or whether `dune exec` is
  explicit enough.
- Which remote Drive area should be considered safe for project-maintainer runs.
