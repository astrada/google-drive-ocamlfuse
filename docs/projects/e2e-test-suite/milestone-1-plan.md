# Milestone 1 Plan

This milestone should produce the first runnable end-to-end test executable. It
should prove the full harness shape before expanding coverage: load local e2e
configuration, validate Drive access, create an isolated remote run root, mount
the working-tree executable, run a small filesystem smoke suite, unmount, and
trash the run root.

## Outcome

After this milestone, a developer with `test/e2e/config.json` configured should
be able to run:

```sh
dune build @install
dune build @e2e
```

The `@e2e` alias should run only the end-to-end suite. `dune runtest` should
continue to run only the existing unit tests.

## Scope

In scope:

- Add a separate OUnit2 executable under `test/e2e/`.
- Add a dedicated `@e2e` dune alias.
- Parse `test/e2e/config.json`.
- Refresh the OAuth access token with the configured client id, client secret,
  and refresh token.
- Use the Drive API through `gapi-ocaml` to preflight credentials.
- Locate or create the configured safe parent folder from `test_folder_path`.
- Create one unique remote run root under the safe parent folder.
- Seed a temporary google-drive-ocamlfuse profile that mounts the run root.
- Start the working-tree `google-drive-ocamlfuse` executable in foreground mode.
- Run a small smoke suite through ordinary filesystem operations.
- Unmount during teardown and trash the remote run root.
- Print enough paths and remote ids to debug failures without printing secrets.

Out of scope:

- Interactive OAuth setup.
- Service-account authentication.
- CI integration.
- Exhaustive filesystem coverage.
- Shared drives, Google Docs modes, xattrs, symlinks, async upload stress tests,
  and recovery scenarios.

## Implementation Phases

### 1. Dune And Skeleton

- Create `test/e2e/dune`.
- Create `test/e2e/e2eSuite.ml` as the OUnit2 entrypoint.
- Ensure the e2e executable is not attached to the existing `runtest` alias.
- Add an `@e2e` alias that depends on the e2e executable and the
  working-tree `gdfuse.exe`.
- Make the harness receive the `gdfuse.exe` path from the alias, an environment
  variable, or a deterministic build-relative path.
- Add a placeholder test that verifies the runner starts and reports the
  selected config path.

Acceptance criteria:

- `dune runtest` behavior is unchanged.
- `dune build @e2e` runs the separate executable.
- Running without `test/e2e/config.json` fails with a clear environment error.

### 2. Configuration Loading

- Add a small e2e config module for `client_id`, `client_secret`,
  `refresh_token`, and `test_folder_path`.
- Parse JSON from `test/e2e/config.json`.
- Reject missing, empty, malformed, or template-placeholder values.
- Redact `client_secret` and `refresh_token` from all normal and failure output.
- Keep `test/e2e/config.template.json` aligned with the parser.

Acceptance criteria:

- Invalid config fails before any Drive or mount operation.
- Error messages name the missing or invalid key without printing secret values.

### 3. Drive API Preflight And Remote Run Root

- Use `gapi-ocaml` to refresh the access token.
- Run a low-cost Drive API preflight such as fetching account metadata or the
  root file metadata.
- Resolve `test_folder_path` as a Drive folder path.
- Create missing safe-area path segments only if the behavior is explicit in the
  implementation and documented in the test output.
- Create a unique run root folder under the safe area, using a name that includes
  the run id.
- Store the run root id in harness state for mounting and teardown.
- Trash the run root in teardown.

Acceptance criteria:

- Auth or API failures are reported as environment failures.
- Each run prints the run id and run root id.
- A failed test still attempts to trash the run root.
- If cleanup fails, the output includes the run root id for manual cleanup.

### 4. Temporary Local Profile

- Create a per-run temporary directory containing app data, cache, logs,
  config, state, and mountpoint.
- Generate a google-drive-ocamlfuse config that sets at least:
  - `client_id`
  - `client_secret`
  - `root_folder`
  - local data/cache/log directories if needed
- Generate state containing the configured refresh token and no user profile
  data from `~/.gdfuse`.
- Use a generated label that cannot collide with normal user labels.
- Configure logs so failures can point to a stable per-run log path.

Acceptance criteria:

- The e2e run does not read or write the user's default `~/.gdfuse` profile.
- The run prints the temporary profile path, mountpoint, and log path.
- Teardown removes local temporary state after successful runs.

### 5. Mount Harness

- Start the working-tree executable in foreground mode with explicit base dir,
  label, config path, and mountpoint.
- Wait for the mountpoint to become usable with bounded polling.
- Capture stdout, stderr, exit status, and app logs.
- Provide a teardown path that unmounts with the platform's available FUSE
  unmount helper.
- Kill the process only after a normal unmount attempt fails or times out.

Acceptance criteria:

- Mount startup timeout produces a clear failure with process output and logs.
- Teardown attempts unmount even when a test assertion fails.
- A busy or failed unmount is reported distinctly from an assertion failure.

### 6. Smoke Tests

Start with a small set that exercises the full lifecycle without making the
suite slow or hard to diagnose:

- Mount an empty run root and verify directory listing works.
- Create a file, write bytes, close it, remount, and read the same bytes.
- Create and remove a directory.
- Rename a file within the same directory.
- Move a file between directories.
- Truncate an existing file and verify size and contents after remount.
- Delete a file and verify it disappears from the mounted filesystem.
- Verify a clean unmount/remount sees the expected persisted state.

Acceptance criteria:

- Assertions are made through the mounted filesystem unless a direct Drive API
  check is needed for harness state.
- Eventual consistency checks use bounded polling with meaningful timeout
  messages.
- Tests are named clearly enough to identify the failed filesystem behavior.

### 7. Reporting And Failure Hygiene

- Print run id, config path, temporary profile path, mountpoint, log path, safe
  folder path, and run root id.
- Redact OAuth secrets from config dumps, command output, and exception text.
- Classify failures as config, environment/preflight, mount lifecycle,
  assertion, or cleanup failures where practical.
- Include cleanup status at the end of the run.

Acceptance criteria:

- A failing run gives enough information to inspect local logs and the trashed
  remote run root.
- Secret values are not printed.
- Successful output remains concise.

## Suggested File Layout

- `test/e2e/dune`
- `test/e2e/e2eSuite.ml`
- `test/e2e/e2eConfig.ml`
- `test/e2e/e2eDrive.ml`
- `test/e2e/e2eHarness.ml`
- `test/e2e/e2eMount.ml`
- `test/e2e/e2eFilesystemTests.ml`
- `test/e2e/config.template.json`
- `test/e2e/config.json` ignored by git

The exact module split can change during implementation, but configuration,
Drive API harness operations, mount lifecycle, and filesystem assertions should
stay separated enough to keep failures readable.

## Milestone Complete When

- `dune build @install` succeeds.
- `dune runtest` succeeds and does not run e2e tests.
- `dune build @e2e` runs the e2e executable.
- A properly configured local run mounts a fresh remote run root, executes the
  smoke tests, unmounts, and trashes the run root.
- A missing or invalid `test/e2e/config.json` fails before network or mount
  operations.
- Cleanup behavior is documented in output and does not silently leave mounted
  filesystems behind.
