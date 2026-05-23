# Milestone 2 Plan

Milestone 2 should harden the end-to-end suite as a reliable developer tool
before adding broader Drive feature coverage. Milestone 1 proved that the suite
can authenticate, create an isolated run root, mount the working-tree
executable, exercise filesystem behavior, unmount, and trash the run root.
Milestone 2 should make that workflow easier to run, diagnose, and extend.

## Outcome

After this milestone, a developer should be able to run either a lightweight
preflight or the full smoke suite:

```sh
make e2e-preflight
make e2e
```

The full suite should report failures with enough local log context, remote ids,
mount status, and cleanup status that the next debugging step is obvious.

## Scope

In scope:

- Split the current single smoke lifecycle into named OUnit cases.
- Isolate each named test under its own mounted subdirectory.
- Add a preflight-only mode that validates local config, OAuth refresh, Drive
  access, safe-folder access, the built executable path, and local FUSE helper
  availability without mounting or mutating test files.
- Add reusable bounded polling helpers for filesystem visibility checks.
- Add configurable timeouts for mount, unmount, filesystem polling, and Drive
  setup operations.
- Improve failure output with log excerpts and cleanup status.
- Add concise local usage documentation for configuring and running the e2e
  suite.

Out of scope:

- CI integration.
- New feature domains such as Google Docs modes, shared drives, xattrs, symlinks,
  async upload stress tests, or interrupted-upload recovery.
- Replacing OUnit2 or changing the milestone 1 authentication model.
- Permanent remote deletion. Run roots should continue to be trashed.

## Implementation Phases

### 1. Runner Modes

- Add a runner mode for preflight-only execution.
- Keep `make e2e` and `dune build @e2e` as the full smoke-suite path.
- Add a Makefile target such as `make e2e-preflight`.
- Add a dune alias such as `@e2e-preflight`, or pass an explicit mode argument to
  the same executable from the Makefile target.
- Keep missing or invalid config failures classified as configuration failures.

Acceptance criteria:

- `make e2e-preflight` validates config and environment without mounting.
- `make e2e` still runs the full smoke suite.
- Both modes redact OAuth secrets.

### 2. Preflight Checks

The preflight mode should validate:

- `test/e2e/config.json` or the `GDFUSE_E2E_CONFIG` override exists and parses.
- Required JSON fields are present, nonempty, and not template placeholders.
- The working-tree executable exists, or `GDFUSE_E2E_GDFUSE_EXE` points to an
  executable file.
- OAuth refresh succeeds.
- A low-cost Drive API request succeeds.
- The configured safe Drive folder can be resolved or created.
- The host has an available FUSE unmount helper, such as `fusermount3`,
  `fusermount`, or `umount`.
- A temporary local root and mountpoint can be created.

Acceptance criteria:

- Preflight output distinguishes config failures, auth/API failures, executable
  lookup failures, FUSE helper failures, and local filesystem failures.
- Preflight does not create a per-run remote root.
- If safe-folder creation is needed, the output states that it created the safe
  parent folder.

### 3. Test Decomposition

Split the current smoke lifecycle into named OUnit tests:

- Mount empty run root and list the root directory.
- Create a file, write bytes, close it, remount, and read the same bytes.
- Create and remove a directory.
- Rename a file within one directory.
- Move a file between directories.
- Truncate an existing file and verify size and contents after remount.
- Delete a file and verify it disappears after remount.

Each test should operate under a unique subdirectory inside the mounted run root,
for example `/test-write-read`, `/test-rename`, and `/test-delete`.

Acceptance criteria:

- A failure names the specific filesystem behavior that failed.
- Tests do not depend on artifacts from earlier tests.
- The suite can still use one remote run root for the overall run.

### 4. Filesystem Polling Helpers

Add small helpers for assertions that may require eventual consistency:

- Wait until a path exists.
- Wait until a path does not exist.
- Wait until a directory listing contains or omits a name.
- Wait until file contents match expected bytes.
- Wait until file size matches expected size.

The helpers should use bounded polling and report:

- the path under test,
- the expected condition,
- the timeout,
- the last observed state when useful.

Acceptance criteria:

- Tests avoid fixed sleeps.
- Timeout failures are specific enough to diagnose the missing condition.
- Polling defaults are conservative but configurable.

### 5. Timeouts And Configuration

Add non-secret runtime knobs for:

- mount startup timeout,
- unmount timeout,
- filesystem polling timeout,
- Drive setup timeout or retry budget,
- whether to keep local temporary state after successful runs.

These can start as environment variables to avoid expanding the JSON file with
non-secret operational settings.

Suggested names:

- `GDFUSE_E2E_MOUNT_TIMEOUT_SECONDS`
- `GDFUSE_E2E_UNMOUNT_TIMEOUT_SECONDS`
- `GDFUSE_E2E_FS_TIMEOUT_SECONDS`
- `GDFUSE_E2E_KEEP_LOCAL`

Acceptance criteria:

- Defaults work for local developer runs.
- Invalid timeout values fail before Drive or mount operations.
- Timeout values are printed in the run summary.

### 6. Diagnostics

Improve failure reporting with:

- last N lines of `gdfuse.log`,
- paths to full `gdfuse.log`, stdout, and stderr captures,
- mount status at teardown,
- unmount result,
- remote cleanup result,
- run id, safe parent id, and run root id.

Keep successful output concise. Detailed log excerpts should appear on failures
or when a verbose mode is enabled.

Acceptance criteria:

- A mount startup failure includes process stdout, stderr, and app-log excerpts.
- A filesystem assertion failure preserves local temporary state for inspection.
- A successful run still removes local temporary state unless configured
  otherwise.
- Cleanup failures are reported in addition to the original test failure.

### 7. Local Documentation

Add a short user-facing e2e README under `test/e2e/` or
`docs/projects/e2e-test-suite/` that documents:

- copying `config.template.json` to `config.json`,
- required OAuth fields,
- meaning of `test_folder_path`,
- `make e2e-preflight`,
- `make e2e`,
- `GDFUSE_E2E_CONFIG`,
- `GDFUSE_E2E_GDFUSE_EXE`,
- timeout overrides,
- where logs and temporary local state are printed,
- why remote run roots are trashed instead of permanently deleted.

Acceptance criteria:

- A developer can set up and run the suite from the documentation alone.
- The docs do not include secrets or real credential values.

## Milestone Complete When

- `dune build @install` succeeds.
- `dune runtest` succeeds and does not run e2e tests.
- `make e2e-preflight` succeeds with a valid local e2e config and does not mount.
- `make e2e` succeeds with a valid local e2e config.
- The smoke behavior is split into named OUnit tests.
- Each test is isolated under its own mounted subdirectory.
- Failure output includes log paths, relevant log excerpts, mount status, remote
  run root id, and cleanup status.
- Successful runs trash the remote run root and remove local temporary state by
  default.
