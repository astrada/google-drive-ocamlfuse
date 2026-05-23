# Milestone 3 Plan

Milestone 3 should add targeted behavioral coverage now that the e2e harness is
usable and diagnosable. The focus should be common data-changing workflows:
uploads, overwrites, partial writes, cache/listing coherence, and delete/trash
semantics.

## Outcome

After this milestone, `make e2e` should give confidence that ordinary file
mutations persist across remounts and that the mounted view stays coherent after
create, update, rename, move, truncate, and delete operations.

## Scope

In scope:

- Add tests for upload persistence after close and remount.
- Add overwrite, append, and partial overwrite tests.
- Add directory listing and cache coherence tests around create, rename, move,
  and delete.
- Add delete/trash semantics checks, using the Drive API only where the mounted
  filesystem cannot observe trash state.
- Add a moderate-size file smoke test that is larger than the existing tiny
  files but not a stress test.
- Add file metadata checks for size, and mtime only if it is reliable enough.
- Improve cleanup diagnostics for leftover remote items before trashing the run
  root.

Out of scope:

- Google Docs export/editability modes.
- Shared folders and shared drives.
- Extended attributes.
- Symlinks and shortcuts.
- Async upload stress tests.
- Crash recovery or interrupted-upload recovery.
- Large-file performance testing.

## Implementation Phases

### 1. Test Helpers

Add reusable helpers for data-changing filesystem tests:

- Write exact bytes to a file.
- Append exact bytes to a file.
- Write exact bytes at an offset.
- Read exact bytes from a file.
- Assert exact file contents with bounded polling.
- Assert file size with bounded polling.
- Assert directory listing contains exactly or at least a set of names.
- Remount and rerun a supplied assertion.

Acceptance criteria:

- New tests do not duplicate low-level file I/O boilerplate.
- Assertion failure messages include path, expected value, observed value, and
  timeout where relevant.
- Helpers keep binary-safe behavior and do not use line-oriented reads for file
  contents.

### 2. Upload Persistence Tests

Add tests for the common close/remount persistence path:

- Create a file, write bytes, close it, remount, and verify contents.
- Overwrite an existing file with shorter contents, remount, and verify contents
  and size.
- Overwrite an existing file with longer contents, remount, and verify contents
  and size.
- Append bytes to an existing file, remount, and verify contents and size.
- Partially overwrite bytes in the middle of an existing file, remount, and
  verify contents and size.

Acceptance criteria:

- Each test operates in its own mounted subdirectory.
- Each test verifies immediate mounted behavior and post-remount behavior.
- Each test confirms size as well as contents when practical.

### 3. Directory Listing And Cache Coherence

Add tests that check the mounted directory view remains coherent:

- Create multiple files, list the directory, and verify all names appear.
- Rename a file, list the directory, and verify the old name disappears and the
  new name appears.
- Move a file between directories, list both directories, and verify source and
  destination views.
- Delete one file from a directory with multiple files, list the directory, and
  verify only the deleted file disappears.
- Remount after the above mutations and recheck listings.

Acceptance criteria:

- Listing checks use bounded polling rather than fixed sleeps.
- Tests distinguish stale-cache failures from file-content failures.
- Tests do not rely on ordering from `Sys.readdir`.

### 4. Delete And Trash Semantics

Add targeted coverage for delete behavior:

- Delete a file through the mounted filesystem and verify it disappears from the
  mounted directory.
- Remount and verify it remains absent.
- Use the Drive API to verify the remote item is trashed when the test can
  identify the remote file id safely.

The Drive API should be used only for the trash-state assertion. The visible
filesystem behavior should still be asserted through the mount.

Acceptance criteria:

- Mounted delete behavior is verified without Drive API shortcuts.
- Trash-state checks are clearly labeled as remote assertions.
- If Drive API lookup cannot reliably identify the deleted item, the trash-state
  assertion should be skipped or deferred rather than making a weak assertion.

### 5. Moderate-Size File Smoke Test

Add one file-content test that is larger than the tiny strings used by the smoke
suite but still quick for local runs.

Suggested shape:

- Generate deterministic binary-safe content around 1-2 MiB.
- Write it to a file.
- Close and remount.
- Verify size and content.
- Overwrite a middle range with deterministic bytes.
- Remount and verify size and content again.

Acceptance criteria:

- The test remains fast enough for ordinary local `make e2e` runs.
- The content generator is deterministic and does not require external files.
- Failure output identifies whether size or content diverged.

### 6. Metadata Basics

Add conservative metadata checks:

- Verify file size after write, overwrite, append, partial overwrite, and
  truncate.
- Consider mtime checks only if the filesystem behavior is stable enough across
  mount/remount and Drive API timing.

Acceptance criteria:

- Size checks are included in mutation tests.
- Mtime checks are either implemented with clear tolerance/polling or explicitly
  deferred with a note in the milestone result.

### 7. Remote Cleanup Diagnostics

Before trashing the run root, collect basic diagnostics for remote leftovers:

- run root id,
- count of immediate children still visible,
- names of immediate children when the count is small,
- whether the run root was successfully trashed.

This should help debug cleanup or cache bugs without permanently deleting
evidence.

Acceptance criteria:

- Diagnostics do not print OAuth secrets.
- Successful runs remain concise.
- Failure runs include enough remote context to inspect the trashed run root.

## Milestone Complete When

- `dune build @install` succeeds.
- `dune runtest` succeeds and does not run e2e tests.
- `make e2e-preflight` succeeds with a valid local e2e config.
- `make e2e` succeeds with the expanded mutation suite.
- Upload, overwrite, append, partial overwrite, listing coherence, delete, and
  moderate-size file tests are present as named OUnit cases.
- Tests remain isolated under per-case mounted subdirectories.
- Remote run roots are still trashed during cleanup.
