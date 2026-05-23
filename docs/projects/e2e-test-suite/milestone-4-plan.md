# Milestone 4 Plan

Milestone 4 should expand the end-to-end suite beyond ordinary file mutation
coverage. Milestones 1 through 3 established the harness, live Drive setup,
diagnostics, filtered case runs, and common create/update/delete workflows. The
next useful step is to cover behavior that depends on Google Drive metadata,
object types, or permission state.

The selected track for this milestone is Option B: Metadata And Permissions.
Option A remains documented as a deferred alternative.

## Outcome

After this milestone, `make e2e` should cover one additional class of
user-visible behavior that unit tests cannot fully validate because it crosses
the executable, FUSE, cache, and Drive API boundary.

## Option A: Google-Native Objects

This option tests mounted behavior for Google-native Drive objects and links.
It is useful if the next priority is confidence around Drive-specific file
types rather than POSIX-style metadata.

In scope:

- Google Docs export behavior in the default docs mode.
- Optional focused coverage for one explicit docs mode such as `desktop` or
  `off`, if the harness can pass the mode without complicating the suite.
- Shortcut or symlink-like behavior where the mounted filesystem exposes a
  stable link target.
- Read-only behavior for Google-native files when content editing is not
  supported by the selected mode.
- Drive API setup for test objects that cannot be created through normal
  filesystem calls.

Out of scope:

- Exhaustively testing all docs modes.
- Editing Google Docs content through office applications.
- Shared drives.
- Cross-account sharing setup.
- Performance testing for exported documents.

Suggested tests:

- Create a Google document in the run root through the Drive API, mount it, and
  verify it appears with the expected exported filename or desktop-link shape.
- Read the mounted representation and verify it is non-empty or has the expected
  desktop-entry structure.
- Remount and verify the object is still visible with stable metadata.
- Create a Drive shortcut to a regular file inside the run root, mount it, and
  verify the shortcut appears as the expected filesystem object.

Acceptance criteria:

- Tests assert user-visible behavior through the mounted filesystem.
- Drive API use is limited to creating Google-native objects and shortcuts that
  cannot be created through ordinary filesystem operations.
- Tests do not require external editor applications.
- Tests remain safe under the configured run root and are cleaned up by trashing
  the run root.

## Option B: Metadata And Permissions

This option tests POSIX-style metadata and permission behavior. It is useful if
the next priority is validating `stat`, `chmod`, `utime`, xattrs, and read-only
semantics across remounts.

In scope:

- File mode persistence after `chmod`.
- Modification time persistence after `utime`.
- Extended attribute set/list/get/remove behavior when supported by the local
  FUSE environment.
- `stat` behavior for size, mode, uid, gid, and timestamps where stable.
- Read-only behavior for files whose Drive metadata says they are not editable,
  if the harness can create that state without a second account.
- Direct Drive API assertions only for appProperties or capabilities that cannot
  be observed reliably through the mounted filesystem.

Out of scope:

- Requiring root privileges.
- Requiring a second Google account for the default suite.
- Mutating ownership with `chown` unless the current user can perform it without
  elevated privileges.
- Exhaustive ACL or Google Drive sharing matrix coverage.
- Platform-specific xattr behavior beyond a single Linux/FUSE smoke path.

Suggested tests:

- Create a file, run `chmod 0600`, verify mounted `stat` permissions, remount,
  and verify permissions again.
- Create a file, run `utime` with a deterministic timestamp, verify mounted
  `stat` mtime with a tolerance, remount, and verify it again.
- Set one `user.*` xattr, list names, read the value, remove it, and verify it
  disappears after remount.
- Use the Drive API to verify `mode` and xattr appProperties only as a
  secondary assertion after mounted behavior passes.
- If feasible without extra account setup, create or locate a non-editable Drive
  object in the run root and verify writes fail with a permission error.

Acceptance criteria:

- Metadata tests use the mounted filesystem as the primary assertion path.
- Timestamp assertions use bounded polling and an explicit tolerance.
- Tests skip or clearly fail as environment unsupported when xattrs are not
  available from the local FUSE stack.
- Permission tests avoid privileged operations.
- Any Drive API metadata assertion is labeled as a remote assertion.

## Implementation Phases

### 1. Record The Selected Track

Record Option B as the selected milestone 4 track and keep Option A as a
deferred alternative for a later milestone.

Acceptance criteria:

- Option B is recorded as the chosen track before code changes begin.
- Required metadata and permission environment assumptions are documented in
  `test/e2e/README.md`.
- Any extra Drive API helper methods are justified by setup or remote-only
  assertions.

### 2. Add Harness Support

Add only the shared helpers needed by the metadata and permissions track:

- Add filesystem helpers for `stat`, `chmod`, `utime`, and xattrs.
- Add Drive API helpers for reading appProperties or capabilities for a known
  remote file id.
- Add environment detection for xattr support if the test cannot be portable.

Acceptance criteria:

- Helpers produce concise, actionable failure messages.
- Helpers do not print OAuth secrets or full credential files.
- Helpers are reused by tests instead of duplicating low-level logic.

### 3. Add Focused OUnit Cases

Add a small set of named OUnit cases:

- `chmod remount stat`
- `utime remount stat`
- `xattr remount roundtrip`
- Optional `read only write denied`

Acceptance criteria:

- Each case uses its own mounted subdirectory.
- Cases are selectable with `make e2e CASE=...`.
- Cases use bounded polling rather than fixed sleeps.
- The full suite remains practical for local execution.

### 4. Document Usage And Limits

Update the e2e README with the selected track's setup notes.

- Document xattr support expectations.
- Document that `chown` and cross-account Drive sharing are not part of the
  default run unless explicitly added later.

Acceptance criteria:

- A developer can tell whether their environment supports the new cases before
  running the full suite.
- Filtered examples are included for the new cases.

## Milestone Complete When

- `dune build @install` succeeds.
- `dune runtest` succeeds and does not run e2e tests.
- `make e2e-preflight` succeeds with a valid local e2e config.
- `make e2e-list` includes the new named cases.
- `make e2e CASE=...` succeeds for at least one new case.
- `make e2e` succeeds with the selected milestone 4 cases.
- Remote run roots are still trashed during cleanup.

## Recommended Choice

Option B is the selected milestone 4 track because the codebase already has unit
coverage for `chmod`, `utime`, `chown`, and xattrs, and the e2e suite can
validate the mounted behavior without introducing a second account or editor
dependencies. The first implementation should focus on `chmod`, `utime`, and
xattrs, then add read-only permission behavior only if it can be created safely
inside the existing single-account test area.
