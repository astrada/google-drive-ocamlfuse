# Milestone 5 Plan

Milestone 5 should cover Google-native Drive objects now that the e2e suite
already validates ordinary file mutations, metadata, permissions, and xattrs.
The focus is the mounted behavior for objects that cannot be created through
normal POSIX file writes: Google Docs-style files and Drive shortcuts.

## Outcome

After this milestone, `make e2e` should include focused coverage that proves
Google-native objects created in the test Drive area appear in the mounted
filesystem with the expected default representation, and that shortcut/link
behavior works through the FUSE boundary.

## Scope

In scope:

- Google Docs visibility in the default configuration.
- Default `desktop` docs representation, because `Config.default` uses
  `download_docs = true` and `document_format = "desktop"`.
- Reading the mounted desktop entry for a Google document.
- Stability of the Google document representation after remount.
- Drive shortcut behavior for a shortcut that points to a normal file inside
  the e2e run root.
- Drive API setup for Google-native fixtures that cannot be created through the
  mounted filesystem.

Out of scope:

- Exhaustively testing all docs modes.
- Exporting and validating office formats such as `docx`, `odt`, `xlsx`, or
  `ods`.
- Editing Google Docs content through office applications.
- Shared drives.
- Cross-account sharing setup.
- Permission matrix testing for Google-native files.
- Performance testing for document export or shortcut resolution.

## Fixture Strategy

Google-native objects should be created through the Drive API before the mount
loads the relevant directory metadata. Creating remote fixtures after the mount
has already listed a directory can make tests depend on metadata cache expiry
rather than product behavior.

Preferred strategy:

- Add a small remote-fixture path in the e2e harness.
- Create the milestone 5 remote fixture directory and its Google-native objects
  before `E2eHarness.start_mount` is called.
- Keep the fixture names deterministic inside the run root, because the run root
  itself is unique and already disposable.
- Put Google-native cases before mutation cases in the OUnit case list, or make
  their setup explicitly mountless so filtered runs and full-suite runs behave
  the same way.

Fallback strategy:

- If a mountless setup path becomes too invasive, keep milestone 5 as one
  consolidated case that creates all Google-native fixtures before the first
  mount and then verifies them through the mounted filesystem.

## Proposed Test Cases

### `google doc desktop entry visible`

Create a Google document through the Drive API under a dedicated e2e fixture
folder before mounting. After mounting, verify that the fixture directory
contains the expected desktop-entry filename, for example `Milestone 5
Doc.desktop`.

Acceptance criteria:

- The test creates the Google document only inside the run root.
- The mounted filename matches the default docs representation.
- Directory listing assertions use bounded polling.
- Drive API use is limited to fixture setup.

### `google doc desktop entry readable`

Read the mounted desktop entry for the Google document and verify that it is
non-empty and contains stable desktop-entry signals such as a desktop entry
header and a URL or executable line.

Acceptance criteria:

- The assertion is made through the mounted filesystem.
- The test does not require an editor or desktop environment.
- The test avoids depending on the full exact generated file body.

### `google doc remount stable`

Remount after the Google document fixture is visible, then verify the desktop
entry is still visible and readable.

Acceptance criteria:

- The same mounted path is valid after remount.
- The desktop-entry content remains non-empty.
- The test distinguishes remount visibility failures from read failures.

### `drive shortcut target visible`

Create a normal file in the fixture area and create a Drive shortcut pointing to
it. Verify that the mounted shortcut is exposed as a link-like object and that
the target path is readable through the mounted filesystem.

Acceptance criteria:

- The shortcut target lives inside the same run root.
- The shortcut is verified through `lstat`, `readlink`, or the most stable local
  mounted behavior exposed by the current implementation.
- The test verifies the target content through the mounted filesystem.
- Remote API use is limited to setup or to creating the shortcut if creating it
  through `Unix.symlink` is not reliable enough.

## Implementation Phases

### 1. Add Drive Fixture Helpers

Add e2e Drive helpers for Google-native fixture setup:

- Create a Google document with MIME type
  `application/vnd.google-apps.document`.
- Create a shortcut with MIME type `application/vnd.google-apps.shortcut` and
  `shortcutDetails.targetId`.
- Optionally create a regular text file through the Drive API if shortcut setup
  needs the target before mount.

Acceptance criteria:

- Helpers return remote ids needed for diagnostics.
- Helpers use `supportsAllDrives` consistently with the existing e2e Drive
  helpers.
- Helpers do not print secrets.

### 2. Add Mountless Fixture Setup

Extend the e2e harness enough to create remote fixtures before the first mount.

Suggested shape:

- Keep the current mounted test path unchanged for ordinary cases.
- Add a Google-native fixture initializer that runs once per e2e run before the
  first mount when any milestone 5 case is selected.
- Record fixture mounted paths in a small local structure used by the milestone
  5 tests.

Acceptance criteria:

- Filtered milestone 5 runs work.
- Full-suite runs work without depending on metadata cache expiry.
- Existing tests continue using the current per-case mounted subdirectories.

### 3. Add OUnit Cases

Add focused named cases:

- `google doc desktop entry visible`
- `google doc desktop entry readable`
- `google doc remount stable`
- `drive shortcut target visible`

Acceptance criteria:

- Cases are visible in `make e2e-list`.
- Cases are selectable with `make e2e CASE=...`.
- Cases use bounded polling and clear failure messages.
- Cases do not require additional OAuth scopes beyond the existing Drive test
  account setup.

### 4. Document Usage And Limits

Update `test/e2e/README.md` with Google-native object notes:

- The default milestone 5 docs checks target the `desktop` docs representation.
- The suite does not open desktop entries or run an editor.
- Non-default docs modes are deferred.
- Shortcut tests use fixtures inside the safe run root.

Acceptance criteria:

- A developer can understand what Google-native behavior is covered.
- The README includes filtered run examples for the new cases.

## Milestone Complete When

- `dune build @install` succeeds.
- `dune runtest` succeeds and does not run e2e tests.
- `make e2e-preflight` succeeds with a valid local e2e config.
- `make e2e-list` includes the new Google-native cases.
- `make e2e CASE="google doc"` succeeds.
- `make e2e CASE="drive shortcut"` succeeds.
- `make e2e` succeeds with the expanded suite.
- Remote run roots are still trashed during cleanup.

## Deferred Follow-Up

After the default desktop representation is covered, a later milestone can add
one explicit non-default docs mode, most likely `msoffice`, if the harness grows
a clean per-run docs-mode override. That should remain separate from milestone 5
so this milestone stays focused on Google-native fixture setup and default
mounted behavior.
