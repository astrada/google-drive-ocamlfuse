# Milestone 6 Plan

Milestone 6 should add one non-default Google Docs export mode to the main e2e
suite. Milestone 5 covers the default `desktop` representation for
Google-native documents; this milestone adds coverage for an exported office
format without making the suite depend on external editors or document
applications.

## Outcome

After this milestone, `make e2e` should include a Google Docs export case that
mounts a separate e2e run with `-docsmode msoffice`, verifies that a Google
document appears as a `.docx` file, and reads that exported file through the
mounted filesystem.

## Scope

In scope:

- One explicit non-default docs mode: `msoffice`.
- Google document export visibility as `.docx`.
- Reading the exported `.docx` through the mounted filesystem.
- Remount stability for the exported `.docx`.
- Inclusion in the main `make e2e` suite.
- A harness change that can run more than one e2e mount profile in a single
  process.

Out of scope:

- Exhaustively testing all docs modes.
- Validating full `.docx` document semantics.
- Opening the exported document with office applications.
- Editing Google Docs through exported office files.
- Uploading modified exported Docs content back to Drive.
- Spreadsheet, presentation, drawing, form, or Apps Script export coverage.
- Cross-account sharing and permission matrix testing.

## Design Constraints

The current e2e suite keeps one mounted run for the process. That works for the
default suite because every case shares the same profile and default config.
`msoffice` cannot safely be tested in that same mounted run because docs mode is
a mount/runtime setting and would change how Google-native documents are mapped.

Milestone 6 should keep the default `desktop` cases and the new `msoffice` case
in the same main suite by adding profile-aware e2e runs:

- Default-profile cases continue sharing the existing default run.
- The `msoffice` case uses a separate run root, local profile, cache, mountpoint,
  and cleanup path.
- Each profile creates its own Google-native fixture before its first mount.
- Cleanup still trashes every remote run root created during the process.

## Proposed Test Case

### `google doc msoffice export readable`

Create a Google document through the Drive API under a dedicated fixture folder
before mounting the `msoffice` profile. Start the mount with `-docsmode
msoffice`, then verify that the document appears as `Milestone 6 Doc.docx`.

Acceptance criteria:

- The case is included in `make e2e` by default.
- The case is visible in `make e2e-list`.
- The case can be selected with `make e2e CASE="msoffice"`.
- The fixture is created only inside the profile run root.
- The mounted fixture directory contains the expected `.docx` filename.
- Reading the `.docx` through the mounted filesystem returns non-empty content.
- The content starts with the stable zip/docx signature `PK`.
- After remount, the same `.docx` path is still visible and readable.
- Assertions use bounded polling and clear failure messages.

## Implementation Phases

### 1. Add Profile-Aware Harness Support

Extend the e2e harness and filesystem-test runner so cases can request a mount
profile.

Suggested shape:

- Add a small profile type, for example `default` and `docs-mode:msoffice`.
- Extend each e2e case with a profile field that defaults to the current default
  profile.
- Replace the single run reference in the filesystem tests with a registry keyed
  by profile.
- Create, mount, remount, and clean up each profile independently.
- Keep setup lazy so filtered runs only create the profiles they need.

Acceptance criteria:

- Existing cases still share one default-profile run.
- The new `msoffice` case creates a distinct profile run.
- Failed runs still print diagnostics for the failing profile.
- At-exit cleanup handles every initialized profile.
- `GDFUSE_E2E_KEEP_LOCAL` applies consistently to all profiles.

### 2. Add Docs-Mode Mount Override

Allow the mount helper to pass a docs-mode argument to `google-drive-ocamlfuse`.

Suggested shape:

- Add an optional `docs_mode` argument to `E2eMount.start`.
- When present, add `-docsmode <mode>` to the executable arguments.
- Keep the persisted e2e config file in the default mode; use the CLI override
  for the alternate profile.
- Use a fresh cache directory for the alternate profile so `-docsmode` cache
  invalidation does not affect default-profile cases.

Acceptance criteria:

- Default-profile mounts keep the existing command line.
- The `msoffice` profile mount command includes `-docsmode msoffice`.
- The override does not persist into the e2e config file.

### 3. Add Msoffice Fixture Support

Reuse the milestone 5 Google-native fixture strategy, but let the profile choose
the expected document entry name.

Suggested shape:

- Keep the Drive helper that creates a Google document.
- Add a profile fixture for `Milestone 6 Doc`.
- For `msoffice`, expect `Milestone 6 Doc.docx`.
- Create the fixture before the `msoffice` mount starts.

Acceptance criteria:

- Fixture creation does not depend on metadata cache expiry.
- Remote ids are printed for diagnostics.
- Fixture names are deterministic inside the unique run root.

### 4. Add The Main-Suite Case

Add the new OUnit case to the main case list:

- `google doc msoffice export readable`

The case should:

- Get the `msoffice` profile run.
- Wait for the fixture directory and `.docx` entry.
- Read the `.docx` through the mounted filesystem.
- Assert non-empty content and `PK` prefix.
- Remount the same profile.
- Re-read and verify the `.docx`.

Acceptance criteria:

- `make e2e-list` includes the case.
- `make e2e CASE="msoffice"` runs only the alternate profile.
- `make e2e CASE="google doc"` runs default Google Docs cases and the msoffice
  case if the filter matches its label.
- `make e2e` runs the default profile and the msoffice profile in one process.

### 5. Document Usage And Limits

Update `test/e2e/README.md` with the new docs-mode coverage.

Acceptance criteria:

- The README explains that the default Google-native checks use `desktop`.
- The README explains that the msoffice check uses its own e2e profile.
- The README includes a filtered run example for `make e2e CASE="msoffice"`.
- The README states that the suite checks exported file presence and readability,
  not office-format semantics or editing.

## Milestone Complete When

- `dune build @install` succeeds.
- `dune runtest` succeeds and does not run e2e tests.
- `make e2e-preflight` succeeds with a valid local e2e config.
- `make e2e-list` includes `google doc msoffice export readable`.
- `make e2e CASE="msoffice"` succeeds.
- `make e2e CASE="google doc"` succeeds.
- `make e2e` succeeds with both default and msoffice profile cases.
- Every created remote run root is trashed during cleanup.

## Deferred Follow-Up

Later milestones can add a second export mode such as `libreoffice`, or add a
write-back-focused case for editable exported Docs. Those should remain separate
until the single-mode export path is stable in the main suite.
