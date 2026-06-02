# End-To-End Tests

This directory contains the optional end-to-end test suite for
`google-drive-ocamlfuse`. The suite uses a real Google Drive account, a real
FUSE mount, and the working-tree executable.

The suite is intentionally separate from `dune runtest`.

## Configure

Copy the template and fill in local OAuth credentials:

```sh
cp test/e2e/config.template.json test/e2e/config.json
```

`test/e2e/config.json` is ignored by git. It must contain:

- `client_id`: OAuth client id.
- `client_secret`: OAuth client secret.
- `refresh_token`: refresh token from an already completed OAuth flow.
- `test_folder_path`: absolute Drive path for the safe parent folder.

The safe parent folder should contain only disposable e2e run folders. The suite
creates one run root inside it for each full run, then moves that run root to
Drive trash during cleanup. Trashing keeps failed-run state available for manual
debugging without leaving it visible in the mounted test area.

## Run

Validate local configuration, OAuth, Drive access, executable lookup, local temp
directory creation, and FUSE helper availability without mounting:

```sh
make e2e-preflight
```

Run the full e2e smoke suite:

```sh
make e2e
```

`make e2e` builds the e2e runner and `google-drive-ocamlfuse`, then executes the
runner directly so case output is visible as it is produced. The `@e2e` Dune
alias is still available for Dune-native workflows; use
`dune build --no-buffer -j 1 @e2e --force` when live action output is needed
through Dune.

List the available named cases:

```sh
make e2e-list
```

Run a matching case or group of cases by label or per-case directory name:

```sh
make e2e CASE="append remount read"
make e2e CASE=test-append-remount-read
make e2e CASE="chmod remount stat"
make e2e CASE="utime remount stat"
make e2e CASE="xattr remount roundtrip"
make e2e CASE="google doc"
make e2e CASE="msoffice"
make e2e CASE="drive shortcut"
```

The full suite creates temporary local profiles, mounts the working-tree
executable, runs named filesystem tests under disposable remote run roots,
unmounts, and trashes the remote run roots.

The metadata cases validate `chmod`, `utime`, and Linux `user.*` extended
attributes through the mounted filesystem. The xattr case is skipped when the
local platform or FUSE stack does not support xattrs. `chown` and cross-account
Drive sharing permissions are not part of the default e2e run.

The Google-native cases create a disposable remote fixture folder before the
mount starts. They validate the default Google Docs `desktop` representation and
Drive shortcut behavior through the mounted filesystem. The msoffice case uses a
separate e2e profile mounted with `-docsmode msoffice` and validates that a
Google document exports as a readable `.docx` file. The suite does not open
desktop entries, run editors, validate full office-file semantics, or test
editing Google-native documents through exported files.

## Overrides

- `GDFUSE_E2E_CONFIG`: path to a config JSON file.
- `GDFUSE_E2E_GDFUSE_EXE`: path to the executable under test.
- `GDFUSE_E2E_MOUNT_TIMEOUT_SECONDS`: mount startup timeout.
- `GDFUSE_E2E_UNMOUNT_TIMEOUT_SECONDS`: unmount timeout.
- `GDFUSE_E2E_FS_TIMEOUT_SECONDS`: filesystem polling timeout.
- `GDFUSE_E2E_ONLY`: substring filter for case labels and per-case directory
  names.
- `GDFUSE_E2E_KEEP_LOCAL`: keep local temporary state after successful runs.
- `GDFUSE_E2E_LOG_EXCERPT_LINES`: number of log lines to show on failure.

Secrets are redacted from normal output. On failure, the suite prints the local
root, mountpoint, log paths, run root id, mount status, and log excerpts.
