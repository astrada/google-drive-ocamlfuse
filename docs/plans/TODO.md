# Testability Refactoring Backlog

This backlog tracks candidates that are not currently active. Keep active
implementation plans as separate files under `docs/plans/`; move completed
plans into `docs/plans/archive/`.

## Candidates

### Root And Well-Known Resource Resolution

Extract root-folder and synthetic resource handling.

Likely scope:

- `get_root_folder_id_from_server`
- `get_root_folder_id`
- `get_root_folder_id_from_context`
- `get_well_known_resource`
- root, trash root, lost+found, and shared-with-me synthetic rows

Why it helps:

- config-sensitive behavior deserves focused tests
- device scope, team drive, configured root folder paths, and synthetic cache
  rows are currently coupled to `Drive`
- simplifies future resolver tests by moving root behavior behind a smaller
  module

Suggested module: `DriveRootResolution`.
