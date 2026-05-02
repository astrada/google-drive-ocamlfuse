# Testability Refactoring Backlog

This backlog tracks remaining candidates after the active
`DriveMetadataRefresh` plan. Keep active implementation plans as separate files
under `docs/plans/`; move completed plans into `docs/plans/archive/`.

## Candidates

### Resource Mapping Helpers

Extract resource construction and Drive-file-to-cache-resource mapping helpers
from `src/drive.ml`.

Likely scope:

- `create_resource`
- `update_resource_from_file`
- filename and duplicate-name disambiguation helpers
- document extension cleanup
- shortcut, symlink, app-property, and resource-key field mapping

Why it helps:

- mostly deterministic logic
- easy to unit test without Context, cache, or Drive requests
- reduces risk around path/name/resource metadata transformations

Suggested module: `DriveResourceMapping`.

### Resource Lookup By Remote Id

Extract the `get_resource_with_id` path and remote-id path reconstruction logic.

Likely scope:

- `get_resource_with_id`
- `get_resource_with_id_from_server`
- remote parent traversal
- shared-with-me path reconstruction
- root-id special case

Why it helps:

- compact extraction target
- covers symlink/shortcut target resolution dependencies
- isolates Drive API traversal from read-side view code

Suggested module: `DriveResourceById`.

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

### Cache Maintenance Helpers

Extract cache-size and cached-resource cleanup helpers.

Likely scope:

- `update_cache_size`
- `shrink_cache`
- `delete_cached_resource`
- `delete_cached_resources`
- memory-buffer and lock cleanup during deletion
- document cache-size accounting

Why it helps:

- concentrates cache cleanup invariants in one module
- makes deletion, download, metadata-refresh, and shrink behavior easier to
  test with fake cache/filesystem ports
- reduces side-effect-heavy helper code in `src/drive.ml`

Suggested module: `DriveCacheMaintenance`.

## Priority Notes

- Do `DriveMetadataRefresh` first; it is the current active plan.
- Prefer `DriveResourceMapping` next for a contained, deterministic win.
- Prefer `DriveResourceById` next if shortcut/read-link behavior is the area of
  concern.
- Save `DriveCacheMaintenance` for a larger pass because it touches cache files,
  memory buffers, locks, and metadata accounting.
