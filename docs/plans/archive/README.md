# Archived Plans

- `otoml-migration/plan.md`: archived staged plan to migrate config handling
  from the legacy `key=value` format to TOML using `otoml`.
- `otoml-migration/implementation-checklist.md`: archived file-by-file
  execution checklist for implementing the TOML migration.
- `gdfuse-readability-refactor/plan.md`: archived plan for splitting
  `bin/gdfuse.ml` into smaller helper modules and simplifying the entrypoint.
- `gdfuse-flow-testing-refactor/plan.md`: archived plan for moving the
  application flow into the library and making it unit testable with faked
  external dependencies.
- `drive-testing-refactor/plan.md`: archived completed plan for extracting the
  `DriveMutations` core and making `Drive` mutation paths unit testable.
- `drive-read-path-extraction/plan.md`: archived completed plan for extracting
  `DriveViews` and `DriveDirectoryReads` and making the read-side view and
  directory-listing paths unit testable.
- `drive-write-path-extraction/plan.md`: archived completed plan for
  extracting `DriveFileMutations` and `DriveUploadDispatch` and making the
  write-side file-mutation and upload-dispatch paths unit testable.
- `drive-metadata-mutation-extraction/plan.md`: archived completed plan for
  extracting `DriveMetadataMutations` and making timestamp, mode, and owner
  metadata mutations unit testable.
- `drive-xattr-extraction/plan.md`: archived completed plan for extracting
  `DriveXattrs` and making xattr behavior unit testable.
- `drive-read-extraction/plan.md`: archived completed plan for extracting
  `DriveReads` and making regular-file read strategy unit testable.
- `drive-fopen-extraction/plan.md`: archived completed plan for extracting
  `DriveOpens` and making file-open access validation unit testable.
- `drive-download-resource-extraction/plan.md`: archived completed plan for
  extracting `DriveDownloads` and making local-content materialization unit
  testable.
- `drive-update-remote-resource-extraction/plan.md`: archived completed plan
  for extracting `DriveRemoteUpdates` and making the metadata-side remote
  update wrapper unit testable.
- `drive-upload-extraction/plan.md`: archived completed plan for extracting
  `DriveUploads` and making the concrete upload attempt unit testable.
- `gdfuse-cli-error-handling/plan.md`: archived completed plan for restoring
  clean CLI handling for `-version`, `-help`, `--help`, and invalid options,
  and for expanding CLI behavior test coverage.
