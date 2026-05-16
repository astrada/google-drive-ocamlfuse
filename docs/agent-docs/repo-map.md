# Repository Map

## Purpose

`google-drive-ocamlfuse` mounts Google Drive as a FUSE filesystem on Linux.
The executable is `google-drive-ocamlfuse`, built from `bin/gdfuse.ml`.

The codebase is mostly one executable plus a flat OCaml library in `src/`.
The `gdfuse` startup flow lives primarily in library modules so it can be
unit tested from `test/`.

## Build And Test

- Build: `dune build @install`
- Test: `dune runtest`
- Format OCaml sources: `make format`
- Install locally: `dune install`
- Clean: `dune clean`

The `Makefile` is only a small wrapper around these dune commands.

## Top-Level Layout

- `bin/gdfuse.ml`: thin executable entrypoint.
- `bin/gdfuseRuntimeDeps.ml`: production wiring for the `gdfuse` flow functor.
- `bin/gdfuseFuse.ml`: FUSE callback table and exception mapping.
- `src/`: core implementation.
- `test/`: OUnit unit tests. Current tests cover support modules, filesystem
  core modules, and `gdfuse` CLI/application-flow behavior, but not full FUSE
  or Google Drive integration.
- `docs/wiki/`: user-facing documentation imported from the project wiki.
- `tools/`: helper scripts, currently minimal. Includes `tools/format_ocaml`
  for running `ocamlformat` over tracked `.ml` and `.mli` files in the repo.
- `google-drive-ocamlfuse.opam`: package metadata and dependencies.
- `flake.nix`: Nix setup.

## Important Source Files

### Entry And Bootstrap

- `bin/gdfuse.ml`
  - Thin executable entrypoint.
  - Instantiates the real `GdfuseFlow` and `GdfuseApp` modules.
  - Hands process argv to the library-visible CLI runner.

- `src/gdfuseCli.ml`
  - Parses CLI flags into explicit CLI outcomes.
  - Builds `application_params` plus FUSE argv fragments for the successful
    path.

- `src/gdfuseApp.ml`
  - Top-level CLI runner above `GdfuseCli` and `GdfuseFlow`.
  - Handles version/help/error output and dispatches successful parses into
    bootstrap-only or mount mode.

- `src/gdfuseCommon.ml`
  - Shared startup types and helpers.
  - Holds `application_params` plus config/state helpers used by the flow.

- `src/gdfuseFlow.ml`
  - Main application-flow orchestration.
  - Handles startup/bootstrap, auth selection, context construction, mount
    mode, bootstrap-only mode, and shutdown.
  - Functorized over coarse external dependencies so the flow can be unit
    tested without real browser/auth/FUSE/process-exit side effects.

- `bin/gdfuseRuntimeDeps.ml`
  - Production dependency implementation for `GdfuseFlow`.
  - Wires real browser launch, OAuth/GAE auth, cache lifecycle hooks, CURL,
    `at_exit`, and FUSE startup.

- `bin/gdfuseFuse.ml`
  - Production FUSE adapter layer.
  - Maps `Drive` exceptions into Unix/FUSE errors and registers callback
    functions with `Fuse.main`.

### Core Filesystem Logic

- `src/drive.ml`
  - The main implementation module.
  - Contains most filesystem behavior: lookup, listing, reads, writes, upload,
    symlinks, Google Drive API requests, and the production wiring for
    filesystem cores.
  - Filename cleanup, duplicate-name disambiguation, cache resource
    construction, and Drive `File` to cache-resource mapping live in
    `DriveResourceMapping`.
  - `get_resource`, `get_folder_id`, and `check_resource_in_cache` delegate
    into `DriveResourceResolver` through `DriveResourceResolverPorts`.
  - Remote-id resource lookup for shortcut targets delegates into
    `DriveResourceById` through `DriveResourceByIdPorts`.
  - FUSE-visible path normalization and trash/lost+found/shared-with-me
    namespace predicates delegate into `DrivePathNamespace`.
  - Google Drive resource-key header construction delegates into
    `DriveResourceKeys`.
  - Filesystem-wide capacity reporting delegates quota and block math into
    `DriveFilesystemStats`.
  - Configured root-folder id resolution and synthetic well-known resource
    creation delegate into `DriveRootResolution` through
    `DriveRootResolutionPorts`.
  - `get_metadata` delegates into `DriveMetadataRefresh` through
    `DriveMetadataRefreshPorts`.
  - Create/delete/rename enter through public `Drive` functions here, and
    those wrappers delegate into `DriveMutations` through
    `DriveMutationPorts`.
  - `fopen` delegates into `DriveOpens` through `DriveOpenPorts`.
  - `get_attr`, `read_link`, and `opendir` delegate into `DriveViews`
    through `DriveViewPorts`.
  - `read_dir` delegates into `DriveDirectoryReads` through
    `DriveDirectoryReadPorts`.
  - `read` delegates into `DriveReads` through `DriveReadPorts`.
  - `download_resource` delegates into `DriveDownloads` through
    `DriveDownloadPorts`.
  - Cache-size accounting, cache shrink, cached-file deletion, memory-buffer
    cleanup, and file-lock cleanup delegate into `DriveCacheMaintenance`
    through `DriveCacheMaintenancePorts`.
  - The metadata-side `update_remote_resource` helper delegates into
    `DriveRemoteUpdates` through `DriveRemoteUpdatePorts`.
  - `write` and `truncate` delegate into `DriveFileMutations` through
    `DriveFileMutationPorts`.
  - `utime`, `chmod`, and `chown` delegate into `DriveMetadataMutations`
    through `DriveMetadataMutationPorts`.
  - `flush`, `fsync`, `release`, and rename-replace upload handoff delegate
    into `DriveUploadDispatch` through `DriveUploadDispatchPorts`.
  - The concrete upload attempt delegates into `DriveUploads` through
    `DriveUploadPorts`.
  - `get_xattr`, `set_xattr`, `list_xattr`, and `remove_xattr` delegate into
    `DriveXattrs` through `DriveXattrPorts`.
  - If a user-visible filesystem operation changes behavior, the fix is
    probably here.

- `src/driveResourceResolver.ml`
  - Path-to-resource resolver core for `get_resource`, `get_folder_id`, and
    `check_resource_in_cache`.
  - Owns metadata freshness boundaries, well-known root dispatch, ordinary
    cache lookup, negative-cache insertion, recursive parent lookup, and stale
    remote-id refresh.
  - Functorized over a narrow boundary so path-resolution behavior can be unit
    tested without real `Context`, Drive API requests, cache files, or retry
    wrappers.

- `src/driveResourceById.ml`
  - Remote-id resource lookup core for shortcut target reconstruction.
  - Owns cache-first lookup, root-folder remote-id special casing,
    parent-chain traversal, shared-with-me path reconstruction, and uncached
    Drive-file mapping to a cache resource.
  - Functorized over a narrow boundary so remote-id lookup behavior can be unit
    tested without real `Context`, cache files, OAuth, or Drive API requests.

- `src/drivePathNamespace.ml`
  - Pure namespace helper module for FUSE-visible path normalization.
  - Owns the root, trash, lost+found, and shared-with-me namespace constants;
    trash-to-cache path mapping; and the predicates used by read, mutation,
    upload, and xattr paths.
  - Does not use production ports because it depends only on the path string
    and `Config.t`.

- `src/driveResourceKeys.ml`
  - Pure helper module for Google Drive resource-key headers.
  - Builds `X-Goog-Drive-Resource-Keys` from remote id/resource-key pairs or
    cached resources, preserving input order while skipping missing ids and
    empty keys.
  - Used by downloads, metadata mutations, uploads, rename/delete/trash
    mutations, and xattr updates through the production `Drive` wrappers.

- `src/driveFilesystemStats.ml`
  - Pure helper module for filesystem-wide capacity reporting.
  - Converts `CacheData.Metadata` quota fields and `Config.team_drive_id` into
    the synthetic `Fuse.Unix_util.statvfs` record returned by `Drive.statfs`.
  - Owns the fixed 4 KiB block size, zero-limit/team-drive fallback policy, and
    placeholder `statvfs` fields.

- `src/driveRootResolution.ml`
  - Configured root-folder and well-known resource core.
  - Owns device-scope root discovery/creation, team-drive and configured
    root-folder resolution, context root-id memoization, and synthetic rows for
    root, trash root, lost+found, and shared-with-me.
  - Functorized over a narrow boundary so root behavior can be unit tested
    without real `Context`, cache files, OAuth, or Drive API requests.

- `src/driveMetadataRefresh.ml`
  - Metadata freshness and Drive change-feed replay core for `get_metadata`.
  - Owns Context/DB metadata selection policy, cache-size resync, metadata
    validity checks, account metadata refresh, start-page-token handling,
    no-change/over-limit/incremental replay branches, and synthetic view
    invalidation.
  - Functorized over a narrow boundary so metadata refresh behavior can be unit
    tested without real `Context`, Drive API requests, cache files, locks, or
    retry wrappers.

- `src/driveMutations.ml`
  - Create/delete/rename core.
  - Functorized over a narrow mutation-port boundary so those mutation paths
    can be unit tested without real Drive, cache-file, or thread side effects.
  - Owns the policy-heavy logic for create, delete/trash, and rename cache
    reconciliation.

- `src/driveOpens.ml`
  - Open-time access-validation core for `fopen`.
  - Owns request-mode classification, filesystem-wide read-only rejection,
    per-resource write-denial policy, and the pure file read-only predicate.
  - Functorized over a narrow boundary so file-open policy can be unit tested
    without real `Context`, Drive API requests, or cache access.

- `src/driveViews.ml`
  - Read-side view core.
  - Owns stat synthesis, link-target reconstruction, and the lookup-only
    `opendir` policy.
  - Functorized over a narrow boundary so those read-side view paths can be
    unit tested without real `Context`, files, or network calls.

- `src/driveDirectoryReads.ml`
  - Directory-listing core for `read_dir`.
  - Owns cache-hit reuse, synthetic-root listing strategy, remote query
    selection, and snapshot rebuild/replacement.
  - Keeps directory-listing policy separate from streaming/content-read logic.

- `src/driveReads.ml`
  - Regular-file read core for `read`.
  - Owns the branch between direct streaming, memory-buffer streaming, local
    cache-file reads, and read-ahead scheduling.
  - Functorized over a narrow boundary so read policy can be unit tested
    without real `Context`, cache files, network calls, or background threads.

- `src/driveDownloads.ml`
  - Local-content materialization core for `download_resource`.
  - Owns the state machine for existing-file reuse, dirty-state reuse,
    `ToDownload` MD5 checks, `Downloading` polling, desktop/document/media
    materialization branches, cache-size accounting order, and final state
    transitions.
  - Functorized over a narrow boundary so materialization policy can be unit
    tested without real `Context`, Drive API requests, cache files, locks, or
    sleeps.

- `src/driveCacheMaintenance.ml`
  - Cache maintenance core for cache-size accounting, cache shrink,
    cached-resource deletion, cached-file deletion, memory-buffer cleanup, and
    file-lock cleanup.
  - Functorized over a narrow boundary so cache accounting and cleanup behavior
    can be unit tested without real `Context`, cache files, locks, or
    filesystem state.

- `src/driveRemoteUpdates.ml`
  - Metadata-side remote-update wrapper.
  - Owns path normalization, read-only rejection, resource lookup before remote
    mutation, `Some file` cache save behavior, `None` purge behavior, and the
    optional local-cache file hook used by `utime`.
  - Functorized over a narrow boundary so wrapper control flow can be unit
    tested without real `Context`, Drive API requests, cache files, or
    filesystem checks.

- `src/driveFileMutations.ml`
  - Local file-mutation core for `write` and `truncate`.
  - Owns write-sink selection, dirty-state updates, and size/accounting
    changes for local content mutation.

- `src/driveMetadataMutations.ml`
  - Metadata-mutation core for `utime`, `chmod`, and `chown`.
  - Owns modified-time patches, mode app-property patches, uid/gid sentinel
    handling, and uid/gid app-property patch construction.
  - Functorized over a narrow boundary so metadata mutation behavior can be
    unit tested without real `Context`, Drive API requests, or cache files.

- `src/driveUploadDispatch.ml`
  - Upload-dispatch core for dirty-state gating and sync-vs-async handoff.
  - Owns the `ToUpload -> Uploading` gate, path re-resolution before dispatch,
    and the branch between direct upload and async enqueueing.

- `src/driveUploads.ml`
  - Concrete upload-attempt core for existing remote resources.
  - Owns outgoing MIME/media selection, upload size detection, early
    `Uploading` state/size updates, zero-byte media omission,
    `FilesResource.update` request shape, reload-by-remote-id reconciliation,
    conditional `Uploading -> Synchronized` transition, and final cache shrink.
  - Functorized over a narrow boundary so upload-attempt behavior can be unit
    tested without real `Context`, Drive API requests, cache files, or
    filesystem checks.

- `src/driveXattrs.ml`
  - Extended-attribute core.
  - Owns cached-xattr parsing, visible xattr reads, xattr create/replace
    validation, encoded length checks, and Drive app-property patch
    construction.
  - Functorized over a narrow boundary so xattr behavior can be unit tested
    without real `Context`, Drive API requests, or cache files.

### Global Runtime State

- `src/context.ml`
  - Defines the runtime context record shared globally through
    `ConcurrentGlobal`.
  - Holds config, state, cache handle, mountpoint info, locks, threads, memory
    buffers, and OAuth verification state.

- `src/state.ml`
  - Persistent OAuth/runtime state stored on disk.

- `src/config.ml`
  - Full configuration schema, defaults, parsing, serialization, and GAPI/CURL
    configuration construction.

- `src/appDir.ml`
  - Computes the application directory layout for config, data, cache, logs,
    and state, including XDG support.

### Metadata And Cache Layers

- `src/cacheData.ml`
  - Data model for cached Drive resources, upload queue entries, and global
    metadata.
  - Resource states are central to the codebase:
    `Synchronized`, `ToDownload`, `Downloading`, `ToUpload`, `Uploading`,
    `NotFound`.

- `src/cache.ml`
  - Front door for cache operations.
  - Dispatches to either SQLite-backed `DbCache` or in-memory `MemoryCache`
    depending on configuration.

- `src/dbCache.ml`
  - SQLite schema and prepared-statement implementation for resources,
    metadata, and upload queue.

- `src/memoryCache.ml`
  - In-memory metadata cache with periodic flush to SQLite.

### File Content Buffering

- `src/buffering.ml`
  - In-memory block cache used for streaming and optional buffered writes.

- `src/bufferPool.ml`
  - Pool of bigarray-backed buffers used by `Buffering`.

### Background Workers

- `src/uploadQueue.ml`
  - Async upload queue and worker thread pool.

- `src/backgroundFolderFetching.ml`
  - Optional folder prefetch thread.

- `src/threadPool.ml`
  - Generic thread pool used by async upload.

### OAuth / HTTP

- `src/oauth2.ml`
  - Request wrapper for authenticated Google API calls.
  - Token refresh and retry logic.
  - Interactive, headless, device, and loopback OAuth flows.

- `src/loopbackServer.ml`
  - Local HTTP callback server for OAuth loopback flow.

- `src/gaeProxy.ml`
  - Legacy proxy-assisted auth flow support.

### Utilities

- `src/utils.ml`
  - Logging, retries, process helpers, filesystem helpers, and misc support.

- `src/keyValueStore.ml`
  - Generic file-backed key/value serialization used by config/state stores.

- `src/mime.ml`
  - MIME mapping/export helpers for Google Docs-style files.

## Tests

Current tests are in:

- `test/testBuffering.ml`
- `test/testBufferPool.ml`
- `test/testConfigRuntime.ml`
- `test/testConfigStore.ml`
- `test/testDriveDirectoryReads.ml`
- `test/testDriveDownloads.ml`
- `test/testDriveCacheMaintenance.ml`
- `test/testDriveFilesystemStats.ml`
- `test/testDrivePathNamespace.ml`
- `test/testDriveResourceKeys.ml`
- `test/testDriveResourceById.ml`
- `test/testDriveRootResolution.ml`
- `test/testDriveRemoteUpdates.ml`
- `test/testDriveUploads.ml`
- `test/testDriveOpens.ml`
- `test/testDriveReads.ml`
- `test/testDriveFileMutations.ml`
- `test/testDriveMetadataMutations.ml`
- `test/testDriveXattrs.ml`
- `test/testDriveUploadDispatch.ml`
- `test/testDriveMutations.ml`
- `test/testDriveViews.ml`
- `test/testGdfuseCli.ml`
- `test/testGdfuseFlow.ml`
- `test/testThreadPool.ml`
- `test/testUtils.ml`

The `gdfuse` flow tests use:

- real `AppDir`
- real `ConfigStore`
- real `Context.StateFileStore`
- real in-memory `Context`
- fake browser/auth/CURL/FUSE/`exit`/`at_exit` boundaries

There are no end-to-end tests for:

- FUSE mount behavior
- Google Drive API integration
- OAuth flows
- rename/delete semantics against live Drive state

The mutation, open-validation, read-side, download/materialization,
cache-maintenance, path-namespace, resource-key header construction,
filesystem-stats, remote-id lookup, root-resolution, remote-update-wrapper,
file-mutation, metadata-mutation, upload-dispatch, upload-attempt, and xattr
cores are covered by focused unit tests in
`test/testDriveMutations.ml`,
`test/testDriveOpens.ml`, `test/testDriveViews.ml`,
`test/testDriveDirectoryReads.ml`, `test/testDriveDownloads.ml`,
`test/testDriveCacheMaintenance.ml`,
`test/testDriveFilesystemStats.ml`,
`test/testDrivePathNamespace.ml`,
`test/testDriveResourceKeys.ml`,
`test/testDriveResourceById.ml`, `test/testDriveRootResolution.ml`,
`test/testDriveRemoteUpdates.ml`, `test/testDriveUploads.ml`,
`test/testDriveReads.ml`,
`test/testDriveFileMutations.ml`, `test/testDriveMetadataMutations.ml`,
`test/testDriveUploadDispatch.ml`, and `test/testDriveXattrs.ml`, but live
Drive and FUSE integration need manual validation for behavior changes in those
paths.

Any change in those areas should be reasoned carefully and ideally verified
manually.
