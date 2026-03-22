# Architecture

## High-Level Model

The executable mounts a FUSE filesystem whose operations are implemented in
`Drive`. `Drive` translates filesystem requests into:

- cache lookups and updates
- local cache-file reads and writes
- Google Drive API calls through `gapi-ocaml`
- background work such as uploads and folder prefetching

The design is stateful. A global `Context.t` stores the current config, state,
cache handle, memory buffers, locks, and background threads.

Startup/shutdown orchestration is library-visible in `src/gdfuseFlow.ml`.
The executable in `bin/gdfuse.ml` is mostly a thin wrapper that parses CLI
arguments, instantiates `GdfuseFlow.Make(GdfuseRuntimeDeps)`, and dispatches
into bootstrap-only or mount mode.

## Runtime Boot Sequence

Startup is split across:

- `src/gdfuseCli.ml`: CLI parsing
- `src/gdfuseFlow.ml`: orchestration
- `bin/gdfuseRuntimeDeps.ml`: production side-effect wiring
- `bin/gdfuseFuse.ml`: FUSE callback registration

The production boot sequence is:

1. Parse CLI arguments.
2. Instantiate `GdfuseFlow.Make(GdfuseRuntimeDeps)`.
3. Resolve config path using `AppDir.get_config_path`.
4. Load or create config and state files.
5. Build application directories with `AppDir.create`.
6. Open log output through injected system deps.
7. Resolve runtime config and build GAPI/CURL config.
8. Create the cache handle with injected cache deps.
9. Optionally clear cache or rebuild it on version mismatch / dirty shutdown.
10. Initialize `Buffering.MemoryBuffers`.
11. Populate and install global `Context`.
12. Ensure OAuth credentials are available.
13. In mount mode, register shutdown through injected `register_exit`.
14. Enter `Fuse.main`, with callbacks delegated to `Drive`.

The no-mountpoint branch runs startup/auth/bootstrap without
entering the FUSE loop.

At FUSE init time, `Drive.init_filesystem` starts background services:

- memory-cache flush thread
- async upload thread pool, if enabled
- background folder prefetching thread, if enabled

See `docs/agent-docs/drive-init-filesystem.md` for the detailed control flow
and lifecycle notes.

Shutdown logic lives in `GdfuseFlow.shutdown`, and the production executable
registers it through `GdfuseRuntimeDeps.System.register_exit`. Shutdown
stops threads, flushes cache state, marks clean shutdown in SQLite, performs
CURL cleanup, and clears the global context.

## Persistent State Layout

The application uses two kinds of persistent files:

- config file: user configuration
- state file: OAuth tokens and saved version

By default these live under `~/.gdfuse/<label>/`, unless:

- `-config` points elsewhere
- XDG mode is active
- config overrides data/cache/log directories

`AppDir.t` computes:

- `config_path`
- `data_dir`
- `cache_dir`
- `log_dir`
- `state_path`
- `app_log_path`
- `curl_log_path`

## Core Data Structures

### `Context.t`

Global runtime state. Important fields:

- `config_store`: parsed config plus file path
- `state_store`: persistent OAuth/runtime state plus file path
- `gapi_config`: request/auth configuration for Google APIs
- `cache`: cache handle describing db path, cache dir, and cache mode
- `memory_buffers`: in-memory data buffers for streaming/write buffering
- `metadata`: cached global metadata
- `file_locks`: per-path mutex table
- `flush_db_thread`, `async_upload_thread`, `folder_fetching_thread`

### `CacheData.Resource.t`

Represents one cached filesystem object and its remote Drive metadata.

Fields mix:

- remote Drive data: `remote_id`, `mime_type`, `size`, `version`, etc.
- Drive-stored app properties: mode bits, uid/gid, symlink target, xattrs
- local cache data: `parent_path`, `path`, `state`, `last_update`

This is the main record used to reconcile local filesystem state and remote
Drive state.

### Resource State Machine

Important states:

- `Synchronized`: cache metadata matches remote state
- `ToDownload`: metadata/content should be refreshed from Drive
- `Downloading`: content transfer is in progress
- `ToUpload`: local cache content changed and must be uploaded
- `Uploading`: upload in progress
- `NotFound`: tombstone-like placeholder for absent paths

Many correctness bugs reduce to incorrect state transitions.

## Cache Architecture

There are two metadata-cache modes:

1. SQLite-backed mode via `DbCache`
2. In-memory metadata mode via `MemoryCache`, with periodic flush to SQLite

`Cache` is the dispatch layer. It chooses the backend based on
`config.metadata_memory_cache`.

The cache stores:

- resource rows
- global metadata row
- upload queue rows

Cached file content is separate from metadata. File bytes live in files inside
the cache directory, keyed by Drive `remote_id`.

## Request Flow

Most remote operations run through:

- `Oauth2.do_request`: authenticated request wrapper
- `Drive.with_retry_default`: Drive-specific exception mapping and retries

The FUSE adapter boundary in `bin/gdfuseFuse.ml` then wraps `Drive` callbacks
through `handle_exception`, `with_drive_op`, and `drive_path_op`; see
`docs/agent-docs/gdfuse-fuse-boundary.md`.

Retry handling is split:

- CURL/network failures and temporary API errors are retried
- auth failures trigger refresh logic
- permanent API failures are mapped to repository-specific exceptions such as
  `File_not_found` or `Permission_denied`

`bin/gdfuseFuse.ml` converts those exceptions into Unix/FUSE errors.

Open-time file access validation lives in `Drive.fopen`; see
`docs/agent-docs/drive-fopen.md`.

Directory-open validation lives in `Drive.opendir`; see
`docs/agent-docs/drive-opendir.md`.

The directory-side `releasedir` / `fsyncdir` callbacks remain adapter-level
no-ops in `bin/gdfuseFuse.ml`; see
`docs/agent-docs/gdfuse-noop-dir-callbacks.md`.

Filesystem-wide capacity reporting lives in `Drive.statfs`; see
`docs/agent-docs/drive-statfs.md`.

Metadata-only `utime` / `chmod` / `chown` requests enter through thin wrappers
over `Drive.update_remote_resource`; see
`docs/agent-docs/drive-chmod-chown-utime.md`.

Path-level stat synthesis lives in `Drive.get_attr`; see
`docs/agent-docs/drive-get-attr.md`.

Regular file and folder creation requests enter through `Drive.mknod` /
`Drive.mkdir`; see `docs/agent-docs/drive-mknod-mkdir.md`.

Symlink-target reads live in `Drive.read_link`; see
`docs/agent-docs/drive-read-link.md`.

Symlink creation requests enter through `Drive.symlink`; see
`docs/agent-docs/drive-symlink.md`.

Extended-attribute handling lives in the xattr paths in `Drive`; see
`docs/agent-docs/drive-xattr.md`.

Visible delete requests enter through `Drive.unlink` / `Drive.rmdir`; see
`docs/agent-docs/drive-unlink-rmdir.md`.

Delete policy then flows through `Drive.delete_remote_resource`; see
`docs/agent-docs/drive-delete-remote-resource.md`.

## Directory And Metadata Fetching

`Drive.read_dir` is the main listing path.

It:

1. Maps special filesystem paths like `/.Trash` into cache semantics.
2. Checks whether the folder listing is already valid in cache.
3. If not cached, queries Drive with `FilesResource.list`.
4. Merges returned files with existing cached resources.
5. Resolves duplicate names using remote-id fingerprints.
6. Writes listing results back into the cache.

Special virtual directories are implemented in `Drive`:

- `/.Trash`
- `/.shared`
- `/lost+found`

## Read Path

`Drive.read` does not always read from a local file directly.

The path is:

1. Resolve the resource from cache / remote metadata.
2. Decide whether to stream or download based on config and file properties.
3. If streaming:
   - stream directly into the FUSE buffer, or
   - stream into in-memory blocks with optional read-ahead
4. If not streaming:
   - ensure a local cached file exists
   - read bytes from the local cache file

Buffered streaming is handled by `Buffering.MemoryBuffers`.

See `docs/agent-docs/drive-read.md` for the top-level branch selection, and
`docs/agent-docs/drive-download-resource.md` for the non-streaming
materialization path behind the local-file branch.

## Write Path

`Drive.write` always works against the local cached file first.

The path is:

1. Resolve resource.
2. Ensure local content exists via `download_resource`.
3. Write data:
   - to memory blocks if `config.write_buffers = true`
   - otherwise directly to the cache file
4. Update cached size if needed.
5. Mark resource state as `ToUpload`.

See `docs/agent-docs/drive-write.md` for the detailed write-side control flow
and the buffered-vs-direct local write split.

See `docs/agent-docs/drive-truncate.md` for the sibling size-mutation path that
flushes buffers, updates cached length, and reuses the same upload lifecycle.

Upload is not performed on every `write`. It is triggered by:

- `flush`
- `fsync`
- `release`

all of which call `upload_if_dirty`.

See `docs/agent-docs/drive-flush-fsync-release.md` for the thin file-callback
layer that gates upload dispatch on the `ToUpload` state.

See `docs/agent-docs/drive-download-resource.md` for the helper that ensures the
local cache file exists before write-side mutation begins.

## Upload Path

Upload logic lives in `Drive.upload`, `Drive.queue_upload`,
`Drive.upload_resource_by_id`, and `UploadQueue`.

The file-level entrypoints that trigger this path are `Drive.flush`,
`Drive.fsync`, and `Drive.release`; see
`docs/agent-docs/drive-flush-fsync-release.md`.

Two modes exist:

1. synchronous upload
2. queued async upload

Shared behavior:

- flush pending memory buffers to the cache file first
- update resource state to `Uploading`
- upload media with `FilesResource.update`
- reconcile returned metadata into cache
- move state back to `Synchronized` if still appropriate

The async path persists queue entries in cache, then a background thread polls
the queue and hands work to `ThreadPool`.

See `docs/agent-docs/upload-queue-queue-resource.md` for the enqueue-time
helper that persists and deduplicates upload-queue rows.

See `docs/agent-docs/upload-queue-start-async-upload-thread.md` for the
runtime-state installation and thread startup that bring this subsystem into
existence.

See `docs/agent-docs/upload-queue-stop-async-upload-thread.md` for the stop
request that asks the poll loop to drain rather than stop immediately.

See `docs/agent-docs/upload-queue-poll-upload-queue.md` for the long-lived
poll loop, its one-second pacing, and its shutdown-drain contract.

See `docs/agent-docs/upload-queue-upload-resource.md` for the poll-thread
helper that selects one queued entry and wraps it in queue-entry state
transitions.

See `docs/agent-docs/thread-pool-create.md` for the generic construction of
the worker pool that async upload startup installs into runtime state.

See `docs/agent-docs/thread-pool-add-work.md` for the generic worker-pool
boundary that waits for capacity, starts one thread, and releases worker slots
even when a worker raises.

See `docs/agent-docs/thread-pool-pending-threads.md` for the observable worker
count that the async-upload shutdown path logs before joining workers.

See `docs/agent-docs/thread-pool-shutdown.md` for the generic join step used
after queue draining to wait for still-running workers.

See `docs/agent-docs/drive-upload-resource-by-id.md` for the worker-side
bridge from queue entries back into the normal request/session upload path.

## Rename / Move Semantics

`Drive.rename` is one of the more complex parts of the repository.

It must handle:

- plain rename inside the same parent
- move between parents
- overwrite vs preserve-target behavior
- trash semantics
- folder moves
- special virtual directories
- `mv_keep_target` content replacement behavior

When working on rename behavior, read that function end-to-end before editing.
It combines remote Drive operations with local cache surgery.

See `docs/agent-docs/drive-rename.md` for the detailed branch structure.

## OAuth Modes

OAuth support is broader than a simple browser flow:

- interactive browser flow
- headless mode
- device flow
- loopback local server flow
- service account flow
- legacy GAE proxy mode

Main modules:

- `src/oauth2.ml`
- `src/loopbackServer.ml`
- `src/gaeProxy.ml`

The current default config enables OAuth loopback on port `8080`.

The orchestration around those modes lives in `GdfuseFlow`, while the real
browser/auth side effects are supplied by `GdfuseRuntimeDeps`.

## Flow Testability

The application flow is now unit testable from `test/` because it was moved
from executable-private modules under `bin/` into library modules under `src/`.

The testing approach is intentionally mixed:

- real `AppDir`
- real `ConfigStore`
- real `Context.StateFileStore`
- real in-memory `Context`
- fake browser/auth/CURL/FUSE/`exit`/`at_exit` deps through
  `GdfuseFlow.Make(FakeDeps)`

This gives good coverage of startup/shutdown sequencing without requiring live
Drive access or a mounted FUSE filesystem.

## Concurrency Model

The codebase uses OCaml system threads plus mutable global state.

Concurrency-sensitive parts:

- `Context` global record
- `MemoryCache`
- `Buffering.MemoryBuffers`
- upload queue and thread pool
- folder prefetch thread

The repo uses dedicated mutexes and `ConcurrentGlobal` wrappers rather than a
single fully-serialized model. Small ordering mistakes can create subtle bugs.

When changing concurrent code, explicitly trace:

- who owns each mutable structure
- when data is persisted to SQLite
- whether a thread may still access stale state during shutdown
