# Repository Map

## Purpose

`google-drive-ocamlfuse` mounts Google Drive as a FUSE filesystem on Linux.
The executable is `google-drive-ocamlfuse`, built from `bin/gdfuse.ml`.

The codebase is mostly one executable plus a flat OCaml library in `src/`.

## Build And Test

- Build: `dune build @install`
- Test: `dune runtest`
- Format OCaml sources: `make format`
- Install locally: `dune install`
- Clean: `dune clean`

The `Makefile` is only a small wrapper around these dune commands.

## Top-Level Layout

- `bin/gdfuse.ml`: CLI entrypoint, application bootstrap, FUSE callback wiring,
  shutdown logic.
- `src/`: core implementation.
- `test/`: OUnit unit tests. Current tests cover support modules, not full FUSE
  or Google Drive integration.
- `docs/wiki/`: user-facing documentation imported from the project wiki.
- `tools/`: helper scripts, currently minimal. Includes `tools/format_ocaml`
  for running `ocamlformat` over tracked `.ml` and `.mli` files in the repo.
- `google-drive-ocamlfuse.opam`: package metadata and dependencies.
- `flake.nix`: Nix setup.

## Important Source Files

### Entry And Bootstrap

- `bin/gdfuse.ml`
  - Parses CLI flags.
  - Resolves app directories and config/state files.
  - Creates the global `Context`.
  - Initializes cache, CURL, OAuth state, and background threads.
  - Registers FUSE operations by delegating to `Drive`.

### Core Filesystem Logic

- `src/drive.ml`
  - The main implementation module.
  - Contains most filesystem behavior: lookup, listing, reads, writes, upload,
    rename, delete, xattrs, symlinks, metadata mapping, and Google Drive API
    requests.
  - If a user-visible filesystem operation changes behavior, the fix is
    probably here.

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
- `test/testThreadPool.ml`
- `test/testUtils.ml`

There are no end-to-end tests for:

- FUSE mount behavior
- Google Drive API integration
- OAuth flows
- rename/delete semantics against live Drive state

Any change in those areas should be reasoned carefully and ideally verified
manually.
