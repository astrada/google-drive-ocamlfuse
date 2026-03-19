# Common Workflows And Change Guidance

## Before Editing

For most changes, inspect these files first:

- `src/gdfuseCli.ml`
- `src/gdfuseFlow.ml`
- `src/drive.ml`
- `src/context.ml`
- `src/cacheData.ml`
- the backend module involved: `cache.ml`, `dbCache.ml`, `memoryCache.ml`,
  `buffering.ml`, `uploadQueue.ml`, or `oauth2.ml`

This repo has a lot of behavior in one large module (`Drive`), so broad
assumptions are risky.

## Where To Change Things

### CLI, startup, mount flags, shutdown

Edit:

- `src/gdfuseCli.ml`
- `src/gdfuseFlow.ml`
- `bin/gdfuseRuntimeDeps.ml`
- `bin/gdfuseFuse.ml`

Typical examples:

- adding a command-line option
- changing setup order
- adjusting shutdown cleanup
- changing which background threads start
- changing how production side effects are wired into `GdfuseFlow.Make`

Only edit `bin/gdfuse.ml` if the top-level executable dispatch itself changes.

### Config schema or defaults

Edit:

- `src/config.ml`
- `src/configStore.ml`
- `src/configRuntime.ml`
- possibly `src/appDir.ml`
- relevant wiki docs in `docs/wiki/Configuration.md`

Be careful to update:

- record type
- lenses
- defaults
- TOML parsing/grouping in `ConfigStore`
- serialization back to file
- runtime validation in `Config.validate`
- runtime override/persistence policy in `ConfigRuntime.resolve`

### Filesystem semantics

Edit:

- `src/drive.ml`

Typical examples:

- `readdir`, `getattr`, `read`, `write`
- create/delete/rename/truncate
- xattr and symlink behavior
- special virtual directories

### Metadata persistence or lookup behavior

Edit:

- `src/cacheData.ml`
- `src/cache.ml`
- `src/dbCache.ml`
- `src/memoryCache.ml`

If the change adds or changes persisted metadata, inspect the SQLite schema and
memory-cache flush path together.

### Buffered I/O and streaming

Edit:

- `src/buffering.ml`
- `src/bufferPool.ml`

This area is performance-sensitive and concurrency-sensitive.

### Async upload behavior

Edit:

- `src/uploadQueue.ml`
- `src/drive.ml`

Check both queue persistence and the upload state machine.

### OAuth / auth failures

Edit:

- `src/gdfuseFlow.ml`
- `src/oauth2.ml`
- `src/loopbackServer.ml`
- `src/gaeProxy.ml`
- `bin/gdfuseRuntimeDeps.ml`

## Practical Rules

### Rule 1: Treat `Drive.rename` as high risk

It mixes:

- Drive API update/delete operations
- cache invalidation
- target replacement
- folder subtree cache cleanup
- state transitions

Read the entire function before changing even a small branch.

### Rule 2: Keep cache metadata and file content changes aligned

If local bytes change, confirm all of these still make sense:

- cache file contents
- `Resource.size`
- `Resource.state`
- upload queue state, if async uploads are enabled

### Rule 3: Respect special virtual paths

The filesystem exposes synthetic locations that are not normal Drive folders:

- `/.Trash`
- `/.shared`
- `/lost+found`

Many path operations first map through `get_path_in_cache`. Changes that ignore
that mapping usually break trash/shared behavior.

### Rule 4: Preserve error translation

Internal exceptions like `File_not_found` and `Permission_denied` are part of
the contract between `Drive` and the FUSE wrapper in `bin/gdfuseFuse.ml`.

If you add a new failure path, either:

- reuse an existing repository exception, or
- ensure it becomes a sensible Unix error at the boundary

### Rule 5: Watch shutdown behavior

There are several background threads and deferred writes. Changes that appear
correct during steady state can still lose data or deadlock on exit.

Relevant shutdown steps:

- stop buffer eviction
- stop metadata flush thread
- stop async upload thread
- stop folder prefetch thread
- `Cache.flush`
- mark clean shutdown in SQLite

## Testing Strategy

## What Exists

- unit tests for buffering, buffer pool, thread pool, utils, config store,
  config runtime, `gdfuse` CLI parsing, and `gdfuse` application flow

## What Does Not Exist

- integration tests for FUSE mount operations
- tests against the real Drive API
- tests for OAuth flows

## Recommended Validation For Risky Changes

- `dune runtest`
- build with `dune build @install`
- if the change affects live filesystem behavior, do a manual mount test
- if the change affects auth, test the specific auth mode you touched

Manual testing matters for:

- rename/move
- uploads and flush/release timing
- trash/delete behavior
- shared/team-drive behavior
- Google Docs export behavior

## Known Structural Constraints

- `src/drive.ml` is large and central; many changes are cross-cutting.
- The codebase still relies on mutable global `Context` for runtime state.
- Application startup/shutdown now has a narrow dependency-injection boundary
  through `GdfuseFlow.Make`, but the rest of the filesystem path is still
  largely concrete.
- A lot of metadata is mirrored both in cache and in Drive app properties.
- Threading is real; this is not a purely single-threaded FUSE wrapper.

## Suggested Reading By Task

### Adding a new mount/config option

Read:

- `src/gdfuseCli.ml`
- `src/gdfuseFlow.ml`
- `src/config.ml`
- `src/configRuntime.ml`
- `src/appDir.ml`

### Fixing wrong file metadata or directory listings

Read:

- `src/drive.ml`
- `src/cacheData.ml`
- `src/cache.ml`
- `src/dbCache.ml`

### Fixing upload bugs

Read:

- `src/drive.ml`
- `src/uploadQueue.ml`
- `src/buffering.ml`
- `src/cacheData.ml`

### Fixing auth or token refresh bugs

Read:

- `src/gdfuseFlow.ml`
- `src/oauth2.ml`
- `src/loopbackServer.ml`
- `src/gaeProxy.ml`
- `src/context.ml`
- `src/state.ml`
- `bin/gdfuseRuntimeDeps.ml`

### Fixing cache corruption / stale state bugs

Read:

- `src/cache.ml`
- `src/dbCache.ml`
- `src/memoryCache.ml`
- `src/cacheData.ml`
- shutdown flow in `src/gdfuseFlow.ml`
