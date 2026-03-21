# `Drive.init_filesystem`

## Purpose

`Drive.init_filesystem` is the FUSE-side runtime bring-up hook for the
filesystem implementation.

It does not build the application context. That already happened earlier in
`GdfuseFlow.setup_application`. Its job is narrower: once FUSE is about to
start serving requests, it starts the background services that the `Drive`
module expects to have running during normal operation.

The function is defined in `src/drive.ml` and exposed in `src/drive.mli`.
Production FUSE wiring calls it from `bin/gdfuseFuse.ml` through
`Fuse.init = init_filesystem`.

## Call Path

The mount-time path is:

1. `GdfuseFlow.run_mount_mode`
2. injected FUSE startup
3. `bin/gdfuseFuse.ml`
4. FUSE `init` callback
5. `Drive.init_filesystem`

This matters because bootstrap-only mode never reaches this function. That is
intentional: bootstrap/auth flows should not start long-lived background
threads.

## Preconditions

`Drive.init_filesystem` assumes:

- `Context.set_ctx` has already installed a valid global `Context.t`
- the cache handle inside the context is initialized
- runtime config has already been resolved
- OAuth/bootstrap validation already completed

It reads everything from the global context and takes no explicit arguments.

The function also assumes it is called once per mounted filesystem instance.
There is no idempotency guard here. Re-entering it would start new background
threads and overwrite the thread handles stored in `Context`.

## Implementation Walkthrough

The body is intentionally small:

```ocaml
let init_filesystem () =
  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  MemoryCache.start_flush_db_thread cache;
  let config = context |. Context.config_lens in
  if config.Config.async_upload_queue then
    UploadQueue.start_async_upload_thread cache
      config.Config.async_upload_threads upload_resource_by_id;
  if config.Config.background_folder_fetching then
    BackgroundFolderFetching.start_folder_fetching_thread cache (fun path ->
        read_dir path |> ignore)
```

Each line starts a distinct subsystem.

### 1. Metadata flush thread

`MemoryCache.start_flush_db_thread cache` is always called, but it only starts a
thread when `cache.CacheData.in_memory = true`.

That means:

- sqlite-backed metadata mode: this call is a no-op
- in-memory metadata mode: start a periodic flush thread

The flush thread sleeps for `cache.CacheData.autosaving_interval` seconds, then
persists dirty in-memory state back to SQLite:

- metadata row
- resource rows
- upload-queue rows

This keeps the fast in-memory metadata cache crash-recoverable enough for the
next startup.

Operationally, this thread is infrastructure for the cache layer, not for
network I/O. `Drive.init_filesystem` starts it here because the mounted
filesystem is the runtime phase where metadata mutations will begin happening.

### 2. Async upload queue

If `config.async_upload_queue = true`, the function starts the async upload
subsystem:

- one poll thread created by `UploadQueue.start_async_upload_thread`
- a `ThreadPool.t` sized by `config.async_upload_threads`
- upload work delegated back into `Drive.upload_resource_by_id`

The separation is important:

- the poll thread watches the cache-backed upload queue
- worker threads perform actual uploads
- the upload callback stays in `Drive`, so upload semantics remain centralized

The poll loop wakes once per second, selects the next queued entry, marks it as
`Uploading`, and submits work to the pool. The worker then:

1. looks up the resource by cache id
2. runs the normal upload path with retry logic
3. removes the queue entry on success

On failure, the queue entry is moved back to `ToUpload` so it can be retried
later.

This is why `Drive.init_filesystem` passes `upload_resource_by_id` instead of
letting `UploadQueue` talk directly to Drive internals: queue scheduling lives
in `UploadQueue`, but resource upload behavior still lives in `Drive`.

### 3. Background folder prefetching

If `config.background_folder_fetching = true`, the function starts a background
folder-fetch thread:

- polling logic lives in `BackgroundFolderFetching`
- actual folder loading is delegated back into `Drive.read_dir`

The callback passed from `Drive.init_filesystem` is:

```ocaml
fun path -> read_dir path |> ignore
```

That reuse is deliberate. Prefetching is not a separate folder-sync code path;
it simply drives the same `Drive.read_dir` logic that a foreground `readdir`
request would use.

The folder-fetch loop repeatedly:

1. selects the next folder resource eligible for fetching
2. calls `read_dir` on that folder path

The selection criteria come from the cache layer. In practice it looks for
folder resources whose state is still `ToDownload` and that are not in trash.

Calling `read_dir` does the expensive part:

- fetch child metadata from Drive when the folder is not already cached
- insert/update child `CacheData.Resource.t` rows
- mark the folder resource itself as `Synchronized`

So this thread is best understood as opportunistic metadata warming for folder
trees.

See `docs/agent-docs/drive-read-dir.md` for the directory-refresh details.

## Config Knobs

`Drive.init_filesystem` is controlled by these runtime config fields:

- `metadata_memory_cache`
- `metadata_memory_cache_saving_interval`
- `async_upload_queue`
- `async_upload_threads`
- `background_folder_fetching`

The relationships are:

- `metadata_memory_cache=true` enables the periodic flush thread indirectly,
  because the cache handle is created in in-memory mode
- `metadata_memory_cache_saving_interval` becomes the flush interval
- `async_upload_queue=true` enables queued/background uploads
- `async_upload_threads` sets pool width for upload workers
- `background_folder_fetching=true` enables folder prefetching

`async_upload_queue_max_length` does not affect startup directly. It is enforced
later when writes are queued by `Drive.queue_upload`.

## Context And Shutdown Contract

Each started subsystem stores its thread handle back into `Context`:

- `Context.flush_db_thread`
- `Context.async_upload_thread`
- `Context.folder_fetching_thread`

Shutdown depends on those handles. `GdfuseFlow.shutdown` reads the current
context, signals each subsystem to stop, then joins the threads:

- `MemoryCache.stop_flush_db_thread ()`
- `UploadQueue.stop_async_upload_thread ()`
- `BackgroundFolderFetching.stop_folder_fetching_thread ()`

Two details matter here:

- the async upload stop path drains queued work before shutting down the thread
  pool
- the folder-prefetch and flush-db threads are simple polling loops that exit
  after their stop flag is observed

Because shutdown is coordinated elsewhere, `Drive.init_filesystem` only starts
threads; it does not own their lifecycle end-to-end.

## Why This Function Exists In `Drive`

At first glance, this function could have lived in `GdfuseFlow`, but keeping it
in `Drive` has a useful property: the FUSE adapter treats it like any other
filesystem callback.

That keeps the ownership boundary simple:

- `GdfuseFlow` prepares global runtime state
- `gdfuseFuse` maps FUSE operations to `Drive`
- `Drive.init_filesystem` performs Drive-specific runtime bring-up

The background services started here are tightly coupled to Drive semantics:

- upload queue workers eventually call Drive upload code
- folder prefetching literally calls `Drive.read_dir`
- metadata flushing supports cache mutations performed by Drive operations

## Maintenance Notes

When changing this area, check these invariants:

- if a new background subsystem is started here, add stop/join logic in
  `GdfuseFlow.shutdown`
- if startup begins depending on new config fields, document them in
  `docs/wiki/Configuration.md` too
- if `read_dir` semantics change, reevaluate background folder fetching because
  it reuses the same path
- if this function ever becomes callable more than once, add explicit
  idempotency protection

## Source Pointers

- `src/drive.ml`: `init_filesystem`, `read_dir`, `upload_resource_by_id`
- `src/memoryCache.ml`: flush-db thread startup and polling loop
- `src/uploadQueue.ml`: async upload poll thread and worker pool
- `src/backgroundFolderFetching.ml`: folder-prefetch poll thread
- `src/gdfuseFlow.ml`: shutdown and thread teardown
- `bin/gdfuseFuse.ml`: FUSE `init` callback wiring
