# `Drive.init_filesystem`

## Purpose

`Drive.init_filesystem` is the FUSE-side runtime bring-up hook for the
filesystem implementation. The public wrapper builds a small runtime from
`Context` and delegates the startup branch policy to `DriveRuntimeServices`.

It does not build the application context. That already happened earlier in
`GdfuseFlow.setup_application`. Its job is narrower: once FUSE is about to
start serving requests, it starts the background services that the `Drive`
module expects to have running during normal operation.

The public function is defined in `src/drive.ml` and exposed in
`src/drive.mli`. Production FUSE wiring calls it from `bin/gdfuseFuse.ml`
through `Fuse.init = init_filesystem`.

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

The public wrapper is intentionally small:

```ocaml
let init_filesystem () =
  RuntimeServiceOps.init_filesystem (drive_runtime_services_runtime ())
```

`drive_runtime_services_runtime` reads the cache and config from `Context`:

```ocaml
type runtime = {
  cache : CacheData.t;
  config : Config.t;
}
```

The startup policy lives in `DriveRuntimeServices`:

```ocaml
let init_filesystem runtime =
  P.start_flush_db_thread runtime.cache;
  if runtime.config.Config.async_upload_queue then
    P.start_async_upload_thread runtime.cache
      runtime.config.Config.async_upload_threads P.upload_resource_by_id;
  if runtime.config.Config.background_folder_fetching then
    P.start_folder_fetching_thread runtime.cache (fun path ->
        P.read_dir path |> ignore)
```

Each branch starts a distinct subsystem through production ports supplied by
`Drive`.

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
network I/O. `DriveRuntimeServices` starts it here because the mounted
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

This is why `DriveRuntimeServices` passes `upload_resource_by_id` instead of
letting `UploadQueue` talk directly to Drive internals: queue scheduling lives
in `UploadQueue`, but resource upload behavior lives in `Drive`.

See `docs/agent-docs/drive-upload-path.md` for the end-to-end upload lifecycle.
See `docs/agent-docs/upload-queue-start-async-upload-thread.md` for the queue
runtime-state installation and the poll-thread startup performed by
`UploadQueue.start_async_upload_thread`.
See `docs/agent-docs/upload-queue-poll-upload-queue.md` for the long-lived
poll loop that paces async dispatch and drains queued work during shutdown.
See `docs/agent-docs/upload-queue-upload-resource.md` for the poll-thread
helper that selects one queued entry, marks it `Uploading`, and decides
delete-vs-requeue around the worker callback.
See `docs/agent-docs/drive-upload-resource-by-id.md` for the worker-side
callback that reloads queued resources by cache id before entering the normal
upload wrapper.
See `docs/agent-docs/drive-upload-resource-with-retry.md` for the common upload
wrapper that the async worker eventually enters.

### 3. Background folder prefetching

If `config.background_folder_fetching = true`, the function starts a background
folder-fetch thread:

- polling logic lives in `BackgroundFolderFetching`
- actual folder loading is delegated back into `Drive.read_dir`

The callback passed from `DriveRuntimeServices` is:

```ocaml
fun path -> read_dir path |> ignore
```

That reuse is deliberate. Prefetching is not a separate folder-sync code path;
it simply drives the same public `Drive.read_dir` path that a foreground
`readdir` request would use. That public wrapper delegates the main
directory-refresh logic into `DriveDirectoryReads`.
If prefetch reaches the root or a well-known synthetic view, configured root id
resolution and synthetic rows are handled by `DriveRootResolution` through the
normal `Drive.read_dir` path.

The folder-fetch loop repeatedly:

1. selects the next folder resource eligible for fetching
2. calls `read_dir` on that folder path

The selection criteria come from the cache layer. In practice it looks for
folder resources whose state is `ToDownload` and that are not in trash.

Calling `read_dir` does the expensive part:

- fetch child metadata from Drive when the folder is not already cached
- insert/update child `CacheData.Resource.t` rows
- mark the folder resource itself as `Synchronized`

So this thread is best understood as opportunistic metadata warming for folder
trees.

See `docs/agent-docs/drive-read-dir.md` for the directory-refresh details.
See `docs/agent-docs/background-folder-fetching-start-thread.md` for the
runtime-state installation and polling-thread startup done by
`BackgroundFolderFetching.start_folder_fetching_thread`.

## Config Knobs

`DriveRuntimeServices.init_filesystem` is controlled by these runtime config
fields:

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

`async_upload_queue_max_length` does not affect startup directly. It is
enforced later when the upload-dispatch layer queues a resource through
`DriveUploadDispatch.queue_upload`.

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

See `docs/agent-docs/upload-queue-stop-async-upload-thread.md` for the queue
side of that stop request, and
`docs/agent-docs/upload-queue-poll-upload-queue.md` for the drain behavior that
follows.
See `docs/agent-docs/background-folder-fetching-stop-thread.md` for the folder
prefetch stop-request helper that shutdown calls before joining that thread.

Because shutdown is coordinated elsewhere, `DriveRuntimeServices` only starts
threads; it does not own their lifecycle end-to-end.

## Why This Function Exists In `Drive`

At first glance, this function could have lived in `GdfuseFlow`, but keeping it
in `Drive` has a useful property: the FUSE adapter treats it like any other
filesystem callback.

That keeps the ownership boundary simple:

- `GdfuseFlow` prepares global runtime state
- `gdfuseFuse` maps FUSE operations to `Drive`
- `Drive.init_filesystem` delegates Drive-specific runtime bring-up to
  `DriveRuntimeServices`

The background services started here are tightly coupled to Drive semantics:

- upload queue workers eventually call Drive upload code
- folder prefetching literally calls the public `Drive.read_dir` path
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
  idempotency protection around the `DriveRuntimeServices` startup path

## Source Pointers

- `src/driveRuntimeServices.ml`: startup branch policy
- `src/drive.ml`: `init_filesystem`, `read_dir`, `upload_resource_by_id`
- `src/driveDirectoryReads.ml`: `read_dir`
- `src/memoryCache.ml`: flush-db thread startup and polling loop
- `src/uploadQueue.ml`: async upload poll thread and worker pool
- `src/backgroundFolderFetching.ml`: folder-prefetch poll thread
- `src/gdfuseFlow.ml`: shutdown and thread teardown
- `bin/gdfuseFuse.ml`: FUSE `init` callback wiring
