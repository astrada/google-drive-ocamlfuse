# `UploadQueue.start_async_upload_thread`

## Purpose

`UploadQueue.start_async_upload_thread` is the startup boundary for the async
upload subsystem.

It owns three things:

- building the `ConcurrentUploadQueue` runtime record
- creating the worker `ThreadPool.t`
- spawning the long-lived poll thread and publishing its handle into `Context`

It does not inspect queued work itself. That begins later in
`poll_upload_queue`.

## Public Signature

`start_async_upload_thread` is part of `src/uploadQueue.mli`:

```ocaml
val start_async_upload_thread :
  CacheData.t -> int -> (int64 -> unit) -> unit
```

The parameters are:

- `cache`: shared cache handle used by the poll loop
- `upload_threads`: maximum worker concurrency
- `upload_resource`: callback for one queued `resource_id`

In production, `DriveRuntimeServices` passes `Drive.upload_resource_by_id`
through the `Drive` runtime-service port as that callback.

## Entire Implementation

```ocaml
let start_async_upload_thread cache upload_threads upload_resource =
  let data =
    {
      stop_async_upload = false;
      upload_resource_by_id = upload_resource;
      thread_pool = ThreadPool.create ~max_threads:upload_threads ();
    }
  in
  ConcurrentUploadQueue.set data;
  let thread = Thread.create poll_upload_queue cache in
  Utils.log_with_header "Starting async upload thread (TID=%d)\n%!"
    (Thread.id thread);
  Context.update_ctx (Context.async_upload_thread ^= Some thread)
```

That is the whole helper.

## High-Level Flow

At a high level, startup does this:

1. build an `UploadQueue.t` runtime record
2. set `stop_async_upload = false`
3. create the worker thread pool
4. publish the runtime record through `ConcurrentUploadQueue.set`
5. spawn the poll thread with `Thread.create poll_upload_queue cache`
6. store the resulting thread handle in `Context.async_upload_thread`

## Installed Runtime State

The installed record is:

```ocaml
{
  stop_async_upload = false;
  upload_resource_by_id = upload_resource;
  thread_pool = ThreadPool.create ~max_threads:upload_threads ();
}
```

That record is the whole queue runtime state.

The important fields are:

- `stop_async_upload`: later flipped by `stop_async_upload_thread`
- `upload_resource_by_id`: callback injected from `Drive`
- `thread_pool`: worker pool shared by all queued uploads

See `docs/agent-docs/thread-pool-create.md` for the generic construction
contract of `ThreadPool.create`.

## Why `set` Happens Before `Thread.create`

The ordering is deliberate:

```ocaml
ConcurrentUploadQueue.set data;
let thread = Thread.create poll_upload_queue cache in
```

`poll_upload_queue` and `upload_resource` both read from
`ConcurrentUploadQueue` immediately. Installing the record first avoids a race
against uninitialized queue runtime state.

## Context Registration

After starting the poll thread, the helper stores it in:

```ocaml
Context.async_upload_thread
```

That is the shutdown-facing side effect of startup. Higher-level shutdown code
does not recreate or rediscover the thread handle; it reads this stored value.

## Single-Start Assumption

There is no explicit guard against calling this helper twice.

A second call would:

- replace the `ConcurrentUploadQueue` runtime state
- create a new poll thread
- overwrite `Context.async_upload_thread`

So the current design assumes one async-upload startup per mounted runtime.

## What `UploadQueue.start_async_upload_thread` Does Not Do

`UploadQueue.start_async_upload_thread` does not:

- enqueue upload work
- inspect the queue for pending entries
- dispatch workers itself
- request shutdown
- join the poll thread

It only installs runtime state and starts the poll thread.

## Related Docs

- `docs/agent-docs/drive-init-filesystem.md`
- `docs/agent-docs/thread-pool-create.md`
- `docs/agent-docs/upload-queue-poll-upload-queue.md`
- `docs/agent-docs/upload-queue-stop-async-upload-thread.md`
- `docs/agent-docs/upload-queue-upload-resource.md`
- `docs/agent-docs/drive-upload-resource-by-id.md`

## Source Pointers

- `src/uploadQueue.ml`: `start_async_upload_thread`
- `src/uploadQueue.ml`: `poll_upload_queue`
- `src/driveRuntimeServices.ml`: startup branch and callback wiring
- `src/drive.ml`: `init_filesystem`
- `src/gdfuseFlow.ml`: shutdown path that later joins the thread
