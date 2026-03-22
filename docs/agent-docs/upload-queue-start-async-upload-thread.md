# `UploadQueue.start_async_upload_thread`

## Purpose

`UploadQueue.start_async_upload_thread` is the async-upload subsystem startup
helper.

It does three concrete things:

- build the runtime state used by the async upload queue
- install that state into `ConcurrentUploadQueue`
- create and register the long-lived poll thread

So this helper is the bring-up boundary between:

- `Drive.init_filesystem`, which decides whether async upload should exist at
  all
- the queue subsystem, which needs a callback, a thread pool, and a running poll
  thread before it can process entries

## Public Signature

`start_async_upload_thread` is part of `src/uploadQueue.mli`:

```ocaml
val start_async_upload_thread :
  CacheData.t -> int -> (int64 -> unit) -> unit
```

The parameters are:

- `cache`: the cache handle shared with the rest of the runtime
- `upload_threads`: maximum worker concurrency for the thread pool
- `upload_resource`: callback used later for one queued resource id

In production, `Drive.init_filesystem` passes `Drive.upload_resource_by_id` as
that callback.

## Entire Implementation

The implementation is:

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

That is the whole control flow.

## High-Level Flow

At a high level, the helper does this:

1. build an `UploadQueue.t` runtime record
2. set `stop_async_upload = false`
3. store the worker callback and a fresh `ThreadPool.t` in that record
4. publish the record through `ConcurrentUploadQueue.set`
5. spawn the poll thread with `Thread.create poll_upload_queue cache`
6. log the new thread id
7. write the poll thread handle back into `Context.async_upload_thread`

So this helper is best read as "install queue runtime state and start the poll
thread now".

## Runtime Record Initialization

The local record built here is:

```ocaml
{
  stop_async_upload = false;
  upload_resource_by_id = upload_resource;
  thread_pool = ThreadPool.create ~max_threads:upload_threads ();
}
```

That record is the entire runtime state for `ConcurrentUploadQueue`.

The important parts are:

- `stop_async_upload = false`:
  startup always clears the stop flag
- `upload_resource_by_id = upload_resource`:
  the queue subsystem stores a plain callback instead of depending directly on
  `Drive`
- `thread_pool = ThreadPool.create ~max_threads:upload_threads ()`:
  worker concurrency is fixed at startup from the configured thread count

## Why The Callback Is Injected

The helper does not hard-code `Drive.upload_resource_by_id`.

Instead it accepts:

```ocaml
(int64 -> unit)
```

and stores that function in `ConcurrentUploadQueue`.

This keeps the ownership boundary clean:

- `UploadQueue` owns queue scheduling and worker lifecycle
- `Drive` owns what it means to upload one queued resource id

In the production filesystem startup path, `Drive.init_filesystem` connects the
two by passing `upload_resource_by_id`.

## `ConcurrentUploadQueue.set` Happens Before Thread Creation

The order in the implementation matters:

```ocaml
ConcurrentUploadQueue.set data;
let thread = Thread.create poll_upload_queue cache in
```

That means the poll thread starts only after the shared runtime state is fully
installed.

This is important because `poll_upload_queue` and `upload_resource` immediately
read from `ConcurrentUploadQueue`. Starting the thread first would create a race
against uninitialized queue runtime state.

## Poll Thread Startup

The thread creation line is:

```ocaml
let thread = Thread.create poll_upload_queue cache
```

So the long-lived thread entrypoint is the queue poll loop itself, not a wrapper
closure.

The only explicit argument passed into that thread is `cache`.

Everything else the poll loop needs later comes from:

- `ConcurrentUploadQueue`
- the queue tables already stored in `cache`

See `docs/agent-docs/upload-queue-poll-upload-queue.md` for the loop that this
thread runs.

## Context Registration

After the thread is created, the helper does:

```ocaml
Context.update_ctx (Context.async_upload_thread ^= Some thread)
```

This is the shutdown-facing side effect of the helper.

It means the poll thread handle becomes part of the global runtime `Context.t`,
where later shutdown code can find it.

Without this step, `GdfuseFlow.shutdown` would have no handle to join.

## Relationship To `GdfuseFlow.shutdown`

The production shutdown path later does:

```ocaml
UploadQueue.stop_async_upload_thread ();
Thread.join async_upload_thread
```

That is only possible because `start_async_upload_thread` stored the spawned
thread in `Context.async_upload_thread`.

So the startup/shutdown split is:

- `start_async_upload_thread`: create the poll thread and publish its handle
- `stop_async_upload_thread` plus `Thread.join`: request drain and wait for exit

## No Idempotency Guard

Like `Drive.init_filesystem`, this helper has no explicit guard against being
called twice.

A second call would:

- overwrite the `ConcurrentUploadQueue` runtime state
- create a new poll thread
- overwrite `Context.async_upload_thread` with the new handle

That would leave the old poll thread outside the normal shutdown handle path.

So the current design assumes one startup call per mounted filesystem runtime.

## Relationship To `stop_async_upload_thread`

The sibling stop helper is:

```ocaml
let stop_async_upload_thread () =
  ConcurrentUploadQueue.update (fun q -> q |> stop_async_upload ^= true)
```

That stop path depends on the startup record created here.

In particular, it assumes:

- `ConcurrentUploadQueue` already contains a valid `UploadQueue.t`
- the poll thread reading that global state is already running

So `start_async_upload_thread` is the prerequisite for the entire stop/drain
contract.

## Relationship To `Drive.init_filesystem`

The production startup call site is:

```ocaml
UploadQueue.start_async_upload_thread cache
  config.Config.async_upload_threads upload_resource_by_id
```

So `Drive.init_filesystem` contributes:

- whether async upload is enabled
- the configured worker-pool width
- the callback back into `Drive`

and `UploadQueue.start_async_upload_thread` contributes:

- queue runtime-state installation
- poll-thread startup
- context registration of the thread handle

## What `UploadQueue.start_async_upload_thread` Does Not Do

`UploadQueue.start_async_upload_thread` does not:

- inspect the upload queue for work itself
- dispatch any queued entry itself
- create upload-queue rows
- join the poll thread
- set the stop flag during runtime

It only brings the async upload subsystem into existence.

## Related Docs

- `docs/agent-docs/drive-init-filesystem.md`
- `docs/agent-docs/upload-queue-poll-upload-queue.md`
- `docs/agent-docs/upload-queue-upload-resource.md`
- `docs/agent-docs/drive-upload-resource-by-id.md`

## Source Pointers

- `src/uploadQueue.ml`: `start_async_upload_thread`
- `src/uploadQueue.ml`: `stop_async_upload_thread`
- `src/uploadQueue.ml`: `poll_upload_queue`
- `src/context.ml`: `async_upload_thread`
- `src/gdfuseFlow.ml`: `stop_async_upload_thread`
