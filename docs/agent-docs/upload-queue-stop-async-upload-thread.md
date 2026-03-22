# `UploadQueue.stop_async_upload_thread`

## Purpose

`UploadQueue.stop_async_upload_thread` is the async-upload stop-request helper.

Its job is deliberately small:

- set the queue subsystem's stop flag to `true`

That is all.

It does not join the poll thread, drain the queue by itself, or shut down the
thread pool directly. It only asks the already-running poll loop to enter drain
mode on its next check.

## Public Signature

`stop_async_upload_thread` is part of `src/uploadQueue.mli`:

```ocaml
val stop_async_upload_thread : unit -> unit
```

It takes no arguments and returns `unit`. The helper relies entirely on the
previously installed `ConcurrentUploadQueue` runtime state.

## Entire Implementation

```ocaml
let stop_async_upload_thread () =
  ConcurrentUploadQueue.update (fun q -> q |> stop_async_upload ^= true)
```

That is the whole helper.

## High-Level Flow

At a high level, the helper does this:

1. load the current `UploadQueue.t` runtime record from `ConcurrentUploadQueue`
2. set `stop_async_upload = true`
3. write the updated record back
4. return immediately

So this helper is synchronous as a state update, but asynchronous as a shutdown
effect.

## The Only Side Effect: Flip The Stop Flag

The helper changes exactly one field:

```ocaml
stop_async_upload
```

The runtime record was created earlier by:

```ocaml
start_async_upload_thread cache upload_threads upload_resource
```

So the stop request is simply: mutate the existing queue runtime record so the
poll loop sees `stop_async_upload = true`.

## Precondition: Startup Already Happened

Because the helper uses:

```ocaml
ConcurrentUploadQueue.update ...
```

it assumes the queue runtime state already exists.

That means `start_async_upload_thread` must have run earlier in the same
process. This helper is not a safe "maybe-stop-if-running" probe.

## What Happens Next

The flag flipped here is consumed by the poll loop:

```ocaml
if d.stop_async_upload then ...
```

`poll_upload_queue` then owns the real drain behavior:

- keep looping while queue rows still exist
- exit only when the queue table becomes empty
- shut down the worker pool before the poll thread returns

See `docs/agent-docs/upload-queue-poll-upload-queue.md` for that contract.

## Not An Immediate Stop

This helper does not interrupt the poll thread in the middle of:

- `Thread.delay 1.0`
- `upload_resource cache`
- worker-thread execution

So after this function returns:

- the poll thread may still be sleeping
- queue entries may still exist
- worker threads may still be running

That is expected. Immediate termination is not the contract.

## Not A Queue Freeze

Setting `stop_async_upload = true` does not itself block code from inserting new
queue entries.

So shutdown still relies on a broader runtime assumption:

- new queued work eventually stops appearing

If new entries keep being created forever, the poll loop's drain condition
(`count_entries = 0`) will never become true.

That is why this helper should be understood as "request drain" and not "force
stop now".

## What It Does Not Do To `Context`

The helper does not modify:

- `Context.async_upload_thread`

That thread handle remains stored in `Context` until higher-level shutdown code
joins the thread.

## What `UploadQueue.stop_async_upload_thread` Does Not Do

`UploadQueue.stop_async_upload_thread` does not:

- join the poll thread
- drain the queue directly
- shut down the thread pool directly
- remove queue entries
- clear `ConcurrentUploadQueue`
- clear `Context.async_upload_thread`

It only sets the stop flag that the poll loop will observe later.

## Related Docs

- `docs/agent-docs/upload-queue-start-async-upload-thread.md`
- `docs/agent-docs/upload-queue-poll-upload-queue.md`
- `docs/agent-docs/drive-init-filesystem.md`

## Source Pointers

- `src/uploadQueue.ml`: `stop_async_upload_thread`
- `src/uploadQueue.ml`: `poll_upload_queue`
- `src/gdfuseFlow.ml`: shutdown path that later joins the poll thread
