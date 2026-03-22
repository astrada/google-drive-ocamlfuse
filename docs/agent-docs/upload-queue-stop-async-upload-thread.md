# `UploadQueue.stop_async_upload_thread`

## Purpose

`UploadQueue.stop_async_upload_thread` is the async-upload stop-request helper.

Its job is deliberately small:

- set the queue subsystem's stop flag to `true`

That is all.

It does not join the poll thread, drain the queue by itself, or shut down the
thread pool directly. Instead, it asks the already-running poll loop to enter
its shutdown-drain path on the next check.

So this helper is the request side of async-upload shutdown, not the whole
shutdown sequence.

## Public Signature

`stop_async_upload_thread` is part of `src/uploadQueue.mli`:

```ocaml
val stop_async_upload_thread : unit -> unit
```

It takes no arguments and returns `unit`.

That shape is important because the helper does not receive:

- the poll thread handle
- the cache handle
- the thread pool

It relies entirely on the previously installed `ConcurrentUploadQueue` runtime
state.

## Entire Implementation

The implementation is:

```ocaml
let stop_async_upload_thread () =
  ConcurrentUploadQueue.update (fun q -> q |> stop_async_upload ^= true)
```

That is the whole control flow.

## High-Level Flow

At a high level, the helper does this:

1. load the current `UploadQueue.t` runtime record from `ConcurrentUploadQueue`
2. set `stop_async_upload = true`
3. write the updated record back
4. return immediately

So this helper is synchronous as a state update, but asynchronous as a shutdown
effect.

## The Only Side Effect: Flip The Stop Flag

The helper does not manipulate several pieces of state. It changes exactly one
field:

```ocaml
stop_async_upload
```

The runtime record was created earlier by:

```ocaml
start_async_upload_thread cache upload_threads upload_resource
```

So the stop request is really:

- mutate the existing queue runtime record so the poll loop sees
  `stop_async_upload = true`

Everything else follows later from the poll loop observing that flag.

## Precondition: Startup Already Happened

Because the helper uses:

```ocaml
ConcurrentUploadQueue.update ...
```

it assumes the queue runtime state already exists.

That means `start_async_upload_thread` must have run earlier in the same
process.

If the concurrent global had never been initialized, `ConcurrentUploadQueue`
would have no current value to update.

So this helper is not a safe "maybe-stop-if-running" probe. It is part of the
normal paired startup/shutdown lifecycle.

## Relationship To `poll_upload_queue`

The flag flipped here is consumed by the poll loop:

```ocaml
if d.stop_async_upload then ...
```

That loop then:

1. counts pending queue entries
2. keeps polling until the queue table becomes empty
3. exits its loop with `Exit`
4. drains remaining worker threads through `ThreadPool.shutdown`

So the real shutdown behavior lives in `poll_upload_queue`, not here.

See `docs/agent-docs/upload-queue-poll-upload-queue.md` for that drain
contract.

## Relationship To `GdfuseFlow.shutdown`

The production shutdown caller is:

```ocaml
UploadQueue.stop_async_upload_thread ();
Thread.join async_upload_thread
```

That call site adds the parts this helper does not provide:

- logging the thread id
- waiting for the poll thread to finish

So the shutdown sequence is intentionally split:

1. `UploadQueue.stop_async_upload_thread ()` requests stop
2. the poll loop drains queued work and exits
3. `Thread.join async_upload_thread` waits for that exit

This is why the helper must stay small. The wait belongs to the owner of the
thread handle, which is the higher-level shutdown path.

## Not An Immediate Stop

This helper does not interrupt the poll thread in the middle of:

- `Thread.delay 1.0`
- `upload_resource cache`
- worker-thread execution

It only changes the flag the poll loop checks between iterations.

So after this function returns:

- the poll thread may still be sleeping
- queue entries may still exist
- worker threads may still be running

That is expected. Immediate termination is not the contract.

## Not A Queue Freeze

Setting `stop_async_upload = true` does not itself block code from inserting new
queue entries.

So the shutdown path still relies on a broader runtime assumption:

- new queued work eventually stops appearing

If new entries keep being created forever, the poll loop's drain condition
(`count_entries = 0`) will never become true.

That is another reason this helper should be understood as "request drain" and
not "force stop now".

## Idempotency Characteristics

After startup, repeated calls are effectively harmless at this layer because the
helper just writes:

- `stop_async_upload = true`

again.

But it is only weakly idempotent:

- it does not verify whether shutdown is already in progress
- it does not verify whether the poll thread has already exited
- it still depends on the concurrent global being initialized first

So "safe to repeat after startup" is not the same as "safe in every state".

## What It Does Not Do To `Context`

The helper does not modify:

- `Context.async_upload_thread`

That thread handle remains stored in `Context` until higher-level shutdown code
joins the thread and later clears the overall context.

So this helper should not be read as "tear down async upload state everywhere".
It only updates the queue-local concurrent-global flag.

## Relationship To `start_async_upload_thread`

The startup helper creates the runtime record with:

- `stop_async_upload = false`

This stop helper later flips the same field to `true`.

So together they form the smallest possible lifecycle pair:

- `start_async_upload_thread`: install runtime state and start the poll thread
- `stop_async_upload_thread`: request that the running poll thread drain and
  exit

See `docs/agent-docs/upload-queue-start-async-upload-thread.md` for the startup
half.

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
- `src/uploadQueue.ml`: `start_async_upload_thread`
- `src/gdfuseFlow.ml`: `stop_async_upload_thread`
