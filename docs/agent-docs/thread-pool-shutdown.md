# `ThreadPool.shutdown`

## Purpose

`ThreadPool.shutdown` is the generic join step for the repository's worker
pool.

It does not stop work submission or own any broader shutdown policy. It only
joins the worker threads that are still registered in the pool table.

## Public Signature

`shutdown` is part of `src/threadPool.mli`:

```ocaml
val shutdown : t -> unit
```

It takes a `ThreadPool.t` and returns `unit`.

## Entire Implementation

```ocaml
let shutdown pool = Hashtbl.iter (fun _ thread -> Thread.join thread) pool.table
```

That is the whole helper.

## High-Level Flow

At a high level, `shutdown` does this:

1. iterate over `pool.table`
2. `Thread.join` each stored worker thread
3. return after those joins complete

So the helper is purely a blocking join pass over the workers that are still
registered in the pool.

## What Gets Joined

The joined threads are exactly the threads still present in:

```ocaml
pool.table
```

That table is populated by `add_work` and normally cleaned up by
`signal_work_done`.

So `shutdown` should be read as:

- "join workers still registered right now"

not as:

- "discover every worker ever started by this pool"

## Blocking Behavior

`Thread.join` blocks until the target thread exits.

So `shutdown` blocks until every iterated worker has completed.

If the table is already empty, the helper returns immediately.

## Not A Stop Request

`shutdown` does not:

- set a stop flag
- block future `add_work` calls from other code
- clear the pool table proactively
- cancel running threads

It assumes higher-level code has already decided when work submission should
stop.

## Minimal Implementation Shape

`shutdown` does not:

- lock `pool.mutex`
- take a snapshot copy of `pool.table`

It simply iterates the live worker table and joins the stored threads.

That makes the helper intentionally small, but it also means changes in this
area should be reasoned about carefully alongside the add/remove paths in the
same table.

## Primary Production Caller

The main production caller is the shutdown tail of `UploadQueue.poll_upload_queue`.

At that point, the upload queue table has already drained. `ThreadPool.shutdown`
is only the generic worker-join step that follows that higher-level queue drain.

See `docs/agent-docs/upload-queue-poll-upload-queue.md` for that outer shutdown
contract.

## Shared Table With The Other Helpers

The worker table joined here is:

- created by `ThreadPool.create`
- populated and cleaned up through `ThreadPool.add_work`
- observed through `ThreadPool.pending_threads`

See the dedicated notes for those neighboring layers:

- `docs/agent-docs/thread-pool-create.md`
- `docs/agent-docs/thread-pool-add-work.md`
- `docs/agent-docs/thread-pool-pending-threads.md`

## What `ThreadPool.shutdown` Does Not Do

`ThreadPool.shutdown` does not:

- own worker-slot accounting
- wake blocked callers waiting in `add_work`
- report which join took longest
- retry or suppress worker failures
- interpret upload-queue or Drive-specific state

It only joins the threads currently registered in the pool.

## Related Docs

- `docs/agent-docs/thread-pool-create.md`
- `docs/agent-docs/thread-pool-add-work.md`
- `docs/agent-docs/thread-pool-pending-threads.md`
- `docs/agent-docs/upload-queue-poll-upload-queue.md`

## Source Pointers

- `src/threadPool.ml`: `shutdown`
- `src/threadPool.ml`: `add_work`
- `src/threadPool.ml`: `signal_work_done`
- `src/threadPool.ml`: `pending_threads`
- `src/uploadQueue.ml`: `poll_upload_queue`
