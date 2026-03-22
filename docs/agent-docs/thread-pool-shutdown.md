# `ThreadPool.shutdown`

## Purpose

`ThreadPool.shutdown` is the generic join step for the repository's worker
pool.

It does not stop work submission, freeze the pool, or own any broader shutdown
policy. It only:

- iterate over the worker threads currently recorded in the pool table
- `Thread.join` each of them
- return after those joins complete

So this helper is the lifecycle endpoint after higher-level code has already
decided that no more useful work should be started.

## Public Signature

`shutdown` is part of `src/threadPool.mli`:

```ocaml
val shutdown : t -> unit
```

It takes the `ThreadPool.t` runtime record and returns `unit`.

The helper does not return a result summary, failure list, or count of joined
workers.

## Entire Implementation

```ocaml
let shutdown pool = Hashtbl.iter (fun _ thread -> Thread.join thread) pool.table
```

That is the whole helper.

## High-Level Flow

At a high level, `shutdown` does this:

1. iterate over `pool.table`
2. call `Thread.join` on each stored `Thread.t`
3. return after those joins finish

So the helper is purely a blocking join pass over the threads that are still
registered in the pool.

## What Gets Joined

The joined threads are exactly the threads still present in:

```ocaml
pool.table
```

That table is populated by `ThreadPool.add_work` when a worker starts and is
normally cleaned up by `signal_work_done` when a worker finishes.

So `shutdown` should be read as:

- "join workers still registered right now"

not as:

- "discover every worker ever started by this pool"

## Blocking Behavior

`Thread.join` blocks until the target thread exits.

So `shutdown` blocks until every iterated worker has completed.

If the table is already empty, the helper returns immediately.

## Relationship To `add_work`

`ThreadPool.add_work` and `ThreadPool.shutdown` own opposite ends of the worker
lifecycle.

The split is:

- `add_work`: wait for capacity, create one thread, and register it in the
  table
- `shutdown`: join the threads still present in that table

See `docs/agent-docs/thread-pool-add-work.md` for the start-side half of that
lifecycle.

## Relationship To `pending_threads`

`ThreadPool.pending_threads` is:

```ocaml
Hashtbl.length pool.table
```

So the same table drives both:

- the count logged before shutdown
- the set of threads that `shutdown` later joins

This is why `UploadQueue.poll_upload_queue` logs:

```ocaml
ThreadPool.pending_threads d.thread_pool
```

immediately before calling:

```ocaml
ThreadPool.shutdown d.thread_pool
```

## Relationship To `UploadQueue.poll_upload_queue`

The main production caller is the shutdown tail of:

```ocaml
UploadQueue.poll_upload_queue
```

That outer helper first waits for the upload queue table to drain, then does a
second drain step:

- queue drain: no more upload-queue rows remain
- thread-pool drain: join any worker threads still recorded as running

So `ThreadPool.shutdown` is the generic worker-pool part of async-upload
shutdown, not the owner of the queue-drain policy itself.

See `docs/agent-docs/upload-queue-poll-upload-queue.md` for that higher-level
shutdown contract.

## Not A Stop Request

`shutdown` does not:

- set a stop flag
- block future `add_work` calls from other code
- clear the pool table proactively
- cancel running threads

It assumes higher-level code has already decided when work submission should
stop.

That assumption is satisfied in the normal async-upload path because
`UploadQueue.poll_upload_queue` only calls `ThreadPool.shutdown` after it has
already entered drain mode and stopped dispatching new queue entries.

## Current Implementation Shape

One implementation detail is worth keeping explicit.

`shutdown` does not:

- lock `pool.mutex`
- take a snapshot copy of `pool.table`

It simply iterates the live hash table and joins the stored threads.

That makes this helper intentionally minimal, but it also means changes in this
area should be reasoned about carefully alongside:

- `signal_work_done`, which removes finished workers from the table
- `add_work`, which inserts new workers into the table

## What `ThreadPool.shutdown` Does Not Do

`ThreadPool.shutdown` does not:

- own worker-slot accounting
- wake blocked callers waiting in `add_work`
- report which join took longest
- retry or suppress worker failures
- interpret upload-queue or Drive-specific state

It only joins the threads currently registered in the pool.

## Related Docs

- `docs/agent-docs/thread-pool-add-work.md`
- `docs/agent-docs/upload-queue-poll-upload-queue.md`
- `docs/agent-docs/upload-queue-start-async-upload-thread.md`
- `docs/agent-docs/upload-queue-upload-resource.md`
- `docs/agent-docs/drive-upload-path.md`

## Source Pointers

- `src/threadPool.ml`: `shutdown`
- `src/threadPool.ml`: `add_work`
- `src/threadPool.ml`: `signal_work_done`
- `src/threadPool.ml`: `pending_threads`
- `src/uploadQueue.ml`: `poll_upload_queue`
