# `ThreadPool.pending_threads`

## Purpose

`ThreadPool.pending_threads` exposes the current occupancy of the generic
worker pool.

This pool has no separate backlog queue, so the helper reports only how many
worker threads are still registered in the live pool table.

## Public Signature

`pending_threads` is part of `src/threadPool.mli`:

```ocaml
val pending_threads : t -> int
```

It takes a `ThreadPool.t` and returns an `int`.

## Entire Implementation

```ocaml
let pending_threads pool = Hashtbl.length pool.table
```

That is the whole helper.

## What The Count Means

At a high level, `pending_threads` answers:

- how many worker threads are still registered in `pool.table` right now

So the count includes worker threads that are still present in the shared pool
table and excludes workers that already completed cleanup through
`signal_work_done`.

It does not count queued-but-not-started work because no such queue exists in
this module.

## Shared Table With The Other Helpers

The same table length is used by:

- `add_work` for capacity gating
- `shutdown` to decide which worker threads to join

So `pending_threads` is the observable view of the same worker-table state that
the other thread-pool helpers mutate or consume.

See `docs/agent-docs/thread-pool-add-work.md` and
`docs/agent-docs/thread-pool-shutdown.md` for those neighboring layers.

## No Additional Synchronization

`pending_threads` does not lock `pool.mutex`.

It simply evaluates:

```ocaml
Hashtbl.length pool.table
```

So the result should be read as a lightweight instantaneous count, not as a
stronger synchronized snapshot API.

## Zero Means No Registered Workers

If `pending_threads pool = 0`, then no workers are currently recorded in the
table.

At that instant:

- `add_work` would not block because of occupancy alone
- `shutdown` would have nothing to join

That does not say anything by itself about higher-level queue state outside the
generic thread pool.

## Primary Production Caller

The main production caller is the shutdown tail of
`UploadQueue.poll_upload_queue`, where the value is logged immediately before
the final `ThreadPool.shutdown`.

## What `ThreadPool.pending_threads` Does Not Do

`ThreadPool.pending_threads` does not:

- count queued-but-not-started jobs
- lock or snapshot the pool state
- wait for any worker to finish
- change worker-pool state
- interpret upload-queue or Drive-specific semantics

It only reports the current size of `pool.table`.

## Related Docs

- `docs/agent-docs/thread-pool-create.md`
- `docs/agent-docs/thread-pool-add-work.md`
- `docs/agent-docs/thread-pool-shutdown.md`
- `docs/agent-docs/upload-queue-poll-upload-queue.md`

## Source Pointers

- `src/threadPool.ml`: `pending_threads`
- `src/threadPool.ml`: `add_work`
- `src/threadPool.ml`: `signal_work_done`
- `src/threadPool.ml`: `shutdown`
- `src/uploadQueue.ml`: `poll_upload_queue`
