# `ThreadPool.pending_threads`

## Purpose

`ThreadPool.pending_threads` exposes the current occupancy of the generic
worker pool.

It does not count queued jobs because this thread pool has no separate backlog
queue. It only reports how many worker threads are still recorded in the live
pool table.

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

## High-Level Meaning

At a high level, `pending_threads` answers:

- how many worker threads are still registered in `pool.table` right now

That means the count includes:

- threads that are actively running
- threads that have finished their worker body but have not yet completed
  cleanup

It does not include:

- work that has not started yet, because no backlog queue exists
- threads that already removed themselves from the table through
  `signal_work_done`

## Relationship To `add_work`

`ThreadPool.add_work` uses the same table length in its admission check:

```ocaml
while Hashtbl.length pool.table >= pool.max_threads do
  Condition.wait pool.condition pool.mutex
done
```

So `pending_threads` is the observable form of the same occupancy value that
`add_work` uses internally for capacity gating.

See `docs/agent-docs/thread-pool-add-work.md` for that admission logic.

## Relationship To `signal_work_done`

The count falls when a worker runs:

```ocaml
Hashtbl.remove pool.table thread_id
```

inside `signal_work_done`.

So the accuracy of `pending_threads` depends directly on the cleanup path that
removes finished workers from the table.

That is why the exception-safe cleanup in `add_work` matters: failed workers
must still call `signal_work_done`, or `pending_threads` would stay inflated.

## Relationship To `shutdown`

`ThreadPool.shutdown` later joins the threads still present in the same table.

So the relationship is:

- `pending_threads`: count how many registered worker threads remain
- `shutdown`: join those registered worker threads

This is why `UploadQueue.poll_upload_queue` logs:

```ocaml
ThreadPool.pending_threads d.thread_pool
```

immediately before calling:

```ocaml
ThreadPool.shutdown d.thread_pool
```

See `docs/agent-docs/thread-pool-shutdown.md` for the join-side behavior.

## Relationship To `UploadQueue.poll_upload_queue`

The main production call site is the shutdown tail of:

```ocaml
UploadQueue.poll_upload_queue
```

At that point, the upload queue table has already drained. `pending_threads`
there is used only to log how many worker threads are still in the generic pool
before the final join step begins.

So this helper is an observation point, not part of the stop protocol itself.

## No Additional Synchronization

`pending_threads` does not lock `pool.mutex`.

It simply evaluates:

```ocaml
Hashtbl.length pool.table
```

That matches the current minimal style of the thread-pool module, but it also
means the result should be read as a lightweight instantaneous count rather than
as a stronger synchronized snapshot API.

## Zero Means No Registered Workers

If `pending_threads pool = 0`, then no workers are currently recorded in the
table.

In practice that means:

- `add_work` admission checks will not block because of occupancy alone
- `shutdown` would have nothing to join at that instant

It does not by itself say anything about higher-level queue state outside the
generic thread pool.

## Tests

The current thread-pool tests use `pending_threads` to verify that a failing
worker still releases its slot:

```ocaml
ThreadPool.pending_threads thread_pool = 0
```

See `test/testThreadPool.ml`.

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
- `docs/agent-docs/drive-upload-path.md`

## Source Pointers

- `src/threadPool.ml`: `pending_threads`
- `src/threadPool.ml`: `add_work`
- `src/threadPool.ml`: `signal_work_done`
- `src/threadPool.ml`: `shutdown`
- `src/uploadQueue.ml`: `poll_upload_queue`
- `test/testThreadPool.ml`
