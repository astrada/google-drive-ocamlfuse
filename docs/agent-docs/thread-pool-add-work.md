# `ThreadPool.add_work`

## Purpose

`ThreadPool.add_work` is the generic worker-execution boundary used by the
async upload subsystem.

It does not append jobs to a separate backlog queue. Instead, it waits for pool
capacity, starts one worker thread, and ensures that worker later releases its
slot through the cleanup path.

## Public Signature

`add_work` is part of `src/threadPool.mli`:

```ocaml
val add_work : ('a -> 'b) -> 'a -> t -> unit
```

The parameters are:

- `f`: worker function
- `x`: single argument passed to that worker
- `pool`: the `ThreadPool.t` runtime record

The return type is `unit`. The helper does not return a job handle or result
future.

## Entire Implementation

```ocaml
let add_work f x pool =
  Utils.with_lock pool.mutex (fun () ->
      while Hashtbl.length pool.table >= pool.max_threads do
        Condition.wait pool.condition pool.mutex
      done;
      let f' x =
        let thread = Thread.self () in
        let thread_id = Thread.id thread in
        Utils.log_with_header "Spawning new thread id=%d from thread pool\n%!"
          thread_id;
        Utils.try_finally
          (fun () -> ignore (f x))
          (fun () -> signal_work_done thread_id pool)
      in
      let thread = Thread.create f' x in
      let thread_id = Thread.id thread in
      Hashtbl.add pool.table thread_id thread)
```

That is the whole helper.

## High-Level Flow

At a high level, `add_work` does this:

1. lock the pool mutex
2. wait while the pool is already full
3. build a wrapped worker closure
4. create one new thread for that worker
5. register that thread in `pool.table`
6. return once the worker has been launched
7. later, when the worker finishes or raises, remove it from the table and
   signal one waiting caller

So caller-side backpressure and worker-side cleanup are both part of the
contract.

## Capacity Gate Is Caller-Blocking

The admission check is:

```ocaml
while Hashtbl.length pool.table >= pool.max_threads do
  Condition.wait pool.condition pool.mutex
done
```

So `add_work` is not a non-blocking queue insert.

If the pool is full, the caller waits until some earlier worker completes and
signals the condition variable.

That detail matters in the upload path because `UploadQueue.upload_resource`
calls `ThreadPool.add_work` from the poll thread itself.

## No Separate Backlog Queue

The pool record contains only:

- the capacity limit
- the synchronization primitives
- the table of currently registered workers

So `add_work` should be read as:

- "start work when a slot exists"

not as:

- "append work to an internal queue that will start later"

## Worker Wrapper And Cleanup

The new thread does not run `f` directly. It runs a wrapper that:

1. derives the worker thread id
2. logs worker start
3. runs `f x` under `Utils.try_finally`
4. calls `signal_work_done thread_id pool` in the cleanup path

That cleanup removes the worker from `pool.table` and signals one waiter on the
pool condition variable.

## Exception-Safe Slot Release

Because the worker body is wrapped in `Utils.try_finally`, a worker that raises
still releases its slot before the exception escapes the thread.

`add_work` therefore preserves worker-slot accounting even on failure. It does
not, however, swallow the worker exception; uncaught thread exceptions still
surface through the OCaml thread runtime.

## Registration Ordering

`add_work` creates the thread and inserts it into `pool.table` while still
holding `pool.mutex`:

```ocaml
let thread = Thread.create f' x in
let thread_id = Thread.id thread in
Hashtbl.add pool.table thread_id thread
```

That ordering avoids a "remove before add" race against the cleanup path, which
also needs `pool.mutex` before it can remove the worker from the table.

## Shared Table With The Other Helpers

The same worker table created by `ThreadPool.create` is later:

- observed by `ThreadPool.pending_threads`
- joined by `ThreadPool.shutdown`

See `docs/agent-docs/thread-pool-create.md`,
`docs/agent-docs/thread-pool-pending-threads.md`, and
`docs/agent-docs/thread-pool-shutdown.md` for those neighboring layers.

## Primary Production Caller

The main production caller is:

```ocaml
ThreadPool.add_work do_work e d.thread_pool
```

inside `UploadQueue.upload_resource`.

That is the boundary where queue-specific work selection stops and generic
worker-pool execution begins.

## What `ThreadPool.add_work` Does Not Do

`ThreadPool.add_work` does not:

- keep a backlog queue of not-yet-started jobs
- return a worker result to the caller
- retry failed work
- cancel running work
- interpret upload-queue or Drive-specific semantics

It only handles capacity gating, worker creation, and worker-slot cleanup.

## Related Docs

- `docs/agent-docs/thread-pool-create.md`
- `docs/agent-docs/thread-pool-pending-threads.md`
- `docs/agent-docs/thread-pool-shutdown.md`
- `docs/agent-docs/upload-queue-upload-resource.md`
- `docs/agent-docs/upload-queue-poll-upload-queue.md`

## Source Pointers

- `src/threadPool.ml`: `add_work`
- `src/threadPool.ml`: `signal_work_done`
- `src/threadPool.ml`: `pending_threads`
- `src/threadPool.ml`: `shutdown`
- `src/uploadQueue.ml`: `upload_resource`
