# `ThreadPool.add_work`

## Purpose

`ThreadPool.add_work` is the generic worker-execution boundary used by the
async upload subsystem.

It does not enqueue work into a separate internal backlog. Instead, it:

- blocks the caller while the pool is already at `max_threads`
- starts a new OCaml thread as soon as a slot is available
- tracks that worker in the pool table until the worker finishes
- releases the slot and wakes one waiter in an exception-safe cleanup path

So this helper is best read as "wait for capacity, then spawn one worker now".

## Public Signature

`add_work` is part of `src/threadPool.mli`:

```ocaml
val add_work : ('a -> 'b) -> 'a -> t -> unit
```

The parameters are:

- `f`: worker function
- `x`: single argument passed to that worker
- `pool`: the `ThreadPool.t` runtime record

The return type is `unit`. The helper does not return a job handle, result
future, or explicit thread id to the caller.

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
2. wait while `pending_threads >= max_threads`
3. build a wrapped worker closure
4. create a new thread for that worker
5. record the new thread in `pool.table`
6. unlock the mutex and return immediately
7. later, when the worker finishes or raises, remove it from the table and
   signal the condition variable

So caller-side backpressure and worker-side cleanup are both part of the
contract.

## Capacity Gate Is Caller-Blocking

The admission check is:

```ocaml
while Hashtbl.length pool.table >= pool.max_threads do
  Condition.wait pool.condition pool.mutex
done
```

This means `add_work` is not a non-blocking queue insert.

If the pool is full, the caller waits until some earlier worker completes and
signals the condition variable.

That detail matters in the upload path because `UploadQueue.upload_resource`
calls `ThreadPool.add_work` from the poll thread itself. A saturated pool can
therefore delay further poll-loop progress.

## No Separate Backlog Queue

There is no internal "pending jobs" queue in `ThreadPool.t`.

The pool record only stores:

- `max_threads`
- one mutex and one condition variable
- a hash table of currently running `Thread.t` values

So `add_work` should be read as:

- "start work when a slot exists"

not as:

- "append work to a queue that will start later"

## Worker Wrapper

The thread does not run `f` directly. It runs the local wrapper:

```ocaml
let f' x = ...
```

That wrapper does three things:

1. derive the current OCaml thread id
2. log worker start
3. run `f x` under `Utils.try_finally` so cleanup always happens

The cleanup path is:

```ocaml
signal_work_done thread_id pool
```

That helper removes the thread id from `pool.table` and signals one waiter on
`pool.condition`.

## Why Cleanup Is Exception-Safe

The worker body is wrapped as:

```ocaml
Utils.try_finally
  (fun () -> ignore (f x))
  (fun () -> signal_work_done thread_id pool)
```

So a worker that raises still releases its slot before the exception escapes
the thread.

This matters directly to async uploads: failed upload workers can requeue their
upload entry without wedging thread-pool slot accounting.

## Exceptions Are Not Swallowed

`add_work` ensures cleanup runs, but it does not swallow worker exceptions.

If `f x` raises:

- `signal_work_done` still runs
- the worker is removed from the pool table
- the exception continues as an uncaught thread exception

So the thread runtime may still log the worker failure even though pool
bookkeeping remains correct.

## Registration Ordering

`add_work` creates the thread and records it in `pool.table` while still
holding `pool.mutex`:

```ocaml
let thread = Thread.create f' x in
let thread_id = Thread.id thread in
Hashtbl.add pool.table thread_id thread
```

That ordering is important.

The new worker may run immediately, but its cleanup path also needs
`pool.mutex`. Because the caller still holds that lock until after the table
entry is added, the worker cannot remove itself from the table before the table
entry exists.

So the current implementation avoids a "remove before add" race on the worker
table.

## Relationship To `pending_threads`

The effective "occupancy" of the pool is the size of:

```ocaml
pool.table
```

That same table drives both:

- the admission check in `add_work`
- `ThreadPool.pending_threads`

So "pending threads" in this module really means:

- currently running or not-yet-cleaned-up worker threads

not:

- queued-but-not-started jobs

## Relationship To `shutdown`

`ThreadPool.shutdown` later does:

```ocaml
Hashtbl.iter (fun _ thread -> Thread.join thread) pool.table
```

So `add_work` contributes the threads that shutdown may later join by storing
them in `pool.table` as soon as they are created.

The broader lifecycle is:

- `add_work`: start one worker and register it
- worker cleanup: remove the worker from the table when done
- `shutdown`: join any workers still present in the table

See `docs/agent-docs/thread-pool-shutdown.md` for the join-side half of that
lifecycle.

## Relationship To `UploadQueue.upload_resource`

The main production caller is:

```ocaml
ThreadPool.add_work do_work e d.thread_pool
```

inside `UploadQueue.upload_resource`.

That means the boundary between queue-specific behavior and generic worker-pool
behavior is:

- `UploadQueue.upload_resource`: choose one queue entry and define queue-entry
  transitions
- `ThreadPool.add_work`: wait for capacity, spawn one worker, and keep slot
  accounting correct until the worker exits

## Tests

The current thread-pool tests cover two important behaviors:

- sequential execution when `max_threads = 1`
- slot release after a worker failure

See `test/testThreadPool.ml`.

## What `ThreadPool.add_work` Does Not Do

`ThreadPool.add_work` does not:

- keep a backlog queue of not-yet-started jobs
- return a worker result to the caller
- retry failed work
- cancel running work
- interpret upload-queue state or Drive-specific semantics

It only handles capacity gating, worker creation, and worker-slot cleanup.

## Related Docs

- `docs/agent-docs/thread-pool-shutdown.md`
- `docs/agent-docs/upload-queue-upload-resource.md`
- `docs/agent-docs/upload-queue-poll-upload-queue.md`
- `docs/agent-docs/upload-queue-start-async-upload-thread.md`
- `docs/agent-docs/drive-upload-resource-by-id.md`
- `docs/agent-docs/drive-upload-path.md`

## Source Pointers

- `src/threadPool.ml`: `add_work`
- `src/threadPool.ml`: `signal_work_done`
- `src/threadPool.ml`: `pending_threads`
- `src/threadPool.ml`: `shutdown`
- `src/uploadQueue.ml`: `upload_resource`
- `test/testThreadPool.ml`
