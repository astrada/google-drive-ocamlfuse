# `ThreadPool.create`

## Purpose

`ThreadPool.create` constructs the generic worker-pool runtime record used by
the async upload subsystem.

It does not start any worker threads or register any global runtime state. It
only builds the mutable data structure that later calls to `add_work` and
`shutdown` operate on.

## Public Signature

`create` is part of `src/threadPool.mli`:

```ocaml
val create :
  ?max_threads:int -> ?mutex:Mutex.t -> ?condition:Condition.t -> unit -> t
```

The optional parameters are:

- `max_threads`: concurrency limit for later `add_work` admission checks
- `mutex`: synchronization primitive guarding the pool state
- `condition`: condition variable used to wake callers waiting for capacity

The result is a fresh `ThreadPool.t`.

## Entire Implementation

```ocaml
let create ?(max_threads = 128) ?mutex ?condition () =
  {
    max_threads;
    mutex = (match mutex with None -> Mutex.create () | Some m -> m);
    condition =
      (match condition with None -> Condition.create () | Some c -> c);
    table = Hashtbl.create max_threads;
  }
```

That is the whole helper.

## High-Level Flow

At a high level, `create` does this:

1. choose the configured `max_threads`, defaulting to `128`
2. reuse the supplied `Mutex.t` or create a fresh one
3. reuse the supplied `Condition.t` or create a fresh one
4. create an empty worker table sized with `Hashtbl.create max_threads`
5. return the assembled `ThreadPool.t`

So this helper is purely local runtime-state construction.

## Constructed Runtime Record

The returned record contains:

```ocaml
{
  max_threads;
  mutex;
  condition;
  table;
}
```

Those fields become the whole shared state for the thread pool:

- `max_threads` controls future capacity checks in `add_work`
- `mutex` protects the pool table and condition waits
- `condition` wakes blocked callers when a worker finishes
- `table` tracks currently running worker threads

## Default `max_threads`

If the caller does not supply `~max_threads`, the helper uses:

```ocaml
128
```

In practice, the async upload subsystem usually overrides that default with:

```ocaml
ThreadPool.create ~max_threads:upload_threads ()
```

from `UploadQueue.start_async_upload_thread`.

So the default is part of the generic thread-pool API, but production upload
behavior is usually shaped by config instead.

## Optional Mutex And Condition Injection

The helper accepts optional `~mutex` and `~condition` parameters.

If either one is omitted, `create` allocates a fresh synchronization primitive:

```ocaml
Mutex.create ()
Condition.create ()
```

If they are supplied, `create` reuses the caller-provided instances directly.

That makes the constructor more flexible for tests or embedding, while the
normal production path still uses fresh synchronization primitives.

## Empty Table At Startup

The pool begins with:

```ocaml
table = Hashtbl.create max_threads
```

So no workers exist yet when `create` returns.

That `Hashtbl.create` argument is only the hash-table initial-size hint. It is
not the worker-capacity limit by itself. The actual concurrency limit is the
separate `max_threads` field checked later by `add_work`.

## No Worker Startup

`ThreadPool.create` does not call:

- `Thread.create`
- `Thread.join`
- `Condition.wait`
- `Condition.signal`

So this helper should not be read as "start a thread pool" in the executor
sense. It only prepares the state needed for later worker creation.

## Relationship To `add_work`

`ThreadPool.create` and `ThreadPool.add_work` own adjacent stages of the worker
pool lifecycle.

The split is:

- `create`: build the pool record and its empty synchronization state
- `add_work`: use that record to wait for capacity and start one worker

See `docs/agent-docs/thread-pool-add-work.md` for the execution-side half.

## Relationship To `shutdown`

`shutdown` later joins the worker threads still present in the table created
here.

So the lifecycle is:

- `create`: initialize the record
- `add_work`: add running worker threads to the table
- `shutdown`: join workers still recorded there

See `docs/agent-docs/thread-pool-shutdown.md` for the join-side half.

## Relationship To `UploadQueue.start_async_upload_thread`

The main production call site is:

```ocaml
ThreadPool.create ~max_threads:upload_threads ()
```

inside `UploadQueue.start_async_upload_thread`.

That means the higher-level split is:

- `UploadQueue.start_async_upload_thread`: decide when the async upload runtime
  should exist and where to store it
- `ThreadPool.create`: build the generic worker-pool record used inside that
  runtime

## Non-Positive `max_threads` Is Not Validated

The constructor does not reject `max_threads <= 0`.

That matters because `add_work` later waits while:

```ocaml
Hashtbl.length pool.table >= pool.max_threads
```

So a pool created with `0` or a negative limit is effectively unusable: the
admission check can never become false under normal operation.

This is a current API assumption rather than an enforced validation rule.

## Tests

The current thread-pool tests exercise `create` indirectly through pools built
with:

```ocaml
ThreadPool.create ~max_threads:1 ()
```

See `test/testThreadPool.ml`.

## What `ThreadPool.create` Does Not Do

`ThreadPool.create` does not:

- start any worker threads
- install global runtime state
- validate `max_threads`
- submit any work
- join or cancel threads

It only constructs the pool record.

## Related Docs

- `docs/agent-docs/thread-pool-add-work.md`
- `docs/agent-docs/thread-pool-shutdown.md`
- `docs/agent-docs/upload-queue-start-async-upload-thread.md`
- `docs/agent-docs/upload-queue-poll-upload-queue.md`
- `docs/agent-docs/drive-upload-path.md`

## Source Pointers

- `src/threadPool.ml`: `create`
- `src/threadPool.ml`: `add_work`
- `src/threadPool.ml`: `shutdown`
- `src/uploadQueue.ml`: `start_async_upload_thread`
- `test/testThreadPool.ml`
