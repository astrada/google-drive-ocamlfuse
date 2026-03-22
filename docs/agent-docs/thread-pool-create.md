# `ThreadPool.create`

## Purpose

`ThreadPool.create` builds the generic worker-pool record used by async upload.

It does not start any worker threads or install any global runtime state. It
only constructs the mutable record that later calls to `add_work`,
`pending_threads`, and `shutdown` operate on.

## Public Signature

`create` is part of `src/threadPool.mli`:

```ocaml
val create :
  ?max_threads:int -> ?mutex:Mutex.t -> ?condition:Condition.t -> unit -> t
```

The optional parameters are:

- `max_threads`: capacity limit later enforced by `add_work`
- `mutex`: lock guarding the pool state
- `condition`: condition variable used by waiting callers

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

1. choose `max_threads`, defaulting to `128`
2. reuse or allocate the pool mutex
3. reuse or allocate the pool condition variable
4. create an empty worker table
5. return the assembled record

So this helper is purely local runtime-state construction.

## Constructed State

The returned record contains:

```ocaml
{
  max_threads;
  mutex;
  condition;
  table;
}
```

Those fields become the whole shared state for the pool:

- `max_threads` drives future admission checks
- `mutex` protects the shared table and condition waits
- `condition` wakes blocked callers when a worker finishes
- `table` tracks currently registered worker threads

## Defaults And Injection

If the caller omits `~max_threads`, the helper uses:

```ocaml
128
```

If the caller omits `~mutex` or `~condition`, the helper allocates fresh
primitives with:

```ocaml
Mutex.create ()
Condition.create ()
```

If they are supplied, `create` reuses them directly.

## Empty Table At Startup

The pool begins with:

```ocaml
table = Hashtbl.create max_threads
```

So no workers exist yet when `create` returns.

That `Hashtbl.create` argument is only the initial-size hint for the hash
table. The actual worker-capacity limit is the separate `max_threads` field.

## No Worker Startup

`ThreadPool.create` does not call:

- `Thread.create`
- `Thread.join`
- `Condition.wait`
- `Condition.signal`

So this helper should not be read as "start a thread pool" in the executor
sense. It only prepares the state needed for later worker creation.

## Non-Positive `max_threads` Is Not Validated

The constructor does not reject `max_threads <= 0`.

That matters because `add_work` later waits while:

```ocaml
Hashtbl.length pool.table >= pool.max_threads
```

So a pool created with `0` or a negative limit is effectively unusable under
the current API shape.

## Primary Production Caller

The main production call site is:

```ocaml
ThreadPool.create ~max_threads:upload_threads ()
```

inside `UploadQueue.start_async_upload_thread`.

See `docs/agent-docs/upload-queue-start-async-upload-thread.md` for that
higher-level startup path.

## What `ThreadPool.create` Does Not Do

`ThreadPool.create` does not:

- start any worker threads
- submit any work
- count running workers
- join or cancel threads
- validate `max_threads`

It only constructs the pool record.

## Related Docs

- `docs/agent-docs/thread-pool-add-work.md`
- `docs/agent-docs/thread-pool-pending-threads.md`
- `docs/agent-docs/thread-pool-shutdown.md`
- `docs/agent-docs/upload-queue-start-async-upload-thread.md`

## Source Pointers

- `src/threadPool.ml`: `create`
- `src/threadPool.ml`: `add_work`
- `src/threadPool.ml`: `pending_threads`
- `src/threadPool.ml`: `shutdown`
- `src/uploadQueue.ml`: `start_async_upload_thread`
