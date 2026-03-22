# `UploadQueue.poll_upload_queue`

## Purpose

`UploadQueue.poll_upload_queue` is the long-lived async-upload poll loop.

It is the thread entrypoint started by `UploadQueue.start_async_upload_thread`,
and it owns two things:

- normal runtime pacing for async upload dispatch
- the shutdown-drain contract for the async upload subsystem

It does not upload a resource itself. Instead, it repeatedly calls the narrower
helper:

```ocaml
upload_resource cache
```

and decides when the poll thread should keep running, start draining, or exit.

See `docs/agent-docs/upload-queue-start-async-upload-thread.md` for the startup
path that installs the runtime state this loop reads.

## Internal Status And Effective Signature

`poll_upload_queue` is internal to `src/uploadQueue.ml`. It is not exposed in
`src/uploadQueue.mli`.

Its effective shape is:

```ocaml
val poll_upload_queue : CacheData.t -> unit
```

The input is the cache handle. Other runtime state, including the stop flag and
thread pool, is read through `ConcurrentUploadQueue`.

## Entire Implementation

The implementation is:

```ocaml
let poll_upload_queue cache =
  let check () =
    let d = ConcurrentUploadQueue.get () in
    if d.stop_async_upload then (
      let entries = Cache.UploadQueue.count_entries cache in
      Utils.log_with_header "Waiting for pending uploads (%d)\n%!" entries;
      if entries = 0 then raise Exit)
  in
  try
    while true do
      check ();
      Thread.delay 1.0;
      upload_resource cache
    done
  with Exit ->
    let d = ConcurrentUploadQueue.get () in
    Utils.log_with_header "Waiting for pending upload threads (%d)...%!"
      (ThreadPool.pending_threads d.thread_pool);
    ThreadPool.shutdown d.thread_pool;
    Utils.log_message "done\n%!"
```

That is the whole control flow.

## High-Level Flow

At a high level, the poll thread does this:

1. check whether async-upload shutdown has been requested
2. if shutdown was requested, inspect how many queue entries remain
3. if shutdown was requested and the queue is empty, exit the loop
4. otherwise sleep for one second
5. call `upload_resource cache` once
6. on loop exit, wait for any still-running worker threads to finish

So this helper is the queue subsystem's pacing and shutdown loop, not its
per-entry worker logic.

## Normal Runtime Behavior

In the ordinary case, `stop_async_upload = false`, so `check ()` does nothing.

The loop then does exactly this each iteration:

1. sleep for one second
2. try to dispatch one queued upload entry through `upload_resource cache`

This has two practical consequences.

First, the poll thread is intentionally periodic rather than event-driven. It
only notices new work on the next wake-up.

Second, one call to `poll_upload_queue` does not try to empty the whole queue at
once. It delegates at most one dispatch attempt per loop iteration.

## One-Second Pacing

The pacing line is literal:

```ocaml
Thread.delay 1.0
```

So the dispatch cadence is hard-coded to one wake-up per second.

That matters for throughput and shutdown behavior:

- newly queued work may wait up to roughly one second before the poll thread
  notices it
- a stop request is also observed at the next loop check, not immediately in the
  middle of sleep

There is no condition variable or wake-up signal for new queue entries here.

## Relationship To `upload_resource`

After each sleep, the loop calls:

```ocaml
upload_resource cache
```

That helper owns the next narrower layer:

- select one `ToUpload` queue entry
- hand it to the thread pool
- mark the entry `Uploading`
- delete or requeue it around the upload callback

So the split is:

- `poll_upload_queue`: "when should the queue try another dispatch?"
- `upload_resource`: "what should happen to one selected queue entry?"

See `docs/agent-docs/upload-queue-upload-resource.md` for that per-entry
dispatch helper.

## What "Pending Uploads" Means Here

During shutdown, the loop uses:

```ocaml
let entries = Cache.UploadQueue.count_entries cache
```

This counts all queue rows, not only rows still in `ToUpload`.

So the stop condition is:

- the queue table is empty

not:

- there are no more `ToUpload` rows

That distinction matters because entries already marked `Uploading` still count
as pending queue work until the worker callback deletes them.

## Shutdown Check And Drain Contract

The stop path begins when `stop_async_upload_thread ()` sets:

```ocaml
stop_async_upload = true
```

After that, each loop iteration runs `check ()`, which:

1. reads the stop flag from `ConcurrentUploadQueue`
2. counts current queue entries
3. logs `"Waiting for pending uploads (%d)"`
4. raises `Exit` only when the queue count reaches zero

So the poll thread does not stop immediately when asked.

Instead it enters drain mode:

- keep looping while queued work still exists
- exit only after the queue table becomes empty

This is the key shutdown contract for the async upload subsystem.

## Shutdown Depends On No New Entries Arriving

Because the exit condition is `entries = 0`, termination depends on the queue
eventually draining fully.

So after shutdown has been requested, the design implicitly assumes:

- no code is still enqueueing new upload entries forever

If new entries keep appearing after the stop flag is set, the poll thread will
keep observing a non-empty queue and will not exit.

In normal shutdown, that assumption is reasonable because the FUSE filesystem is
already winding down.

## Exit Path Uses `Exit` As A Local Control Signal

The loop is wrapped in:

```ocaml
try ... with Exit -> ...
```

So `Exit` is being used here as the internal control-flow signal for
"queue-drain complete; now finish shutdown".

That means this helper only treats `Exit` as the planned shutdown path. It does
not add a broad catch-all handler for arbitrary runtime exceptions.

## Final Thread-Pool Drain

After `Exit`, the helper does:

```ocaml
let d = ConcurrentUploadQueue.get () in
Utils.log_with_header "Waiting for pending upload threads (%d)...%!"
  (ThreadPool.pending_threads d.thread_pool);
ThreadPool.shutdown d.thread_pool;
Utils.log_message "done\n%!"
```

This is a second drain step, separate from queue-table draining.

The distinction is:

- queue drain: wait until there are no more queue rows
- thread-pool drain: join any worker threads still recorded as running

That separation matters because a worker can remove its queue entry before the
worker thread itself has fully completed and been joined.

## Relationship To `GdfuseFlow.shutdown`

The production shutdown path does:

```ocaml
UploadQueue.stop_async_upload_thread ();
Thread.join async_upload_thread
```

So `GdfuseFlow.shutdown` relies on `poll_upload_queue` to provide a clean join
boundary:

- stop flag requested first
- queue drained by the poll loop
- worker threads drained by the poll loop
- poll thread finally exits
- only then does `Thread.join async_upload_thread` return

See `docs/agent-docs/drive-init-filesystem.md` for where this poll thread is
started and how shutdown coordinates with it.

## Interaction With Worker-Pool Saturation

`poll_upload_queue` itself calls `upload_resource cache`, and that helper may
block inside `ThreadPool.add_work` while the pool is full.

So the poll loop's pacing is shaped by both:

- its own one-second sleep
- worker-slot availability in the thread pool

In other words, "poll every second" is an upper bound on dispatch attempts, not
a guarantee that one new worker starts every second.

## Idle Behavior

If the stop flag is not set and the queue is empty, the helper does not log or
raise. It simply:

1. sleeps
2. calls `upload_resource cache`
3. lets that helper return `()`
4. repeats

So empty-queue idling is intentionally quiet at the poll-loop layer.

## What `UploadQueue.poll_upload_queue` Does Not Do

`UploadQueue.poll_upload_queue` does not:

- insert queue entries
- decide whether async upload is enabled
- upload a resource directly
- reload resources by cache id
- own the callback logic for delete-vs-requeue of one queue entry

It only paces and terminates the async upload poll thread.

## Related Docs

- `docs/agent-docs/drive-init-filesystem.md`
- `docs/agent-docs/upload-queue-start-async-upload-thread.md`
- `docs/agent-docs/upload-queue-upload-resource.md`
- `docs/agent-docs/drive-upload-resource-by-id.md`
- `docs/agent-docs/drive-upload-path.md`

## Source Pointers

- `src/uploadQueue.ml`: `poll_upload_queue`
- `src/uploadQueue.ml`: `upload_resource`
- `src/uploadQueue.ml`: `start_async_upload_thread`
- `src/uploadQueue.ml`: `stop_async_upload_thread`
- `src/gdfuseFlow.ml`: `stop_async_upload_thread`
- `src/threadPool.ml`: `shutdown`
