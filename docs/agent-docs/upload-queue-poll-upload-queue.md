# `UploadQueue.poll_upload_queue`

## Purpose

`UploadQueue.poll_upload_queue` is the long-lived async-upload poll loop.

It is the thread entrypoint started by `UploadQueue.start_async_upload_thread`.
This note owns the two behaviors that matter most:

- normal runtime pacing for async upload dispatch
- the shutdown-drain contract for the async upload subsystem

It does not upload a resource directly. It repeatedly calls the narrower helper
`upload_resource cache` and decides when the poll thread should keep running,
start draining, or exit.

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

That is the whole loop.

## High-Level Flow

At a high level, the poll thread does this:

1. check whether shutdown has been requested
2. if so, count current queue rows
3. if shutdown was requested and the queue is empty, exit the loop
4. otherwise sleep for one second
5. call `upload_resource cache` once
6. after loop exit, shut down the worker pool

So this helper owns pacing and drain semantics, not per-entry worker logic.

## Normal Runtime Behavior

In the ordinary case, `stop_async_upload = false`, so `check ()` does nothing.

Each loop iteration then does exactly this:

1. sleep for one second
2. try one dispatch through `upload_resource cache`

Two consequences follow directly:

- the queue is polled rather than signaled, so new work is only seen on the
  next wake-up
- one loop iteration attempts at most one dispatch

## One-Second Pacing

The pacing line is literal:

```ocaml
Thread.delay 1.0
```

So the dispatch cadence is hard-coded to one wake-up per second.

That matters for both throughput and shutdown behavior:

- newly queued work may wait up to roughly one second before the poll thread
  notices it
- a stop request is also observed at the next loop check, not in the middle of
  a sleep

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

So the split is simple:

- `poll_upload_queue`: when to attempt dispatch
- `upload_resource`: what to do with one selected entry

See `docs/agent-docs/upload-queue-upload-resource.md` for that per-entry
dispatch helper.

## Drain Mode And Stop Flag

The stop path begins when `stop_async_upload_thread ()` flips:

```ocaml
stop_async_upload = true
```

After that, each loop iteration runs `check ()`, which:

1. reads the stop flag from `ConcurrentUploadQueue`
2. counts current queue entries
3. logs `"Waiting for pending uploads (%d)"`
4. raises `Exit` only when the queue count reaches zero

So the poll thread does not stop immediately. It enters drain mode:

- keep looping while queued work still exists
- exit only after the queue table becomes empty

This file owns that shutdown contract. The stop helper only flips the flag.

## What "Pending Uploads" Means Here

The drain check uses:

```ocaml
let entries = Cache.UploadQueue.count_entries cache
```

This counts all queue rows, not only `ToUpload`.

So an entry already marked `Uploading` still counts as pending until the worker
callback deletes it.

## Drain Depends On No New Entries Arriving

Because the exit condition is `entries = 0`, termination depends on the queue
eventually draining fully.

So after shutdown has been requested, the design implicitly assumes:

- no code is still enqueueing new upload entries forever

If new entries keep appearing after the stop flag is set, the poll thread will
keep observing a non-empty queue and will not exit.

## Exit Path Uses `Exit` As A Local Control Signal

The loop is wrapped in:

```ocaml
try ... with Exit -> ...
```

So `Exit` is being used here as the internal control-flow signal for
"queue-drain complete; now finish shutdown".

That means this helper only treats `Exit` as the planned shutdown path. It does
not add a broad catch-all handler for arbitrary runtime exceptions.

## Final Thread-Pool Shutdown

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
- `docs/agent-docs/upload-queue-stop-async-upload-thread.md`
- `docs/agent-docs/upload-queue-upload-resource.md`
- `docs/agent-docs/drive-upload-resource-by-id.md`
- `docs/agent-docs/drive-upload-path.md`

## Source Pointers

- `src/uploadQueue.ml`: `poll_upload_queue`
- `src/uploadQueue.ml`: `upload_resource`
- `src/uploadQueue.ml`: `start_async_upload_thread`
- `src/uploadQueue.ml`: `stop_async_upload_thread`
- `src/gdfuseFlow.ml`: shutdown path that later joins the poll thread
- `src/threadPool.ml`: `shutdown`
