# `UploadQueue.upload_resource`

## Purpose

`UploadQueue.upload_resource` is the poll-thread helper that advances one queued
upload entry into actual worker execution.

It sits between:

- `poll_upload_queue`, which asks for one unit of work
- `Drive.upload_resource_by_id`, which is the callback that eventually re-enters
  the normal request/session upload path

Its job is narrower than either of those layers. It:

- selects one queue entry currently in queue state `ToUpload`
- hands that entry to the thread pool
- flips the queue entry to `Uploading` in the worker
- deletes the entry on success or restores it to `ToUpload` on failure

So this helper owns one-entry selection and worker handoff.

## Internal Status And Effective Signature

`upload_resource` is internal to `src/uploadQueue.ml`. It is not exposed in
`src/uploadQueue.mli`.

Its effective shape is:

```ocaml
val upload_resource : CacheData.t -> unit
```

The input is the cache handle. Everything else comes from the global
`ConcurrentUploadQueue` runtime state installed by `start_async_upload_thread`.

## Entire Implementation

```ocaml
let upload_resource cache =
  let d = ConcurrentUploadQueue.get () in
  let upload = d.upload_resource_by_id in
  let upload_entry = Cache.UploadQueue.select_next_resource cache in
  let do_work e =
    let entry_id = e.CacheData.UploadEntry.id in
    let resource_id = e.CacheData.UploadEntry.resource_id in
    Utils.log_with_header "Uploading queued entry (id=%Ld) resource_id=%Ld.\n%!"
      entry_id resource_id;
    Cache.UploadQueue.update_entry_state cache
      CacheData.UploadEntry.State.Uploading entry_id;
    (try upload resource_id
     with e ->
       Utils.log_with_header "Upload failed for queued entry (id=%Ld).\n%!"
         entry_id;
       Cache.UploadQueue.update_entry_state cache
         CacheData.UploadEntry.State.ToUpload entry_id;
       raise e);
    Utils.log_with_header "Removing queued entry (id=%Ld).\n%!" entry_id;
    Cache.UploadQueue.delete_upload_entry cache e
  in
  match upload_entry with
  | Some e -> ThreadPool.add_work do_work e d.thread_pool
  | None -> ()
```

That is the whole helper.

## High-Level Flow

At a high level, the helper does this:

1. load the configured upload callback and thread pool from
   `ConcurrentUploadQueue`
2. select one queued entry in state `ToUpload`
3. if none exists, return immediately
4. otherwise schedule a worker callback for that entry through `ThreadPool`
5. inside the worker:
   - mark the queue entry `Uploading`
   - call the configured upload callback with `resource_id`
   - on success, delete the queue entry
   - on failure, restore queue state to `ToUpload` and re-raise

So this helper is best read as "dispatch one queued upload entry now".

## Runtime Dependencies

The helper begins with:

```ocaml
let d = ConcurrentUploadQueue.get () in
let upload = d.upload_resource_by_id in
```

So `upload_resource` does not receive its worker callback or thread pool as
explicit arguments. Both come from the runtime record installed earlier by
`start_async_upload_thread`.

In normal production flow, the callback is `Drive.upload_resource_by_id`.

## Queue Selection: One `ToUpload` Entry

The queue lookup is:

```ocaml
let upload_entry = Cache.UploadQueue.select_next_resource cache
```

This means the helper works only on entries whose queue state is still
`ToUpload`.

If an entry has already been marked `Uploading`, this selector will not pick it
again.

### No Explicit Ordering Guarantee

There is no explicit FIFO or age-based ordering guarantee in the current
selection path:

- the SQLite backend queries `WHERE state = 'ToUpload'` with no `ORDER BY`
- the memory backend scans a hash table and returns the first matching entry it
  encounters

So `select_next_resource` should be read as:

- "return some queued `ToUpload` entry"

not as:

- "return the oldest queued entry"

## Empty-Queue Behavior

If `select_next_resource` returns `None`, the helper does exactly this:

```ocaml
| None -> ()
```

There is no log line and no retry loop at this layer. An empty queue is a
normal idle condition.

## Worker Callback: `do_work`

The actual queue-entry transition logic lives in the local worker closure:

```ocaml
let do_work e = ...
```

That closure owns the queue-entry lifecycle for a single piece of work.

## Step 1: Mark Queue Entry `Uploading`

Inside the worker, the first queue-side state change is:

```ocaml
Cache.UploadQueue.update_entry_state cache
  CacheData.UploadEntry.State.Uploading entry_id
```

At this layer, the queue-entry state machine is:

```text
ToUpload -> Uploading -> deleted
```

or, on failure:

```text
ToUpload -> Uploading -> ToUpload
```

## Step 2: Call The Upload Callback By `resource_id`

The worker then does:

```ocaml
upload resource_id
```

Two details matter:

- the callback receives the cache resource id, not the remote Drive file id
- `UploadQueue.upload_resource` does not know anything about visible paths or
  request/session machinery itself

In the production path, the callback is `Drive.upload_resource_by_id`, which
then reloads the resource row and runs the shared upload wrapper.

## Step 3: Failure Path Requeues The Entry

If the callback raises, `do_work` catches that exception long enough to:

1. log the failure
2. set the queue entry back to `ToUpload`
3. re-raise the exception

So retry policy at this layer is:

- do not swallow the failure
- do make the queue entry selectable again for a later poll cycle

## Step 4: Success Path Deletes The Entry

If the callback returns normally, the worker finally does:

```ocaml
Cache.UploadQueue.delete_upload_entry cache e
```

So queue entry deletion is success-driven, not attempt-driven.

## `ThreadPool.add_work` Is The Execution Boundary

The dispatch step is:

```ocaml
ThreadPool.add_work do_work e d.thread_pool
```

`ThreadPool.add_work` does not keep a separate backlog queue. Instead it waits
while `pending_threads >= max_threads` and starts a new worker once a slot is
free.

So `UploadQueue.upload_resource` can block the poll thread while the worker pool
is full.

See `docs/agent-docs/thread-pool-add-work.md` for the generic thread-pool
contract around caller blocking, worker registration, and exception-safe slot
release.

## Relationship To `Drive.upload_resource_by_id`

`UploadQueue.upload_resource` and `Drive.upload_resource_by_id` own adjacent but
different layers.

This helper owns:

- queue-entry selection
- queue-entry state updates
- success deletion vs failure requeue
- worker-thread handoff

`Drive.upload_resource_by_id` owns:

- resource-row reload by cache resource id
- entry into `do_request`
- handoff to `upload_resource_with_retry`

## Failure Path And Thread-Pool Cleanup

On failure, `do_work` re-raises after restoring queue state to `ToUpload`.

`ThreadPool.add_work` removes the worker from its internal table in an
exception-safe cleanup path, so a failed upload worker still releases its
thread-pool slot before the exception escapes the thread.

That means the queue entry is made selectable again without wedging worker-slot
accounting at the thread-pool layer.

## What `UploadQueue.upload_resource` Does Not Do

`UploadQueue.upload_resource` does not:

- insert upload-queue entries
- choose whether async upload is enabled
- normalize visible filesystem paths
- reload resources from the cache by path
- perform the actual Drive request itself
- own the outer polling loop or shutdown logic

It only advances one queued upload entry into worker execution.

## Related Docs

- `docs/agent-docs/drive-init-filesystem.md`
- `docs/agent-docs/upload-queue-queue-resource.md`
- `docs/agent-docs/upload-queue-poll-upload-queue.md`
- `docs/agent-docs/upload-queue-start-async-upload-thread.md`
- `docs/agent-docs/thread-pool-add-work.md`
- `docs/agent-docs/drive-queue-upload.md`
- `docs/agent-docs/drive-upload-resource-by-id.md`
- `docs/agent-docs/drive-upload-resource-with-retry.md`

## Source Pointers

- `src/uploadQueue.ml`: `upload_resource`
- `src/uploadQueue.ml`: `poll_upload_queue`
- `src/uploadQueue.ml`: `start_async_upload_thread`
- `src/cache.ml`: `UploadQueue.select_next_resource`
- `src/threadPool.ml`: `add_work`
