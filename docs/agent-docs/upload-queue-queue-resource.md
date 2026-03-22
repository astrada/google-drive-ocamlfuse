# `UploadQueue.queue_resource`

## Purpose

`UploadQueue.queue_resource` is the queue-side persistence helper for async
uploads.

It is the function that takes an already resolved dirty resource and turns that
into a cache-backed upload-queue entry when async upload is enabled.

Its responsibilities are:

- enforce the optional queue-length limit
- deduplicate queue entries by `resource_id` as far as the current backend
  allows
- insert a new upload-entry row in queue state `ToUpload` when needed

So this helper is the async enqueue step that sits between:

- `Drive.queue_upload`, which decides to use the async path at all
- the poll-thread subsystem, which later selects and executes queued entries

## Public Signature

`queue_resource` is part of `src/uploadQueue.mli`:

```ocaml
val queue_resource : CacheData.t -> Config.t -> CacheData.Resource.t -> unit
```

The parameters are:

- `cache`: the cache handle that stores upload-queue rows
- `config`: runtime config, used here for queue-length limiting
- `resource`: the already resolved resource row being queued

The helper is synchronous and returns `unit`. It does not return a queue entry
handle to the caller.

## Entire Implementation

The implementation is:

```ocaml
let queue_resource cache config resource =
  let resource_id = resource.CacheData.Resource.id in
  let wait_for_slot () =
    let check () =
      let max_length = config.Config.async_upload_queue_max_length in
      let entries = Cache.UploadQueue.count_entries cache in
      if entries >= max_length then
        Utils.log_with_header
          "Waiting for pending uploads (%d) to get below the limit (%d)\n%!"
          entries max_length
      else (
        Utils.log_with_header "Pending uploads (%d) below the limit (%d)\n%!"
          entries max_length;
        raise Exit)
    in
    try
      while true do
        check ();
        Thread.delay 1.0
      done
    with Exit -> ()
  in
  let queue_r () =
    let upload_entry =
      {
        CacheData.UploadEntry.id = 0L;
        resource_id;
        state = CacheData.UploadEntry.State.(to_string ToUpload);
        last_update = Unix.gettimeofday ();
      }
    in
    let inserted_upload_entry =
      Cache.UploadQueue.insert_upload_entry cache upload_entry
    in
    Utils.log_with_header
      "END: Resource id=%Ld queued for uploading (entry id=%Ld)\n%!" resource_id
      inserted_upload_entry.CacheData.UploadEntry.id
  in
  Utils.log_with_header "BEGIN: Queue resource id=%Ld for uploading\n%!"
    resource_id;
  let upload_entry_with_resource_id =
    Cache.UploadQueue.select_with_resource_id cache resource_id
  in
  match upload_entry_with_resource_id with
  | None ->
      if config.Config.async_upload_queue_max_length > 0 then wait_for_slot ();
      queue_r ()
  | Some e ->
      Utils.log_with_header
        "END: Resource with id=%Ld already queued (entry id=%Ld)\n%!"
        resource_id e.CacheData.UploadEntry.id
```

That is the whole control flow.

## High-Level Flow

At a high level, the helper does this:

1. extract the cache resource id
2. log enqueue begin
3. ask whether a queue entry already exists for that resource id
4. if one already exists, log and return
5. otherwise, optionally wait for queue length to fall below the configured
   limit
6. insert a new upload-entry row in state `ToUpload`
7. log the inserted queue-entry id

So this helper is pure queue-row persistence and admission control. It does not
start upload execution itself.

## Queue Entry Shape

When the helper inserts a row, it creates:

```ocaml
{
  CacheData.UploadEntry.id = 0L;
  resource_id;
  state = CacheData.UploadEntry.State.(to_string ToUpload);
  last_update = Unix.gettimeofday ();
}
```

The important points are:

- queue entries are keyed by cache `resource_id`, not by visible path
- newly inserted rows always start in queue state `ToUpload`
- `last_update` is set at insertion time

So `queue_resource` is the place where upload work first becomes visible to the
async queue subsystem.

## Deduplication By `resource_id`

Before inserting, the helper checks:

```ocaml
Cache.UploadQueue.select_with_resource_id cache resource_id
```

If that returns an entry, `queue_resource` does not insert another row.

So the intended contract is:

- at most one queued entry per resource id at a time

This is why repeated async queue handoffs for the same resource are usually
collapsed instead of growing the queue.

## Backend-Specific Deduplication Quirk

One implementation detail is worth documenting explicitly.

The two cache backends do not implement `select_with_resource_id` identically:

- the SQLite backend only searches rows whose queue state is `ToUpload`
- the memory backend returns the first row for that `resource_id` regardless of
  queue-entry state

There is also no SQLite uniqueness constraint on `upload_queue.resource_id`.

So the intended deduplication rule is clear, but the exact behavior is not
fully uniform across backends once a queue entry has already advanced to
`Uploading`.

That is a maintenance quirk worth keeping in mind if duplicate queue rows ever
appear in sqlite-backed async mode.

## Queue-Length Limit

If:

```ocaml
config.Config.async_upload_queue_max_length > 0
```

then the helper may block in:

```ocaml
wait_for_slot ()
```

That inner loop:

1. counts current queue entries
2. logs whether the queue is still above the limit
3. sleeps for one second
4. repeats until `entries < max_length`

So this is a simple polling backpressure mechanism, not a condition-variable
based queue admission system.

## What The Length Limit Counts

The length check uses:

```ocaml
Cache.UploadQueue.count_entries cache
```

That counts all queue rows, not only rows still in `ToUpload`.

So the admission limit is really:

- total upload-queue table size

not:

- number of not-yet-started queue entries

This means in-progress `Uploading` rows still count against the configured queue
length limit until they are deleted.

## Order Of Checks

The helper's branch order is:

1. deduplication check by `resource_id`
2. optional wait for queue length
3. insertion

That order matters.

If a matching queue row already exists, `queue_resource` returns immediately and
does not wait on the queue-length limit at all.

So the queue limit only applies when the helper is about to insert a genuinely
new row.

## Synchronous Behavior

`queue_resource` does not hand work to the thread pool.

It does not call:

- `upload_resource`
- `poll_upload_queue`
- `Drive.upload_resource_by_id`

So even though it is part of the async upload pipeline, this helper itself is a
synchronous local cache/database operation.

The later worker-side execution happens only after the poll thread wakes and
selects the queued row.

## Relationship To `Drive.queue_upload`

The normal caller is the async branch of:

```ocaml
Drive.queue_upload
```

That caller already did two important things:

- chose async mode from config
- flushed memory buffers for the resource to disk

So `UploadQueue.queue_resource` should be read as the downstream queue-row
operation, not as the place where upload policy is chosen.

See `docs/agent-docs/drive-queue-upload.md` for that caller-side policy split.

## Relationship To Resource State

This helper does not update the resource row itself.

The resource-side state transition has already happened earlier, typically to:

- `CacheData.Resource.State.Uploading` in the file-callback-trigger path, or
- `ToUpload` in some direct caller paths such as rename-replace

So queue-entry state and resource state remain separate here.

This helper only persists:

- `CacheData.UploadEntry.State.ToUpload`

for the queue row.

## Relationship To The Poll Thread

Rows inserted here are later consumed by:

- `UploadQueue.poll_upload_queue`
- `UploadQueue.upload_resource`

The split is:

- `queue_resource`: persist and deduplicate the row
- `poll_upload_queue`: decide when to attempt dispatch
- `upload_resource`: take one selected row and hand it to a worker thread

See `docs/agent-docs/upload-queue-poll-upload-queue.md` and
`docs/agent-docs/upload-queue-upload-resource.md` for those later stages.

## Logging Pattern

The helper always logs:

- a `BEGIN` line before the dedup check
- an `END` line either for:
  - successful insertion, or
  - "already queued"

So enqueue attempts are visible in logs even when they collapse into no-op
deduplication.

## What `UploadQueue.queue_resource` Does Not Do

`UploadQueue.queue_resource` does not:

- decide whether async upload is enabled
- resolve a visible path
- flush memory buffers
- upload any file content
- start a worker thread directly
- delete or requeue entries after execution

It only persists queue-side work for later execution.

## Related Docs

- `docs/agent-docs/drive-queue-upload.md`
- `docs/agent-docs/upload-queue-start-async-upload-thread.md`
- `docs/agent-docs/upload-queue-poll-upload-queue.md`
- `docs/agent-docs/upload-queue-upload-resource.md`

## Source Pointers

- `src/uploadQueue.ml`: `queue_resource`
- `src/cache.ml`: `UploadQueue.select_with_resource_id`
- `src/cache.ml`: `UploadQueue.insert_upload_entry`
- `src/cache.ml`: `UploadQueue.count_entries`
