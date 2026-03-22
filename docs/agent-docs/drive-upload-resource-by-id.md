# `Drive.upload_resource_by_id`

## Purpose

`Drive.upload_resource_by_id` is the async-worker bridge from the upload queue
back into the normal `Drive` upload machinery.

It is intentionally small. Its job is only to:

- load the current cached resource row by cache resource id
- re-enter the authenticated request/session layer
- run `upload_resource_with_retry`

This helper does not decide queue policy, poll the queue, or build the actual
upload request.

## Signature

```ocaml
val upload_resource_by_id : int64 -> unit
```

The input id is the cache resource id:

```ocaml
CacheData.Resource.id
```

It is not the remote Google Drive file id. That distinction matters because the
helper reloads a local cache row first and only reaches the remote id later
through the normal upload helpers.

## Entire Implementation

The implementation is:

```ocaml
let upload_resource_by_id resource_id =
  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  let resource = Cache.Resource.select_resource_with_id cache resource_id in
  match resource with
  | Some r -> do_request (upload_resource_with_retry r) |> ignore
  | None ->
      Utils.log_with_header
        "Cannot find queued resource to upload with resource_id=%Ld.\n%!"
        resource_id
```

That is the whole control flow.

## High-Level Flow

At a high level, the helper does this:

1. read the current global `Context`
2. get the cache handle from that context
3. reload the resource row by cache id
4. if the row exists, run `do_request (upload_resource_with_retry r)`
5. otherwise, log and return

So this helper is the worker-side handoff from queue state back into the normal
request/session upload path.

## Where It Is Used

`Drive.init_filesystem` passes this helper to:

```ocaml
UploadQueue.start_async_upload_thread cache
  config.Config.async_upload_threads upload_resource_by_id
```

That means the queue subsystem owns:

- polling upload entries
- marking queue entries `Uploading`
- deleting or requeuing entries
- scheduling work on the thread pool

but `Drive` still owns the actual resource upload behavior.

See `docs/agent-docs/drive-init-filesystem.md` for the startup side of that
registration.

## Cache Reload By Resource Id

The lookup is:

```ocaml
Cache.Resource.select_resource_with_id cache resource_id
```

This has two important consequences.

First, the worker uploads the current cache row, not a stale copy captured when
the queue entry was created.

Second, the queue entry only needs to store the stable local resource id. It
does not need to duplicate the full resource record or remote file metadata.

## Request Boundary: `do_request`

If the row exists, the helper executes:

```ocaml
do_request (upload_resource_with_retry r) |> ignore
```

This is the point where the background worker re-enters the same authenticated
request/session layer used by foreground Drive operations.

That matters because `upload_resource_with_retry r` is still a session
computation. `upload_resource_by_id` is the adapter that actually runs it from a
plain thread-pool callback.

The ignored result is not hiding useful data here. The session computation
returns `unit`.

## Missing-Row Behavior

If the cache row no longer exists, the helper does not raise. It only logs:

```ocaml
"Cannot find queued resource to upload with resource_id=%Ld.\n%!"
```

and returns `unit`.

This is a deliberate behavior boundary worth remembering:

- queue entry selection already happened upstream
- this helper treats a vanished resource row as "nothing uploadable now"
- because it does not raise, the surrounding queue worker treats the callback as
  successful and removes the queue entry

That means a missing cached resource row is effectively a drop-on-sight case for
the queued upload entry.

So if queued uploads appear to disappear silently, inspect both:

- whether the resource row still exists by cache id
- what the queue worker does with the entry around this callback

## Relationship To `upload_resource_with_retry`

This helper does not upload directly. Its success path is:

```ocaml
upload_resource_with_retry r
```

So once the cache row is found and `do_request` starts, the worker shares the
same downstream behavior as the synchronous upload path:

- flush buffered writes to disk
- normalize request failures
- retry temporary failures
- delegate the actual media update to `upload`

See `docs/agent-docs/drive-upload-resource-with-retry.md` for that common
wrapper.

## Relationship To `UploadQueue`

`upload_resource_by_id` is not the queue worker loop itself.

The surrounding queue layer in `UploadQueue` does the rest:

1. select the next upload entry
2. mark the queue entry `Uploading`
3. call this helper with `entry.resource_id`
4. on success, delete the queue entry
5. on exception, restore the queue entry to `ToUpload`

So this helper should be read as the callback body, not as the queue engine.

## Naming Quirk

The name `upload_resource_by_id` can be misread as "upload by remote file id".

In this codebase it really means:

- upload by cached resource row id

That is the id stored in the upload queue and the one resolved through
`Cache.Resource.select_resource_with_id`.

## What `Drive.upload_resource_by_id` Does Not Do

`Drive.upload_resource_by_id` does not:

- resolve a visible filesystem path
- inspect resource dirtiness
- choose sync vs async mode
- manipulate upload-queue entry state directly
- build the `FilesResource.update` request

It only reloads the queued resource row and runs the normal upload wrapper for
it.

## Related Docs

- `docs/agent-docs/drive-init-filesystem.md`
- `docs/agent-docs/drive-queue-upload.md`
- `docs/agent-docs/drive-upload-resource-with-retry.md`
- `docs/agent-docs/drive-upload.md`

## Source Pointers

- `src/drive.ml`: `upload_resource_by_id`
- `src/drive.ml`: `init_filesystem`
- `src/uploadQueue.ml`: `upload_resource`
- `src/uploadQueue.ml`: `start_async_upload_thread`
