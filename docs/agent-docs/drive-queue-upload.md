# `Drive.queue_upload`

## Purpose

`Drive.queue_upload` is the resource-based dispatcher that chooses between:

- immediate synchronous upload execution
- asynchronous upload-queue insertion

It sits below the path-based helper `upload_with_retry` and above the actual
upload implementation and queue machinery.

So this helper is the point where the upload path stops dealing in visible
paths and starts dealing in a resolved `CacheData.Resource.t`.

## Signature

```ocaml
val queue_upload : CacheData.Resource.t -> unit GapiMonad.SessionM.t
```

The input is already a resolved cached resource row.

The result is still a session/request computation, because the synchronous
branch can continue directly into request-side upload work.

## Entire Implementation

The implementation is:

```ocaml
let queue_upload resource =
  let context = Context.get_ctx () in
  let config = context |. Context.config_lens in
  if config.Config.async_upload_queue then (
    let cache = context.Context.cache in
    flush_memory_buffers resource;
    UploadQueue.queue_resource cache config resource;
    SessionM.return ())
  else upload_resource_with_retry resource
```

That is the whole control flow.

## High-Level Flow

At a high level, `queue_upload` does this:

1. read `Context` and `Config`
2. inspect `Config.async_upload_queue`
3. if async upload is enabled:
   - flush memory buffers
   - persist/deduplicate an upload-queue entry
   - return `unit`
4. otherwise:
   - run `upload_resource_with_retry resource`

So this helper is purely a dispatch-policy branch.

## Branch 1: Synchronous Mode

If:

```ocaml
config.Config.async_upload_queue = false
```

then `queue_upload` does:

```ocaml
upload_resource_with_retry resource
```

in the current request flow.

That means:

- no upload-queue entry is created
- no background worker is involved
- the caller stays on the direct upload path immediately

The later retry behavior comes from `upload_resource_with_retry`, not from any
logic inside `queue_upload` itself.

See `docs/agent-docs/drive-upload-resource-with-retry.md` for the common
wrapper that both the direct sync branch and the async worker path eventually
share.

## Branch 2: Asynchronous Mode

If:

```ocaml
config.Config.async_upload_queue = true
```

then `queue_upload` does three things:

1. `flush_memory_buffers resource`
2. `UploadQueue.queue_resource cache config resource`
3. `SessionM.return ()`

So async mode separates:

- making sure dirty bytes are on disk now
- from actually performing the network upload later

The helper itself returns as soon as the queue-side handoff is complete.

## Why Async Mode Flushes Buffers Here

The explicit:

```ocaml
flush_memory_buffers resource
```

in the async branch is important because the background worker will later read
the cache file from disk, not any in-memory write buffer.

Without that flush, async queue insertion could succeed while the on-disk cache
file still lagged behind the most recent buffered writes.

## Why Sync Mode Does Not Flush Here

The synchronous branch does not call `flush_memory_buffers` directly here
because:

```ocaml
upload_resource_with_retry resource
```

already flushes memory buffers at the start of its own body.

So both branches ensure buffered writes reach disk before upload reads from the
cache file, but they do it at different layers:

- async mode: in `queue_upload`
- sync mode: in `upload_resource_with_retry`

See `docs/agent-docs/drive-upload-resource-with-retry.md` for that downstream
wrapper.

## Relationship To `UploadQueue.queue_resource`

The async branch delegates queue persistence to:

```ocaml
UploadQueue.queue_resource cache config resource
```

That downstream helper:

- deduplicates by `resource_id`
- creates an upload-entry row in queue state `ToUpload` when needed
- can wait for queue length to fall below
  `config.async_upload_queue_max_length`

So `queue_upload` should be read as:

- "decide whether to enqueue or upload directly"

not as:

- "implement the queue mechanics itself"

See `docs/agent-docs/drive-upload-resource-by-id.md` for the worker-side
callback that the async queue eventually invokes after entry selection.

## Relationship To Resource State

One subtle but important point is that `queue_upload` does not itself change the
resource state.

In the common file-callback path, that state transition has already happened in
`start_uploading_if_dirty`, which flips:

- `ToUpload -> Uploading`

before `upload_if_dirty` ever reaches this helper.

But there are also direct callers, such as the rename-replace path, that call
`queue_upload` after marking a target resource `ToUpload` themselves.

So this helper assumes the caller has already prepared resource state as needed;
it does not enforce a single uniform state transition policy on its own.

## Queue Entry State Is Separate

In async mode, the queue entry created by `UploadQueue.queue_resource` has its
own state machine, separate from the resource row's state.

That means:

- resource state lives in `CacheData.Resource.State`
- queue entry state lives in `CacheData.UploadEntry.State`

Do not read queue insertion as a direct substitute for updating the resource
row itself.

## Relationship To `upload_with_retry`

`upload_with_retry path` is the immediate caller that resolves a visible path
and then hands the resulting resource here:

```ocaml
get_resource path_in_cache trashed >>= fun resource -> queue_upload resource
```

So the split is:

- `upload_with_retry`: path normalization and current-resource resolution
- `queue_upload`: dispatch policy based on config

See `docs/agent-docs/drive-upload-with-retry.md` for the path-side half.

## Naming Quirk: It Does Not Always Queue

The name `queue_upload` is slightly misleading if read too literally.

In synchronous mode, nothing is queued.

The helper immediately enters `upload_resource_with_retry resource`.

So the name reflects the general role in the upload pipeline, not the behavior
of every branch.

## Error Surface

`queue_upload` has no local exception handling.

Possible failures therefore come from:

- `flush_memory_buffers`
- `UploadQueue.queue_resource`
- `upload_resource_with_retry`

At this layer it simply propagates those failures.

## What `Drive.queue_upload` Does Not Do

`Drive.queue_upload` does not:

- resolve a visible path
- decide whether a resource is dirty
- update the resource state itself
- implement queue polling or worker execution
- perform the final network upload in async mode

It only chooses direct upload vs queue handoff for an already resolved
resource.

## Related Docs

- `docs/agent-docs/drive-upload-with-retry.md`
- `docs/agent-docs/drive-upload-resource-by-id.md`
- `docs/agent-docs/drive-upload-path.md`
- `docs/agent-docs/drive-flush-fsync-release.md`
- `docs/agent-docs/drive-rename.md`

## Source Pointers

- `src/drive.ml`: `queue_upload`
- `src/drive.ml`: `flush_memory_buffers`
- `src/drive.ml`: `upload_resource_with_retry`
- `src/drive.ml`: `upload_with_retry`
- `src/uploadQueue.ml`: `queue_resource`
