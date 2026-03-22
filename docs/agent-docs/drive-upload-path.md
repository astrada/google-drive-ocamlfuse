# `Drive` Upload Path

## Purpose

This note documents the content-upload path centered on:

- `Drive.queue_upload`
- `Drive.upload_resource_with_retry`
- `Drive.upload`

It is the path used to push locally modified file content back to Google Drive
after the resource already has a remote file id.

It is not the same as:

- initial remote file creation in `create_remote_resource`
- metadata-only Drive patches such as `chmod`, `chown`, or `setxattr`

Those update other parts of Drive state, but they do not go through this
content-upload pipeline.

See `docs/agent-docs/drive-create-remote-resource.md` for the remote-creation
side of the lifecycle.

The thin FUSE-side callbacks that usually trigger this path are documented in
`docs/agent-docs/drive-flush-fsync-release.md`.

## Scope In The Write Lifecycle

The content-upload path starts only after some earlier operation has made a file
dirty.

The main dirtying operations are:

- `write`, which downloads the current content if needed, writes locally, and
  sets the resource state to `ToUpload`
- `truncate`, which updates the local cached file and sets `ToUpload`
- one rename-replace path, which copies cache content into the target resource,
  marks the target `ToUpload`, and calls `queue_upload` directly

So the upload lifecycle has three phases:

1. local mutation marks a resource dirty
2. a later trigger schedules upload work
3. the upload path flushes buffers, retries transient failures, and patches the
   remote file

## Resource State Machine For Uploads

The relevant resource states are:

- `Synchronized`: local cache content matches Drive
- `ToUpload`: local content changed and must be uploaded
- `Uploading`: an upload is in progress or has been scheduled to start

The usual transition is:

```text
Synchronized -> ToUpload -> Uploading -> Synchronized
```

Two nuances matter:

- `flush`, `fsync`, and `release` only schedule work when the current state is
  exactly `ToUpload`
- the async queue also has its own entry states, separate from resource states

## Phase 1: Marking A Resource Dirty

### `write`

`Drive.write` first resolves the resource and ensures local content exists:

1. `get_resource path_in_cache trashed`
2. `with_retry download_resource resource`
3. write to either memory buffers or the cache file

See `docs/agent-docs/drive-write.md` for the full local-mutation flow, and
`docs/agent-docs/drive-download-resource.md` for the lock/state logic behind
that "ensure local content exists" step.

After the local write, it marks the resource dirty:

- if the write extended the file, it updates `size` and `state = ToUpload`
- otherwise it updates only the state to `ToUpload`

If the file grew, it also calls `shrink_cache ~file_size` to account for the new
local cache usage.

### `truncate`

`Drive.truncate` follows the same basic pattern:

- resolve resource
- flush write buffers first
- ensure a local cache file exists
- change the local file size
- set `size` and `state = ToUpload`

See `docs/agent-docs/drive-truncate.md` for the exact ordering, including the
signed cache-size delta and the metadata-before-`truncate(2)` update order.

### Rename-Replace Special Case

There is one rename flow where replacing a target file's content copies the
source cache file into the target cache file, marks the target resource
`ToUpload`, and immediately calls `queue_upload target_resource`.

That path is important because it shows `queue_upload` is the general content
upload dispatcher, not just something reached from `flush`/`fsync`/`release`.

## Phase 2: Scheduling Upload Work

### Cheap Dirty Check: `start_uploading_if_dirty`

`flush`, `fsync`, and `release` all call:

```ocaml
upload_if_dirty path
```

`upload_if_dirty` first runs `start_uploading_if_dirty path`, which:

- normalizes the path with `get_path_in_cache`
- uses `lookup_resource` directly
- checks whether the cached row is currently `ToUpload`

If so, it immediately flips the resource state to `Uploading` and returns `true`.
Otherwise it returns `false`.

This function is deliberately cheap:

- it does not call `get_resource`
- it does not refresh metadata
- it acts as the idempotency gate for repeated `flush`/`fsync`/`release` calls

So repeated close/sync callbacks do not keep rescheduling the same upload once
the resource has already left `ToUpload`.

See `docs/agent-docs/drive-start-uploading-if-dirty.md` for the helper-focused
view of this exact state gate.

See `docs/agent-docs/drive-flush-fsync-release.md` for the boundary-layer view
of those three callbacks and the fact that they are identical at the `Drive`
layer.

### `upload_if_dirty`

If `start_uploading_if_dirty` returns `true`, `upload_if_dirty` launches:

```ocaml
do_request (upload_with_retry path) |> ignore
```

That means the actual upload dispatch runs in the request/session machinery, but
the scheduling decision was made locally first.

See `docs/agent-docs/drive-upload-if-dirty.md` for the helper-focused view of
that bridge into `do_request (upload_with_retry path)`.

### `upload_with_retry`

`upload_with_retry path` is the path-based bridge into the actual upload
dispatcher:

1. normalize the visible path
2. resolve the current resource with `get_resource`
3. call `queue_upload resource`

Using `get_resource` here is important because the actual upload should operate
on the latest cached row, not the stale row that `start_uploading_if_dirty` may
have seen during its fast local check.

See `docs/agent-docs/drive-upload-with-retry.md` for the helper-focused view of
this path-resolution step and the naming quirk around "with_retry".

## Phase 3: Dispatch Policy In `queue_upload`

`queue_upload resource` selects between synchronous and asynchronous upload
execution.

See `docs/agent-docs/drive-queue-upload.md` for the helper-focused view of this
dispatcher and its naming quirk.

### Synchronous Mode

If `config.async_upload_queue = false`, it simply runs:

```ocaml
upload_resource_with_retry resource
```

in the current request flow.

### Asynchronous Mode

If `config.async_upload_queue = true`, it does two things:

1. `flush_memory_buffers resource`
2. `UploadQueue.queue_resource cache config resource`

and returns immediately.

That means async mode separates:

- getting dirty bytes onto disk
- actually performing the network upload later

### Queue Semantics

`UploadQueue.queue_resource` deduplicates by `resource_id`:

- if an upload entry already exists for the resource, it does not add another
- otherwise it inserts an upload-queue row in state `ToUpload`

If `config.async_upload_queue_max_length > 0`, it blocks before enqueueing until
the queue length drops below the configured limit.

The queue entry state is not the same as the resource state:

- resource state lives in `CacheData.Resource.State`
- queue entry state lives in `CacheData.UploadEntry.State`

Keeping those distinct is important when debugging async behavior.

## Async Worker Path

When async upload is enabled, `Drive.init_filesystem` starts the poll thread and
worker pool in `UploadQueue`.

The async flow is:

1. poll thread selects the next queue entry
2. queue entry state becomes `Uploading`
3. worker calls `Drive.upload_resource_by_id resource_id`
4. `upload_resource_by_id` loads the current resource row by cache id
5. it runs `do_request (upload_resource_with_retry r)`
6. on success the queue entry is deleted
7. on failure the queue entry returns to queue state `ToUpload`

So async mode changes when the upload happens, but not which Drive upload logic
ultimately runs.

## The Actual Upload: `upload_resource_with_retry`

`upload_resource_with_retry resource` is the narrow wrapper around the real
network operation:

1. `flush_memory_buffers resource`
2. `with_retry (fun r -> try_with_default (upload r)) resource`

The buffer flush is unconditional when `write_buffers` is enabled. This is the
last guard that ensures buffered local writes actually reach the cache file
before the upload reads from disk.

## Buffer Flush Requirement

`upload` reads bytes from:

```ocaml
let content_path = Cache.get_content_path cache resource
```

and then creates a `GapiMediaResource` from that file path.

So if a write path updates only in-memory buffers and does not flush them before
upload, the upload would send stale on-disk content.

That is why buffer flushing appears in multiple places:

- before direct upload retry execution
- before async enqueue
- in some other local content-manipulation paths such as `truncate`

## The Actual Network Operation: `upload`

`upload resource` assumes the resource already has a remote id:

```ocaml
let remote_id = resource |. CacheData.Resource.remote_id |> Option.get
```

So this path is for updating an existing remote file, not creating a new remote
file shell.

The function then:

1. resolves `content_path`
2. computes the outgoing MIME type
3. builds `GapiMediaResource.create_file_resource`
4. updates the cached resource state and size to `Uploading`
5. sends `FilesResource.update`
6. updates the cached row from the returned Drive file metadata
7. marks the resource `Synchronized` if it is still in `Uploading`
8. runs `shrink_cache ()`

### MIME Type Selection

If `config.autodetect_mime = true`, the content type is passed as `""`, letting
Drive infer it.

Otherwise, the upload path prefers:

- the cached resource MIME type, if present and non-empty
- otherwise the MIME type detected by the local media resource helper

### Zero-Byte Files

If the local content length is `0L`, `upload` sends:

- the metadata patch
- no media body (`media_source = None`)

So empty-file uploads still update remote metadata, especially modified time,
without requiring a media payload.

### Request Shape

The upload call is:

```ocaml
FilesResource.update
  ~enforceSingleParent:true
  ~supportsAllDrives:true
  ~std_params:file_std_params
  ?media_source
  ~custom_headers
  ~fileId:remote_id
  file_patch
```

Two details are worth remembering:

- `file_patch` only forces `modifiedTime = now`
- `custom_headers` may carry Drive resource keys

The returned `file` object is then treated as the new source of truth for the
cached resource row.

## Post-Upload Reconciliation

After `FilesResource.update` succeeds, `upload`:

1. updates the resource from the returned file metadata
2. reloads by remote id from the cache if another row now owns that remote id
3. if the resource state is still `Uploading`, sets the final state to
   `Synchronized`
4. saves the updated resource

That reloaded-by-remote-id step is the same convergence pattern used elsewhere
in `Drive`: remote id is treated as the durable identity, while paths can shift.

## Retry Semantics

`with_retry` handles `Utils.Temporary_error` specially.

On a retryable failure it:

1. waits with exponential backoff
2. refreshes the remote file metadata with `FilesResource.get`
3. rewrites the cached resource from that fresh server state
4. preserves the high-level operation direction:
   - `ToUpload` for upload retries
   - `ToDownload` for download retries
5. retries the original operation

So upload retries are not blind resends against stale metadata. The code
refreshes its view of the remote file between attempts.

If retries are exhausted, the upload path raises `IO_error`.

## Distinction Between Queue State And Resource State

There are two parallel state machines during async uploads:

- resource state: `ToUpload`, `Uploading`, `Synchronized`, etc.
- upload-entry state: queue-local `ToUpload` / `Uploading`

They answer different questions:

- resource state: what is true of the cached file?
- queue entry state: what is true of the async scheduling record?

Confusing the two makes async upload bugs hard to reason about.

## Config Knobs

The upload path is primarily shaped by these config fields:

- `write_buffers`
- `autodetect_mime`
- `async_upload_queue`
- `async_upload_threads`
- `async_upload_queue_max_length`

Their effects are:

- `write_buffers`: writes can accumulate in memory and therefore must be flushed
  before upload
- `autodetect_mime`: upload content type is delegated to Drive when enabled
- `async_upload_queue`: switches between direct upload and queued upload
- `async_upload_threads`: sizes the async worker pool
- `async_upload_queue_max_length`: optionally backpressures enqueueing

## Maintenance Notes

When changing this area, watch these invariants:

- local write paths must set `ToUpload`, or later flush/sync callbacks will not
  schedule uploads
- any path that uploads file content must flush memory buffers first when
  `write_buffers` is enabled
- async queue deduplication is by `resource_id`, not by path
- `flush`, `fsync`, and `release` are intentionally the same upload trigger
- this path assumes a `remote_id` already exists; remote file creation is a
  separate concern
- resource-state transitions and upload-entry-state transitions are related but
  not interchangeable

## Related Docs

- `docs/agent-docs/drive-init-filesystem.md`
- `docs/agent-docs/drive-get-resource.md`

## Source Pointers

- `src/drive.ml`: `write`
- `src/drive.ml`: `truncate`
- `src/drive.ml`: `start_uploading_if_dirty`
- `src/drive.ml`: `queue_upload`
- `src/drive.ml`: `upload_resource_with_retry`
- `src/drive.ml`: `upload`
- `src/uploadQueue.ml`: async queue polling and worker dispatch
