# `Drive` Upload Path

## Purpose

This note is the end-to-end map of the content-upload lifecycle.

Its job is to show how the pieces fit together:

- which paths mark a resource dirty
- which callbacks notice that dirty state
- where the code switches from local state checks to request/session work
- where sync-vs-async policy is chosen
- where the actual `FilesResource.update` request finally happens

It is intentionally an overview note. The helper-specific docs own the detailed
semantics of each stage.

This path is only for file-content uploads after a resource already has a remote
Drive file id. It is not the same as:

- initial remote object creation in `create_remote_resource`
- metadata-only mutations such as `chmod`, `chown`, `utime`, or xattr updates

See `docs/agent-docs/drive-create-remote-resource.md` for remote object
creation.

## Scope In The Write Lifecycle

The upload pipeline starts only after some earlier operation has already made a
resource dirty.

The main producers are:

- `Drive.write`
- `Drive.truncate`
- one rename-replace path inside `Drive.rename`

Those paths are responsible for creating or updating the authoritative local
cache file and setting the resource state to `ToUpload`.

After that, the later upload lifecycle is:

1. a trigger callback notices the `ToUpload` state
2. the code enters the request/session layer
3. dispatch policy chooses direct upload or async queueing
4. the common upload wrapper flushes buffers and retries temporary failures
5. `Drive.upload` performs one concrete `FilesResource.update` attempt

## Resource And Queue State

The relevant resource states are:

- `Synchronized`
- `ToUpload`
- `Uploading`

The usual resource-state progression is:

```text
Synchronized -> ToUpload -> Uploading -> Synchronized
```

There is also a separate async queue-entry state machine in
`CacheData.UploadEntry.State`.

That distinction matters:

- resource state answers "what is true of the cached file?"
- queue-entry state answers "what is true of the async scheduling record?"

The two are related, but they are not interchangeable.

## End-To-End Flow

### 1. Dirtying Operations

`Drive.write` and `Drive.truncate` delegate local mutation to
`DriveFileMutations`. They stop after local mutation and cache updates. They do
not upload immediately.

What they contribute to the later upload path is:

- a usable local cache file
- `state = ToUpload`
- updated size/accounting where needed

See `docs/agent-docs/drive-write.md` and
`docs/agent-docs/drive-truncate.md` for those mutation-side details.

### 2. File Callbacks Trigger Upload Dispatch

The usual trigger callbacks are:

- `Drive.flush`
- `Drive.fsync`
- `Drive.release`

At the `Drive` layer they are intentionally identical and all delegate to:

```ocaml
upload_if_dirty path
```

See `docs/agent-docs/drive-flush-fsync-release.md` for the FUSE-boundary view
of those callbacks.

### 3. Cheap Local Gate: `DriveUploadDispatch.start_uploading_if_dirty`

The upload-dispatch core begins with the local gate:

```ocaml
start_uploading_if_dirty runtime path
```

That helper is intentionally cheap:

- it normalizes the path
- it uses `lookup_resource`, not `get_resource`
- it only starts work when the current cached state is exactly `ToUpload`
- it flips the cached row to `Uploading`

This is the main repeated-callback suppression point for `flush` / `fsync` /
`release`.

See `docs/agent-docs/drive-start-uploading-if-dirty.md` for the exact local
state contract.

### 4. Request-Side Re-Resolution

If the local gate returns `true`, the `Drive`-level wrapper enters the
request/session layer:

```ocaml
do_request upload_request |> ignore
```

`DriveUploadDispatch.upload_with_retry` then:

1. normalizes the visible path again
2. resolves the current resource with `get_resource`
3. hands that row to `queue_upload`

This second lookup is deliberate. The upload path should use the current
authoritative resource row, not only the cached row seen during the cheap local
gate.

See `docs/agent-docs/drive-upload-if-dirty.md` and
`docs/agent-docs/drive-upload-with-retry.md` for those two handoff layers.

### 5. Dispatch Policy In `DriveUploadDispatch.queue_upload`

`DriveUploadDispatch.queue_upload` is where the path stops dealing in visible
paths and starts dealing in a resolved `CacheData.Resource.t`.

It chooses between two modes.

### Direct Synchronous Mode

If `config.async_upload_queue = false`, it calls the direct-upload port:

```ocaml
upload_now_with_retry resource
```

In production wiring, that port is `upload_resource_with_retry`.

### Async Queue Mode

If `config.async_upload_queue = true`, it:

1. flushes memory buffers for the resource
2. inserts or reuses an upload-queue entry keyed by `resource_id`
3. returns without waiting for the network upload to complete

Later, the queue worker reloads the current resource row by cache id and calls
back into `Drive.upload_resource_by_id`.

See `docs/agent-docs/drive-queue-upload.md` for the dispatch branch,
`docs/agent-docs/upload-queue-queue-resource.md` for the queue-row insertion
step,
`docs/agent-docs/upload-queue-upload-resource.md` for the queue-side poll
helper and worker handoff,
`docs/agent-docs/drive-upload-resource-by-id.md` for the worker callback, and
`docs/agent-docs/drive-init-filesystem.md` for queue-thread startup.

### 6. Common Upload Execution

Once execution reaches `upload_resource_with_retry`, the synchronous path and
the async worker path share the same downstream behavior:

1. flush memory buffers to disk
2. run `Drive.upload`, which delegates to `DriveUploads`
3. normalize request failures through `try_with_default`
4. retry only `Utils.Temporary_error` through `with_retry`

See `docs/agent-docs/drive-upload-resource-with-retry.md` for that wrapper.

### 7. Concrete Network Upload

`DriveUploads` is the actual remote update attempt. `Drive.upload` is the
Drive-level helper that builds the production runtime and delegates to it.

At a high level it:

1. reads the on-disk cache file
2. chooses the outgoing MIME/media representation
3. updates local cached state to `Uploading`
4. sends `FilesResource.update`
5. rebuilds cache state from the returned Drive metadata
6. returns to `Synchronized` only if no newer local state superseded it

This is the point where the upload pipeline stops being about dispatch and retry
policy and actually mutates the remote file.

See `docs/agent-docs/drive-upload.md` for the request shape and post-upload
cache reconciliation rules.

## Why Buffer Flushing Appears In Multiple Places

The upload pipeline has to defend against dirty write buffers that have not yet
reached the on-disk cache file.

That is why buffer flushing appears in more than one stage:

- `truncate` flushes before changing file length
- `DriveUploadDispatch.queue_upload` flushes before async queue handoff
- `upload_resource_with_retry` flushes before the actual upload attempt

Those are not redundant copies of the same responsibility. They protect
different handoff boundaries.

## Config Knobs

The upload path is primarily shaped by these config fields:

- `write_buffers`
- `editable_docs`
- document export-format settings such as `document_format` and
  `spreadsheet_format`
- `autodetect_mime`
- `async_upload_queue`
- `async_upload_threads`
- `async_upload_queue_max_length`

Their main effects are:

- `write_buffers`: dirty bytes may live in memory until a later flush point
- `editable_docs`: allows write/upload flow for Google-native files unless
  their configured format is `desktop`
- per-type document format settings: choose the local exported representation
  and, when `editable_docs = true`, the MIME type used on re-upload
- `autodetect_mime`: `upload` may delegate MIME detection to Drive
- `async_upload_queue`: choose direct upload vs queue handoff
- `async_upload_threads`: size the async worker pool
- `async_upload_queue_max_length`: optionally backpressure enqueueing

## Maintenance Notes

When changing this area, watch these invariants:

- dirtying paths must set `ToUpload`, or the trigger callbacks will never start
  upload dispatch
- any path that uploads file content must make sure buffered writes have reached
  the cache file first
- async queue deduplication is by cache `resource_id`, not by visible path or
  remote id
- `flush`, `fsync`, and `release` are intentionally the same trigger at the
  `Drive` layer
- this path assumes a remote file already exists; initial remote creation is a
  separate lifecycle
- resource state and queue-entry state must be reasoned about separately

## Related Docs

- `docs/agent-docs/drive-write.md`
- `docs/agent-docs/drive-truncate.md`
- `docs/agent-docs/drive-flush-fsync-release.md`
- `docs/agent-docs/drive-start-uploading-if-dirty.md`
- `docs/agent-docs/drive-upload-if-dirty.md`
- `docs/agent-docs/drive-upload-with-retry.md`
- `docs/agent-docs/upload-queue-queue-resource.md`
- `docs/agent-docs/upload-queue-start-async-upload-thread.md`
- `docs/agent-docs/upload-queue-poll-upload-queue.md`
- `docs/agent-docs/drive-queue-upload.md`
- `docs/agent-docs/upload-queue-upload-resource.md`
- `docs/agent-docs/drive-upload-resource-by-id.md`
- `docs/agent-docs/drive-upload-resource-with-retry.md`
- `docs/agent-docs/drive-upload.md`

## Source Pointers

- `src/drive.ml`: `write`
- `src/driveFileMutations.ml`: `write`
- `src/drive.ml`: `truncate`
- `src/driveFileMutations.ml`: `truncate`
- `src/drive.ml`: `upload_if_dirty`
- `src/driveUploadDispatch.ml`: `start_uploading_if_dirty`
- `src/driveUploadDispatch.ml`: `upload_if_dirty`
- `src/driveUploadDispatch.ml`: `upload_with_retry`
- `src/driveUploadDispatch.ml`: `queue_upload`
- `src/drive.ml`: `upload_resource_by_id`
- `src/drive.ml`: `upload_resource_with_retry`
- `src/drive.ml`: `upload`
- `src/driveUploads.ml`: concrete upload attempt
- `src/uploadQueue.ml`: async queue polling and worker dispatch
