# `Drive.upload_resource_with_retry`

## Purpose

`Drive.upload_resource_with_retry` is the narrow wrapper that sits immediately
in front of the real network upload operation.

It is the point where the two upload entry paths converge:

- the synchronous branch from `queue_upload`
- the async worker path from `upload_resource_by_id`

Its job is not to resolve a path or choose sync vs async policy. Instead, it:

- makes sure buffered writes are on disk
- runs `upload`
- retries transient failures through the shared retry helper

## Signature

```ocaml
val upload_resource_with_retry :
  CacheData.Resource.t -> unit GapiMonad.SessionM.m
```

The input is already a resolved cached resource row.

The result is still a session computation, so callers either continue inside the
current request flow or execute it through `do_request`.

## Entry Paths

This helper is reached from two main places.

### 1. Direct Synchronous Dispatch

`queue_upload` uses it directly when:

```ocaml
config.Config.async_upload_queue = false
```

### 2. Async Worker Path

When async upload is enabled, the worker path is:

1. `UploadQueue` selects a queued entry
2. worker calls `Drive.upload_resource_by_id resource_id`
3. `upload_resource_by_id` loads the current resource row by cache id
4. it runs:

```ocaml
do_request (upload_resource_with_retry r) |> ignore
```

So this wrapper is the common convergence point before the real upload logic.

## Entire Implementation

The implementation is:

```ocaml
let upload_resource_with_retry resource =
  flush_memory_buffers resource;
  with_retry (fun r -> try_with_default (upload r)) resource
```

That is the whole control flow.

## High-Level Flow

At a high level, the wrapper does this:

1. flush buffered local writes for the resource
2. call `upload`
3. normalize service/request failures through `try_with_default`
4. retry only when the result becomes `Utils.Temporary_error`

Everything else belongs to the helpers it calls.

## Step 1: Unconditional Buffer Flush

The first line is:

```ocaml
flush_memory_buffers resource
```

This happens unconditionally, even if the caller already flushed earlier.

That is deliberate because this wrapper is the last common point before the
upload reads from the on-disk cache file.

So it protects both:

- the direct sync branch
- the async worker branch

from uploading stale disk content when writes were buffered in memory.

## Step 2: Run `upload`

The actual network operation is delegated to:

```ocaml
upload r
```

That deeper helper handles the real upload work:

- reading the cache file
- computing MIME type
- setting `modifiedTime`
- calling `FilesResource.update`
- reconciling the returned metadata into cache

So `upload_resource_with_retry` should be read as a wrapper around `upload`, not
as the place where the upload request itself is built.

See `docs/agent-docs/drive-upload-path.md` for the actual `upload` behavior.

## Step 3: Error Normalization Through `try_with_default`

The wrapper does not hand `upload` straight to `with_retry`.

Instead it runs:

```ocaml
try_with_default (upload r)
```

That means Drive/service/request exceptions first pass through
`handle_default_exceptions`, which can translate them into repository-level
exceptions such as:

- `Permission_denied`
- `File_not_found`
- `Invalid_operation`
- `IO_error`
- `Utils.Temporary_error`

This matters because `with_retry` only retries one class of failure:

- `Utils.Temporary_error`

So `try_with_default` is what turns transient backend/rate-limit style failures
into something the retry loop can recognize.

## Step 4: Retry Policy In `with_retry`

The outer helper is:

```ocaml
with_retry (fun r -> try_with_default (upload r)) resource
```

Its behavior is:

- success: return normally
- `Utils.Temporary_error`:
  retry with exponential backoff until `Utils.max_retries`
- any other exception:
  propagate immediately

If the retry budget is exhausted, `with_retry` raises `IO_error`.

So retry is intentionally narrow and only applies to failures that have already
been classified as temporary.

## Retry Refresh Behavior

On each `Utils.Temporary_error`, `with_retry` does more than sleep and retry.

It also:

1. waits with exponential backoff
2. refreshes the current remote file metadata with `FilesResource.get`
3. rebuilds a cached resource row from that file
4. writes the refreshed row back into cache
5. retries using that refreshed row

So the retry loop re-enters `upload` with a refreshed resource view rather than
blindly retrying against the original row forever.

## Retry State Quirk

One implementation detail is worth documenting literally.

When `with_retry` refreshes the cached row after a temporary failure, it chooses
the new state from the original wrapper input:

- if the original input state was `ToUpload`, the refreshed row keeps
  `ToUpload`
- otherwise the refreshed row is rebuilt with `ToDownload`

That means the retry wrapper is not purely "leave state alone and retry". It
can deliberately reclassify the refreshed row based on the original call
context.

This is a useful maintenance point if upload-retry state behavior ever becomes
surprising.

## Relationship To `upload_resource_by_id`

The async worker path reaches this helper through:

```ocaml
do_request (upload_resource_with_retry r) |> ignore
```

So if async uploads and direct synchronous uploads behave differently, compare:

- what resource row each path passes in
- whether the row state differs before this wrapper starts

Once both paths enter `upload_resource_with_retry`, they share the same flush,
normalization, and retry behavior.

## Relationship To `queue_upload`

`queue_upload` is the upstream dispatcher that decides whether this helper runs
now or later.

The split is:

- `queue_upload`: choose sync vs async mode
- `upload_resource_with_retry`: run the common flush + normalize + retry wrapper

See `docs/agent-docs/drive-queue-upload.md` for that dispatch branch.

## What `Drive.upload_resource_with_retry` Does Not Do

`Drive.upload_resource_with_retry` does not:

- resolve a visible path
- decide whether upload should start
- create upload-queue entries
- implement the actual `FilesResource.update` request itself

It only wraps the real upload operation with the shared flush and retry policy.

## Related Docs

- `docs/agent-docs/drive-queue-upload.md`
- `docs/agent-docs/drive-upload-path.md`
- `docs/agent-docs/drive-upload-with-retry.md`
- `docs/agent-docs/drive-init-filesystem.md`

## Source Pointers

- `src/drive.ml`: `upload_resource_with_retry`
- `src/drive.ml`: `upload`
- `src/drive.ml`: `with_retry`
- `src/drive.ml`: `try_with_default`
- `src/drive.ml`: `upload_resource_by_id`
