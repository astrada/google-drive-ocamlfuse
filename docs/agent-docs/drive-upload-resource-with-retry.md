# `Drive.upload_resource_with_retry`

## Purpose

`Drive.upload_resource_with_retry` is the narrow wrapper immediately in front of
the real network upload operation.

It does not resolve paths, choose sync-vs-async policy, or own queue behavior.
Its responsibility is narrower:

- flush buffered local writes to disk
- run one concrete `upload` attempt
- normalize failures through `try_with_default`
- retry only temporary failures through `with_retry`

So this helper is the common execution wrapper shared by:

- the direct synchronous branch from `queue_upload`
- the async worker path after `upload_resource_by_id` reloads the resource row

## Signature

```ocaml
val upload_resource_with_retry :
  CacheData.Resource.t -> unit GapiMonad.SessionM.m
```

The input is already a resolved cached resource row.

The result is still a session computation, so callers either continue inside the
current request flow or execute it through `do_request`.

## Upstream Callers

This helper is reached from two places:

- `queue_upload` in direct synchronous mode
- `upload_resource_by_id` after the async worker reloads the current row by
  cache id

The details of those upstream layers live in:

- `docs/agent-docs/drive-queue-upload.md`
- `docs/agent-docs/drive-upload-resource-by-id.md`

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
3. translate request/service failures through `try_with_default`
4. let `with_retry` retry only `Utils.Temporary_error`

Everything else belongs to the helpers it calls.

## Step 1: Flush Memory Buffers

The first line is:

```ocaml
flush_memory_buffers resource
```

This is the last common pre-upload guard before the code reads the on-disk cache
file.

It matters even when an earlier layer may already have flushed:

- the direct path benefits from a final "bytes really reached disk" check
- the async worker path must not trust stale disk content after queue delay

So this helper owns the final flush-before-upload guarantee.

## Step 2: Run `upload`

The actual network operation is delegated to:

```ocaml
upload r
```

That deeper helper owns:

- MIME/media selection
- `FilesResource.update`
- post-response cache reconciliation

So `upload_resource_with_retry` should be read as a wrapper around `upload`, not
as the place where the request itself is built.

See `docs/agent-docs/drive-upload.md` for the concrete upload attempt.

## Step 3: Error Normalization Through `try_with_default`

The wrapper does not hand `upload` straight to `with_retry`.

Instead it runs:

```ocaml
try_with_default (upload r)
```

That means service/request failures first pass through the repository's default
exception mapping. In practice, this is what turns transient backend/rate-limit
style failures into `Utils.Temporary_error`, which the retry loop can recognize.

Without that normalization step, `with_retry` would not know which failures are
supposed to be retried.

## Step 4: Retry Policy In `with_retry`

The outer helper is:

```ocaml
with_retry (fun r -> try_with_default (upload r)) resource
```

Its behavior is:

- success: return normally
- `Utils.Temporary_error`: retry with exponential backoff until
  `Utils.max_retries`
- any other exception: propagate immediately

If the retry budget is exhausted, `with_retry` raises `IO_error`.

So retry at this layer is intentionally narrow.

## Retry Refresh Behavior

On each `Utils.Temporary_error`, `with_retry` does more than sleep and retry.

It also:

1. refreshes the remote file metadata with `FilesResource.get`
2. rebuilds a cached resource row from that server response
3. writes the refreshed row back into cache
4. retries using that refreshed resource view

So the retry loop is not a blind resend against stale local metadata.

## Retry State Quirk

One implementation detail is worth keeping in mind.

When `with_retry` refreshes the cached row after a temporary failure, it chooses
the new state from the original wrapper input:

- if the original input state was `ToUpload`, the refreshed row keeps
  `ToUpload`
- otherwise the refreshed row is rebuilt with `ToDownload`

So the retry wrapper can deliberately reclassify the refreshed row depending on
the operation context it started with.

## Boundaries

Upstream:

- `queue_upload` decides whether this helper runs now or later
- `upload_resource_by_id` is the worker-side bridge that re-enters
  `do_request`

Downstream:

- `upload` performs the actual `FilesResource.update`

Once execution enters `upload_resource_with_retry`, the direct path and the
async worker path share the same flush, normalization, and retry behavior.

## What `Drive.upload_resource_with_retry` Does Not Do

`Drive.upload_resource_with_retry` does not:

- resolve a visible path
- inspect whether upload should start
- create upload-queue entries
- choose sync-vs-async dispatch
- build the `FilesResource.update` request itself

It only wraps one upload attempt with the shared flush and retry policy.

## Related Docs

- `docs/agent-docs/drive-upload-path.md`
- `docs/agent-docs/drive-queue-upload.md`
- `docs/agent-docs/drive-upload-resource-by-id.md`
- `docs/agent-docs/drive-upload.md`

## Source Pointers

- `src/drive.ml`: `upload_resource_with_retry`
- `src/drive.ml`: `upload`
- `src/drive.ml`: `with_retry`
- `src/drive.ml`: `try_with_default`
- `src/drive.ml`: `upload_resource_by_id`
