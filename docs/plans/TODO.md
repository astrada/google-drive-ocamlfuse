# Testability Refactoring Backlog

This backlog tracks candidates that are not currently active. Keep active
implementation plans as separate files under `docs/plans/`; move completed
plans into `docs/plans/archive/`.

## Candidates

### Filesystem Stats

Suggested module: `DriveFilesystemStats`.

Extract `statfs` and its quota/block calculation helpers from `Drive`.

This would put the quota, team-drive, and block-size behavior behind a small
testable boundary. The behavior is user-visible through FUSE filesystem stats
and depends on configuration such as team-drive mode.

### Request Handling And Retry

Suggested module: `DriveRequestHandling`.

Extract default request exception handling and retry wrappers from `Drive`,
including:

- `match_service_error`
- `handle_default_exceptions`
- `try_with_default`
- `with_retry_default`

This boundary would centralize how service errors are translated into Drive
exceptions and how retryable operations are wrapped. It is higher impact than
the smaller pure extractions because it affects many entrypoints, but it would
give the exception policy direct unit coverage.

### Runtime Service Startup

Suggested module: `DriveRuntimeServices`.

Extract the startup orchestration currently in `init_filesystem`, including
memory-cache flushing, async upload queue startup, and background folder
fetching.

The current behavior coordinates several long-running services. A dedicated
module with injectable ports would make startup branches easier to test without
starting real background work.

### Streaming And Buffering Adapters

Suggested module: `DriveStreaming`.

Extract Drive media download and streaming helpers from `Drive`, including:

- `download_media`
- `stream_resource`
- `start_buffer_eviction_thread`
- `stream_resource_to_memory_buffer`
- `stream_resource_to_read_ahead_buffers`

This is a larger candidate because it touches memory-buffer lifecycle,
read-ahead buffering, cache paths, and Drive download calls. It should likely be
planned after the smaller boundaries above are complete.

### Upload Worker Bridge

Suggested module: `DriveUploadWorkerBridge`.

Extract the async upload bridge from `Drive`, including:

- `upload_resource_with_retry`
- `upload_resource_by_id`

This would isolate the behavior that reloads cached resources by ID, applies
upload retry policy, and reports upload failures from the background worker
path. It is a medium-sized candidate that should be coordinated with the request
handling boundary if both are active.
