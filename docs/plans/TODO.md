# Testability Refactoring Backlog

This backlog tracks candidates that are not currently active. Keep active
implementation plans as separate files under `docs/plans/`; move completed
plans into `docs/plans/archive/`.

## Candidates

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
