# Testability Refactoring Backlog

This backlog tracks candidates that are not currently active. Keep active
implementation plans as separate files under `docs/plans/`; move completed
plans into `docs/plans/archive/`.

## Candidates

### Upload Worker Bridge

Suggested module: `DriveUploadWorkerBridge`.

Extract the async upload bridge from `Drive`, including:

- `upload_resource_with_retry`
- `upload_resource_by_id`

This would isolate the behavior that reloads cached resources by ID, applies
upload retry policy, and reports upload failures from the background worker
path. It is a medium-sized candidate that should be coordinated with the request
handling boundary if both are active.
