# `Drive.upload_if_dirty`

## Purpose

`Drive.upload_if_dirty` is the narrow bridge between:

- the local cached-state gate in `start_uploading_if_dirty`
- the request/session-based upload path in `upload_with_retry`

It does not decide whether a file is dirty in a rich sense, and it does not
implement upload logic itself. Its job is simpler:

- if the local gate says "start now", enter the request machinery
- otherwise do nothing

So this helper is the immediate dispatch bridge behind `flush`, `fsync`, and
`release`.

## Signature

```ocaml
val upload_if_dirty : string -> unit
```

It takes a visible path and returns `unit`.

That return type is important: the function does not report whether upload
actually happened or completed. It only triggers the next stage when
appropriate.

## Entire Implementation

The implementation is:

```ocaml
let upload_if_dirty path =
  if start_uploading_if_dirty path then
    do_request (upload_with_retry path) |> ignore
```

That is the whole control flow.

## High-Level Flow

At a high level, `upload_if_dirty` does this:

1. call `start_uploading_if_dirty path`
2. if that returns `false`, return immediately
3. if that returns `true`, run `do_request (upload_with_retry path)`
4. discard the request result and return `unit`

So the helper is a pure branch point between:

- a no-op path
- a request-entering path

## Relationship To `start_uploading_if_dirty`

The first phase is:

```ocaml
start_uploading_if_dirty path
```

That helper performs the cheap local check:

- normalize path
- look up the cached row
- require exact state `ToUpload`
- flip the row to `Uploading`

If that phase returns `false`, `upload_if_dirty` stops immediately.

That means in the `false` branch, this helper does not:

- call `do_request`
- call `get_resource`
- refresh metadata
- touch the network

See `docs/agent-docs/drive-start-uploading-if-dirty.md` for the gate itself.

## The Meaning Of The `false` Branch

The `false` branch is intentionally cheap.

It covers cases such as:

- the path is not in cache
- the cached state is already `Uploading`
- the cached state is already `Synchronized`
- the cached row is in some non-upload state like `ToDownload`

So repeated `flush` / `fsync` / `release` calls can collapse into a quick
no-op once the resource has already left `ToUpload`.

## The Meaning Of The `true` Branch

If the local gate returns `true`, `upload_if_dirty` runs:

```ocaml
do_request (upload_with_retry path) |> ignore
```

This is the important boundary crossing in the helper.

It means:

- the cheap local decision has already been made
- now the code enters the authenticated request/session machinery
- the later upload path can do full path resolution and Drive-side work

So `upload_if_dirty` is the handoff point from local cache state into the
repository's normal request execution model.

## Why `do_request` Matters

`upload_with_retry path` has type:

```ocaml
string -> unit GapiMonad.SessionM.m
```

So it cannot run by itself from this plain synchronous helper.

`do_request` is what turns that session-based computation into an executed
operation in the current thread.

That is the main reason `upload_if_dirty` exists as a separate helper instead
of having `flush` / `fsync` / `release` call `start_uploading_if_dirty`
directly and stop there.

## Relationship To `upload_with_retry`

`upload_if_dirty` does not resolve the resource itself.

That work begins one step later in:

```ocaml
upload_with_retry path
```

which:

1. normalizes the visible path again
2. resolves the current resource with `get_resource`
3. calls `queue_upload resource`

So `upload_if_dirty` should be read as:

- "enter the upload request path now"

not as:

- "perform the full upload logic here"

See `docs/agent-docs/drive-upload-path.md` for the later phases.

## Synchronous Entry, Config-Dependent Completion

`upload_if_dirty` always enters the request path synchronously in the current
call stack when the gate returns `true`.

But what happens after that depends on `queue_upload` policy:

- with `async_upload_queue = false`, the upload may run synchronously all the
  way through
- with `async_upload_queue = true`, the request path may stop after enqueueing
  async work

So `upload_if_dirty` is synchronous as a dispatcher, but not necessarily as a
"wait until upload completed" primitive.

## Relationship To File Callbacks

The direct callers are:

- `Drive.flush`
- `Drive.fsync`
- `Drive.release`

Those wrappers are intentionally identical at the `Drive` layer:

```ocaml
let flush path _ = upload_if_dirty path
let fsync path _ _ = upload_if_dirty path
let release path _ _ = upload_if_dirty path
```

So if upload triggering behavior changes for those callbacks, this helper is one
of the first places to inspect.

## Error Surface

`upload_if_dirty` has no local exception handling of its own.

That means:

- in the `false` branch, it usually returns quietly
- in the `true` branch, any exception comes from the later request/upload path

At the FUSE boundary, those exceptions are then translated by the
`gdfuseFuse.ml` wrapper used by `flush`, `fsync`, and `release`.

See `docs/agent-docs/gdfuse-fuse-boundary.md` for that translation layer.

## What `Drive.upload_if_dirty` Does Not Do

`Drive.upload_if_dirty` does not:

- inspect cached state itself beyond delegating to `start_uploading_if_dirty`
- resolve the resource itself
- choose sync vs async upload policy itself
- flush memory buffers itself
- perform the actual upload itself

It only bridges the local state gate into the request/session upload path.

## Related Docs

- `docs/agent-docs/drive-start-uploading-if-dirty.md`
- `docs/agent-docs/drive-flush-fsync-release.md`
- `docs/agent-docs/drive-upload-path.md`
- `docs/agent-docs/gdfuse-fuse-boundary.md`

## Source Pointers

- `src/drive.ml`: `upload_if_dirty`
- `src/drive.ml`: `start_uploading_if_dirty`
- `src/drive.ml`: `upload_with_retry`
- `src/drive.ml`: `flush`
- `src/drive.ml`: `fsync`
- `src/drive.ml`: `release`
