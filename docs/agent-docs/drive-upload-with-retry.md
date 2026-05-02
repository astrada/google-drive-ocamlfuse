# `DriveUploadDispatch.upload_with_retry`

## Purpose

`DriveUploadDispatch.upload_with_retry` is the path-based bridge from the
upload trigger layer into the resource-based upload dispatcher.

It takes a visible path, resolves the current resource through the normal
lookup machinery, and then hands that resolved row to `queue_upload`.

So this helper sits between:

- `upload_if_dirty`, which decides whether to enter the request path at all
- `queue_upload`, which decides sync vs async dispatch and eventually reaches
  the real upload implementation

## Signature

```ocaml
val upload_with_retry : runtime -> string -> unit GapiMonad.SessionM.m
```

The return type matters:

- this is a session/request computation, not a plain synchronous helper
- it must be run through `do_request` or another session executor

That request is executed by the `Drive`-level wrapper through:

```ocaml
do_request upload_request |> ignore
```

## Entire Implementation

The implementation is:

```ocaml
let upload_with_retry runtime path =
  let path_in_cache, trashed = get_path_in_cache path runtime.config in
  get_resource path_in_cache trashed >>= fun resource ->
  queue_upload runtime resource
```

That is the whole control flow.

## High-Level Flow

At a high level, `upload_with_retry` does this:

1. normalize the visible path into `(path_in_cache, trashed)`
2. resolve the current resource with `get_resource`
3. call `queue_upload runtime resource`

There is no extra local branching beyond that.

## Path Normalization

The helper begins with:

```ocaml
let path_in_cache, trashed = get_path_in_cache path runtime.config
```

So, unlike the cheaper local gate in `start_uploading_if_dirty`, this function
uses the full visible-path normalization path again before it proceeds.

That keeps the later upload dispatch aligned with the repository's normal
namespace model, including trash-view path mapping.

## Re-Resolution Through `get_resource`

The most important behavior here is:

```ocaml
get_resource path_in_cache trashed
```

This is deliberately stronger than the earlier `lookup_resource` used by
`start_uploading_if_dirty`.

Key consequences:

- metadata may be refreshed first through `get_metadata`
- stale cache rows may be repaired
- the resource can be reloaded from remote metadata if needed
- missing paths can fail through the normal lookup path

So `upload_with_retry` is the point where the upload flow stops trusting only
the earlier cheap cached-state check and asks for the current authoritative row.

## Why Re-Resolve After The Local Gate

The preceding helper, `start_uploading_if_dirty`, only performs a cheap local
decision and state transition.

It does not return the resource it found, and it does not guarantee that the
earlier cached row is the best row to upload from.

`upload_with_retry` fixes that by resolving again through `get_resource` before
dispatching to `queue_upload`.

This is why the upload flow is split into:

1. cheap local "should we enter upload?" gate
2. full path-based resolution before actual dispatch

## Relationship To `queue_upload`

After resolution, the helper does exactly one thing:

```ocaml
queue_upload runtime resource
```

That means all downstream policy lives in `queue_upload`, including:

- synchronous vs asynchronous upload execution
- flush-before-enqueue behavior in async mode
- the eventual call to `upload_resource_with_retry` in the synchronous branch

So `upload_with_retry` itself does not decide how the upload runs after the
resource has been resolved.

See `docs/agent-docs/drive-queue-upload.md` for the helper-focused view of that
dispatch-policy branch.

See `docs/agent-docs/drive-upload-path.md` for the dispatch policy behind
`queue_upload`.

## Naming Quirk: No Retry Loop Here

The name `upload_with_retry` is easy to over-read.

This helper does not itself contain a retry loop.

It only:

- resolves the resource by path
- delegates to `queue_upload`

The actual retry behavior appears later, downstream:

- synchronous mode reaches `upload_resource_with_retry`
- async mode eventually reaches the same logic through the worker path

So the "with_retry" part of the name describes the broader downstream upload
path it enters, not the body of this helper alone.

## Error Surface

This helper has no local exception handling.

Possible failures therefore come from:

- `get_resource`
- `queue_upload`

At this layer it does not translate or suppress those failures.

When reached from `upload_if_dirty`, the enclosing `do_request` call and the
later FUSE boundary wrapper determine how those exceptions surface.

## Relationship To `upload_if_dirty`

`DriveUploadDispatch.upload_if_dirty` builds the optional request that contains
this session computation, and `Drive.upload_if_dirty` executes it through
`do_request`.

The split is:

- `upload_if_dirty`: decide whether to enter the request path and execute it
- `upload_with_retry`: resolve the path inside the request flow and hand off to
  `queue_upload`

So if upload triggering is happening too often, the first suspect is usually
`start_uploading_if_dirty` or `upload_if_dirty`.

If the wrong resource row is being used for an upload, this helper is one of
the first places to inspect.

See `docs/agent-docs/drive-upload-if-dirty.md` for the caller side.

## What `DriveUploadDispatch.upload_with_retry` Does Not Do

`DriveUploadDispatch.upload_with_retry` does not:

- inspect cached state itself before entering the request path
- flush memory buffers itself
- choose sync vs async policy itself
- perform the actual upload itself
- implement a retry loop in its own body

It only turns a visible path into the current resource row and hands that row
to `queue_upload`.

## Related Docs

- `docs/agent-docs/drive-upload-if-dirty.md`
- `docs/agent-docs/drive-start-uploading-if-dirty.md`
- `docs/agent-docs/drive-upload-path.md`
- `docs/agent-docs/drive-get-resource.md`

## Source Pointers

- `src/driveUploadDispatch.ml`: `upload_with_retry`
- `src/drive.ml`: `get_path_in_cache`
- `src/driveResourceResolver.ml`: `get_resource` policy
- `src/drive.ml`: thin `get_resource` wrapper
- `src/driveUploadDispatch.ml`: `queue_upload`
- `src/drive.ml`: `upload_if_dirty`
