# `Drive.flush`, `Drive.fsync`, And `Drive.release`

## Purpose

`Drive.flush`, `Drive.fsync`, and `Drive.release` are the FUSE-facing file
callbacks that trigger the upload lifecycle after local file mutation.

In this repository they are intentionally thin and intentionally identical at
the `Drive` layer. They do not implement three different sync policies.
Instead, all three ask the same question:

- is this path still marked `ToUpload`?

If yes, they start the upload dispatch path. If not, they do nothing.

So these functions are best understood as upload-trigger entrypoints, not as
distinct data-flush primitives.

## Signatures And FUSE Boundary

```ocaml
val flush : string -> 'a -> unit
val fsync : string -> 'a -> 'b -> unit
val release : string -> 'a -> 'b -> unit
```

The production FUSE adapter wires:

```ocaml
let release path flags hnd =
  Utils.log_with_header "release %s %s\n%!" path (Utils.flags_to_string flags);
  with_drive_op ~label:"release" ~param:path (fun () ->
      Drive.release path flags hnd)

let flush path file_descr =
  Utils.log_with_header "flush %s %d\n%!" path file_descr;
  with_drive_op ~label:"flush" ~param:path (fun () ->
      Drive.flush path file_descr)

let fsync path ds file_descr =
  Utils.log_with_header "fsync %s %b %d\n%!" path ds file_descr;
  with_drive_op ~label:"fsync" ~param:path (fun () ->
      Drive.fsync path ds file_descr)
```

So all three:

- log the visible path plus the extra FUSE parameters
- run through the shared `with_drive_op` exception-mapping wrapper
- surface repository errors as Unix/FUSE errors through `handle_exception`

The extra parameters are not used by the `Drive` implementations themselves.

## Entire Implementations

The `Drive` implementations are:

```ocaml
let flush path file_descr = upload_if_dirty path
let fsync path ds file_descr = upload_if_dirty path
let release path flags hnd = upload_if_dirty path
```

That is the entire control flow.

At the `Drive` layer:

- `flush` ignores `file_descr`
- `fsync` ignores `ds` and `file_descr`
- `release` ignores `flags` and `hnd`

The only semantic input is the path.

## The Shared Helper: `upload_if_dirty`

All three wrappers delegate to:

```ocaml
let upload_if_dirty path =
  if start_uploading_if_dirty path then
    do_request (upload_with_retry path) |> ignore
```

So the work is split into two phases:

1. a cheap local gate: `start_uploading_if_dirty`
2. the real request/session dispatch: `upload_with_retry`

## Phase 1: Cheap Dirty-State Gate

`start_uploading_if_dirty path` is the key idempotency check.

It:

1. normalizes the path with `get_path_in_cache`
2. looks up the cached row with `lookup_resource`
3. checks whether the current state is exactly `ToUpload`

If the row is not found, or is in any other state, it returns `false`.

If the row is `ToUpload`, it immediately flips the cached state to
`Uploading` and returns `true`.

That means these callbacks only schedule upload work once per
`ToUpload -> Uploading` transition.

Repeated `flush`, `fsync`, or `release` calls on the same dirty file do not
keep re-entering the upload path after the first state flip.

## Phase 2: Path-Based Upload Dispatch

If the gate returns `true`, `upload_if_dirty` runs:

```ocaml
do_request (upload_with_retry path) |> ignore
```

`upload_with_retry path` then:

1. normalizes the path again
2. resolves the current resource with `get_resource`
3. calls `queue_upload resource`

That second-stage `get_resource` matters because the actual upload dispatch
should use the latest cached row, not only the row seen by the cheap local
gate.

## Why Three Callbacks Share One Implementation

At the repository level, these three callbacks all mean:

- "a write-capable file operation boundary has been reached"

The code does not try to give them different persistence guarantees.

Instead it treats them as equivalent chances to notice:

- this file was dirtied earlier by `write`, `truncate`, or a rename-replace
  path
- the upload path should now start if it has not already started

So the distinction between "flush", "fsync", and "release" mostly exists at the
FUSE boundary and in logging, not in the `Drive` implementation.

## Relationship To The Dirtying Operations

These callbacks do not make a file dirty themselves.

They depend on an earlier path having already set:

- `state = ToUpload`

The main producers of that state are:

- `Drive.write`
- `Drive.truncate`
- one `Drive.rename` replacement path

So the upload lifecycle is intentionally split:

1. local mutation marks the resource dirty
2. one of these callbacks notices the dirty state
3. the upload path actually dispatches the remote update

See `docs/agent-docs/drive-write.md` and
`docs/agent-docs/drive-truncate.md` for the mutation-side half.

## Relationship To Sync Versus Async Upload

These callbacks do not decide whether upload runs synchronously or through the
background queue.

They only reach `queue_upload` through `upload_with_retry`.

`queue_upload` then chooses:

- direct upload, if `async_upload_queue = false`
- queue insertion, if `async_upload_queue = true`

So a successful `flush`, `fsync`, or `release` does not necessarily mean the
network upload has already completed when the callback returns.

See `docs/agent-docs/drive-upload-path.md` for the full dispatch policy and
async worker flow.

## File Callbacks Only

It is easy to overgeneralize this behavior to directories, but the FUSE adapter
does not do that.

The directory callbacks:

- `releasedir`
- `fsyncdir`

only log in `bin/gdfuseFuse.ml` and do not call into `Drive`.

So this upload-trigger behavior is specific to file callbacks.

See `docs/agent-docs/gdfuse-noop-dir-callbacks.md` for the adapter-side note on
those two no-op directory callbacks.

## What These Wrappers Do Not Do

`Drive.flush`, `Drive.fsync`, and `Drive.release` do not:

- flush memory buffers directly
- inspect file descriptors or open flags
- force a metadata refresh before the cheap dirty-state check
- perform the actual upload logic themselves
- guarantee an immediate network write in async mode

They only gate and launch the upload path when the cached resource is still
marked `ToUpload`.

## Related Docs

- `docs/agent-docs/drive-upload-path.md`
- `docs/agent-docs/drive-write.md`
- `docs/agent-docs/drive-truncate.md`

## Source Pointers

- `src/drive.ml`: `start_uploading_if_dirty`
- `src/drive.ml`: `upload_if_dirty`
- `src/drive.ml`: `flush`
- `src/drive.ml`: `fsync`
- `src/drive.ml`: `release`
- `bin/gdfuseFuse.ml`: `flush`
- `bin/gdfuseFuse.ml`: `fsync`
- `bin/gdfuseFuse.ml`: `release`
- `bin/gdfuseFuse.ml`: `releasedir`
- `bin/gdfuseFuse.ml`: `fsyncdir`
