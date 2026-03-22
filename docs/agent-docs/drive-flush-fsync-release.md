# `Drive.flush`, `Drive.fsync`, And `Drive.release`

## Purpose

`Drive.flush`, `Drive.fsync`, and `Drive.release` are the FUSE-facing file
callbacks that trigger upload dispatch after local mutation.

In this repository they are intentionally thin and intentionally identical at
the `Drive` layer. They do not implement three different sync policies.

Their only `Drive`-level question is:

- is this path still in `ToUpload` state?

If yes, they launch the upload pipeline. If not, they return.

So these functions are best understood as file-callback entrypoints into the
upload lifecycle, not as three distinct persistence primitives.

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

- log the visible path and extra FUSE parameters
- run through the shared `with_drive_op` exception-mapping wrapper
- rely on `handle_exception` in `bin/gdfuseFuse.ml` for Unix/FUSE error mapping

The extra parameters are ignored by the `Drive` implementations themselves.

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

## Shared Drive-Layer Contract

All three wrappers hand the work to:

```ocaml
upload_if_dirty path
```

That downstream helper does two things:

1. ask the cheap local gate whether this path is still uploadable now
2. if yes, enter `do_request (upload_with_retry path)`

The detailed semantics of those helper layers live in:

- `docs/agent-docs/drive-start-uploading-if-dirty.md`
- `docs/agent-docs/drive-upload-if-dirty.md`
- `docs/agent-docs/drive-upload-with-retry.md`

This wrapper note intentionally stops at the callback boundary.

## Why Three Callbacks Share One Implementation

At the repository level, these callbacks all mean:

- a write-capable file-operation boundary has been reached

The code does not try to give them different persistence guarantees at the
`Drive` layer.

Instead, all three are treated as equivalent chances to notice:

- the file was dirtied earlier
- upload dispatch should start if it has not already started

So the distinction between `flush`, `fsync`, and `release` mostly exists at the
FUSE boundary and in logging, not in the `Drive` implementation.

## Relationship To Dirtying Operations

These callbacks do not make a file dirty themselves.

They depend on an earlier path having already set:

- `state = ToUpload`

The main producers are:

- `Drive.write`
- `Drive.truncate`
- one rename-replace path in `Drive.rename`

So the lifecycle split is:

1. local mutation marks the resource dirty
2. one of these callbacks notices that dirty state
3. the downstream upload pipeline handles dispatch and remote update

See `docs/agent-docs/drive-write.md` and
`docs/agent-docs/drive-truncate.md` for the mutation-side half.

## Relationship To Sync Versus Async Upload

These callbacks do not choose sync-vs-async policy themselves.

They only enter the upload pipeline. Later:

- `queue_upload` performs the policy split
- the async queue may return before network upload completes

So a successful `flush`, `fsync`, or `release` does not imply that the remote
file update has already finished when the callback returns.

See `docs/agent-docs/drive-upload-path.md` for the end-to-end overview and
`docs/agent-docs/drive-queue-upload.md` for the policy branch.

## File Callbacks Only

This behavior is specific to file callbacks.

The directory callbacks:

- `releasedir`
- `fsyncdir`

remain adapter-level no-ops in `bin/gdfuseFuse.ml` and do not call into
`Drive`.

See `docs/agent-docs/gdfuse-noop-dir-callbacks.md` for those directory-side
hooks.

## What These Wrappers Do Not Do

`Drive.flush`, `Drive.fsync`, and `Drive.release` do not:

- flush write buffers directly
- inspect file descriptors or open flags
- refresh metadata directly
- decide sync-vs-async upload policy directly
- perform the actual network upload themselves
- guarantee immediate remote completion in async mode

They only hand the visible path to the upload-dispatch gate.

## Related Docs

- `docs/agent-docs/drive-start-uploading-if-dirty.md`
- `docs/agent-docs/drive-upload-if-dirty.md`
- `docs/agent-docs/drive-upload-with-retry.md`
- `docs/agent-docs/drive-upload-path.md`
- `docs/agent-docs/drive-write.md`
- `docs/agent-docs/drive-truncate.md`

## Source Pointers

- `src/drive.ml`: `flush`
- `src/drive.ml`: `fsync`
- `src/drive.ml`: `release`
- `src/drive.ml`: `upload_if_dirty`
- `bin/gdfuseFuse.ml`: `flush`
- `bin/gdfuseFuse.ml`: `fsync`
- `bin/gdfuseFuse.ml`: `release`
- `bin/gdfuseFuse.ml`: `releasedir`
- `bin/gdfuseFuse.ml`: `fsyncdir`
