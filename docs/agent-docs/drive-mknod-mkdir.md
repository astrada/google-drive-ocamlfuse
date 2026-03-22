# `Drive.mknod` And `Drive.mkdir`

## Purpose

`Drive.mknod` and `Drive.mkdir` are the FUSE-facing entrypoints for creating:

- a regular file shell
- a folder

Like the other thin wrapper functions in `Drive`, they contain almost no logic
of their own. Their job is to take the incoming POSIX create request, preserve
the caller-supplied path and mode, and hand the operation to the shared remote
creation helper with the correct folder-vs-non-folder intent.

So these functions are best understood as boundary adapters over
`Drive.create_remote_resource`.

## Signatures And FUSE Boundary

```ocaml
val mknod : string -> int -> unit
val mkdir : string -> int -> unit
```

The production FUSE adapter wires:

```ocaml
let mknod path mode =
  Utils.log_with_header "mknod %s %o\n%!" path mode;
  with_drive_op ~label:"mknod" ~param:path (fun () -> Drive.mknod path mode)

let mkdir path mode =
  Utils.log_with_header "mkdir %s %o\n%!" path mode;
  with_drive_op ~label:"mkdir" ~param:path (fun () -> Drive.mkdir path mode)
```

So both entrypoints:

- log the visible path and requested mode
- run through the shared `with_drive_op` error-translation wrapper
- rely on `handle_exception` in `bin/gdfuseFuse.ml` for Unix/FUSE error mapping

`Drive.mknod` and `Drive.mkdir` themselves do not contain any special error
mapping.

## Entire Implementations

The implementations are:

```ocaml
let mknod path mode = create_remote_resource false path mode
let mkdir path mode = create_remote_resource true path mode
```

That is the whole control flow.

Everything else is delegated to `create_remote_resource`.

## The Only Semantic Difference: `is_folder`

The only hard-coded distinction between the two wrappers is:

- `mknod` passes `is_folder = false`
- `mkdir` passes `is_folder = true`

That small bit changes the downstream creation mode completely.

### `mknod`

With `is_folder = false` and no `link_target`, the shared helper takes the
regular-file branch:

- it creates a normal Drive file
- it chooses the MIME type from filename/config rules
- it stores the provided mode bits in app properties

In this repository, `mknod` is therefore regular-file creation, not special
device creation. There is no separate block-device, char-device, FIFO, or
socket branch behind this entrypoint.

### `mkdir`

With `is_folder = true`, the helper takes the folder branch:

- it forces the folder MIME type
- it stores the provided mode bits in app properties
- it creates a Drive folder directly with one `FilesResource.create` request

So the wrapper does not need any separate folder logic beyond setting that
boolean.

## The `mode` Argument Is Preserved, Not Interpreted Locally

Both wrappers pass the caller-supplied `mode` straight through.

They do not:

- mask it
- rewrite it
- validate it locally

The creation helper later serializes those mode bits into Drive app properties.

That is a create-time persistence rule, not the full visible permission story.
Later `getattr` handling still applies the normal masking logic when it turns
the cached row back into `st_perm`.

See `docs/agent-docs/drive-get-attr.md` for the read-side stat synthesis.

## Immediate Effects After Success

Because both wrappers go through `create_remote_resource`, successful creation
has the same immediate postconditions as the symlink path:

1. the remote object is created first through `FilesResource.create`
2. any cached `NotFound` tombstone for that path is deleted
3. a new cached resource row is inserted in state `Synchronized`

For `mknod`, that means a remote empty file shell exists immediately, but there
is still no uploaded content beyond that empty object.

For `mkdir`, there is no later upload phase at all. The folder is fully created
by the single remote create request.

## What These Wrappers Delegate

`Drive.mknod` and `Drive.mkdir` do not perform any of the following directly:

- path normalization
- parent lookup
- read-only enforcement
- trash-namespace rejection
- `/lost+found` rejection
- MIME-type selection
- cache insertion

All of that happens inside `create_remote_resource`.

## Relationship To `Drive.symlink`

These wrappers are siblings of `Drive.symlink`.

All three call the same shared helper, but with different fixed arguments:

- `mknod`: non-folder, no `link_target`, caller mode preserved
- `mkdir`: folder, no `link_target`, caller mode preserved
- `symlink`: non-folder, `link_target` present, fixed mode `0o777`

So if creation behavior differs across files, folders, and symlinks, the first
thing to inspect is which fixed wrapper arguments reach
`create_remote_resource`.

See `docs/agent-docs/drive-symlink.md` for the symlink-specific variant.

## What They Do Not Do

`Drive.mknod` and `Drive.mkdir` do not:

- create a local content cache file
- enqueue upload work
- write file bytes
- perform duplicate-name reconciliation

They stop at remote object creation and cache seeding.

## Related Docs

- `docs/agent-docs/drive-create-remote-resource.md`
- `docs/agent-docs/drive-symlink.md`
- `docs/agent-docs/drive-get-attr.md`

## Source Pointers

- `src/drive.ml`: `mknod`
- `src/drive.ml`: `mkdir`
- `src/drive.ml`: `create_remote_resource`
- `bin/gdfuseFuse.ml`: `mknod`
- `bin/gdfuseFuse.ml`: `mkdir`
