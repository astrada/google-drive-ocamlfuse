# `Drive.opendir`

## Purpose

`Drive.opendir` is the directory-open validation path used by the FUSE
`opendir` callback.

It is even thinner than `Drive.fopen`. It does not inspect access mode, it does
not allocate a directory handle, and it does not list children. Its entire job
is to resolve the visible path through the normal `Drive` lookup machinery and
fail if that lookup does not succeed.

So this function is best understood as an existence gate for directory-open
requests, not as a directory-listing or handle-construction step.

The public `Drive.opendir` function in `src/drive.ml` is a thin adapter.
It builds a small runtime from `Context`, runs `DriveViews.Make.opendir`
through `do_request`, then returns `None`.

## Signature

```ocaml
val opendir : string -> 'a -> 'b option
```

The arguments are:

- `path`: visible FUSE path
- `flags`: the FUSE / Unix open flags passed by the adapter

The return value is always `None`.

Like `fopen`, that is an intentional part of the current design: open-time
validation happens, but no persistent handle object is created.

## FUSE Boundary

The production FUSE adapter wires:

```ocaml
let opendir path file_info =
  let flags = GdfuseFuseNative.flags_of_file_info file_info in
  Utils.log_with_header "opendir %s %s\n%!" path (Utils.flags_to_string flags);
  let handle =
    with_drive_op ~label:"opendir" ~param:path (fun () ->
        Drive.opendir path flags)
  in
  GdfuseFuseNative.file_info_update_of_handle handle
```

So repository exceptions raised by `Drive.opendir` are translated at the
boundary into Unix/FUSE errors.

The native FUSE 3 callback returns `Fuse.file_info_update`. Since
`Drive.opendir` currently returns `None`, this is
`Fuse.default_file_info_update`.

The most important visible one here is:

- `File_not_found` -> `ENOENT`

`Drive.opendir` itself does not translate exceptions directly.

## Public Wrapper

The public wrapper is:

```ocaml
let opendir path flags =
  do_request (ViewOps.opendir (drive_view_runtime ()) path) |> ignore;
  None
```

That is the whole public control flow.

So `Drive.opendir` does exactly three things:

1. build the small read-side runtime
2. delegate lookup validation to `DriveViews`
3. return `None`

The core implementation lives in `src/driveViews.ml`. `src/drive.ml` provides
the public wrapper and production ports.

## Path Normalization

Inside `DriveViews`, `opendir` begins with:

```ocaml
let path_in_cache, trashed = get_path_in_cache path config
```

That means it inherits the normal namespace mapping rules before lookup:

- `/.Trash/...` is mapped into `(path, trashed = true)`
- ordinary paths stay in the normal namespace
- special roots still depend on later `get_resource` behavior

So a directory open against `"/.Trash"` or `"/.shared"` goes through the same
path translation model as later `readdir` and `getattr` calls.

## Lookup Is The Only Real Work

After normalization, `DriveViews.opendir` runs:

```ocaml
do_request (get_resource path_in_cache trashed) |> ignore
```

That means the function inherits all of `get_resource`'s behavior:

- metadata may be refreshed first through `get_metadata`
- stale cache rows may be repaired
- virtual roots such as root, trash-root view, `lost+found`, and `.shared`
  can be synthesized
- missing paths fail through the normal lookup error path

See `docs/agent-docs/drive-get-resource.md` for the full resolution contract.

## Flags Are Ignored

Although the signature accepts `flags`, the implementation never inspects them.

So unlike `Drive.fopen`, `opendir` has:

- no read-only vs write-capable distinction
- no filesystem read-only guard
- no per-resource editability check

Directory open is treated as a pure existence/lookup operation.

## No Directory-Kind Verification

One current implementation detail is easy to miss:

`opendir` does not check that the resolved resource is actually a folder.

If `get_resource` successfully resolves an existing non-directory resource,
`opendir` will still return `None` successfully.

So the current code relies on the FUSE caller/kernel to invoke `opendir` only
for paths that are already known to be directories.

This is different from a stricter design that would explicitly raise an
`ENOTDIR`-like error inside `Drive.opendir`.

## No Handle State

After successful lookup, `opendir` simply returns:

```ocaml
None
```

It does not:

- pin the resolved resource
- cache child entries
- establish per-open directory state
- prepare data for `readdir`

Later directory operations still work by path, not by dereferencing an object
created here.

## Relationship To `Drive.read_dir`

`opendir` is not the directory-listing path. `Drive.read_dir` is.

The split is:

- `opendir`: validate that lookup succeeds
- `read_dir`: decide whether the child snapshot is reusable, or rebuild it from
  Drive

So a successful `opendir` does not mean the child listing is already cached or
prepared. `read_dir` still performs the actual directory enumeration and, when
needed, the remote fetch and snapshot replacement work.

See `docs/agent-docs/drive-read-dir.md` for the listing-side behavior.

## Relationship To `Drive.fopen`

`Drive.opendir` and `Drive.fopen` are similar in one way:

- both are validation-only entrypoints
- both return `None`

But they differ in an important policy sense:

- `fopen` interprets flags and enforces write access rules
- `opendir` ignores flags and only checks lookup success

So `opendir` is the directory-side open counterpart to `fopen`, but not a
directory-specific copy of the same logic.

See `docs/agent-docs/drive-fopen.md` for the file-open side.

The later directory-close and directory-sync callbacks are adapter-level
no-ops; see `docs/agent-docs/gdfuse-noop-dir-callbacks.md`.

## What `Drive.opendir` Does Not Do

`Drive.opendir` does not:

- inspect or preserve open flags
- verify that the resource is a folder
- enforce read-only or editability rules
- list children
- return a real directory handle

It only validates that the path resolves through `get_resource`.

## Related Docs

- `docs/agent-docs/drive-fopen.md`
- `docs/agent-docs/drive-read-dir.md`
- `docs/agent-docs/drive-get-resource.md`

## Source Pointers

- `src/drive.ml`: `opendir`
- `src/drive.ml`: `DriveViewPorts`
- `src/driveViews.ml`: `opendir`
- `src/drive.ml`: `drive_view_runtime`
- `test/testDriveViews.ml`
- `bin/gdfuseFuse.ml`: `opendir`
