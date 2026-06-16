# `gdfuseFuse` No-Op Directory Callbacks

## Purpose

This note documents the two registered FUSE directory callbacks in
`bin/gdfuseFuse.ml` that are intentionally no-ops:

- `releasedir`
- `fsyncdir`

They matter because they sit beside active file callbacks like `release`,
`flush`, and `fsync`, but they do not forward into `Drive` at all.

So this is an adapter-level design choice, not a missing implementation inside
`Drive`.

## Where They Are Registered

`start_filesystem` passes them into `Fuse.main` through the native operations
record:

```ocaml
{
  Fuse.default_operations with
  ...
  opendir;
  releasedir;
  fsyncdir;
  ...
}
```

So these callbacks are live parts of the mounted filesystem contract, even
though their bodies are small.

## Implementations

The implementations are:

```ocaml
let releasedir path file_info =
  let flags = GdfuseFuseNative.flags_of_file_info file_info in
  Utils.log_with_header "releasedir %s %s\n%!" path
    (Utils.flags_to_string flags)

let fsyncdir path ds file_info =
  let file_handle = GdfuseFuseNative.file_handle_as_int file_info in
  Utils.log_with_header "fsyncdir %s %b %d\n%!" path ds file_handle
```

That is the entire behavior.

Both callbacks:

- log their arguments
- return `unit`
- perform no `Drive` call
- perform no `with_drive_op` error translation

So they cannot raise repository-specific exceptions such as `File_not_found` or
`Permission_denied` unless logging itself were to fail unexpectedly.

## No `Drive` Counterpart

Unlike many other FUSE operations in this repository, there is no:

- `Drive.releasedir`
- `Drive.fsyncdir`

The adapter stops at the log line.

That is a meaningful architectural distinction:

- file close/sync callbacks have repository logic in `Drive`
- directory close/sync callbacks do not

## Why This Is Plausible In The Current Design

The rest of the codebase makes this design choice coherent.

### 1. Directory Opens Do Not Create Handle State

`Drive.opendir` only validates lookup and returns `None`.

It does not create a persistent directory handle or per-open directory object.

So there is no directory-side state that `releasedir` needs to tear down later.

### 2. Directory Reads Work By Path, Not By Open Handle

`readdir` calls `Drive.read_dir path` directly each time.

The real directory work happens there:

- cache-validity checking
- remote listing when needed
- snapshot replacement

So there is no separate buffered directory stream that `fsyncdir` would need to
flush.

### 3. Upload Triggers Are File-Specific

The write lifecycle is path-based and file-content-based.

`Drive.flush`, `Drive.fsync`, and `Drive.release` matter because dirty file
content can be sitting in memory buffers or waiting in `ToUpload` state.

Directory operations in this repository do not have an analogous local-content
pipeline, so the directory-side siblings can be empty.

## Contrast With File Callbacks

The closest active file-side counterparts are:

- `release`
- `flush`
- `fsync`

Those call into `Drive` and can trigger upload dispatch.

By contrast:

- `releasedir` only logs
- `fsyncdir` only logs

So "close" and "sync" are only operationally meaningful for files in the
current implementation.

See `docs/agent-docs/drive-flush-fsync-release.md` for the file-side behavior.

## Operational Consequences

These no-op callbacks imply:

- no directory-close cleanup runs inside the repository
- no directory-sync persistence step exists
- directory callback errors cannot come from repository exception mapping here
- directory semantics rely on `opendir` and `readdir`, not on close/sync hooks

That is useful context when debugging a user report that mentions:

- closing a directory handle
- syncing a directory
- expecting directory close/sync to flush metadata changes

In the current design, those expectations do not map to any repository-side
logic beyond logging.

## Relationship To `Drive.opendir`

`opendir` is the active directory-open gate:

- it delegates lookup validation to `DriveViews`
- it returns `None`

These no-op callbacks are its post-open siblings at the FUSE boundary.

Together, the current directory-open lifecycle is:

1. `opendir` validates lookup
2. `readdir` performs actual listing work by path
3. `releasedir` and `fsyncdir` do nothing beyond logging

See `docs/agent-docs/drive-opendir.md` for the open-time half.

## What These Callbacks Do Not Do

`releasedir` and `fsyncdir` do not:

- call `Drive`
- translate repository exceptions
- free per-directory repository state
- flush cached directory listings
- force metadata refresh

They are adapter-level log-only hooks.

## Related Docs

- `docs/agent-docs/drive-opendir.md`
- `docs/agent-docs/drive-read-dir.md`
- `docs/agent-docs/drive-flush-fsync-release.md`

## Source Pointers

- `bin/gdfuseFuse.ml`: `releasedir`
- `bin/gdfuseFuse.ml`: `fsyncdir`
- `bin/gdfuseFuse.ml`: `start_filesystem`
