# `gdfuseFuse` FUSE Boundary Helpers

## Purpose

This note documents the small but central adapter layer at the top of
`bin/gdfuseFuse.ml`:

- `handle_exception`
- `with_drive_op`
- `drive_path_op`

These helpers are the main bridge between:

- repository-level exceptions and return values in `Drive`
- Unix/FUSE-facing callback behavior

Most callback functions in `bin/gdfuseFuse.ml` stay thin precisely because this
layer centralizes logging, exception translation, and the common path-op shape.

Native FUSE 3 value conversion lives in `src/gdfuseFuseNative.ml`. That module
keeps pure boundary conversions testable outside a mounted filesystem:

- `Fuse.file_info` handles and flags are converted to the integer handles and
  flag lists still expected by `Drive`
- `Fuse.timestamp` values are converted to float timestamps for `Drive.utime`
- unsupported nonzero `rename` flags are rejected before calling `Drive.rename`
- `readdir` names are converted to `Fuse.dir_entry` records

## The Three Helpers

At a high level, the helpers split responsibilities like this:

1. `handle_exception` maps repository exceptions to `Unix.Unix_error`
2. `with_drive_op` runs one callback and routes failures through
   `handle_exception`
3. `drive_path_op` is the convenience wrapper for simple path-only operations

So the control flow for a typical callback is:

```text
FUSE callback -> drive_path_op / with_drive_op -> Drive.* -> exception mapping
```

## `handle_exception`

`handle_exception e label param` is the actual translation table.

Its mappings are:

- `Drive.File_not_found` -> `Unix.ENOENT`
- `Drive.Permission_denied` -> `Unix.EACCES`
- `Drive.Directory_not_empty` -> `Unix.ENOTEMPTY`
- `Drive.IO_error` -> `Unix.EIO`
- `Drive.No_attribute` -> `Unix.EUNKNOWNERR 61`
- `Drive.Existing_attribute` -> `Unix.EEXIST`
- `Drive.Invalid_operation` -> `Unix.EINVAL`

Each translated exception is re-raised as:

```ocaml
Unix.Unix_error (errno, label, param)
```

So the chosen `label` and `param` become part of the visible Unix error payload.

## Logging Behavior Inside `handle_exception`

Not every mapping branch logs the same way.

### Explicit Repository Error Logs

These branches emit a short structured log message before raising:

- `File_not_found`
- `Permission_denied`
- `Directory_not_empty`
- `IO_error`

### Silent Direct Translations

These branches translate without their own extra log line:

- `No_attribute`
- `Existing_attribute`
- `Invalid_operation`

So xattr and invalid-operation failures rely more heavily on outer logging or
caller context than the file-not-found and permission branches do.

### Pass-Through Cases

Two branches deliberately avoid repository-to-Unix remapping:

- `Unix.Unix_error _ as e`:
  log with `Utils.log_exception e`, then re-raise unchanged
- `Oauth2.InvalidRefreshToken as e`:
  re-raise unchanged

The second case is important because auth-refresh failure is treated as a
higher-level control-flow problem, not as an ordinary filesystem errno.

### Catch-All

Any other exception:

1. is logged with `Utils.log_exception`
2. is remapped to `Unix.Unix_error (Unix.EIO, label, param)`

So unexpected failures collapse to a generic I/O error at the FUSE boundary.

## `with_drive_op`

`with_drive_op` is the general callback runner:

```ocaml
let with_drive_op ?(log_exception = false) ~label ~param f =
  try f ()
  with e ->
    if log_exception then Utils.log_exception e;
    handle_exception e label param
```

Its job is simple:

- run `f`
- on failure, optionally log the raw exception first
- hand the exception to `handle_exception`

This is the common wrapper used by most FUSE callbacks in the file.

## Meaning Of `label` And `param`

The `label`/`param` pair is operationally important because it becomes the
second and third fields of the eventual `Unix.Unix_error`.

That means this helper layer is not just choosing an errno. It is also choosing
which operation name and which string argument surface at the boundary.

Examples:

- `with_drive_op ~label:"write" ~param:path ...`
- `with_drive_op ~label:"chmod" ~param:path ...`
- `with_drive_op ~label:"symlink" ~param:target ...`

That last example is a useful quirk: the `symlink` adapter uses the link target
as `param`, not the link path.

## `log_exception = true`

Most callbacks use the default `log_exception = false`.

The notable exceptions are:

- `init_filesystem`
- `statfs`

Those callbacks ask `with_drive_op` to log the raw exception before it reaches
`handle_exception`.

That is useful for higher-level startup/runtime failures where the generic
errno translation can otherwise lose too much detail.

## `drive_path_op`

`drive_path_op` is the convenience wrapper for callbacks that:

- take exactly one visible path argument
- want the path used as the Unix error `param`
- do not need extra per-callback argument logging first

Its implementation is:

```ocaml
let drive_path_op ~name ?(label = name) path op =
  Utils.log_with_header "%s %s\n%!" name path;
  with_drive_op ~label ~param:path (fun () -> op path)
```

So it contributes two things:

1. a standard `"name path"` log line
2. the standard `with_drive_op` boundary translation with `param = path`

## Where `drive_path_op` Is Used

The current simple path-only users are:

- `getattr` via `drive_path_op ~name:"getattr" ~label:"stat"`
- `unlink`
- `rmdir`
- `listxattr`
- `readlink`

That means these callbacks all share the same adapter pattern:

- one path argument
- one standard pre-call log line
- one direct `Drive.* path` invocation
- one common exception-translation path

## Why Many Other Callbacks Skip `drive_path_op`

Many callbacks go straight to `with_drive_op` instead because they need one of
these variations:

- extra arguments in the log line, such as `write`, `truncate`, or `utime`
- a different chosen `param`, such as `symlink`
- `log_exception = true`, such as `init_filesystem` and `statfs`
- post-call handling, such as `read` returning a byte count or `readdir`
  prepending `"."` and `".."`

So `drive_path_op` is intentionally narrow. It covers only the trivial
path-only shape.

## Relationship To `Drive`

This adapter layer does not implement filesystem semantics itself.

It does not decide:

- whether a path is valid
- whether a mutation is allowed
- how cache state changes
- how Drive API calls are made

That all lives below, primarily in `Drive`.

What this layer decides is:

- how callbacks are logged at the boundary
- how repository exceptions become Unix/FUSE errors
- which callbacks get the simple path-wrapper treatment

## Relationship To The No-Op Directory Callbacks

The contrast with `releasedir` and `fsyncdir` is useful.

Those two callbacks:

- log directly
- do not call `with_drive_op`
- do not call `Drive`

So they bypass this boundary layer almost entirely.

See `docs/agent-docs/gdfuse-noop-dir-callbacks.md` for that special-case pair.

## Maintenance Notes

### Add New Repository Exceptions Here

If a new `Drive` exception is intended to become a specific errno, it needs a
new `handle_exception` branch. Otherwise it will fall into the catch-all `EIO`
path.

### Be Careful With `label` And `param`

Changing those strings changes the visible `Unix.Unix_error` payload and the
associated logs. That can affect debugging, tests, and user-facing error
messages.

### `log_exception` Changes Logging Volume

Enabling `log_exception = true` is useful for diagnosing failures, but it also
adds extra raw exception logging before translation. That changes the log
surface even when the eventual errno mapping stays the same.

## Related Docs

- `docs/agent-docs/architecture.md`
- `docs/agent-docs/gdfuse-noop-dir-callbacks.md`
- `docs/agent-docs/application-flow.md`

## Source Pointers

- `bin/gdfuseFuse.ml`: `handle_exception`
- `bin/gdfuseFuse.ml`: `with_drive_op`
- `bin/gdfuseFuse.ml`: `drive_path_op`
- `bin/gdfuseFuse.ml`: `start_filesystem`
