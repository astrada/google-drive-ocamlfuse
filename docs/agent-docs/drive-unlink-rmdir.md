# `Drive.unlink` And `Drive.rmdir`

## Purpose

`Drive.unlink` and `Drive.rmdir` are the FUSE-facing delete entrypoints.

Like `Drive.symlink`, they are intentionally thin. They do not implement
trashing, permanent deletion, namespace policy, or folder emptiness checks
themselves. Their only job is to expose two POSIX delete operations and attach
the correct folder-vs-non-folder intent before delegating to the shared delete
policy layer.

That means these functions are best understood as boundary adapters over
`Drive.delete_remote_resource`.

## Signatures And FUSE Boundary

```ocaml
val unlink : string -> unit
val rmdir : string -> unit
```

The production FUSE adapter wires:

```ocaml
let unlink path = drive_path_op ~name:"unlink" path Drive.unlink
let rmdir path = drive_path_op ~name:"rmdir" path Drive.rmdir
```

So both entrypoints use the shared `drive_path_op` boundary wrapper in
`bin/gdfuseFuse.ml`:

- it logs the operation name and path
- it runs the `Drive` function
- it maps repository exceptions into Unix/FUSE errors through
  `handle_exception`

For delete flows, the most important visible mappings are:

- `File_not_found` -> `ENOENT`
- `Permission_denied` -> `EACCES`
- `Directory_not_empty` -> `ENOTEMPTY`

## Entire Implementations

The implementations are:

```ocaml
let unlink path = delete_remote_resource false path
let rmdir path = delete_remote_resource true path
```

That is the entire control flow.

Everything interesting happens below `delete_remote_resource`.

## The Only Semantic Difference: `is_folder`

The only thing these wrappers contribute is the fixed `is_folder` bit:

- `unlink` passes `false`
- `rmdir` passes `true`

That bit is consumed later by the lower delete helpers.

It determines whether the delete flow should:

- run `check_if_empty`
- treat the request as a directory deletion
- clean up cached descendants after success

So the distinction between the two public entrypoints is small in code but
important in behavior.

## What They Delegate To `delete_remote_resource`

After the wrapper chooses the `is_folder` bit, `delete_remote_resource` owns:

- visible-path normalization through `get_path_in_cache`
- the `skip_trash` override from runtime `Context`
- the `delete_forever_in_trash_folder` config behavior
- the branch between `trash_resource` and `delete_resource`
- execution through `do_request`

See `docs/agent-docs/drive-delete-remote-resource.md` for that policy layer.

## Why The Caller Contract Matters

Neither `unlink` nor `rmdir` re-checks the actual resource kind before
delegating.

They trust the caller to choose the correct entrypoint.

That assumption is safe at the normal FUSE boundary, where the kernel invokes
`unlink` for non-directories and `rmdir` for directories. But it is an
important maintenance invariant inside the repository:

- calling `unlink` on a directory path would pass `is_folder = false`
- that can skip the folder emptiness check
- and it can skip descendant cache cleanup after success

So these wrappers are thin by design, but they are also intentionally reliant
on the caller preserving POSIX operation kind.

## Relationship To Folder Emptiness Checking

`rmdir` does not perform its own child lookup.

Instead, by passing `is_folder = true`, it causes the lower helper to call:

```ocaml
check_if_empty remote_id true trashed
```

before trashing or deleting the directory.

`unlink` passes `false`, so the same guard becomes a no-op.

This is why the tiny wrapper difference is enough to produce the visible
`rmdir`-style `ENOTEMPTY` behavior.

See `docs/agent-docs/drive-check-if-empty.md` for the actual emptiness probe.

## Relationship To Trash vs Permanent Delete

The wrappers themselves do not decide whether the target is:

- moved to trash, or
- deleted permanently

That choice is made later by `delete_remote_resource` based on:

- `Context.skip_trash`
- whether the path resolves into the trash namespace
- `Config.delete_forever_in_trash_folder`

So `unlink` and `rmdir` are operation-kind selectors, not policy selectors.

## What These Wrappers Do Not Do

`Drive.unlink` and `Drive.rmdir` do not:

- normalize the path themselves
- resolve the resource themselves
- verify the actual resource kind themselves
- choose trash versus permanent deletion themselves
- perform cache cleanup themselves
- translate exceptions themselves

They only choose the folder-intent bit and hand off the request.

## Related Docs

- `docs/agent-docs/drive-delete-remote-resource.md`
- `docs/agent-docs/drive-check-if-empty.md`

## Source Pointers

- `src/drive.ml`: `unlink`
- `src/drive.ml`: `rmdir`
- `src/drive.ml`: `delete_remote_resource`
- `bin/gdfuseFuse.ml`: `drive_path_op`
- `bin/gdfuseFuse.ml`: `unlink`
- `bin/gdfuseFuse.ml`: `rmdir`
