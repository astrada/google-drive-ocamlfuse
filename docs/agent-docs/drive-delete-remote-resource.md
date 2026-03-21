# `Drive.delete_remote_resource`

## Purpose

`Drive.delete_remote_resource` is the small but important policy wrapper behind:

- `Drive.unlink`
- `Drive.rmdir`

Its job is not to implement trashing or permanent deletion itself. Instead, it
decides which of the two lower-level paths should run for a given delete
request:

- `trash_resource`
- `delete_resource`

So this function is the deletion policy switch for the mounted filesystem.

## Signature And Entry Points

```ocaml
val delete_remote_resource : bool -> string -> unit
```

The parameters are:

- `is_folder`: distinguishes `unlink` from `rmdir`
- `path`: visible FUSE path

The public wrappers are:

```ocaml
let unlink path = delete_remote_resource false path
let rmdir path = delete_remote_resource true path
```

So `delete_remote_resource` is the shared path-level layer, while
`unlink`/`rmdir` only provide the folder/non-folder intent.

At the FUSE boundary, `bin/gdfuseFuse.ml` wires:

```ocaml
let unlink path = drive_path_op ~name:"unlink" path Drive.unlink
let rmdir path = drive_path_op ~name:"rmdir" path Drive.rmdir
```

so any repository exception raised below is translated into a Unix/FUSE error.

## High-Level Flow

At a high level, `delete_remote_resource` does this:

1. read `Context` and `Config`
2. normalize the visible path with `get_path_in_cache`
3. inspect the resulting `trashed` flag
4. choose between `delete_resource` and `trash_resource`
5. execute the chosen request through `do_request`

In code, the decision is:

```ocaml
if context.skip_trash
   || (trashed && config.delete_forever_in_trash_folder)
then delete_resource is_folder path
else trash_resource is_folder trashed path
```

That is the whole function.

## Path Normalization Matters

The wrapper starts with:

```ocaml
let _, trashed = get_path_in_cache path config
```

It only needs the `trashed` half of the normalized path, but that flag is the
key input to the policy decision.

This means visible paths under `/.Trash/...` are not treated as ordinary paths.
Once `get_path_in_cache` maps them into the trash namespace, the wrapper can
apply a different delete policy.

## Branch 1: Forced Permanent Deletion

The wrapper chooses `delete_resource` in two situations.

### `Context.skip_trash = true`

This is the global "bypass trash" mode.

It comes from the CLI `-skiptrash` option and is stored in `Context` during
startup.

When enabled, every `unlink`/`rmdir` request goes straight to permanent
deletion, regardless of whether the path is in the trash namespace already.

This is the strongest policy override in the function.

### Trashed Path With `delete_forever_in_trash_folder = true`

If the normalized path is already in the trash namespace and:

```ocaml
config.delete_forever_in_trash_folder = true
```

then the wrapper also chooses `delete_resource`.

So deleting something from `/.Trash/...` can mean "delete forever" rather than
"trash again", depending on config.

This is the main reason the function inspects `trashed` explicitly instead of
only delegating to `trash_resource`.

## Branch 2: Normal Trashing

If neither permanent-deletion condition holds, the wrapper chooses:

```ocaml
trash_resource is_folder trashed path
```

This is the ordinary deletion policy for visible non-trash paths.

Important nuance: the wrapper passes both:

- `is_folder`
- the normalized `trashed` flag

down to `trash_resource`, so the lower-level helper can still enforce its own
namespace restrictions and folder-specific behavior.

## What This Wrapper Delegates

`delete_remote_resource` deliberately does not duplicate the detailed rules of
the two underlying helpers.

## Delegated To `trash_resource`

When the trash branch is chosen, that helper still owns:

- rejecting attempts to trash something already in the trash namespace
- rejecting trash operations under `/lost+found`
- checking folder emptiness before trashing a directory
- updating the cache row to `trashed = true`
- invalidating the trash-bin cache
- trashing cached descendants for folder rows

## Delegated To `delete_resource`

When the permanent-delete branch is chosen, that helper still owns:

- checking folder emptiness before deletion
- issuing `FilesResource.delete`
- purging the local cache row
- deleting cached descendants for folders

So `delete_remote_resource` should be understood as policy selection, not as the
place where delete/trash correctness is implemented.

See `docs/agent-docs/drive-update-remote-resource.md` for the shared mutation
wrapper that both helpers use under the hood.
See `docs/agent-docs/drive-check-if-empty.md` for the folder-emptiness guard
those helpers both rely on before removing directories.

## Folder Behavior Is Selected Up Front

The `is_folder` parameter is passed unchanged into whichever helper is chosen.

That means the wrapper itself does not inspect the actual resource kind before
choosing the branch. It trusts the caller:

- `unlink` says "treat this as a non-folder delete request"
- `rmdir` says "treat this as a folder delete request"

The downstream helper then uses that information when deciding whether to run
the emptiness check and how much cache state to clean up.

## Why Trash-Namespace Paths Need Special Policy

Without this wrapper, a delete request under `/.Trash/...` would have no clear
place to decide whether the user wants:

- another trash-style mutation
- or a true permanent delete

`trash_resource` alone cannot represent that policy, because it rejects
`trashed = true` outright.

So `delete_remote_resource` is the bridge between:

- visible filesystem delete semantics
- the internal distinction between trashing and permanent deletion

## Relationship To `unlink` And `rmdir`

`unlink` and `rmdir` in this repository are intentionally thin wrappers.

They do not add extra path checks or custom logging around deletion policy.
Their only semantic contribution is the `is_folder` bit they pass into
`delete_remote_resource`.

So if delete behavior appears inconsistent between files and directories, the
first places to inspect are:

- the caller's chosen `is_folder` value
- `delete_remote_resource`
- the downstream `trash_resource` / `delete_resource` helpers

## Maintenance Notes

### `skip_trash` Is Runtime Context, Not Config

The strongest permanent-delete switch comes from `Context.skip_trash`, which is
set from CLI startup parameters, not from the persistent config file.

So delete behavior can change across launches even with identical config.

### Trash-Namespace Deletes Are Config-Sensitive

A delete inside `/.Trash/...` does not have one universal meaning. It depends
on `config.delete_forever_in_trash_folder`.

### This Wrapper Has No Read-Only Guard Of Its Own

Read-only enforcement happens downstream:

- `trash_resource` and `delete_resource` both go through
  `update_remote_resource`
- `update_remote_resource` rejects mutations when the filesystem is read-only

So this wrapper inherits mutability rules indirectly rather than checking them
itself.

### The Interesting Logic Is In The Branch Selection

Because the function is so small, it is easy to overlook. But any change to
trash policy, CLI deletion modes, or trash-folder semantics belongs here first.
