# `Drive.check_if_empty`

## Purpose

`Drive.check_if_empty` is the folder-safety gate used before trashing or
permanently deleting directories.

Its contract is simple:

- if the target is not a folder, do nothing
- if the target is a folder and has no children in the relevant namespace, do
  nothing
- otherwise raise `Directory_not_empty`

So this helper is what stops `rmdir`-style flows from deleting non-empty
folders.

## Signature

```ocaml
val check_if_empty : string -> bool -> bool -> unit GapiMonad.SessionM.m
```

The parameters are:

- `remote_id`: Drive id of the folder candidate
- `is_folder`: whether the caller wants folder emptiness checking at all
- `trashed`: whether the check is being performed in the trash namespace

This is an internal helper. It does not resolve a visible path itself.

## Callers

The current callers are:

- `trash_resource`
- `delete_resource`

Both resolve the resource first, extract its `remote_id`, and then call:

```ocaml
check_if_empty remote_id is_folder trashed
```

So `check_if_empty` sits below the path-level delete policy and above the actual
remote trash/delete request.

See `docs/agent-docs/drive-delete-remote-resource.md` for the higher-level
delete-vs-trash branch selection.

## High-Level Flow

At a high level, the helper does this:

1. if `is_folder = false`, return immediately
2. build a Drive query for one child under `remote_id`
3. query the server with `FilesResource.list`
4. if any child is returned, raise `Directory_not_empty`
5. otherwise return `unit`

That is the whole function.

## Non-Folder Fast Path

The first branch is:

```ocaml
if is_folder then ... else SessionM.return ()
```

So files never incur the remote emptiness query.

This is why the higher-level delete wrapper can pass a simple caller-provided
`is_folder` bit instead of forcing every delete path to re-inspect the resource
kind here.

See `docs/agent-docs/drive-unlink-rmdir.md` for the thin public wrappers that
originate that bit at the FUSE boundary.

## The Remote Query

For folders, the helper builds:

```ocaml
let q =
  Printf.sprintf "'%s' in parents and trashed = %b" remote_id trashed
```

This means the emptiness test is namespace-sensitive:

- normal-folder deletion checks for non-trashed children
- trash-folder deletion checks for trashed children

So the helper asks whether the folder is empty in the same namespace the caller
is operating in.

## Cheap Existence Probe, Not Full Listing

The request intentionally asks for the minimum amount of data:

```ocaml
std_params.fields = "files(id)"
pageSize = 1
```

So `check_if_empty` does not enumerate the whole folder. It only asks:

- is there at least one matching child?

If yes, that is enough to reject the delete/trash request.

This keeps the helper cheap even for large folders.

## Shared Drive / Team Drive Awareness

Like the other Drive list helpers, the request includes:

- `supportsAllDrives = true`
- `driveId = config.team_drive_id`
- `includeItemsFromAllDrives = (config.team_drive_id <> "")`
- `corpora = "drive"` for team-drive mode, otherwise `"user"`

So the emptiness check uses the same Drive-scope configuration as the rest of
the repository's metadata and listing paths.

## Server-Authoritative, Not Cache-Authoritative

One important design point is that `check_if_empty` does not inspect the local
cache for children. It always queries Drive.

That means:

- it can see children that are not currently present in the local cache
- it does not trust cached descendant rows as the source of truth
- it protects delete/trash decisions against stale local folder snapshots

So this helper is authoritative in a way that some read-side cache paths are
not.

## Success And Failure Contract

After the list call, the helper checks:

```ocaml
if children.FileList.files = [] then
  SessionM.return ()
else
  raise Directory_not_empty
```

So the result is binary:

- no children: success
- at least one child: fail with `Directory_not_empty`

There is no partial success state and no child metadata is returned to the
caller.

## How `Directory_not_empty` Escapes

`check_if_empty` does not translate its own exception.

Instead, the exception propagates through:

- `trash_resource` or `delete_resource`
- `delete_remote_resource`
- `unlink` or `rmdir`
- the FUSE boundary in `bin/gdfuseFuse.ml`

where `Directory_not_empty` becomes `ENOTEMPTY`.

So this small helper is the real source of the visible "directory not empty"
error for folder deletion.

## What It Does Not Check

`check_if_empty` is deliberately narrow. It does not:

- verify the path or resolve the resource by name
- check read-only mode
- decide between trashing and permanent deletion
- clean up any local cache state
- recursively inspect descendants

It only asks whether one or more direct children exist in the relevant Drive
namespace.

All other deletion invariants live in higher layers.

## Relationship To Trash Semantics

The `trashed` parameter is easy to overlook, but it is important.

Because the query includes `trashed = <bool>`, a folder can be considered empty
or non-empty differently depending on whether the caller is operating:

- in the ordinary filesystem namespace
- or inside `/.Trash/...`

That is why `delete_remote_resource` must choose the delete/trash branch first
and then let the lower helper pass the right `trashed` value into
`check_if_empty`.

## Maintenance Notes

### This Helper Trusts The Caller's `is_folder`

If the caller passes `is_folder = false`, the check is skipped entirely. So any
future change to `unlink`/`rmdir` semantics must preserve that caller-side
contract carefully.

### The Query Is Intentionally Minimal

Do not "improve" this helper into a full child listing unless the behavior
actually requires it. `pageSize = 1` and `fields = "files(id)"` are deliberate.

### Remote Truth Wins Over Cache

If local cache cleanup or descendant bookkeeping changes elsewhere, this helper
should usually stay server-authoritative. That is what makes it a reliable
delete guard.
