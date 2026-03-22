# `Drive.symlink`

## Purpose

`Drive.symlink` is the FUSE-facing create-link entrypoint.

Unlike most of the other `Drive` path handlers, it contains almost no logic of
its own. Its job is to take the POSIX `symlink(target, linkpath)` request and
forward it into the shared remote-creation path with the right fixed arguments.

That means this function is best read as a boundary adapter:

- it exposes the link-creation API to FUSE
- it marks the operation as "create a non-folder with a link target"
- it lets `create_remote_resource` decide whether that becomes:
  - a stored symlink backed by app properties, or
  - a native Drive shortcut

## Signature And FUSE Boundary

```ocaml
val symlink : string -> string -> unit
```

The production FUSE adapter wires:

```ocaml
let symlink target linkpath =
  with_drive_op ~label:"symlink" ~param:target (fun () ->
      Drive.symlink target linkpath)
```

So repository exceptions raised during creation are translated at the FUSE
boundary into Unix/FUSE errors. `Drive.symlink` itself does not contain any
special error mapping.

## Entire Implementation

The implementation is:

```ocaml
let symlink target linkpath =
  create_remote_resource ~link_target:target false linkpath 0o777
```

That single call fixes all of the high-level semantics:

- `link_target = target`
- `is_folder = false`
- `path = linkpath`
- `mode = 0o777`

Everything else is delegated.

## What The Fixed Arguments Mean

## `link_target = target`

The visible symlink target string is passed straight through.

`Drive.symlink` does not normalize or reinterpret it first.

That matters because the downstream helper uses the raw target in two different
ways:

- if the operation becomes a stored symlink, the literal target string is kept
  in app properties
- if the operation becomes a Drive shortcut, a relative target is resolved
  against `Filename.dirname linkpath`

So the meaning of a relative target is intentionally based on the link's parent
directory, not on the mount root.

## `is_folder = false`

This forces the create path onto the file/symlink/shortcut branch.

`Drive.symlink` never creates a Drive folder. The only question is which
symlink-like representation the helper chooses for the new non-folder object.

## `mode = 0o777`

The symlink entrypoint always passes `0o777`.

That does not mean the final cached object is always a plain file with mode
`0777`:

- for stored symlinks, `create_remote_resource` overwrites the mode bits with
  `0o120777`
- for Drive shortcuts, later stat synthesis treats the object as `S_LNK`
  because of its shortcut mime type, with symlink-like permission masking

So the fixed mode is part of the handoff contract, not the full visible stat
result.

## Shortcut Versus Stored Symlink

The main behavioral branch is not in `Drive.symlink` itself. It lives in
`create_remote_resource`.

The delegated helper creates a Drive shortcut when:

- the target is relative, or
- the target is absolute and inside the current mountpoint

It creates a stored symlink when:

- the target is absolute and outside the mountpoint

That means the same FUSE `symlink` operation can produce two different remote
representations depending on the target string.

See `docs/agent-docs/drive-create-remote-resource.md` for the full decision
tree and the exact target-resolution flow.

## Immediate Effects After Success

Because `Drive.symlink` goes through `create_remote_resource`, successful
creation has the same immediate postconditions as the other create entrypoints:

1. the remote object is created first through `FilesResource.create`
2. any cached `NotFound` tombstone at `linkpath` is deleted
3. a new cached resource row is inserted in state `Synchronized`

There is no separate upload step for the link itself.

For a shortcut, the cached row also carries the target information needed by
later `read_link` and `get_attr` handling.

## Relationship To Read-Side Link Handling

`Drive.symlink` chooses the creation path, while later reads flow through:

- `Drive.read_link` for the target string
- `Drive.get_attr` for `S_LNK`, permissions, and visible size

So the create-time shortcut-versus-stored-symlink decision directly determines
what later read-side code sees:

- stored symlinks usually expose the literal preserved target
- shortcuts may reconstruct the target from `target_id` and the current cached
  target path

See `docs/agent-docs/drive-read-link.md` for the read-side consequences.

## What `Drive.symlink` Does Not Do

`Drive.symlink` does not:

- normalize `linkpath` itself
- validate permissions itself
- decide shortcut versus stored symlink itself
- create a local content file
- enqueue upload work

All of that belongs to the shared creation helper and the later read-side code.

## Related Docs

- `docs/agent-docs/drive-create-remote-resource.md`
- `docs/agent-docs/drive-read-link.md`
- `docs/agent-docs/drive-get-attr.md`

## Source Pointers

- `src/drive.ml`: `symlink`
- `src/drive.ml`: `create_remote_resource`
- `bin/gdfuseFuse.ml`: `symlink`
