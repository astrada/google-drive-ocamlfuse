# `Drive.chmod`, `Drive.chown`, And `Drive.utime`

## Purpose

`Drive.chmod`, `Drive.chown`, and `Drive.utime` are the FUSE-facing metadata
mutation entrypoints for existing resources.

They are thinner than the full read/write paths, but they are not pure
one-liners. Each one builds one small metadata patch for the resolved remote
file and then delegates the shared path normalization, read-only enforcement,
resource lookup, and cache reconciliation to `Drive.update_remote_resource`.

So these functions are best understood as narrow metadata-patch adapters over
the shared update wrapper.

## Signatures And FUSE Boundary

```ocaml
val utime : string -> float -> float -> unit
val chmod : string -> int -> unit
val chown : string -> int -> int -> unit
```

The production FUSE adapter wires:

```ocaml
let utime path atime mtime =
  Utils.log_with_header "utime %s %f %f\n%!" path atime mtime;
  with_drive_op ~label:"utime" ~param:path (fun () ->
      Drive.utime path atime mtime)

let chmod path mode =
  Utils.log_with_header "chmod %s %o\n%!" path mode;
  with_drive_op ~label:"chmod" ~param:path (fun () -> Drive.chmod path mode)

let chown path uid gid =
  Utils.log_with_header "chown %s %d %d\n%!" path uid gid;
  with_drive_op ~label:"chown" ~param:path (fun () -> Drive.chown path uid gid)
```

So all three:

- log the visible path plus the syscall parameters
- run through `with_drive_op`
- rely on `handle_exception` in `bin/gdfuseFuse.ml` for Unix/FUSE error mapping

None of the three entrypoints contains its own special exception translation.

## Shared Shape In `Drive`

All three functions follow the same high-level pattern:

1. define a callback that receives the resolved `resource`
2. derive `remote_id` from that resource
3. build a `File` patch
4. call `FilesResource.update`
5. return `Some patched_file`
6. pass that callback into `update_remote_resource`
7. execute the request through `do_request`

That means they all inherit the same wrapper behavior:

- filesystem read-only rejection
- visible-path normalization through `get_path_in_cache`
- current-resource lookup through `get_resource`
- default cache-row refresh from the returned `File`, unless overridden

See `docs/agent-docs/drive-update-remote-resource.md` for that shared control
flow.

## Shared Remote Update Shape

All three callbacks issue a Drive metadata patch of the form:

```ocaml
FilesResource.update
  ~enforceSingleParent:true
  ~supportsAllDrives:true
  ~std_params:file_std_params
  ~custom_headers
  ~fileId:remote_id
  file_patch
```

So these are all metadata-only updates against an existing remote object.

They do not:

- upload content bytes
- enqueue upload work
- create a new remote file shell

## `Drive.utime`

`utime path atime mtime` is the timestamp mutation entrypoint.

Its remote patch is:

```ocaml
let file_patch =
  File.empty |> File.modifiedTime ^= Netdate.create mtime
```

So only `mtime` is sent to Drive.

This is the main semantic limitation to remember:

- the FUSE entrypoint receives both `atime` and `mtime`
- the remote Drive update uses only `mtime`

### Local Cache File Hook

`utime` is the only one of the three that uses
`update_remote_resource ~update_file_in_cache`.

It passes:

```ocaml
~update_file_in_cache:(fun content_path ->
  Unix.utimes content_path atime mtime)
```

So if a synchronized local cache file already exists on disk, `utime` mirrors
both timestamps onto that local file after the remote patch succeeds.

That hook is deliberately narrow because `update_remote_resource` only runs it
when:

- the resource state is `Synchronized`
- and the cache file already exists

So `atime` is a local-cache consistency detail, not a persisted remote Drive
attribute.

## `Drive.chmod`

`chmod path mode` builds a one-property app-properties patch:

```ocaml
let file_patch =
  File.empty
  |> File.appProperties
     ^= [ CacheData.Resource.mode_to_app_property mode ]
```

So the function stores the requested mode bits in Drive app properties and then
relies on the default cache-save path to rebuild the cached row from the
returned `File`.

It does not mask or reinterpret the mode locally first.

Any later visible permission masking happens in `Drive.get_attr` when the cache
row is converted back into `st_perm`.

## `Drive.chown`

`chown path uid gid` builds app-properties patches for the cached uid/gid
metadata.

The key detail is the helper:

```ocaml
let id_to_string id =
  let id64 = Int64.of_int id in
  let minus_one_32_unsigned = Int64.pred (Int64.shift_left 1L 32) in
  if id64 = Int64.minus_one || id64 = minus_one_32_unsigned then ""
  else string_of_int id
```

So both of these inputs mean "do not send this side":

- `-1`
- `4294967295` as the 32-bit unsigned all-ones sentinel

After that conversion:

- `uid` is included only if its string is non-empty
- `gid` is included only if its string is non-empty

That matches the usual POSIX `chown` convention where one side can be left
unchanged.

## Relationship To `Drive.get_attr`

These functions write metadata that later becomes visible through
`Drive.get_attr`.

The main read-side consequences are:

- `chmod` updates the stored mode bits that later feed `st_perm`
- `chown` updates the stored uid/gid that later feed `st_uid` and `st_gid`
- `utime` updates the remote modified time that later feeds timestamp fields

So these are mutation-side companions to the stat-synthesis path.

See `docs/agent-docs/drive-get-attr.md` for the read-side view.

## What These Functions Do Not Do

`Drive.chmod`, `Drive.chown`, and `Drive.utime` do not:

- mark the resource `ToUpload`
- use the upload queue
- create or download local file content
- perform custom cache surgery like rename or delete paths do

They are in-place metadata mutations on an already existing resource.

## Related Docs

- `docs/agent-docs/drive-update-remote-resource.md`
- `docs/agent-docs/drive-get-attr.md`
- `docs/agent-docs/drive-xattr.md`

## Source Pointers

- `src/drive.ml`: `utime`
- `src/drive.ml`: `chmod`
- `src/drive.ml`: `chown`
- `src/drive.ml`: `update_remote_resource`
- `bin/gdfuseFuse.ml`: `utime`
- `bin/gdfuseFuse.ml`: `chmod`
- `bin/gdfuseFuse.ml`: `chown`
