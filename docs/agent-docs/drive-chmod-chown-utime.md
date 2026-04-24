# Metadata Mutations

## Purpose

`Drive.utime`, `Drive.chmod`, and `Drive.chown` are the FUSE-facing metadata
mutation entrypoints for existing resources.

The public `Drive` functions are thin wrappers over `DriveMetadataMutations`.
They build a `DriveMetadataMutations.runtime` from `Context`, call
`MetadataMutationOps`, and execute the session through `do_request`:

```ocaml
let chmod path mode =
  do_request
    (MetadataMutationOps.chmod (drive_metadata_mutation_runtime ()) path mode)
  |> ignore
```

`MetadataMutationOps` is
`DriveMetadataMutations.Make(DriveMetadataMutationPorts)`.

## Implementation Boundary

`DriveMetadataMutations` owns the metadata-specific behavior:

- building the `utime` modified-time patch
- building the `chmod` mode app-property patch
- converting `chown` sentinel ids into omitted app properties
- building the `chown` uid/gid app-property patch
- passing the `utime` local-cache timestamp hook into the update wrapper

The production ports in `DriveMetadataMutationPorts` own the surrounding
effects:

- resource-key header construction
- retried `FilesResource.update` calls
- shared `update_remote_resource` control flow
- local cache-file timestamp updates through `Unix.utimes`

The runtime passed to `DriveMetadataMutations` contains the cache handle and
config:

```ocaml
type runtime = {
  cache : CacheData.t;
  config : Config.t;
}
```

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

## Shared Shape

All three `DriveMetadataMutations` operations follow the same high-level
pattern:

1. define a callback that receives the resolved `resource`
2. derive `remote_id` from that resource
3. build a `File` patch
4. call the `remote_update` port
5. return `Some patched_file`
6. pass that callback into the `update_remote_resource` port

That means they inherit the wrapper behavior:

- filesystem read-only rejection
- visible-path normalization through `get_path_in_cache`
- current-resource lookup through `get_resource`
- default cache-row refresh from the returned `File`

See `docs/agent-docs/drive-update-remote-resource.md` for that shared control
flow.

## Shared Remote Update Shape

All three callbacks issue a Drive metadata patch through the `remote_update`
port. In production, that port calls:

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

## `utime`

`DriveMetadataMutations.utime runtime path atime mtime` is the timestamp
mutation path.

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

`utime` is the only one of the three operations that passes
`~update_file_in_cache` into the update wrapper.

It passes:

```ocaml
~update_file_in_cache:(fun content_path ->
  P.update_file_times content_path atime mtime)
```

In production, `P.update_file_times` is `Unix.utimes`.

So if a synchronized local cache file already exists on disk, `utime` mirrors
both timestamps onto that local file after the remote patch succeeds.

That hook is deliberately narrow because `update_remote_resource` only runs it
when:

- the resource state is `Synchronized`
- and the cache file already exists

So `atime` is a local-cache consistency detail, not a persisted remote Drive
attribute.

## `chmod`

`DriveMetadataMutations.chmod runtime path mode` builds a one-property
app-properties patch:

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

## `chown`

`DriveMetadataMutations.chown runtime path uid gid` builds app-properties
patches for the cached uid/gid metadata.

The key detail is the id conversion helper:

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
- when both are present, the patch order is uid first, then gid

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

`DriveMetadataMutations.utime`, `DriveMetadataMutations.chmod`, and
`DriveMetadataMutations.chown` do not:

- mark the resource `ToUpload`
- use the upload queue
- create or download local file content
- perform custom cache surgery like rename or delete paths do

They are in-place metadata mutations on an already existing resource.

## Test Coverage

`test/testDriveMetadataMutations.ml` covers the metadata behavior through fake
ports:

- `utime` remote modified-time patching
- `utime` local-cache timestamp hook wiring
- `chmod` mode app-property patching
- `chown` uid/gid app-property patching and ordering
- `chown` handling for `-1` and unsigned 32-bit all-ones sentinels

## Related Docs

- `docs/agent-docs/drive-update-remote-resource.md`
- `docs/agent-docs/drive-get-attr.md`
- `docs/agent-docs/drive-xattr.md`

## Source Pointers

- `src/driveMetadataMutations.ml`: `utime`
- `src/driveMetadataMutations.ml`: `chmod`
- `src/driveMetadataMutations.ml`: `chown`
- `src/drive.ml`: `DriveMetadataMutationPorts`
- `src/drive.ml`: `update_remote_resource`
- `bin/gdfuseFuse.ml`: `utime`
- `bin/gdfuseFuse.ml`: `chmod`
- `bin/gdfuseFuse.ml`: `chown`
