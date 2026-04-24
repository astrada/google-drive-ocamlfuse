# Xattr Handling

## Purpose

This note documents the extended-attribute path implemented by `DriveXattrs`
and exposed through the public `Drive` entrypoints:

- `Drive.get_xattr`
- `Drive.list_xattr`
- `Drive.set_xattr`
- `Drive.remove_xattr`

In this repository, xattrs are not stored in a local filesystem-specific side
table. They are represented through Google Drive `appProperties`, cached inside
`CacheData.Resource.xattrs`, and exposed through the FUSE xattr callbacks.

So the xattr path is a small metadata sub-protocol layered on top of Drive file
metadata.

## Implementation Boundary

The production entrypoints in `Drive` are thin wrappers. Each one builds a
`DriveXattrs.runtime` from `Context`, calls `XattrOps`, and executes the
session through `do_request`:

```ocaml
let get_xattr path name =
  do_request (XattrOps.get_xattr (drive_xattr_runtime ()) path name) |> fst
```

`XattrOps` is `DriveXattrs.Make(DriveXattrPorts)`.

`DriveXattrs` owns the xattr-specific behavior:

- parsing cached xattr blobs
- enforcing read and mutation contracts
- building Drive `appProperties` patches
- choosing xattr-specific exceptions

The production ports in `DriveXattrPorts` own the surrounding effects:

- path normalization through `get_path_in_cache`
- resource lookup through `get_resource`
- resource-key header construction
- retried `FilesResource.update` calls
- update-wrapper control flow through `MutationOps.update_remote_resource`

The runtime passed to `DriveXattrs` contains the cache handle and config:

```ocaml
type runtime = {
  cache : CacheData.t;
  config : Config.t;
}
```

## Storage Model

The storage pipeline has three layers.

### 1. Remote Drive Representation

On the Drive side, xattrs live in `File.appProperties` with an internal `x-`
prefix:

```ocaml
let xattr_to_app_property name value = ("x-" ^ name, value)
let xattr_no_value_to_app_property name = ("x-" ^ name, "")
```

So a visible xattr name like `"user.foo"` is sent to Drive as an app property
named `"x-user.foo"`.

### 2. Cached Resource Representation

When a `File` response is turned into a cached resource row,
`update_resource_from_file` sets:

```ocaml
xattrs = CacheData.Resource.get_xattrs file.File.appProperties
```

`get_xattrs` filters `appProperties` down to keys starting with `x-` and then
serializes them into one string.

### 3. Parsed In-Memory Representation

`DriveXattrs` uses:

```ocaml
CacheData.Resource.parse_xattrs resource.CacheData.Resource.xattrs
```

to convert that serialized string into:

```ocaml
(string * string) list
```

of visible `(name, value)` pairs.

The public xattr API does not expose the internal `x-` prefix. That prefix
exists only in the Drive `appProperties` layer.

## Cached Serialization Format

The serialized cached string is built by `render_xattrs` as a sequence of:

```text
%S:%S;
```

pairs, where the stored app-property key has its leading `x-` removed first.

So the cached blob is an internal transport format, not a user-visible API.

`parse_xattrs` is the inverse of that encoding and is used by all four xattr
entrypoints.

## Read Paths

`DriveXattrs.get_xattr` and `DriveXattrs.list_xattr` are read-only metadata
paths. They do not call Drive and do not touch local cache-file contents.

Both functions:

1. normalize the visible path with `get_path_in_cache`
2. resolve the resource with `get_resource`
3. parse `resource.xattrs` with `parse_xattrs`

So even xattr reads inherit the normal path-resolution and metadata-refresh
behavior of `get_resource`.

## `get_xattr`

`get_xattr path name` does:

```ocaml
try List.assoc name xattrs with Not_found -> raise No_attribute
```

So the contract is:

- if the xattr exists, return its string value
- otherwise raise `No_attribute`

## `list_xattr`

`list_xattr path` maps the parsed pairs down to names:

```ocaml
let keys = List.map (fun (n, _) -> n) xattrs
```

So listing returns the visible xattr names only, without the internal `x-`
prefix and without values.

## Mutation Paths

`DriveXattrs.set_xattr` and `DriveXattrs.remove_xattr` run through the
`update_remote_resource` port.

That means they inherit:

- path normalization
- resource resolution through `get_resource`
- filesystem read-only enforcement
- cache refresh from the returned `File`

In production, `DriveXattrPorts.update_remote_resource` delegates to
`MutationOps.update_remote_resource`. See
`docs/agent-docs/drive-update-remote-resource.md` for the wrapper contract.

## `set_xattr`

`set_xattr path name value xflags` performs four kinds of validation before it
sends a Drive patch.

### 1. Current Existence Check

It parses the current xattrs and computes:

```ocaml
let existing = List.mem_assoc name xattrs
```

### 2. `xflags` Enforcement

It then applies the FUSE xattr mode:

- `Fuse.CREATE`: fail with `Existing_attribute` if the key already exists
- `Fuse.REPLACE`: fail with `No_attribute` if the key does not exist
- `Fuse.AUTO`: allow create-or-replace behavior

So the repository preserves the usual xattr create/replace contract locally
before sending the Drive update.

### 3. Length Limit

Before patching Drive, it computes:

```ocaml
let attribute_length = json_length name + json_length value
```

and rejects the change if:

```ocaml
attribute_length > max_attribute_length
```

with `Invalid_operation`.

Two details matter:

- `max_attribute_length = 126`
- the limit uses JSON-escaped lengths, not just raw string lengths

`json_length` does this by round-tripping through `Yojson.Safe.to_string` and
subtracting the surrounding quotes.

So escaping overhead counts toward the limit.

### 4. Remote Patch

If validation passes, `set_xattr` builds:

```ocaml
File.empty |> File.appProperties
  ^= [ CacheData.Resource.xattr_to_app_property name value ]
```

and sends that through the `remote_update` port. In production, that port calls
`FilesResource.update` with:

- `~enforceSingleParent:true`
- `~supportsAllDrives:true`
- `~std_params:file_std_params`
- resource-key headers from the current cached resource

On success it returns `Some patched_file`, so `update_remote_resource` refreshes
the cached resource row from the server response.

## `remove_xattr`

`remove_xattr path name` is the mirror operation.

It first parses the current xattrs and requires:

```ocaml
List.mem_assoc name xattrs
```

Otherwise it raises `No_attribute`.

If the attribute exists, it builds a one-property patch with:

```ocaml
CacheData.Resource.xattr_no_value_to_app_property name
```

which encodes the key as `("x-" ^ name, "")`.

So the client-side removal protocol is "send the prefixed key with an empty
string value" and then trust the returned Drive file metadata to describe the
new app-property state.

As with `set_xattr`, the cache is then refreshed from the returned
`patched_file`.

## Error Model

The xattr entrypoints use repository-specific exceptions for the main xattr
error cases:

- missing attribute on read/remove/replace -> `No_attribute`
- create-existing conflict -> `Existing_attribute`
- oversize encoded attribute -> `Invalid_operation`

Those exceptions are later translated at the FUSE boundary into platform
appropriate xattr-related Unix errors.

## Why The Cache Refresh Matters

The mutation paths do not manually rewrite `resource.xattrs`.

Instead they rely on the normal post-update cache refresh:

1. Drive returns an updated `File`
2. `update_resource_from_file` rebuilds `resource.xattrs` from
   `file.appProperties`
3. the updated row is saved back into the cache

This is important because it makes the cache depend on the authoritative server
response, not on local assumptions about how Drive applied the patch.

## What These Paths Do Not Do

The xattr handlers do not:

- touch local cache-file contents
- queue uploads
- use `update_file_in_cache`
- mutate `resource.xattrs` directly
- bypass `get_resource`
- expose the internal `x-` app-property prefix to callers

They are pure metadata paths.

## Test Coverage

`test/testDriveXattrs.ml` covers the xattr behavior through fake ports:

- read success and missing-attribute failures
- list behavior
- trash-path normalization
- `Fuse.AUTO`, `Fuse.CREATE`, and `Fuse.REPLACE`
- JSON-escaped length limits
- set/remove app-property patches
- validation failures that must not call the remote update port

## Maintenance Notes

### The `x-` Prefix Is Internal

User-facing xattr names do not include `x-`. If you change the prefixing rule,
you must update both the encoding helpers and the app-property parsing path.

### Removal Semantics Depend On The Returned `File`

The client requests removal by sending an empty value, but the real cache state
is whatever comes back in the patched `File.appProperties`.

### Length Checking Is Escaping-Aware

The attribute limit is based on `json_length`, not raw string length. Changes
to escaping or serialization rules can therefore change which xattrs are
accepted.
