# `Drive.create_remote_resource`

## Purpose

`Drive.create_remote_resource` is the common remote-creation path for:

- `mknod`
- `mkdir`
- `symlink`

It creates the remote Drive object first, then seeds the local cache with a
`Synchronized` resource row.

This is the complementary path to the upload pipeline:

- `create_remote_resource` creates the remote file/folder/symlink shell
- later local content changes use the upload path to push bytes into that remote
  object

## Entry Points

The public wrappers are:

```ocaml
let mknod path mode = create_remote_resource false path mode
let mkdir path mode = create_remote_resource true path mode
let symlink target linkpath =
  create_remote_resource ~link_target:target false linkpath 0o777
```

So the parameter roles are:

- `is_folder = true` for `mkdir`
- `is_folder = false` for regular files and symlinks
- `link_target = Some ...` only for the symlink entrypoint

## High-Level Flow

At a high level, `create_remote_resource` does this:

1. normalize the visible path into `(path_in_cache, trashed)`
2. reject creation in namespaces that are not writable
3. resolve the parent resource and parent remote id
4. choose whether the new item is:
   - a regular file
   - a folder
   - a stored symlink
   - a Drive shortcut
5. build the `File` payload for `FilesResource.create`
6. create the remote object
7. delete any cached `NotFound` tombstone for the same path
8. insert the new cached resource in state `Synchronized`

## Early Permission Checks

Before talking to Drive, the function rejects a few cases:

- creating in the trash view: `if trashed then raise Permission_denied`
- creating under `/lost+found`: `if is_lost_and_found path trashed config then
  raise Permission_denied`
- creating while the filesystem is read-only:
  `if is_filesystem_read_only () then raise Permission_denied`

These checks are important because the rest of the function assumes it is
creating a real writable Drive object under a normal parent.

## Parent Resolution

The actual creation path starts by resolving the parent:

```ocaml
get_resource parent_path trashed >>= fun parent_resource ->
let parent_id =
  parent_resource |. CacheData.Resource.remote_id |> Option.get
```

So `create_remote_resource` depends on the same path-resolution machinery as the
rest of `Drive`.

That gives it:

- the canonical parent cache row
- the parent Drive id to place in `File.parents`

If parent lookup fails, creation fails before any remote side effect happens.

## Four Creation Modes

Although the signature looks simple, the function actually supports four
creation modes.

### 1. Regular File

This is the `mknod` path with no `link_target`.

The new Drive file gets:

- `File.name = basename(path_in_cache)`
- `parents = [ parent_id ]`
- `mimeType = ""` if `autodetect_mime = true`
- otherwise `Mime.map_filename_to_mime_type name`
- app properties containing only the mode bits

### 2. Folder

This is the `mkdir` path.

The new Drive object gets:

- `mimeType = application/vnd.google-apps.folder`
- app properties containing the requested mode bits

There is no separate post-create upload step. The folder is fully created by the
single `FilesResource.create` call.

### 3. Stored Symlink

This is a symlink represented through Drive app properties rather than through
Drive shortcuts.

That branch is chosen when:

- `link_target` is absolute
- and it is outside the mountpoint

In that case the remote file gets:

- normal file creation semantics
- `link_target` stored in app properties
- mode bits forced to symlink mode `0o120777`

So this is not a native Drive shortcut. It is a regular Drive file carrying the
symlink target as application metadata.

### 4. Drive Shortcut

This is the symlink path that maps onto native Drive shortcuts.

That branch is chosen when:

- `link_target` is relative
- or `link_target` is absolute and inside the current mountpoint

In that case:

- `mimeType = application/vnd.google-apps.shortcut`
- `shortcutDetails.targetId` is populated with the target resource's remote id

So the visible POSIX symlink API is implemented using a Drive shortcut whenever
the target can be interpreted as another object inside the mounted Drive view.

## Symlink Versus Shortcut Decision

The deciding logic is:

```ocaml
match link_target with
| None -> false
| Some tp ->
    if not (Filename.is_relative tp) then
      ExtString.String.starts_with tp mountpoint_path
    else true
```

That means:

- no `link_target`: not a shortcut
- relative target: always treated as a shortcut candidate
- absolute target inside the mountpoint: shortcut candidate
- absolute target outside the mountpoint: stored symlink

There is an explicit code comment noting a limitation:

- relative targets outside the mountpoint do not work correctly with this model

That limitation follows from the fact that relative targets are unconditionally
treated as shortcut candidates.

## Resolving Shortcut Targets

When the function decides to create a Drive shortcut, it must resolve the target
resource id first.

The target-resolution flow is:

1. if the target is relative, resolve it against `Filename.dirname path`
2. if the result begins with the mountpoint path, strip the mountpoint prefix
3. trim a trailing slash
4. normalize the target path with `Utils.normalize_absolute_path`
5. translate it with `get_path_in_cache`
6. resolve the target resource with `get_resource`
7. read the target resource's `remote_id`

One important guard exists here:

```ocaml
if CacheData.Resource.is_shortcut resource then
  Permission_denied
```

So the implementation explicitly forbids shortcuts to shortcuts because Drive
does not support them.

## Stored Symlink Metadata

For non-shortcut symlinks, the target is stored in app properties.

The function also enforces:

```ocaml
if json_length link > max_link_target_length then
  raise Invalid_operation
```

So long symlink targets are rejected before the remote create request is sent.

The stored metadata includes:

- `CacheData.Resource.link_target_to_app_property link`
- `CacheData.Resource.mode_to_app_property 0o120777`

That is why later read-side code can treat the object as a symlink even though
it is not a Drive shortcut.

## Request Shape

After all branching, the function builds:

```ocaml
{
  File.empty with
  File.name;
  parents = [ parent_id ];
  mimeType;
  appProperties;
  shortcutDetails = { File.ShortcutDetails.empty with targetId = target_id };
}
```

and sends:

```ocaml
FilesResource.create
  ~enforceSingleParent:true
  ~supportsAllDrives:true
  ~std_params:file_std_params
  file
```

So all creation modes ultimately funnel through one Drive `create` call with
different `mimeType`, `appProperties`, and `shortcutDetails`.

## Cache Update After Remote Create

Once Drive returns the created file metadata, `create_remote_resource` does two
important cache operations.

### 1. Delete `NotFound` Tombstones

It first removes any cached negative-cache row for the same path:

```ocaml
Cache.Resource.delete_not_found_resource_with_path cache path_in_cache
```

This matters because `get_resource` can cache path misses as `NotFound`.
Creating a real object at that path must clear the tombstone immediately.

### 2. Insert A Synchronized Resource Row

Then it creates a fresh local resource and inserts it into cache with:

```ocaml
insert_resource_into_cache
  ~state:CacheData.Resource.State.Synchronized
  ?link_target
  cache
  new_resource
  created_file
```

That `Synchronized` state is important:

- the remote object already exists
- there is nothing to upload yet
- later content writes will move the resource to `ToUpload`

So creation does not go through the upload queue.

## Why `?link_target` Is Passed Into Cache Insertion

`update_resource_from_file` treats shortcuts specially:

- for shortcut mime types, it uses the optional `link_target` argument
- for non-shortcut resources, it reads link target data from app properties

So passing `?link_target` here preserves a usable target string in the cached row
immediately after creating a shortcut.

Later, if needed, `fetch_link_target` can reconstruct the target from
`target_id` by resolving the target resource and prefixing the mountpoint path.

## Read-Side Consequences

Creation choices here directly affect later behavior:

- plain stored symlinks expose their target from cached app properties
- Drive shortcuts expose link targets through `target_id` resolution
- both are surfaced through `readlink`
- shortcuts are presented as symlink-like objects in attribute handling

See `docs/agent-docs/drive-get-attr.md` for how that later attribute handling
turns these cached shapes into visible `st_kind`, `st_size`, and permission
results.

So the symlink-versus-shortcut decision is not just an implementation detail. It
changes how later lookup and `readlink` reconstruction work.

## What This Function Does Not Do

`create_remote_resource` does not:

- upload file content
- create a local cache file with bytes in it
- enqueue async upload work
- reconcile duplicate names in the parent directory after creation

For a new regular file, the remote side starts as an empty Drive file. Actual
content appears only after later local writes and the upload path run.

## Config Knobs That Matter

The main config interactions here are:

- `read_only`: blocks creation entirely through `is_filesystem_read_only`
- `autodetect_mime`: controls regular-file MIME type selection
- `lost_and_found`: indirectly matters because creation inside that synthetic
  namespace is forbidden

The mountpoint path stored in `Context.mountpoint_path` is also operationally
important because it drives shortcut-vs-symlink classification for absolute
targets.

## Maintenance Notes

When changing this area, watch these invariants:

- creation in the trash namespace and `lost+found` must remain forbidden unless
  the virtual namespace model changes
- shortcut target resolution depends on visible-path normalization and the
  current mountpoint prefix
- stored symlinks and Drive shortcuts are intentionally different
  representations
- successful creation must clear any `NotFound` tombstone for the path
- remote creation inserts a `Synchronized` cache row and does not use the upload
  queue
- shortcuts to shortcuts are intentionally denied

## Related Docs

- `docs/agent-docs/drive-upload-path.md`
- `docs/agent-docs/drive-get-resource.md`

## Source Pointers

- `src/drive.ml`: `create_remote_resource`
- `src/drive.ml`: `mknod`
- `src/drive.ml`: `mkdir`
- `src/drive.ml`: `symlink`
- `src/drive.ml`: `fetch_link_target`
- `src/drive.ml`: `read_link`
