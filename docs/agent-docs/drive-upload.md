# Concrete Upload Attempt

## Purpose

`DriveUploads` owns the concrete network-upload attempt in the content upload
lifecycle.

It turns:

- one resolved cached resource row
- one on-disk cache file

into a Drive `FilesResource.update` request, then reconciles the returned
metadata back into the local cache.

The public helper in `src/drive.ml` remains:

```ocaml
val upload : CacheData.Resource.t -> unit GapiMonad.SessionM.m
```

In production that helper builds a `DriveUploads.runtime` from `Context` and
delegates to `UploadOps.upload`.

## Boundary

`DriveUploads` does not resolve visible paths, decide whether a resource is
dirty, choose sync-vs-async dispatch, flush memory buffers, or retry failures.

Those surrounding responsibilities live in:

- `DriveUploadDispatch`
- `Drive.upload_resource_with_retry`
- `Drive.upload_resource_by_id`
- `UploadQueue`

`DriveUploads` owns one concrete upload attempt and the cache reconciliation
that follows a successful Drive response.

## Runtime And Ports

The runtime is:

```ocaml
type runtime = {
  cache : CacheData.t;
  config : Config.t;
}
```

Production wiring lives in `DriveUploadPorts` in `src/drive.ml`.

Important ports include:

- `get_content_path`
- `create_file_resource`
- `update_cached_resource_state_and_size`
- `build_resource_keys_header_from_resource`, backed by `DriveResourceKeys`
- `remote_update`
- `update_resource_from_file`
- `select_first_resource_with_remote_id`
- `update_cached_resource`
- `shrink_cache`

The production `update_resource_from_file` port delegates Drive `File` to cache
resource mapping to `DriveResourceMapping`.

The production `remote_update` port is the only place in this boundary that
mentions `FilesResource.update`, `file_std_params`, `with_retry_default`,
`enforceSingleParent`, or `supportsAllDrives`.

## High-Level Flow

`DriveUploads.Make(P).upload runtime resource` does this:

1. compute the cache-file path
2. choose the outgoing MIME type
3. create the media resource from the cache file
4. update cached state and size to `Uploading`
5. build the Drive metadata patch
6. call the `remote_update` port
7. rebuild from the returned `File`
8. reload the current cache row by returned remote id
9. conditionally return `Uploading` to `Synchronized`
10. write the final cache row
11. run `shrink_cache ()`

## Preconditions

### Existing Remote Id

The helper reads:

```ocaml
resource |. CacheData.Resource.remote_id |> Option.get
```

So it updates an existing Drive object. It is not the creation path for new
remote files.

### Cache File Source

The source bytes come from:

```ocaml
P.get_content_path runtime.cache resource
```

`upload_resource_with_retry` flushes memory buffers before this helper runs so
the on-disk cache file is current.

### Path Resolution Already Happened

This helper does not call `get_resource`. By the time execution reaches
`DriveUploads`, the dispatch layer has already supplied the resource row to
upload.

## MIME Type Selection

`DriveUploads.content_type_for_upload runtime resource content_path` preserves
the current branch order.

### Editable Exported Google Documents

If the resource is a Google document and `config.editable_docs = true`, the
helper derives the outgoing MIME type from the configured export format:

```ocaml
let fmt = CacheData.Resource.get_format resource config in
CacheData.Resource.mime_type_of_format fmt
```

This branch does not create a preliminary media resource for MIME detection.
The write-open path rejects desktop-formatted Google documents earlier through
`is_file_read_only`.

### `autodetect_mime = true`

If autodetection is enabled, the selected content type is:

```ocaml
""
```

The final media resource is still created with that explicit empty content
type, preserving the existing Drive-inference behavior.

### `autodetect_mime = false`

Otherwise, the helper creates a preliminary file media resource and compares:

- the cached resource MIME type
- the MIME type detected from the local file

It prefers the cached MIME type when it is non-empty, otherwise it uses the
detected MIME type. This keeps uploads aligned with cached Drive metadata when
possible.

## Media Source

After choosing the content type, the helper builds the actual upload source:

```ocaml
let file_source =
  P.create_file_resource ~content_type content_path
```

The upload size comes from:

```ocaml
P.media_content_length file_source
```

That size is authoritative for the upload attempt, even if
`resource.CacheData.Resource.size` says something else.

## Early Cache Update

Before the network request, the helper calls:

```ocaml
P.update_cached_resource_state_and_size runtime.cache
  CacheData.Resource.State.Uploading
  size
  resource.CacheData.Resource.id
```

So each concrete upload attempt moves the cached row into `Uploading` and
updates cached size to the actual media-source length before Drive responds.

## Zero-Byte Uploads

The media body is selected as:

```ocaml
let media_source =
  if size = 0L then None else Some file_source
```

Zero-byte uploads still patch metadata such as `modifiedTime`, but omit the
media body.

## Request Shape

The metadata patch is:

```ocaml
File.empty |> File.modifiedTime ^= P.now ()
```

Headers come from:

```ocaml
P.build_resource_keys_header_from_resource resource
```

The production port delegates header construction to `DriveResourceKeys`, which
builds `X-Goog-Drive-Resource-Keys` from the cached resource's remote id and
resource key when both are present.

The production request is:

```ocaml
FilesResource.update
  ~enforceSingleParent:true
  ~supportsAllDrives:true
  ~std_params:file_std_params
  ?media_source
  ~custom_headers
  ~fileId:remote_id
  file_patch
```

## Reconciliation

After `remote_update` returns a Drive `File`, the helper first rebuilds the
input resource from the response:

```ocaml
let resource = P.update_resource_from_file resource file
```

It then reloads by returned remote id:

```ocaml
let reloaded_resource =
  P.select_first_resource_with_remote_id runtime.cache file.File.id
in
let resource = Option.default resource reloaded_resource
```

That gives the cache one chance to preserve a newer local row for the same
remote object.

## Conditional Return To `Synchronized`

The state transition is deliberately conditional:

```ocaml
match resource.CacheData.Resource.state with
| CacheData.Resource.State.Uploading ->
    Some CacheData.Resource.State.Synchronized
| _ -> None
```

Then the final row is rebuilt and saved:

```ocaml
let updated_resource = P.update_resource_from_file ?state resource file in
P.update_cached_resource runtime.cache updated_resource
```

This means:

- if the current row is still `Uploading`, the upload completion marks it
  `Synchronized`
- if a newer local state such as `ToUpload` is present, that state is preserved

This guard prevents a stale upload completion from overwriting a later local
change.

## Final Cache Accounting

After the final cache row is saved, the helper calls:

```ocaml
P.shrink_cache ()
```

`shrink_cache` runs only after the final cache update succeeds.
The production port delegates the cache-size accounting to
`DriveCacheMaintenance`.

## Error Surface

`DriveUploads.upload` has no local exception handling of its own.

Possible failures come from:

- content-path or media-source creation
- the `remote_update` port
- cache update helpers

Failure normalization and retry live in `Drive.upload_resource_with_retry`.

## Test Coverage

`test/testDriveUploads.ml` covers the concrete upload attempt with fake ports:

- MIME selection for editable documents, autodetect mode, cached MIME, and
  detected MIME fallback
- final media-source construction and zero-byte media omission
- early `Uploading` state/size update
- request headers, remote id, and modified-time patch wiring
- reload-by-returned-remote-id behavior
- conditional `Uploading -> Synchronized` transition
- preservation of reloaded `ToUpload` state
- final cache update before shrink
- failure ordering for media creation, remote update, and final cache update

## Related Docs

- `docs/agent-docs/drive-upload-resource-with-retry.md`
- `docs/agent-docs/drive-queue-upload.md`
- `docs/agent-docs/drive-upload-path.md`
- `docs/agent-docs/drive-download-resource.md`

## Source Pointers

- `src/driveUploads.ml`: concrete upload attempt
- `src/drive.ml`: `DriveUploadPorts`, `UploadOps`, and `upload`
- `src/drive.ml`: `upload_resource_with_retry`
- `src/driveResourceMapping.ml`: post-upload resource mapping
- `test/testDriveUploads.ml`
