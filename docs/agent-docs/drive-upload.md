# `Drive.upload`

## Purpose

`Drive.upload` is the actual network-upload function in the content upload
lifecycle.

It is the helper that turns:

- one resolved cached resource row
- one on-disk cache file

into a `FilesResource.update` request against Google Drive, then reconciles the
returned metadata back into the local cache.

So this is the point where the upload pipeline stops being about dispatch and
retry policy and actually performs the remote file update.

## Signature

```ocaml
val upload : CacheData.Resource.t -> unit GapiMonad.SessionM.m
```

The input is already a resolved cached resource row.

This helper does not resolve a visible path and does not create its own retry
loop. It assumes the caller has already reached the actual upload stage.

## Entire Implementation

At a high level, the body does this:

1. compute the cache-file path
2. choose the outgoing MIME type
3. create the media resource from the cache file
4. update cached state and size to `Uploading`
5. build the Drive metadata patch
6. call `FilesResource.update`
7. rebuild and save the cached row from the returned `File`
8. run `shrink_cache ()`

That is the whole purpose of the function.

## Preconditions

`upload` assumes a few things are already true.

### 1. The Resource Already Has A Remote Id

It reads:

```ocaml
let remote_id = resource |. CacheData.Resource.remote_id |> Option.get
```

So this helper is only for updating an existing remote file, not for initial
remote object creation.

### 2. The Cache File Is The Upload Source

It reads:

```ocaml
let content_path = Cache.get_content_path cache resource
```

So the source of truth for bytes at this stage is the local cache file on disk.

The surrounding wrapper `upload_resource_with_retry` flushes memory buffers
before this function runs so that the cache file is up to date.

### 3. Path Resolution Already Happened Earlier

This helper does not call `get_resource`.

By the time execution reaches `upload`, the path-based and dispatch layers have
already supplied the resource row that should be uploaded.

## MIME Type Selection

The first real policy branch is content type selection.

### `autodetect_mime = true`

If:

```ocaml
config.Config.autodetect_mime = true
```

then:

```ocaml
let content_type = ""
```

So Drive is allowed to infer the MIME type.

### `autodetect_mime = false`

Otherwise, the function creates a file media resource and compares:

- the cached resource MIME type
- the MIME type inferred from the local file resource

The rule is:

- if the cached resource MIME type is present and non-empty, prefer it
- otherwise use the file helper's detected content type

This is the implementation's MIME-type workaround for keeping uploads aligned
with the cached resource metadata when possible.

## Media Resource Creation

After choosing the content type, the function builds:

```ocaml
let file_source =
  GapiMediaResource.create_file_resource ~content_type content_path
```

and then reads:

```ocaml
let size = file_source.GapiMediaResource.content_length
```

So the upload size is determined from the local cache file, not from the cached
`resource.size` field.

## Early Cache Update: State And Size

Before the network request is sent, the function does:

```ocaml
update_cached_resource_state_and_size cache CacheData.Resource.State.Uploading
  size resource.CacheData.Resource.id
```

So entering the real upload function has two immediate local consequences:

- state becomes `Uploading`
- cached size is updated to the actual media-source length

This means the cache row is moved into its in-progress upload state before the
Drive request completes.

## Zero-Byte Uploads

The function then chooses:

```ocaml
let media_source =
  if file_source.GapiMediaResource.content_length = 0L then None
  else Some file_source
```

So zero-byte uploads are handled as:

- metadata patch only
- no media body

This lets empty files still update remote metadata such as modified time
without sending a file payload.

## Request Shape

The Drive patch is:

```ocaml
let file_patch = File.empty |> File.modifiedTime ^= GapiDate.now ()
```

So the only explicit metadata field forced here is `modifiedTime`.

The request itself is:

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

Two details matter:

- `custom_headers` comes from `build_resource_keys_header_from_resource`
- `media_source` is omitted entirely for zero-byte uploads

## Immediate Post-Request Rebuild

After `FilesResource.update` returns, the function first does:

```ocaml
let resource = update_resource_from_file resource file
```

So the returned `File` is immediately treated as the new remote truth for the
resource row.

It then logs the successful upload using the returned MIME type.

## Why It Reloads By Remote Id

After that immediate rebuild, the function still does an extra cache lookup:

```ocaml
let reloaded_resource =
  Cache.Resource.select_first_resource_with_remote_id cache file.File.id
```

and then:

```ocaml
let resource = Option.default resource reloaded_resource
```

This is a subtle but important step.

It means the final cache write is based on:

- the freshly rebuilt resource row from the response, unless
- some newer cache row for the same remote id is already present

So the function gives the cache one chance to preserve more recent local state
that may have been written while the network request was in flight.

## Conditional Return To `Synchronized`

The next step is:

```ocaml
let state =
  match resource.CacheData.Resource.state with
  | CacheData.Resource.State.Uploading ->
      Some CacheData.Resource.State.Synchronized
  | _ -> None
```

Then:

```ocaml
let updated_resource = update_resource_from_file ?state resource file in
update_cached_resource cache updated_resource
```

This is one of the most important correctness guards in the function.

It means:

- if the most recent row for this remote id is still `Uploading`, the function
  marks it back to `Synchronized`
- otherwise it preserves whatever newer state already exists

So a later local change is not blindly overwritten by a stale "upload
completed" conclusion.

## Final Cache Accounting

At the end, the function calls:

```ocaml
shrink_cache ()
```

This is the normal post-upload cache-maintenance step after the cache row has
been reconciled.

The helper then returns:

```ocaml
SessionM.return ()
```

## Relationship To `upload_resource_with_retry`

`upload` does not flush memory buffers or retry failures itself.

Those concerns live one layer up in:

- `upload_resource_with_retry`

So the split is:

- `upload_resource_with_retry`: flush + normalize errors + retry temporary failures
- `upload`: perform one concrete upload attempt and reconcile the result

See `docs/agent-docs/drive-upload-resource-with-retry.md` for that wrapper.

## Relationship To `queue_upload`

`queue_upload` decides whether this helper is reached:

- immediately in synchronous mode
- later through the async worker path

But once execution enters `upload`, those earlier dispatch differences no
longer matter. The actual request and reconciliation logic is shared.

See `docs/agent-docs/drive-queue-upload.md` for the dispatch side.

## Error Surface

`upload` has no local exception handling of its own.

Possible failures come from:

- media-source creation
- `FilesResource.update`
- cache update helpers

The surrounding wrapper layers decide which of those failures are retried or
translated.

## What `Drive.upload` Does Not Do

`Drive.upload` does not:

- resolve a visible path
- decide whether upload should start
- flush memory buffers itself
- retry failures itself
- choose sync vs async dispatch itself

It only performs one upload attempt and reconciles the returned metadata.

## Related Docs

- `docs/agent-docs/drive-upload-resource-with-retry.md`
- `docs/agent-docs/drive-queue-upload.md`
- `docs/agent-docs/drive-upload-path.md`
- `docs/agent-docs/drive-download-resource.md`

## Source Pointers

- `src/drive.ml`: `upload`
- `src/drive.ml`: `update_cached_resource_state_and_size`
- `src/drive.ml`: `update_resource_from_file`
- `src/drive.ml`: `update_cached_resource`
- `src/drive.ml`: `build_resource_keys_header_from_resource`
