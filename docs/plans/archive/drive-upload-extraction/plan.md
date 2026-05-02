# Extract Drive Upload

## Goal

Move the concrete `Drive.upload` attempt out of `src/drive.ml` into a small
testable module that follows the current functorized runtime pattern used by
`DriveDownloads`, `DriveRemoteUpdates`, `DriveFileMutations`,
`DriveUploadDispatch`, `DriveMetadataMutations`, `DriveMutations`, and
`DriveXattrs`.

The extraction target is:

- `Drive.upload`

The production behavior should stay unchanged. `src/drive.ml` should retain
the authenticated request wrappers, retry wrappers, async-worker bridge, and
Google Drive API adapters, while the new module owns the policy for one upload
attempt and its cache reconciliation.

## Current Problem

`Drive.upload` is the remaining dense block in the upload pipeline. It is
already below the path-resolution, dirty-state, queueing, and retry layers, but
it still mixes several behaviors in one production-only function:

- cache content-path resolution
- outgoing MIME type selection
- local media-source construction
- upload size detection from the cache file
- early cache state/size transition to `Uploading`
- zero-byte media omission
- Drive metadata patch construction
- resource-key header construction
- retried `FilesResource.update`
- immediate cache-row rebuild from the returned Drive file
- reload-by-remote-id before final cache write
- conditional return from `Uploading` to `Synchronized`
- preservation of newer local states such as `ToUpload`
- final cache shrink

Today those behaviors can only be tested indirectly through the broader upload
pipeline or with real production adapters.

## Scope

This plan targets the concrete upload attempt only:

- extract `Drive.upload`
- keep `Drive.upload_resource_with_retry` as the Drive-level flush,
  `try_with_default`, and retry wrapper
- keep `Drive.upload_resource_by_id` as the Drive-level async-worker bridge
- keep `DriveUploadDispatch` unchanged

After this extraction, the upload path should still look like:

```text
DriveUploadDispatch.queue_upload
  -> Drive.upload_resource_with_retry
  -> DriveUploads.Make(...).upload
```

`upload_resource_with_retry` should continue to call `flush_memory_buffers`
before the concrete upload attempt.

## Proposed Shape

Add:

- `src/driveUploads.ml`
- `src/driveUploads.mli`
- `test/testDriveUploads.ml`

Expose:

```ocaml
type runtime = {
  cache : CacheData.t;
  config : Config.t;
}

module type PORTS = sig
  val get_content_path : CacheData.t -> CacheData.Resource.t -> string

  val create_file_resource :
    ?content_type:string -> string -> GapiMediaResource.t

  val media_content_type : GapiMediaResource.t -> string
  val media_content_length : GapiMediaResource.t -> int64

  val update_cached_resource_state_and_size :
    CacheData.t ->
    CacheData.Resource.State.t ->
    int64 ->
    int64 ->
    unit

  val build_resource_keys_header_from_resource :
    CacheData.Resource.t -> GapiCore.Header.t list

  val now : unit -> Netdate.t

  val remote_update :
    media_source:GapiMediaResource.t option ->
    custom_headers:GapiCore.Header.t list ->
    fileId:string ->
    GapiDriveV3Model.File.t ->
    GapiDriveV3Model.File.t GapiMonad.SessionM.m

  val update_resource_from_file :
    ?state:CacheData.Resource.State.t ->
    CacheData.Resource.t ->
    GapiDriveV3Model.File.t ->
    CacheData.Resource.t

  val select_first_resource_with_remote_id :
    CacheData.t -> string -> CacheData.Resource.t option

  val update_cached_resource : CacheData.t -> CacheData.Resource.t -> unit
  val shrink_cache : unit -> unit
end

module Make (P : PORTS) : sig
  val content_type_for_upload :
    runtime -> CacheData.Resource.t -> string -> string

  val next_state_after_upload :
    CacheData.Resource.t -> CacheData.Resource.State.t option

  val upload :
    runtime -> CacheData.Resource.t -> unit GapiMonad.SessionM.m
end
```

`content_type_for_upload` and `next_state_after_upload` should be exposed from
the functor result because they are small policy helpers worth testing directly
when useful. If the implementation is clearer with private helpers only, the
same behavior can still be covered through `upload`.

## Production Wiring

In `src/drive.ml`, add a ports module near the current `upload` block:

```ocaml
module DriveUploadPorts = struct
  let get_content_path = Cache.get_content_path
  let create_file_resource = GapiMediaResource.create_file_resource
  let media_content_type media = media |. GapiMediaResource.content_type
  let media_content_length media = media.GapiMediaResource.content_length

  let update_cached_resource_state_and_size =
    update_cached_resource_state_and_size

  let build_resource_keys_header_from_resource =
    build_resource_keys_header_from_resource

  let now = GapiDate.now

  let remote_update ~media_source ~custom_headers ~fileId file_patch =
    with_retry_default
      (FilesResource.update ~enforceSingleParent:true ~supportsAllDrives:true
         ~std_params:file_std_params ?media_source ~custom_headers
         ~fileId file_patch)

  let update_resource_from_file ?state resource file =
    update_resource_from_file ?state resource file

  let select_first_resource_with_remote_id =
    Cache.Resource.select_first_resource_with_remote_id

  let update_cached_resource = update_cached_resource
  let shrink_cache = shrink_cache
end

module UploadOps = DriveUploads.Make (DriveUploadPorts)

let drive_upload_runtime () =
  let context = Context.get_ctx () in
  {
    DriveUploads.cache = context.Context.cache;
    config = context |. Context.config_lens;
  }
```

Replace the current `upload` body with:

```ocaml
let upload resource =
  UploadOps.upload (drive_upload_runtime ()) resource
```

Keep the wrappers below it unchanged except for calling the thin `upload`:

```ocaml
let upload_resource_with_retry resource =
  flush_memory_buffers resource;
  with_retry (fun r -> try_with_default (upload r)) resource
```

## Extracted Behavior

The extracted implementation should preserve the current flow.

### Content Path

Start by computing:

```ocaml
let content_path = P.get_content_path runtime.cache resource
```

That path is the source file for the upload attempt.

### MIME Type Selection

Preserve the current content-type branch order.

#### Editable Google Documents

If:

```ocaml
CacheData.Resource.is_document resource && runtime.config.Config.editable_docs
```

then choose:

```ocaml
let fmt = CacheData.Resource.get_format resource runtime.config in
CacheData.Resource.mime_type_of_format fmt
```

This branch should not create a preliminary media resource for MIME
autodetection.

#### Autodetect MIME

If `runtime.config.Config.autodetect_mime = true`, choose the empty string:

```ocaml
""
```

The final media resource should still be created with that content type, which
preserves the current explicit-empty-content-type behavior.

#### Cached Resource MIME Fallback

If autodetect is disabled and the resource is not an editable document:

1. create a preliminary media resource with `P.create_file_resource content_path`
2. read its detected MIME type through `P.media_content_type`
3. read the cached resource MIME type with `Option.get`
4. prefer the cached resource MIME type when it is non-empty
5. otherwise use the detected MIME type

This preserves the existing workaround for keeping uploads aligned with cached
metadata when possible.

### Final Media Resource

After choosing `content_type`, build the actual upload source:

```ocaml
let file_source =
  P.create_file_resource ~content_type content_path
```

Then use:

```ocaml
let size = P.media_content_length file_source
```

The upload size must come from the media source, not from
`resource.CacheData.Resource.size`.

### Early Cache State And Size Update

Before the remote request, call:

```ocaml
P.update_cached_resource_state_and_size runtime.cache
  CacheData.Resource.State.Uploading
  size
  resource.CacheData.Resource.id
```

This must happen even for zero-byte files and before `P.remote_update`.

### Remote Id And Zero-Byte Uploads

Preserve the current precondition:

```ocaml
let remote_id = resource.CacheData.Resource.remote_id |> Option.get
```

For the media body:

```ocaml
let media_source =
  if size = 0L then None else Some file_source
```

So zero-byte uploads still send only the metadata patch and omit the media
source.

### Drive Patch And Request

Build the file patch as:

```ocaml
let file_patch =
  GapiDriveV3Model.File.empty
  |> GapiDriveV3Model.File.modifiedTime ^= P.now ()
```

Build headers through:

```ocaml
P.build_resource_keys_header_from_resource resource
```

Then call:

```ocaml
P.remote_update ~media_source ~custom_headers ~fileId:remote_id file_patch
```

The production port should be the only place that mentions
`FilesResource.update`, `file_std_params`, `with_retry_default`, or
`supportsAllDrives`.

### Immediate Rebuild And Final Reload

After the remote request returns `file`, preserve the current two-step
reconciliation:

1. rebuild the original resource once from the returned file
2. try to reload the current cached row by `file.File.id`

In outline:

```ocaml
let resource = P.update_resource_from_file resource file in
let reloaded_resource =
  P.select_first_resource_with_remote_id runtime.cache file.File.id
in
let resource = Option.default resource reloaded_resource
```

This preserves the chance to reconcile against a newer local cache row for the
same remote id.

### Conditional Return To `Synchronized`

Preserve the state guard:

```ocaml
let state =
  match resource.CacheData.Resource.state with
  | CacheData.Resource.State.Uploading ->
      Some CacheData.Resource.State.Synchronized
  | _ -> None
```

The final cache row should be rebuilt with:

```ocaml
let updated_resource =
  P.update_resource_from_file ?state resource file
```

This means:

- `Uploading` becomes `Synchronized`
- newer states such as `ToUpload` are preserved

### Final Cache Write And Shrink

Finish with:

```ocaml
P.update_cached_resource runtime.cache updated_resource;
P.shrink_cache ();
SessionM.return ()
```

`P.shrink_cache` should run only after the final cache update succeeds.

### Exception Behavior

The extracted `upload` should not add local exception handling.

Existing behavior should remain:

- exceptions from content-path/media construction propagate
- exceptions from `P.remote_update` propagate after the early state/size update
- exceptions before final reconciliation do not run `update_cached_resource` or
  `shrink_cache`

Failure normalization and retry remain in `upload_resource_with_retry`.

## Unit Test Plan

Use fake ports with a trace log, following the style in
`test/testDriveDownloads.ml`, `test/testDriveRemoteUpdates.ml`, and
`test/testDriveUploadDispatch.ml`.

Cover MIME selection:

- editable Google document uses `CacheData.Resource.get_format` and
  `mime_type_of_format`
- editable document MIME selection does not create a preliminary media resource
- `autodetect_mime = true` chooses `""`
- `autodetect_mime = true` creates the final media resource with
  `content_type = ""`
- `autodetect_mime = false` with cached non-empty MIME prefers cached MIME
- `autodetect_mime = false` with cached empty MIME falls back to detected MIME

Cover media-source and request shape:

- final media resource is created from `content_path`
- upload size comes from the final media resource
- early state/size update uses `Uploading`, media length, and resource id
- non-empty files pass `Some media_source` to `remote_update`
- zero-byte files pass `None` to `remote_update`
- remote update receives the resource-key headers and remote id
- file patch receives the injected `now ()` value

Cover ordering:

- content path lookup happens before media construction
- state/size update happens before `remote_update`
- final `update_cached_resource` happens before `shrink_cache`

Cover cache reconciliation:

- when no reloaded resource exists, final update is based on the rebuilt
  original resource
- when a reloaded `Uploading` resource exists, final state becomes
  `Synchronized`
- when a reloaded `ToUpload` resource exists, final state remains `ToUpload`
- `update_resource_from_file` is called for the immediate rebuild and again for
  the final cache row
- returned Drive file id is used for the reload-by-remote-id lookup

Cover exception behavior:

- media construction failure propagates without state/size update, remote
  update, final cache update, or shrink
- remote update failure propagates after state/size update and without final
  cache update or shrink
- final cache update failure propagates without shrink

Do not test `upload_resource_with_retry` in this new suite unless the
implementation moves that wrapper too. Its existing behavior is documented in
`docs/agent-docs/drive-upload-resource-with-retry.md` and remains a separate
Drive-level wrapper in this plan.

## Acceptance Criteria

- `src/drive.ml` no longer contains the concrete `upload` branch logic.
- `Drive.upload` remains available as a thin compatibility helper with the same
  signature.
- `upload_resource_with_retry`, `upload_resource_by_id`, `DriveUploadDispatch`,
  and `UploadQueue` continue to call the same downstream behavior.
- MIME selection, zero-byte media omission, early state/size update, remote
  update request shape, reload-by-remote-id behavior, conditional
  `Synchronized` transition, final cache write, and cache shrink are unit
  tested without real `Context`, Drive API requests, cache files, or filesystem
  checks.
- `dune build @install` and `dune runtest` pass.

## Agent Docs Follow-Up

After implementation, update the agent docs to describe the current state
directly:

- `docs/agent-docs/drive-upload.md`
- `docs/agent-docs/drive-upload-resource-with-retry.md`
- `docs/agent-docs/drive-upload-resource-by-id.md`
- `docs/agent-docs/drive-upload-path.md`
- `docs/agent-docs/drive-queue-upload.md`
- `docs/agent-docs/architecture.md`
- `docs/agent-docs/repo-map.md`
- `docs/agent-docs/README.md`

Avoid before/after language. The docs should describe `DriveUploads` as the
current implementation boundary for the concrete upload attempt, while keeping
`upload_resource_with_retry`, `upload_resource_by_id`, and
`DriveUploadDispatch` documented as the surrounding wrappers.
