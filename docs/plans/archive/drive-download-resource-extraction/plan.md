# Extract Drive Download Resource

## Goal

Move the local-content materialization policy out of `src/drive.ml` into a
testable module that follows the current functorized runtime pattern used by
`DriveOpens`, `DriveReads`, `DriveFileMutations`, `DriveUploadDispatch`,
`DriveMetadataMutations`, `DriveMutations`, `DriveViews`, and `DriveXattrs`.

The extraction target is:

- `Drive.download_resource`
- the document desktop-format predicate used by `download_resource`

The production behavior should stay unchanged. `src/drive.ml` should retain the
Drive API, filesystem, locking, cache, retry, and logging adapters, while the
new module owns the state machine and branch policy that decides whether and how
local content is materialized.

## Current Problem

`download_resource` is one of the remaining policy-heavy blocks in
`src/drive.ml`. It is the shared helper that turns an already-resolved
`CacheData.Resource.t` into a usable local cache file path.

It currently mixes several concerns in one function:

- local content-path resolution
- cache state reload by remote id
- state-based reuse vs refresh decisions
- MD5 shortcut handling for `ToDownload`
- per-resource locking through `Context.file_locks`
- cache shrinking before materialization
- document-specific cache-size accounting
- desktop-entry and HTML redirect synthesis
- Google document export selection
- ordinary media downloads
- zero-byte local file creation
- failure recovery from `Downloading` back to `ToDownload`
- exponential backoff while another thread is downloading
- final transition to `Synchronized`

That makes it hard to unit test the behavior without real `Context`, cache
files, mutexes, Drive API requests, or long waits.

## Proposed Shape

Add:

- `src/driveDownloads.ml`
- `src/driveDownloads.mli`
- `test/testDriveDownloads.ml`

Expose:

```ocaml
exception File_not_found

type runtime = {
  cache : CacheData.t;
  config : Config.t;
}

module type PORTS = sig
  val get_content_path : CacheData.t -> CacheData.Resource.t -> string

  val select_first_resource_with_remote_id :
    CacheData.t -> string -> CacheData.Resource.t option

  val file_exists : string -> bool
  val check_md5_checksum : CacheData.Resource.t -> CacheData.t -> bool

  val update_cached_resource_state :
    CacheData.t -> CacheData.Resource.State.t -> int64 -> unit

  val update_cache_size_for_documents :
    CacheData.t -> CacheData.Resource.t -> string -> (int64 -> int64) -> unit

  val shrink_cache : ?file_size:int64 -> unit -> unit

  val with_resource_lock :
    CacheData.Resource.t -> unit GapiMonad.SessionM.m -> unit GapiMonad.SessionM.m

  val create_desktop_entry :
    CacheData.Resource.t -> string -> Config.t -> unit

  val create_html_with_redirect :
    CacheData.Resource.t -> string -> Config.t -> unit

  val download_export_link_to_file :
    string -> string -> unit GapiMonad.SessionM.m

  val export_document_to_file :
    string -> file_id:string -> mime_type:string -> unit GapiMonad.SessionM.m

  val download_media_to_file :
    string -> CacheData.Resource.t -> unit GapiMonad.SessionM.m

  val create_empty_file : string -> unit
  val wait_exponential_backoff : int -> unit

  val handle_download_exception : exn -> 'a GapiMonad.SessionM.m
end

val is_desktop_format : Config.t -> CacheData.Resource.t -> bool

module Make (P : PORTS) : sig
  val download_resource :
    runtime -> CacheData.Resource.t -> string GapiMonad.SessionM.m
end
```

`DriveDownloads` should own the state-machine policy. Production ports should
own real cache access, filesystem I/O, GAPI calls, locking primitives, wait
behavior, and exception mapping.

## Production Wiring

In `src/drive.ml`, add a ports module near the current `download_resource`
helpers:

```ocaml
module DriveDownloadPorts = struct
  let get_content_path = Cache.get_content_path

  let select_first_resource_with_remote_id cache remote_id =
    Cache.Resource.select_first_resource_with_remote_id cache remote_id

  let file_exists = Sys.file_exists
  let check_md5_checksum = check_md5_checksum
  let update_cached_resource_state = update_cached_resource_state
  let update_cache_size_for_documents = update_cache_size_for_documents
  let shrink_cache ?file_size () = shrink_cache ?file_size ()

  let with_resource_lock resource request =
    let mutex =
      Context.with_ctx_lock (fun () ->
          let context = Context.get_ctx () in
          let remote_id = resource.CacheData.Resource.remote_id |> Option.get in
          match Utils.safe_find context.Context.file_locks remote_id with
          | None ->
              let mutex = Mutex.create () in
              Hashtbl.add context.Context.file_locks remote_id mutex;
              mutex
          | Some mutex -> mutex)
    in
    Utils.with_lock_m mutex request

  let create_desktop_entry = create_desktop_entry
  let create_html_with_redirect = create_html_with_redirect

  let download_export_link_to_file content_path export_link =
    let destination = GapiMediaResource.TargetFile content_path in
    let media_download = { GapiMediaResource.destination; range_spec = "" } in
    GapiService.get ~media_download export_link GapiRequest.parse_empty_response

  let export_document_to_file content_path ~file_id ~mime_type =
    let destination = GapiMediaResource.TargetFile content_path in
    let media_download = { GapiMediaResource.destination; range_spec = "" } in
    with_retry_default
      (FilesResource.export ~media_download ~fileId:file_id ~mimeType:mime_type)
    >>= fun () -> SessionM.return ()

  let download_media_to_file content_path resource =
    let destination = GapiMediaResource.TargetFile content_path in
    let media_download = { GapiMediaResource.destination; range_spec = "" } in
    download_media media_download resource >>= fun _ -> SessionM.return ()

  let create_empty_file content_path =
    close_out (open_out content_path)

  let wait_exponential_backoff = GapiUtils.wait_exponential_backoff
  let handle_download_exception = handle_default_exceptions
end
```

Instantiate:

```ocaml
module DownloadOps = DriveDownloads.Make (DriveDownloadPorts)
```

Add:

```ocaml
let drive_download_runtime () =
  let context = Context.get_ctx () in
  {
    DriveDownloads.cache = context.Context.cache;
    config = context |. Context.config_lens;
  }
```

Replace `download_resource` with:

```ocaml
let download_resource resource =
  DownloadOps.download_resource (drive_download_runtime ()) resource
```

Keep `with_retry download_resource resource` call sites unchanged.

Keep `download_media` in `src/drive.ml`. It is also used by streaming reads,
and its abusive-file retry behavior is a production Drive API adapter rather
than materialization state-machine policy.

## Extracted Behavior

The extracted `download_resource` should preserve the current flow.

### Content Path

Compute:

```ocaml
let content_path = P.get_content_path runtime.cache resource
```

Return `content_path` on every successful path.

### State Reload

Reload the current resource by remote id on each check:

```ocaml
Option.map_default
  (P.select_first_resource_with_remote_id runtime.cache)
  (Some resource)
  resource.CacheData.Resource.remote_id
```

This preserves the current fallback to the original resource when `remote_id =
None`.

### Reuse States

For `Synchronized`, `ToUpload`, and `Uploading`:

- return immediately if `P.file_exists content_path`
- otherwise run locked materialization

This must preserve the current dirty-resource behavior: local dirty content is
authoritative when the cache file exists.

### `ToDownload`

For `ToDownload`, first check:

```ocaml
P.check_md5_checksum reloaded_resource runtime.cache
```

If true:

- mark the original resource id `Synchronized`
- return without materializing

If false:

- run locked materialization

### `Downloading`

For `Downloading`:

- if `n <= 300`, wait with `P.wait_exponential_backoff (min n 6)` and retry
- if `n > 300`, fall back to the same MD5-check-or-materialize logic used for
  `ToDownload`

The tests should use a fake wait port so this path is fast.

### `NotFound`

Raise `File_not_found`.

The new module should alias the existing exception identity:

```ocaml
exception File_not_found = DriveMutations.File_not_found
```

## Materialization Behavior

Actual refresh should run through:

```ocaml
P.with_resource_lock resource do_download
```

Inside `do_download`, preserve this ordering:

1. shrink cache for `resource.size`, defaulting to `0L`
2. branch by materialization kind
3. update document cache-size accounting after materialization
4. mark the original resource id `Synchronized`

### Desktop-Format Documents

If `is_desktop_format runtime.config resource`:

- shrink cache before writing
- subtract document cache-size accounting with `Int64.neg`
- create HTML redirect when `runtime.config.Config.desktop_entry_as_html = true`
- otherwise create a desktop entry
- do not set state to `Downloading` before writing
- finish by adding document cache-size accounting and setting `Synchronized`

### Exportable Documents

For Google documents that are not desktop-format:

- shrink cache before downloading
- set state to `Downloading`
- subtract document cache-size accounting with `Int64.neg`
- compute the configured format with `CacheData.Resource.get_format`
- compute the export MIME type with `CacheData.Resource.mime_type_of_format`
- parse cached export links with `CacheData.Resource.parse_export_links`
- if the MIME type is present, call `P.download_export_link_to_file`
- otherwise call `P.export_document_to_file`
- on failure after setting `Downloading`, restore state to `ToDownload` before
  calling `P.handle_download_exception`
- finish by adding document cache-size accounting and setting `Synchronized`

### Ordinary Non-Empty Files

For non-document resources with `size > 0`:

- shrink cache before downloading
- set state to `Downloading`
- subtract document cache-size accounting; this should be a no-op for ordinary
  nonzero files
- call `P.download_media_to_file content_path resource`
- on failure after setting `Downloading`, restore state to `ToDownload` before
  calling `P.handle_download_exception`
- finish by adding document cache-size accounting and setting `Synchronized`

### Zero-Byte Non-Document Files

For non-document resources with `size <= 0` or missing size:

- shrink cache before creating the file
- set state to `Downloading`
- subtract document cache-size accounting; this should be a no-op
- call `P.create_empty_file content_path`
- finish by adding document cache-size accounting and setting `Synchronized`

## Unit Test Plan

Use fake ports with a trace log, following the style in
`test/testDriveReads.ml`, `test/testDriveOpens.ml`, and
`test/testDriveFileMutations.ml`.

Cover state-based reuse:

- `Synchronized` with existing content returns `content_path` and does not lock
  or materialize
- `ToUpload` with existing content returns `content_path` and does not lock or
  materialize
- `Uploading` with existing content returns `content_path` and does not lock or
  materialize
- the same states with missing content run locked materialization

Cover `ToDownload`:

- MD5 match marks the original resource id `Synchronized` and skips
  materialization
- MD5 mismatch runs locked materialization

Cover `Downloading`:

- waits with capped backoff values and polls again while state remains
  `Downloading`
- after state changes to `Synchronized` with content, returns without
  materializing
- after the stuck threshold, falls back to MD5-check-or-materialize

Cover `NotFound`:

- raises `DriveDownloads.File_not_found`
- does not lock or materialize

Cover materialization branches:

- desktop document with `desktop_entry_as_html = false` creates desktop entry,
  does not set `Downloading`, and then marks `Synchronized`
- desktop document with `desktop_entry_as_html = true` creates HTML redirect
- exportable document with a cached export link uses
  `download_export_link_to_file`
- exportable document without a matching link uses `export_document_to_file`
- ordinary non-empty file uses `download_media_to_file`
- zero-byte ordinary file uses `create_empty_file`

Cover ordering:

- lock wraps only materialization, not the initial state check
- shrink happens before materialization
- document cache-size subtraction happens before replacing document content
- document cache-size addition happens after materialization
- final state is set to `Synchronized` after successful materialization
- failed API materialization restores `ToDownload` before delegating to
  `handle_download_exception`

Cover return values:

- every successful branch returns the content path from `P.get_content_path`
- reloaded resources are selected by `resource.remote_id`
- when `remote_id = None`, state checks use the original resource

## Acceptance Criteria

- `src/drive.ml` no longer contains the `download_resource` state-machine
  branch logic.
- The public behavior of callers using `with_retry download_resource resource`
  remains unchanged.
- Desktop documents, exportable documents, ordinary media files, zero-byte
  files, dirty-state reuse, MD5 shortcut reuse, `Downloading` waits, stuck
  download recovery, and `NotFound` are unit tested without real `Context`,
  Drive API requests, cache files, locks, or sleeps.
- `download_media` remains available in `src/drive.ml` for streaming reads.
- `dune build @install` and `dune runtest` pass.

## Agent Docs Follow-Up

After implementation, update the agent docs to describe the current state
directly:

- `docs/agent-docs/drive-download-resource.md`
- `docs/agent-docs/architecture.md`
- `docs/agent-docs/repo-map.md`
- `docs/agent-docs/README.md`

Avoid before/after language. The docs should describe `DriveDownloads` as the
current implementation boundary for local content materialization.
