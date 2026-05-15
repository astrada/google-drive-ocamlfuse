# Extract Drive Cache Maintenance

## Goal

Move cache-size accounting and cached-resource cleanup policy out of
`src/drive.ml` into a focused, functorized `DriveCacheMaintenance` module.

The extraction target is:

- `update_cache_size`
- `shrink_cache`
- `delete_memory_buffers`
- `delete_from_context`
- `delete_cached_resource`
- `delete_cached_resources`
- `update_cache_size_for_documents`

The production behavior should stay unchanged. `src/drive.ml` should keep thin
wrappers for the existing helper shapes because many existing ports already
depend on them.

## Current Problem

Cache maintenance is still one of the denser side-effect blocks in
`src/drive.ml`. It mixes several concerns that are individually important but
hard to test together:

- metadata cache-size accounting in the cache database and global `Context`
- metadata-lock ownership for shrink and document-size adjustment paths
- cache eviction selection and state transitions during shrink
- local cached-file deletion and returned-size accounting
- memory-buffer cleanup for remote ids
- file-lock table cleanup for remote ids
- optional metadata behavior during single-resource deletion
- document and desktop-entry cache-size correction after materialization
- exception logging when document cache-size stat calls fail

The helper block is used by several extracted modules through production ports:

- `DriveDownloads` calls `shrink_cache` and
  `update_cache_size_for_documents`
- `DriveFileMutations` calls `shrink_cache` after writes and truncates
- `DriveUploads` calls `shrink_cache` after upload reconciliation
- `DriveResourceResolver` calls `delete_cached_resource` for stale orphan rows
- `DriveMutations` calls `delete_cached_resource` during mutation cleanup
- `DriveMetadataRefresh` calls `delete_cached_resources` when the change feed
  reports deleted remote files
- `DriveMutationPorts.replace_target_contents` updates cache-size accounting
  after copying replacement content

This means regressions in cache accounting can surface through reads, writes,
uploads, metadata refresh, and delete/rename behavior.

## Proposed Shape

Add:

- `src/driveCacheMaintenance.ml`
- `src/driveCacheMaintenance.mli`
- `test/testDriveCacheMaintenance.ml`

Expose:

```ocaml
type runtime = {
  cache : CacheData.t;
  config : Config.t;
  metadata : CacheData.Metadata.t option;
}

module type PORTS = sig
  val with_metadata_lock : (unit -> 'a) -> 'a

  val update_cache_size_in_db : CacheData.t -> int64 -> unit
  val update_context_metadata :
    (CacheData.Metadata.t -> CacheData.Metadata.t) -> unit

  val select_resources_order_by_last_update :
    CacheData.t -> CacheData.Resource.t list

  val update_cached_resource_state :
    CacheData.t -> CacheData.Resource.State.t -> int64 -> unit

  val delete_files_from_cache :
    CacheData.t -> CacheData.Resource.t list -> int64

  val delete_resource : CacheData.t -> CacheData.Resource.t -> unit
  val delete_resources : CacheData.t -> CacheData.Resource.t list -> unit

  val remove_memory_buffers : string -> unit
  val remove_file_lock : string -> unit

  val file_exists : string -> bool
  val stat_file : string -> Unix.LargeFile.stats
  val log_exception : exn -> unit
end

module Make (P : PORTS) : sig
  val update_cache_size :
    int64 -> CacheData.Metadata.t -> CacheData.t -> unit

  val shrink_cache : runtime -> ?file_size:int64 -> unit -> unit

  val delete_memory_buffers : CacheData.Resource.t -> unit
  val delete_from_context : CacheData.Resource.t -> unit

  val delete_cached_resource : runtime -> CacheData.Resource.t -> unit

  val delete_cached_resources :
    runtime -> CacheData.Metadata.t -> CacheData.Resource.t list -> unit

  val update_cache_size_for_documents :
    runtime ->
    CacheData.Resource.t ->
    string ->
    (int64 -> int64) ->
    unit
end
```

The exact public names can stay close to the current `Drive` helper names.
`runtime.metadata` is optional because the current single-resource deletion path
only updates cache-size accounting when `Context.metadata` is present.
`shrink_cache` and `update_cache_size_for_documents` should preserve their
current metadata requirements by requiring metadata from the runtime and raising
the same kind of option failure if metadata is unexpectedly unavailable.

## Extracted Behavior

### Cache-Size Update

`update_cache_size delta metadata cache` should preserve the current behavior:

- log the requested delta
- if `delta = 0L`, do not update the cache database or context metadata
- otherwise call `P.update_cache_size_in_db cache delta`
- compute the new cache size from the supplied `metadata.cache_size + delta`
- update global metadata through `P.update_context_metadata`, preserving the
  current atomic `Context.update_ctx` shape

The production wrapper should preserve the current `Drive.update_cache_size`
call shape because `replace_target_contents` currently calls it directly.
The context update should preserve the current context metadata fields while
changing only `cache_size` to the computed value, matching the current
`Context.update_ctx` callback.

### Shrink

`shrink_cache runtime ?file_size ()` should preserve the current flow:

- run under `P.with_metadata_lock`
- read metadata from `runtime.metadata` using the same required-metadata
  behavior as the current `Context.metadata_lens` call
- compute `max_cache_size = config.max_cache_size_mb * Utils.mb`
- compute `target_size = metadata.cache_size + file_size`
- if `target_size <= max_cache_size`, call `update_cache_size file_size`
- otherwise select resources ordered by last update
- fold through that list until the projected size is under the max
- include the original `file_size` in the final accounting delta
- call `update_cache_size total_delta` before changing selected resource states
- mark selected resources `ToDownload`
- call `P.delete_files_from_cache cache resources_to_free` and ignore the
  returned size, matching the current shrink path

The extraction should not change the existing behavior for negative
`file_size` values. Truncate can call `shrink_cache ~file_size:(size - old_size)`
with a negative delta, and the helper should continue to update metadata rather
than forcing eviction.

### Context Cleanup

`delete_memory_buffers resource` and `delete_from_context resource` should
preserve the remote-id checks:

- do nothing when `resource.remote_id = None`
- when `Some remote_id`, remove memory buffers for that remote id
- when `Some remote_id`, remove the file-lock table entry for that remote id

The production ports should keep the current `Context.with_ctx_lock` behavior
around file-lock removal.

### Cached Resource Deletion

`delete_cached_resource runtime resource` should preserve the current order:

- delete the cache DB row with `P.delete_resource runtime.cache resource`
- delete local cached files with `P.delete_files_from_cache runtime.cache
  [ resource ]`
- if `runtime.metadata = Some metadata`, call `update_cache_size` with the
  negative returned file size
- clean memory buffers and file locks for the resource

`delete_cached_resources runtime metadata resources` should preserve the batch
flow:

- delete cache DB rows with `P.delete_resources runtime.cache resources`
- delete local cached files with `P.delete_files_from_cache runtime.cache
  resources`
- call `update_cache_size` with the negative returned file size
- clean memory buffers and file locks for every resource

### Document Cache-Size Adjustment

`update_cache_size_for_documents runtime resource content_path op` should
preserve the current intentionally narrow condition:

- run under `P.with_metadata_lock`
- read metadata from `runtime.metadata` using the same required-metadata
  behavior as the current `Context.metadata_lens` call
- only act when `resource.size = Some 0L`
- only act when `P.file_exists content_path`
- stat `content_path`
- compute `delta = op stats.st_size`
- call `update_cache_size delta`
- log and swallow exceptions raised by the stat/accounting block

Do not rename or broaden the helper during this pass. Even though the helper is
called "for documents", the current condition is based on cached resource size
and local file existence, and the extraction should keep that behavior.

## Production Wiring

In `src/drive.ml`, add a ports module near the existing resource-cache helpers:

```ocaml
module DriveCacheMaintenancePorts = struct
  let with_metadata_lock f =
    let context = Context.get_ctx () in
    Utils.with_lock context.Context.metadata_lock f

  let update_cache_size_in_db = Cache.Metadata.update_cache_size

  let update_context_metadata f =
    Context.update_ctx (fun context ->
        let metadata = f (context.Context.metadata |. GapiLens.option_get) in
        context |> Context.metadata ^= Some metadata)

  let select_resources_order_by_last_update =
    Cache.Resource.select_resources_order_by_last_update

  let update_cached_resource_state = update_cached_resource_state
  let delete_files_from_cache = Cache.delete_files_from_cache
  let delete_resource = Cache.Resource.delete_resource
  let delete_resources = Cache.Resource.delete_resources

  let remove_memory_buffers remote_id =
    let context = Context.get_ctx () in
    Buffering.MemoryBuffers.remove_buffers remote_id context.Context.memory_buffers

  let remove_file_lock remote_id =
    Context.with_ctx_lock (fun () ->
        let context = Context.get_ctx () in
        Hashtbl.remove context.Context.file_locks remote_id)

  let file_exists = Sys.file_exists
  let stat_file = Unix.LargeFile.stat
  let log_exception = Utils.log_exception
end
```

Instantiate:

```ocaml
module CacheMaintenanceOps =
  DriveCacheMaintenance.Make (DriveCacheMaintenancePorts)
```

Add:

```ocaml
let drive_cache_maintenance_runtime () =
  let context = Context.get_ctx () in
  {
    DriveCacheMaintenance.cache = context.Context.cache;
    config = context |. Context.config_lens;
    metadata = context.Context.metadata;
  }
```

Then keep the existing helper names as thin wrappers:

```ocaml
let update_cache_size delta metadata cache =
  CacheMaintenanceOps.update_cache_size delta metadata cache

let shrink_cache ?file_size () =
  CacheMaintenanceOps.shrink_cache
    (drive_cache_maintenance_runtime ()) ?file_size ()

let delete_cached_resource resource =
  CacheMaintenanceOps.delete_cached_resource
    (drive_cache_maintenance_runtime ()) resource

let delete_cached_resources metadata cache resources =
  let runtime =
    { (drive_cache_maintenance_runtime ()) with
      DriveCacheMaintenance.cache = cache
    }
  in
  CacheMaintenanceOps.delete_cached_resources runtime metadata resources

let update_cache_size_for_documents cache resource content_path op =
  let runtime =
    { (drive_cache_maintenance_runtime ()) with
      DriveCacheMaintenance.cache = cache
    }
  in
  CacheMaintenanceOps.update_cache_size_for_documents runtime resource
    content_path op
```

If OCaml record punning makes the cache override unclear, prefer a small helper
that accepts an explicit cache and builds the runtime. The wrappers should stay
easy to audit because other production ports call them directly.

## Implementation Steps

1. Create `driveCacheMaintenance.mli` with `runtime`, `PORTS`, and functor
   signatures.
2. Create `driveCacheMaintenance.ml`.
3. Move `update_cache_size` into the new module and keep the `Drive` wrapper.
4. Move `shrink_cache` into the new module with explicit runtime and ports.
5. Move memory-buffer and file-lock cleanup into the new module behind
   `remove_memory_buffers` and `remove_file_lock` ports.
6. Move single and batch cached-resource deletion into the new module.
7. Move `update_cache_size_for_documents` into the new module.
8. Wire `DriveCacheMaintenance.Make` into `src/drive.ml`.
9. Preserve existing production helper names so existing ports in
   `DriveDownloads`, `DriveFileMutations`, `DriveUploads`,
   `DriveResourceResolver`, `DriveMutations`, and `DriveMetadataRefresh` do not
   need behavioral changes.
10. Add `test/testDriveCacheMaintenance.ml`.
11. Register the suite in `test/testSuite.ml`.
12. Run `ocamlformat` only on touched OCaml files.
13. Run `dune build @install` and `dune runtest` sequentially.

## Unit Test Plan

Use fake ports with trace lists and in-memory resource/metadata values.

Cover cache-size accounting:

- zero delta logs/skips DB and context updates
- positive delta updates DB before context metadata
- negative delta updates DB and context metadata with the reduced size
- context metadata receives the original metadata with only `cache_size`
  changed, while other fields come from the current context metadata

Cover shrink behavior:

- under-limit target only accounts for `file_size`
- exact-limit target only accounts for `file_size`
- over-limit target selects resources ordered by last update
- eviction stops as soon as projected size is under the max
- total delta includes both the new file size and freed resource sizes
- selected resources are marked `ToDownload`
- cached files are deleted after accounting and state updates
- negative `file_size` does not force eviction

Cover cleanup behavior:

- resources without `remote_id` do not remove buffers or file locks
- resources with `remote_id` remove both buffers and file locks
- single deletion deletes the DB row, deletes local files, updates metadata
  only when metadata is present, and then cleans runtime state
- batch deletion deletes DB rows, deletes local files, updates metadata with
  the negative returned size, and cleans every resource

Cover document-size adjustment:

- resources with size other than `Some 0L` are ignored
- missing local files are ignored
- existing zero-size resources stat the local file and apply `op`
- negative and identity `op` functions produce the expected deltas
- stat/accounting exceptions are logged and swallowed
- the helper runs under the metadata lock

Existing tests for downloads, writes/truncates, uploads, metadata refresh,
resource resolution, and mutations should continue to pass through the `Drive`
wrappers.

## Acceptance Criteria

- `src/drive.ml` no longer contains the cache-size/shrink/deletion policy
  bodies listed in this plan.
- Existing `Drive` helper names and call shapes remain available.
- Existing extracted modules do not need behavior changes to use cache
  maintenance.
- Focused unit tests cover accounting, shrink selection, deletion cleanup, and
  document-size adjustment without real `Context`, cache files, locks, or
  filesystem state.
- `dune build @install` and `dune runtest` pass.

## Agent Docs Follow-Up

After implementation, update agent docs to describe the current state directly:

- `docs/agent-docs/architecture.md`
- `docs/agent-docs/repo-map.md`
- `docs/agent-docs/drive-download-resource.md`
- `docs/agent-docs/drive-write.md`
- `docs/agent-docs/drive-truncate.md`
- `docs/agent-docs/drive-upload.md`
- `docs/agent-docs/drive-get-metadata.md`
- `docs/agent-docs/drive-get-resource.md`

Avoid before/after language. The docs should describe `DriveCacheMaintenance`
as the current implementation boundary for cache-size accounting, cache shrink,
cached-file deletion, and cache-resource cleanup.
