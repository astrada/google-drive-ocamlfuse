# Extract `get_metadata` Into `DriveMetadataRefresh`

## Summary

Extract the metadata freshness and change-feed reconciliation policy from
`src/drive.ml` into a functorized module, while keeping Context access, locks,
cache backends, Drive API request construction, retry wrappers, and
`do_request` in production ports.

Target module:

- `src/driveMetadataRefresh.ml`
- `src/driveMetadataRefresh.mli`
- `test/testDriveMetadataRefresh.ml`

Keep public behavior unchanged:

```ocaml
val Drive.get_metadata : unit -> CacheData.Metadata.t
```

`Drive.statfs`, `DriveResourceResolver`, and other callers should continue to
call `Drive.get_metadata`.

## Proposed Interface

```ocaml
module Change = GapiDriveV3Model.Change
module File = GapiDriveV3Model.File

type account_metadata = {
  display_name : string;
  storage_quota_limit : int64;
  storage_quota_usage : int64;
}

type runtime = {
  cache : CacheData.t;
  config : Config.t;
}

module type PORTS = sig
  val with_metadata_lock : (unit -> 'a) -> 'a
  val get_context_metadata : unit -> CacheData.Metadata.t option
  val set_context_metadata : CacheData.Metadata.t option -> unit

  val select_metadata : CacheData.t -> CacheData.Metadata.t option
  val insert_metadata : CacheData.t -> CacheData.Metadata.t -> unit
  val compute_cache_size : CacheData.t -> int64

  val metadata_is_valid : int -> CacheData.Metadata.t -> bool
  val now : unit -> float
  val run_request : 'a GapiMonad.SessionM.m -> 'a
  val with_default_retry : 'a GapiMonad.SessionM.m -> 'a GapiMonad.SessionM.m

  val request_account_metadata :
    unit -> account_metadata GapiMonad.SessionM.m

  val request_new_start_page_token : unit -> string GapiMonad.SessionM.m

  val probe_remaining_changes :
    start_page_token:string -> string GapiMonad.SessionM.m

  val list_changes :
    start_page_token:string ->
    (Change.t list * string) GapiMonad.SessionM.m

  val update_all_timestamps : CacheData.t -> float -> unit
  val invalidate_all_resources : CacheData.t -> unit
  val invalidate_resources : CacheData.t -> int64 list -> unit
  val invalidate_trash_bin : CacheData.t -> unit
  val invalidate_path : CacheData.t -> string -> unit

  val select_resources_with_remote_id :
    CacheData.t -> string -> CacheData.Resource.t list

  val trash_resources :
    CacheData.t -> CacheData.Resource.t list -> unit

  val delete_cached_resources :
    CacheData.Metadata.t -> CacheData.t -> CacheData.Resource.t list -> unit

  val build_resource_tables :
    string ->
    bool ->
    (string, int) Hashtbl.t * (string, CacheData.Resource.t) Hashtbl.t

  val get_unique_filename_from_file :
    File.t -> (string, int) Hashtbl.t -> string

  val create_resource : string -> CacheData.Resource.t

  val insert_resource_from_file :
    CacheData.t -> CacheData.Resource.t -> File.t -> CacheData.Resource.t

  val update_resource_from_file :
    CacheData.Resource.t -> File.t -> CacheData.Resource.t

  val update_cached_resource : CacheData.t -> CacheData.Resource.t -> unit

  val lost_and_found_directory : string
  val shared_with_me_directory : string
end

module Make (P : PORTS) : sig
  val get_metadata : runtime -> CacheData.Metadata.t
end
```

## Implementation Details

`DriveMetadataRefresh.get_metadata` should own:

- metadata lock orchestration through `P.with_metadata_lock`
- context-first metadata lookup
- DB metadata fallback and cache-size resync
- metadata validity decision
- refresh decision
- start-page-token policy
- metadata row construction with `clean_shutdown = false`
- no-change, over-limit, and incremental-change reconciliation branches
- final metadata persistence to DB and Context

`src/drive.ml` should keep:

- `AboutResource.get`
- `ChangesResource.getStartPageToken`
- `ChangesResource.list`
- `with_retry_default`
- `do_request`
- `changes_std_params`
- `change_limit`
- Context/cache production wiring

`Drive.get_metadata` becomes a thin wrapper:

```ocaml
module MetadataRefreshOps =
  DriveMetadataRefresh.Make (DriveMetadataRefreshPorts)

let drive_metadata_refresh_runtime () =
  let context = Context.get_ctx () in
  {
    DriveMetadataRefresh.cache = context.Context.cache;
    config = context |. Context.config_lens;
  }

let get_metadata () =
  MetadataRefreshOps.get_metadata (drive_metadata_refresh_runtime ())
```

## Behavior To Preserve

- Context metadata is preferred over DB metadata.
- DB-loaded metadata has `cache_size` recomputed before use.
- Valid metadata returns without Drive API calls.
- Refresh carries forward previous `cache_size`.
- Empty old start token requests a fresh start-page token.
- No-change branch updates all resource timestamps.
- Over-limit branch requests a new token and invalidates all invalidable
  resources.
- Normal replay updates timestamps before applying changes.
- First-time metadata creation does not replay a historical change list.
- New resources are added only when anchored under a cached synchronized parent.
- Unanchored new resources are ignored.
- Existing resources update only when the Drive version is newer.
- Updated resources are invalidated after metadata refresh.
- Trashed changes call `trash_resources`.
- Removed changes call `delete_cached_resources`.
- Synthetic trash, lost+found, and shared roots are invalidated only when
  changes were processed.
- Final refreshed metadata is inserted into cache and stored in Context.

## Test Plan

Add `test/testDriveMetadataRefresh.ml` with fake ports covering:

- valid Context metadata returns immediately
- missing Context metadata loads DB metadata, recomputes cache size, stores it in
  Context
- valid DB metadata returns without API calls
- missing metadata refreshes from Drive
- stale metadata with no changes updates all resource timestamps
- stale metadata with over-limit changes invalidates all resources and stores a
  fresh token
- normal incremental replay adds newly discoverable resources under
  synchronized parents
- unanchored new resources are ignored
- existing resources update only for newer Drive versions
- updated resources are invalidated after cache update
- trashed changes call `trash_resources`
- removed changes call `delete_cached_resources`
- synthetic roots invalidate when changes are processed, respecting
  `disable_trash` and `lost_and_found`
- first-time metadata creation skips full replay
- API/request exceptions propagate through `run_request`

## Documentation Updates

After implementation, update:

- `docs/agent-docs/drive-get-metadata.md`
- `docs/agent-docs/architecture.md`
- `docs/agent-docs/repo-map.md`
- any source pointers that still say `src/drive.ml` owns the metadata refresh
  policy

## Validation

Run:

```sh
dune build @install
dune runtest
git diff --check
```
