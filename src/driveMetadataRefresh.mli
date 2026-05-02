module Change = GapiDriveV3Model.Change
module File = GapiDriveV3Model.File

type account_metadata = {
  display_name : string;
  storage_quota_limit : int64;
  storage_quota_usage : int64;
}

type runtime = { cache : CacheData.t; config : Config.t }

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
  val request_account_metadata : unit -> account_metadata GapiMonad.SessionM.m
  val request_new_start_page_token : unit -> string GapiMonad.SessionM.m

  val probe_remaining_changes :
    start_page_token:string -> string GapiMonad.SessionM.m

  val list_changes :
    start_page_token:string -> (Change.t list * string) GapiMonad.SessionM.m

  val update_all_timestamps : CacheData.t -> float -> unit
  val invalidate_all_resources : CacheData.t -> unit
  val invalidate_resources : CacheData.t -> int64 list -> unit
  val invalidate_trash_bin : CacheData.t -> unit
  val invalidate_path : CacheData.t -> string -> unit

  val select_resources_with_remote_id :
    CacheData.t -> string -> CacheData.Resource.t list

  val trash_resources : CacheData.t -> CacheData.Resource.t list -> unit

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
