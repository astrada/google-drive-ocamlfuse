module File = GapiDriveV3Model.File

type runtime = DriveRuntime.base = { cache : CacheData.t; config : Config.t }

module type PORTS = sig
  include DrivePortFragments.PATH_LOOKUP
  include DrivePortFragments.RESOURCE_LOOKUP

  val get_folder_id : string -> bool -> string GapiMonad.SessionM.m
  val is_lost_and_found_root : string -> bool -> Config.t -> bool
  val is_shared_with_me_root : string -> bool -> Config.t -> bool
  val check_resource_in_cache : CacheData.t -> string -> bool -> bool

  val select_resources_with_parent_path :
    CacheData.t -> string -> bool -> CacheData.Resource.t list

  val list_files : string -> File.t list GapiMonad.SessionM.m

  val build_resource_tables :
    string ->
    bool ->
    (string, int) Hashtbl.t * (string, CacheData.Resource.t) Hashtbl.t

  val update_resource_from_file :
    CacheData.Resource.t -> File.t -> CacheData.Resource.t

  val get_unique_filename_from_file :
    File.t -> (string, int) Hashtbl.t -> string

  val create_resource : string -> CacheData.Resource.t

  val insert_resources :
    CacheData.t ->
    CacheData.Resource.t list ->
    string ->
    bool ->
    CacheData.Resource.t list

  val update_cached_resource : CacheData.t -> CacheData.Resource.t -> unit
  val current_time : unit -> float
end

module Make (P : PORTS) : sig
  val read_dir : runtime -> string -> string list GapiMonad.SessionM.m
end
