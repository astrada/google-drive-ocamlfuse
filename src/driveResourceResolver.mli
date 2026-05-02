module File = GapiDriveV3Model.File

exception File_not_found

type runtime = { cache : CacheData.t; config : Config.t }

module type PORTS = sig
  val root_directory : string
  val lost_and_found_directory : string
  val shared_with_me_directory : string
  val get_metadata : unit -> CacheData.Metadata.t
  val current_metadata_last_update : unit -> float
  val get_root_folder_id : unit -> string
  val get_well_known_resource : string -> bool -> CacheData.Resource.t
  val is_lost_and_found_root : string -> bool -> Config.t -> bool
  val is_shared_with_me_root : string -> bool -> Config.t -> bool

  val lookup_resource :
    CacheData.t -> string -> bool -> CacheData.Resource.t option

  val create_resource : string -> CacheData.Resource.t

  val insert_resource :
    CacheData.t -> CacheData.Resource.t -> CacheData.Resource.t

  val insert_resource_from_file :
    CacheData.t -> CacheData.Resource.t -> File.t -> CacheData.Resource.t

  val update_resource_from_file :
    CacheData.Resource.t -> File.t -> CacheData.Resource.t

  val update_cached_resource : CacheData.t -> CacheData.Resource.t -> unit
  val delete_cached_resource : CacheData.Resource.t -> unit

  val select_first_resource_with_remote_id :
    CacheData.t -> string -> CacheData.Resource.t option

  val find_file_in_folder :
    parent_folder_id:string ->
    name:string ->
    trashed:bool ->
    File.t option GapiMonad.SessionM.m

  val get_file_by_remote_id : string -> File.t GapiMonad.SessionM.m
  val with_default_retry : 'a GapiMonad.SessionM.m -> 'a GapiMonad.SessionM.m
end

module Make (P : PORTS) : sig
  val check_resource_in_cache : runtime -> string -> bool -> bool
  val get_folder_id : runtime -> string -> bool -> string GapiMonad.SessionM.m

  val get_resource :
    runtime -> string -> bool -> CacheData.Resource.t GapiMonad.SessionM.m
end
