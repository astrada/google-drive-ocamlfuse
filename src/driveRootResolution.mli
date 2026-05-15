module File = GapiDriveV3Model.File

val root_directory : string
val default_root_folder_id : string
val trash_directory : string
val trash_directory_name_length : int
val trash_directory_base_path : string
val lost_and_found_directory : string
val shared_with_me_directory : string
val device_scope : string
val device_root_folder : string
val is_lost_and_found_root : string -> bool -> Config.t -> bool
val is_shared_with_me_root : string -> bool -> Config.t -> bool

type runtime = {
  cache : CacheData.t;
  config : Config.t;
  root_folder_id : string option;
}

module type PORTS = sig
  val folder_mime_type : string
  val create_resource : string -> CacheData.Resource.t

  val find_file_in_folder :
    parent_folder_id:string ->
    name:string ->
    trashed:bool ->
    File.t option GapiMonad.SessionM.m

  val get_file_by_remote_id : string -> File.t GapiMonad.SessionM.m
  val create_folder : name:string -> File.t GapiMonad.SessionM.m
  val run_request : 'a GapiMonad.SessionM.m -> 'a
  val set_context_root_folder_id : string -> unit

  val lookup_resource :
    CacheData.t -> string -> bool -> CacheData.Resource.t option

  val insert_resource :
    CacheData.t -> label:string -> CacheData.Resource.t -> CacheData.Resource.t
end

module Make (P : PORTS) : sig
  val create_root_resource : string -> bool -> CacheData.Resource.t
  val create_well_known_resource : string -> CacheData.Resource.t
  val get_root_folder_id_from_server : Config.t -> string GapiMonad.SessionM.m
  val get_root_folder_id : Config.t -> string GapiMonad.SessionM.m
  val get_root_folder_id_from_context : runtime -> string

  val get_well_known_resource :
    runtime -> string -> bool -> CacheData.Resource.t
end
