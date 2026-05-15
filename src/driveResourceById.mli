module File = GapiDriveV3Model.File

type runtime = { cache : CacheData.t }

module type PORTS = sig
  val root_directory : string
  val shared_with_me_directory : string
  val get_root_folder_id : unit -> string
  val get_well_known_resource : string -> bool -> CacheData.Resource.t

  val select_first_resource_with_remote_id :
    CacheData.t -> string -> CacheData.Resource.t option

  val clean_filename : string -> string
  val create_resource : string -> CacheData.Resource.t

  val update_resource_from_file :
    CacheData.Resource.t -> File.t -> CacheData.Resource.t

  val get_file_by_remote_id : string -> File.t GapiMonad.SessionM.m
end

module Make (P : PORTS) : sig
  val get_resource_with_id_from_server :
    string -> CacheData.Resource.t GapiMonad.SessionM.m

  val get_resource_with_id :
    runtime -> string -> CacheData.Resource.t GapiMonad.SessionM.m
end
