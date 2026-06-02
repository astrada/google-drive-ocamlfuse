exception Permission_denied

type runtime = DriveRuntime.base = { cache : CacheData.t; config : Config.t }

module type PORTS = sig
  include DrivePortFragments.PATH_LOOKUP
  include DrivePortFragments.RESOURCE_LOOKUP

  val get_content_path : CacheData.t -> CacheData.Resource.t -> string
  val file_exists : string -> bool

  val update_resource_from_file :
    CacheData.Resource.t -> GapiDriveV3Model.File.t -> CacheData.Resource.t

  val update_cached_resource : CacheData.t -> CacheData.Resource.t -> unit
end

module Make (P : PORTS) : sig
  val default_save_resource_to_db :
    CacheData.t -> CacheData.Resource.t -> GapiDriveV3Model.File.t -> unit

  val update_remote_resource :
    runtime ->
    string ->
    ?update_file_in_cache:(string -> unit) ->
    ?save_to_db:
      (CacheData.t -> CacheData.Resource.t -> GapiDriveV3Model.File.t -> unit) ->
    ?purge_cache:(CacheData.t -> CacheData.Resource.t -> unit) ->
    (CacheData.Resource.t ->
    GapiDriveV3Model.File.t option GapiMonad.SessionM.m) ->
    unit GapiMonad.SessionM.m
end
