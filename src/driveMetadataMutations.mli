type runtime = DriveRuntime.base = { cache : CacheData.t; config : Config.t }

module type PORTS = sig
  include DrivePortFragments.RESOURCE_KEYS

  val remote_update :
    custom_headers:GapiCore.Header.t list ->
    fileId:string ->
    GapiDriveV3Model.File.t ->
    GapiDriveV3Model.File.t GapiMonad.SessionM.m

  val update_remote_resource :
    runtime ->
    string ->
    ?update_file_in_cache:(string -> unit) ->
    (CacheData.Resource.t ->
    GapiDriveV3Model.File.t option GapiMonad.SessionM.m) ->
    unit GapiMonad.SessionM.m

  val update_file_times : string -> float -> float -> unit
end

module Make (P : PORTS) : sig
  val utime : runtime -> string -> float -> float -> unit GapiMonad.SessionM.m
  val chmod : runtime -> string -> int -> unit GapiMonad.SessionM.m
  val chown : runtime -> string -> int -> int -> unit GapiMonad.SessionM.m
end
