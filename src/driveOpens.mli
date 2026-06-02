exception Permission_denied

type runtime = DriveRuntime.config_only = { config : Config.t }

module type PORTS = sig
  include DrivePortFragments.PATH_LOOKUP
  include DrivePortFragments.RESOURCE_LOOKUP
end

val is_file_read_only : Config.t -> CacheData.Resource.t -> bool

module Make (P : PORTS) : sig
  val fopen :
    runtime -> string -> Unix.open_flag list -> unit GapiMonad.SessionM.m
end
