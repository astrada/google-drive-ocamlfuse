exception Permission_denied

type runtime = { config : Config.t }

module type PORTS = sig
  val get_path_in_cache : string -> Config.t -> string * bool
  val get_resource : string -> bool -> CacheData.Resource.t GapiMonad.SessionM.m
end

val is_file_read_only : Config.t -> CacheData.Resource.t -> bool

module Make (P : PORTS) : sig
  val fopen :
    runtime -> string -> Unix.open_flag list -> unit GapiMonad.SessionM.m
end
