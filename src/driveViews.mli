type runtime = {
  cache : CacheData.t;
  config : Config.t;
  mountpoint_path : string;
  mountpoint_stats : Unix.LargeFile.stats;
}

module type PORTS = sig
  include DrivePortFragments.PATH_LOOKUP
  include DrivePortFragments.RESOURCE_LOOKUP

  val get_resource_with_id :
    string -> CacheData.t -> CacheData.Resource.t GapiMonad.SessionM.m

  val update_cached_resource : CacheData.t -> CacheData.Resource.t -> unit
  val materialize_for_stat : CacheData.Resource.t -> string GapiMonad.SessionM.m
  val file_exists : string -> bool
  val stat_file : string -> Unix.LargeFile.stats
  val is_file_read_only : CacheData.Resource.t -> bool
  val is_lost_and_found_root : string -> bool -> Config.t -> bool
  val is_shared_with_me_root : string -> bool -> Config.t -> bool
end

module Make (P : PORTS) : sig
  val read_link : runtime -> string -> string GapiMonad.SessionM.m
  val get_attr : runtime -> string -> Unix.LargeFile.stats GapiMonad.SessionM.m
  val opendir : runtime -> string -> unit GapiMonad.SessionM.m
end
