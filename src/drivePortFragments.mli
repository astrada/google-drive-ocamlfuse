module type PATH_LOOKUP = sig
  val get_path_in_cache : string -> Config.t -> string * bool
end

module type RESOURCE_LOOKUP = sig
  val get_resource : string -> bool -> CacheData.Resource.t GapiMonad.SessionM.m
end

module type RESOURCE_KEYS = sig
  val build_resource_keys_header_from_resource :
    CacheData.Resource.t -> GapiCore.Header.t list
end
