type runtime = { cache : CacheData.t }

module type PORTS = sig
  val flush_memory_buffers : CacheData.Resource.t -> unit
  val upload : CacheData.Resource.t -> unit GapiMonad.SessionM.m
  val try_with_default : unit GapiMonad.SessionM.m -> unit GapiMonad.SessionM.m

  val with_resource_retry :
    (CacheData.Resource.t -> unit GapiMonad.SessionM.m) ->
    CacheData.Resource.t ->
    unit GapiMonad.SessionM.m

  val select_resource_with_id :
    CacheData.t -> int64 -> CacheData.Resource.t option

  val run_request : unit GapiMonad.SessionM.m -> unit
  val log_missing_resource : int64 -> unit
end

module Make (P : PORTS) : sig
  val upload_resource_with_retry :
    CacheData.Resource.t -> unit GapiMonad.SessionM.m

  val upload_resource_by_id : runtime -> int64 -> unit
end
