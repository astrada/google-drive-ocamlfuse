type runtime = DriveRuntime.cache_only = { cache : CacheData.t }

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

module Make (P : PORTS) = struct
  let upload_resource_with_retry resource =
    P.flush_memory_buffers resource;
    P.with_resource_retry
      (fun refreshed_resource ->
        P.try_with_default (P.upload refreshed_resource))
      resource

  let upload_resource_by_id runtime resource_id =
    match P.select_resource_with_id runtime.cache resource_id with
    | Some resource -> P.run_request (upload_resource_with_retry resource)
    | None -> P.log_missing_resource resource_id
end
