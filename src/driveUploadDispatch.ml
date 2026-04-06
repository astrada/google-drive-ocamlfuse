open GapiMonad
open GapiMonad.SessionM.Infix

type runtime = { cache : CacheData.t; config : Config.t }

module type PORTS = sig
  val get_path_in_cache : string -> Config.t -> string * bool

  val lookup_resource :
    CacheData.t -> string -> bool -> CacheData.Resource.t option

  val update_cached_resource_state :
    CacheData.t -> CacheData.Resource.State.t -> int64 -> unit

  val get_resource : string -> bool -> CacheData.Resource.t SessionM.m
  val flush_memory_buffers : CacheData.Resource.t -> unit

  val enqueue_async_upload :
    CacheData.t -> Config.t -> CacheData.Resource.t -> unit

  val upload_now_with_retry : CacheData.Resource.t -> unit SessionM.m
end

module Make (P : PORTS) = struct
  let start_uploading_if_dirty runtime path =
    let path_in_cache, trashed = P.get_path_in_cache path runtime.config in
    let resource = P.lookup_resource runtime.cache path_in_cache trashed in
    match resource with
    | None -> false
    | Some resource ->
        if resource.CacheData.Resource.state = CacheData.Resource.State.ToUpload
        then (
          P.update_cached_resource_state runtime.cache
            CacheData.Resource.State.Uploading resource.CacheData.Resource.id;
          true)
        else false

  let queue_upload runtime resource =
    if runtime.config.Config.async_upload_queue then (
      P.flush_memory_buffers resource;
      P.enqueue_async_upload runtime.cache runtime.config resource;
      SessionM.return ())
    else P.upload_now_with_retry resource

  let upload_with_retry runtime path =
    let path_in_cache, trashed = P.get_path_in_cache path runtime.config in
    P.get_resource path_in_cache trashed >>= fun resource ->
    queue_upload runtime resource

  let upload_if_dirty runtime path =
    if start_uploading_if_dirty runtime path then
      Some (upload_with_retry runtime path)
    else None
end
