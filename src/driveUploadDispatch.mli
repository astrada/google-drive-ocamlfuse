type runtime = DriveRuntime.base = { cache : CacheData.t; config : Config.t }

module type PORTS = sig
  val get_path_in_cache : string -> Config.t -> string * bool

  val lookup_resource :
    CacheData.t -> string -> bool -> CacheData.Resource.t option

  val update_cached_resource_state :
    CacheData.t -> CacheData.Resource.State.t -> int64 -> unit

  val get_resource : string -> bool -> CacheData.Resource.t GapiMonad.SessionM.m
  val flush_memory_buffers : CacheData.Resource.t -> unit

  val enqueue_async_upload :
    CacheData.t -> Config.t -> CacheData.Resource.t -> unit

  val upload_now_with_retry : CacheData.Resource.t -> unit GapiMonad.SessionM.m
end

module Make (P : PORTS) : sig
  val start_uploading_if_dirty : runtime -> string -> bool

  val queue_upload :
    runtime -> CacheData.Resource.t -> unit GapiMonad.SessionM.m

  val upload_with_retry : runtime -> string -> unit GapiMonad.SessionM.m
  val upload_if_dirty : runtime -> string -> unit GapiMonad.SessionM.m option
end
