val start_async_upload_thread :
  CacheData.t -> int -> (int64 -> unit) -> unit

val stop_async_upload_thread : unit -> unit

val queue_resource : CacheData.t -> CacheData.Resource.t -> unit

