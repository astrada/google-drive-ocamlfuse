type runtime = { cache : CacheData.t; config : Config.t }

type io_buffer =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module type PORTS = sig
  val get_path_in_cache : string -> Config.t -> string * bool
  val get_resource : string -> bool -> CacheData.Resource.t GapiMonad.SessionM.m

  val stream_resource :
    int64 -> io_buffer -> CacheData.Resource.t -> unit GapiMonad.SessionM.m

  val stream_resource_to_memory_buffer :
    int64 -> io_buffer -> CacheData.Resource.t -> unit GapiMonad.SessionM.m

  val stream_resource_to_read_ahead_buffers :
    int64 ->
    CacheData.Resource.t ->
    unit GapiMonad.SessionM.m list GapiMonad.SessionM.m

  val flush_memory_buffers : CacheData.Resource.t -> unit
  val ensure_local_content : CacheData.Resource.t -> string GapiMonad.SessionM.m
  val read_local_file : string -> io_buffer -> int64 -> int
  val enqueue_async_request : unit GapiMonad.SessionM.m -> unit
end

module Make (P : PORTS) : sig
  val read : runtime -> string -> io_buffer -> int64 -> int GapiMonad.SessionM.m
end
