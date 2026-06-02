type runtime = DriveRuntime.base = { cache : CacheData.t; config : Config.t }

type io_buffer =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module type PORTS = sig
  val get_path_in_cache : string -> Config.t -> string * bool
  val get_resource : string -> bool -> CacheData.Resource.t GapiMonad.SessionM.m
  val ensure_local_content : CacheData.Resource.t -> string GapiMonad.SessionM.m
  val flush_memory_buffers : CacheData.Resource.t -> unit

  val write_to_memory_buffers :
    CacheData.Resource.t -> string -> io_buffer -> int64 -> int

  val write_to_file : string -> io_buffer -> int64 -> int
  val truncate_local_file : string -> int64 -> unit
  val file_exists : string -> bool
  val update_cached_resource : CacheData.t -> CacheData.Resource.t -> unit

  val update_cached_resource_state :
    CacheData.t -> CacheData.Resource.State.t -> int64 -> unit

  val shrink_cache : ?file_size:int64 -> unit -> unit
end

module Make (P : PORTS) : sig
  val write :
    runtime -> string -> io_buffer -> int64 -> int GapiMonad.SessionM.m

  val truncate : runtime -> string -> int64 -> unit GapiMonad.SessionM.m
end
