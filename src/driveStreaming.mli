type io_buffer =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type runtime = {
  config : Config.t;
  memory_buffers : Buffering.MemoryBuffers.t;
  buffer_eviction_thread : Thread.t option;
}

module type PORTS = sig
  include DrivePortFragments.RESOURCE_KEYS

  val get_media :
    acknowledge_abuse:bool ->
    media_download:GapiMediaResource.download ->
    custom_headers:GapiCore.Header.t list ->
    file_id:string ->
    GapiDriveV3Model.File.t GapiMonad.SessionM.m

  val match_service_error : string -> exn -> bool
  val handle_default_exceptions : exn -> 'a GapiMonad.SessionM.m
  val with_retry_default : 'a GapiMonad.SessionM.m -> 'a GapiMonad.SessionM.m
  val create_eviction_thread : Buffering.MemoryBuffers.t -> Thread.t
  val set_buffer_eviction_thread : Thread.t -> unit

  val read_block :
    string ->
    int64 ->
    int64 ->
    (int64 -> io_buffer -> unit GapiMonad.SessionM.m) ->
    ?dest_arr:io_buffer ->
    Buffering.MemoryBuffers.t ->
    unit GapiMonad.SessionM.m

  val read_ahead :
    int ->
    string ->
    int64 ->
    int64 ->
    (int64 -> io_buffer -> unit GapiMonad.SessionM.m) ->
    Buffering.MemoryBuffers.t ->
    unit GapiMonad.SessionM.m list GapiMonad.SessionM.m

  val with_resource_retry :
    CacheData.Resource.t ->
    unit GapiMonad.SessionM.m ->
    unit GapiMonad.SessionM.m
end

module Make (P : PORTS) : sig
  val download_media :
    runtime ->
    GapiMediaResource.download ->
    CacheData.Resource.t ->
    GapiDriveV3Model.File.t GapiMonad.SessionM.m

  val stream_resource :
    runtime ->
    int64 ->
    io_buffer ->
    CacheData.Resource.t ->
    unit GapiMonad.SessionM.m

  val start_buffer_eviction_thread : runtime -> unit

  val stream_resource_to_memory_buffer :
    runtime ->
    int64 ->
    io_buffer ->
    CacheData.Resource.t ->
    unit GapiMonad.SessionM.m

  val stream_resource_to_read_ahead_buffers :
    runtime ->
    int64 ->
    CacheData.Resource.t ->
    unit GapiMonad.SessionM.m list GapiMonad.SessionM.m
end
