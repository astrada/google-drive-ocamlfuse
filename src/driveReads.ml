open GapiMonad
open GapiMonad.SessionM.Infix

type runtime = DriveRuntime.base = { cache : CacheData.t; config : Config.t }

type io_buffer =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module type PORTS = sig
  val get_path_in_cache : string -> Config.t -> string * bool
  val get_resource : string -> bool -> CacheData.Resource.t SessionM.m

  val stream_resource :
    int64 -> io_buffer -> CacheData.Resource.t -> unit SessionM.m

  val stream_resource_to_memory_buffer :
    int64 -> io_buffer -> CacheData.Resource.t -> unit SessionM.m

  val stream_resource_to_read_ahead_buffers :
    int64 -> CacheData.Resource.t -> unit SessionM.m list SessionM.m

  val flush_memory_buffers : CacheData.Resource.t -> unit
  val ensure_local_content : CacheData.Resource.t -> string SessionM.m
  val read_local_file : string -> io_buffer -> int64 -> int
  val enqueue_async_request : unit SessionM.m -> unit
end

module Make (P : PORTS) = struct
  type read_result = Streamed | LocalFile of string

  let read_foreground runtime path_in_cache trashed buf offset =
    P.get_resource path_in_cache trashed >>= fun resource ->
    let to_stream, to_memory_buffer =
      CacheData.Resource.to_stream runtime.config resource
    in
    if to_stream then
      (if to_memory_buffer then
         P.stream_resource_to_memory_buffer offset buf resource
       else P.stream_resource offset buf resource)
      >>= fun () -> SessionM.return Streamed
    else (
      P.flush_memory_buffers resource;
      P.ensure_local_content resource >>= fun content_path ->
      SessionM.return (LocalFile content_path))

  let build_read_ahead_requests runtime path_in_cache trashed offset =
    if runtime.config.Config.read_ahead_buffers > 0 then
      P.get_resource path_in_cache trashed >>= fun resource ->
      let to_stream, to_memory_buffer =
        CacheData.Resource.to_stream runtime.config resource
      in
      if to_stream && to_memory_buffer then
        P.stream_resource_to_read_ahead_buffers offset resource
      else SessionM.return []
    else SessionM.return []

  let read runtime path buf offset =
    let path_in_cache, trashed = P.get_path_in_cache path runtime.config in
    read_foreground runtime path_in_cache trashed buf offset
    >>= fun read_result ->
    build_read_ahead_requests runtime path_in_cache trashed offset
    >>= fun read_ahead_requests ->
    List.iter P.enqueue_async_request read_ahead_requests;
    match read_result with
    | Streamed -> SessionM.return (Bigarray.Array1.dim buf)
    | LocalFile content_path ->
        SessionM.return (P.read_local_file content_path buf offset)
end
