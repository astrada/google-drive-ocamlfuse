open GapiMonad
open GapiMonad.SessionM.Infix

type io_buffer =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type runtime = {
  config : Config.t;
  memory_buffers : Buffering.MemoryBuffers.t;
  buffer_eviction_thread : Thread.t option;
}

module type PORTS = sig
  val build_resource_keys_header_from_resource :
    CacheData.Resource.t -> GapiCore.Header.t list

  val get_media :
    acknowledge_abuse:bool ->
    media_download:GapiMediaResource.download ->
    custom_headers:GapiCore.Header.t list ->
    file_id:string ->
    GapiDriveV3Model.File.t SessionM.m

  val match_service_error : string -> exn -> bool
  val handle_default_exceptions : exn -> 'a SessionM.m
  val with_retry_default : 'a SessionM.m -> 'a SessionM.m
  val create_eviction_thread : Buffering.MemoryBuffers.t -> Thread.t
  val set_buffer_eviction_thread : Thread.t -> unit

  val read_block :
    string ->
    int64 ->
    int64 ->
    (int64 -> io_buffer -> unit SessionM.m) ->
    ?dest_arr:io_buffer ->
    Buffering.MemoryBuffers.t ->
    unit SessionM.m

  val read_ahead :
    int ->
    string ->
    int64 ->
    int64 ->
    (int64 -> io_buffer -> unit SessionM.m) ->
    Buffering.MemoryBuffers.t ->
    unit SessionM.m list SessionM.m

  val with_resource_retry :
    CacheData.Resource.t -> unit SessionM.m -> unit SessionM.m
end

module Make (P : PORTS) = struct
  let download_media runtime media_download resource =
    let file_id = resource.CacheData.Resource.remote_id |> Option.get in
    let custom_headers = P.build_resource_keys_header_from_resource resource in
    Utils.try_with_m
      (P.get_media ~acknowledge_abuse:false ~media_download ~custom_headers
         ~file_id) (fun e ->
        if
          P.match_service_error "cannotDownloadAbusiveFile" e
          && runtime.config.Config.acknowledge_abuse
        then (
          Utils.log_with_header
            "Warning: abusive file detected, but downloading anyway (fileId=%s)\n\
             %!"
            file_id;
          P.with_retry_default
            (P.get_media ~acknowledge_abuse:true ~media_download ~custom_headers
               ~file_id)
          >>= fun file -> SessionM.return file)
        else P.handle_default_exceptions e)

  let stream_resource runtime offset buffer resource =
    let length = Bigarray.Array1.dim buffer in
    let finish = Int64.add offset (Int64.of_int (length - 1)) in
    Utils.log_with_header
      "BEGIN: Stream resource (id=%Ld, offset=%Ld, finish=%Ld, length=%d)\n%!"
      resource.CacheData.Resource.id offset finish length;
    let destination = GapiMediaResource.ArrayBuffer buffer in
    let range_spec =
      GapiMediaResource.generate_range_spec [ (Some offset, Some finish) ]
    in
    let media_download = { GapiMediaResource.destination; range_spec } in
    download_media runtime media_download resource >>= fun _ ->
    Utils.log_with_header
      "END: Stream resource (id=%Ld, offset=%Ld, finish=%Ld, length=%d)\n%!"
      resource.CacheData.Resource.id offset finish length;
    SessionM.return ()

  let start_buffer_eviction_thread runtime =
    if runtime.config.Config.stream_large_files then
      if Option.is_none runtime.buffer_eviction_thread then (
        let thread = P.create_eviction_thread runtime.memory_buffers in
        Utils.log_with_header "Starting buffer eviction thread (TID=%d)\n%!"
          (Thread.id thread);
        P.set_buffer_eviction_thread thread)

  let stream_resource_to_memory_buffer runtime offset buffer resource =
    start_buffer_eviction_thread runtime;
    let remote_id = resource.CacheData.Resource.remote_id |> Option.get in
    P.read_block remote_id offset
      (resource.CacheData.Resource.size |> Option.get)
      (fun start_pos block_buffer ->
        stream_resource runtime start_pos block_buffer resource)
      ~dest_arr:buffer runtime.memory_buffers
    >>= fun () -> SessionM.return ()

  let stream_resource_to_read_ahead_buffers runtime offset resource =
    start_buffer_eviction_thread runtime;
    let remote_id = resource.CacheData.Resource.remote_id |> Option.get in
    P.read_ahead runtime.config.Config.read_ahead_buffers remote_id offset
      (resource.CacheData.Resource.size |> Option.get)
      (fun start_pos block_buffer ->
        stream_resource runtime start_pos block_buffer resource)
      runtime.memory_buffers
    >>= fun ms ->
    List.map (fun m -> P.with_resource_retry resource m) ms |> SessionM.return
end
