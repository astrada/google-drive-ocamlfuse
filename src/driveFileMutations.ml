open GapiLens.Infix
open GapiMonad
open GapiMonad.SessionM.Infix

type runtime = DriveRuntime.base = { cache : CacheData.t; config : Config.t }

type io_buffer =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module type PORTS = sig
  val get_path_in_cache : string -> Config.t -> string * bool
  val get_resource : string -> bool -> CacheData.Resource.t SessionM.m
  val ensure_local_content : CacheData.Resource.t -> string SessionM.m
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

module Make (P : PORTS) = struct
  let mark_resource_to_upload runtime resource =
    P.update_cached_resource_state runtime.cache
      CacheData.Resource.State.ToUpload resource.CacheData.Resource.id

  let write runtime path buf offset =
    let path_in_cache, trashed = P.get_path_in_cache path runtime.config in
    P.get_resource path_in_cache trashed >>= fun resource ->
    P.ensure_local_content resource >>= fun content_path ->
    Utils.log_with_header "BEGIN: Writing local file (path=%s, trashed=%b)\n%!"
      path_in_cache trashed;
    let bytes =
      if runtime.config.Config.write_buffers then
        P.write_to_memory_buffers resource content_path buf offset
      else P.write_to_file content_path buf offset
    in
    Utils.log_with_header
      "END: Writing local file (path=%s, trashed=%b, bytes=%d)\n%!"
      path_in_cache trashed bytes;
    let top_offset = Int64.add offset (Int64.of_int bytes) in
    let file_size = Option.default 0L resource.CacheData.Resource.size in
    if top_offset > file_size then (
      let updated_resource =
        resource
        |> CacheData.Resource.size ^= Some top_offset
        |> CacheData.Resource.state ^= CacheData.Resource.State.ToUpload
      in
      P.update_cached_resource runtime.cache updated_resource;
      let file_size = Int64.sub top_offset file_size in
      P.shrink_cache ~file_size ())
    else mark_resource_to_upload runtime resource;
    SessionM.return bytes

  let truncate runtime path size =
    let path_in_cache, trashed = P.get_path_in_cache path runtime.config in
    P.get_resource path_in_cache trashed >>= fun resource ->
    P.flush_memory_buffers resource;
    P.ensure_local_content resource >>= fun content_path ->
    let remote_id =
      resource |. CacheData.Resource.remote_id |. GapiLens.option_get
    in
    Utils.log_with_header "BEGIN: Truncating file (remote id=%s)\n%!" remote_id;
    let updated_resource =
      resource
      |> CacheData.Resource.size ^= Some size
      |> CacheData.Resource.state ^= CacheData.Resource.State.ToUpload
    in
    P.update_cached_resource runtime.cache updated_resource;
    let file_size =
      Int64.sub size (Option.default 0L resource.CacheData.Resource.size)
    in
    P.shrink_cache ~file_size ();
    if P.file_exists content_path then P.truncate_local_file content_path size
    else
      Utils.log_with_header
        "Warning: file %s does not exists (remote id=%s)\n%!" content_path
        remote_id;
    Utils.log_with_header "END: Truncating file (remote id=%s)\n%!" remote_id;
    SessionM.return ()
end
