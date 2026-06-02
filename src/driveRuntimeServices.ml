type runtime = DriveRuntime.base = { cache : CacheData.t; config : Config.t }

module type PORTS = sig
  val start_flush_db_thread : CacheData.t -> unit
  val start_async_upload_thread : CacheData.t -> int -> (int64 -> unit) -> unit
  val start_folder_fetching_thread : CacheData.t -> (string -> unit) -> unit
  val upload_resource_by_id : int64 -> unit
  val read_dir : string -> string list
end

module Make (P : PORTS) = struct
  let init_filesystem runtime =
    P.start_flush_db_thread runtime.cache;
    if runtime.config.Config.async_upload_queue then
      P.start_async_upload_thread runtime.cache
        runtime.config.Config.async_upload_threads P.upload_resource_by_id;
    if runtime.config.Config.background_folder_fetching then
      P.start_folder_fetching_thread runtime.cache (fun path ->
          P.read_dir path |> ignore)
end
