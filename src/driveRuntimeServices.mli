type runtime = DriveRuntime.base = { cache : CacheData.t; config : Config.t }

module type PORTS = sig
  val start_flush_db_thread : CacheData.t -> unit
  val start_async_upload_thread : CacheData.t -> int -> (int64 -> unit) -> unit
  val start_folder_fetching_thread : CacheData.t -> (string -> unit) -> unit
  val upload_resource_by_id : int64 -> unit
  val read_dir : string -> string list
end

module Make (P : PORTS) : sig
  val init_filesystem : runtime -> unit
end
