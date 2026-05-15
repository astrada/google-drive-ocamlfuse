type runtime = {
  cache : CacheData.t;
  config : Config.t;
  metadata : CacheData.Metadata.t option;
}

module type PORTS = sig
  val with_metadata_lock : (unit -> 'a) -> 'a
  val update_cache_size_in_db : CacheData.t -> int64 -> unit

  val update_context_metadata :
    (CacheData.Metadata.t -> CacheData.Metadata.t) -> unit

  val select_resources_order_by_last_update :
    CacheData.t -> CacheData.Resource.t list

  val update_cached_resource_state :
    CacheData.t -> CacheData.Resource.State.t -> int64 -> unit

  val delete_files_from_cache :
    CacheData.t -> CacheData.Resource.t list -> int64

  val delete_resource : CacheData.t -> CacheData.Resource.t -> unit
  val delete_resources : CacheData.t -> CacheData.Resource.t list -> unit
  val remove_memory_buffers : string -> unit
  val remove_file_lock : string -> unit
  val file_exists : string -> bool
  val stat_file : string -> Unix.LargeFile.stats
  val log_exception : exn -> unit
end

module Make (P : PORTS) : sig
  val update_cache_size : int64 -> CacheData.Metadata.t -> CacheData.t -> unit
  val shrink_cache : runtime -> ?file_size:int64 -> unit -> unit
  val delete_memory_buffers : CacheData.Resource.t -> unit
  val delete_from_context : CacheData.Resource.t -> unit
  val delete_cached_resource : runtime -> CacheData.Resource.t -> unit

  val delete_cached_resources :
    runtime -> CacheData.Metadata.t -> CacheData.Resource.t list -> unit

  val update_cache_size_for_documents :
    runtime -> CacheData.Resource.t -> string -> (int64 -> int64) -> unit
end
