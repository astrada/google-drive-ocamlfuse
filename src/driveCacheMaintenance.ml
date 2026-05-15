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

module Make (P : PORTS) = struct
  let metadata runtime = Option.get runtime.metadata

  let update_cache_size delta metadata cache =
    Utils.log_with_header "BEGIN: Updating cache size (delta=%Ld) in db\n%!"
      delta;
    if delta = 0L then
      Utils.log_with_header "END: No need to update cache size\n%!"
    else (
      P.update_cache_size_in_db cache delta;
      let cache_size = Int64.add metadata.CacheData.Metadata.cache_size delta in
      P.update_context_metadata (fun current_metadata ->
          let metadata =
            { current_metadata with CacheData.Metadata.cache_size }
          in
          Utils.log_with_header
            "END: Updating cache size (new size=%Ld) in db\n%!"
            metadata.CacheData.Metadata.cache_size;
          metadata))

  let shrink_cache runtime ?(file_size = 0L) () =
    P.with_metadata_lock (fun () ->
        let metadata = metadata runtime in
        let max_cache_size =
          Int64.mul
            (Int64.of_int runtime.config.Config.max_cache_size_mb)
            Utils.mb
        in
        let target_size =
          Int64.add metadata.CacheData.Metadata.cache_size file_size
        in
        if target_size > max_cache_size then (
          let resources =
            P.select_resources_order_by_last_update runtime.cache
          in
          let _new_cache_size, total_delta, resources_to_free =
            List.fold_left
              (fun (new_cache_size, delta, rs) resource ->
                if new_cache_size <= max_cache_size then
                  (new_cache_size, delta, rs)
                else
                  let size_to_free =
                    Option.default 0L resource.CacheData.Resource.size
                  in
                  let new_size = Int64.sub new_cache_size size_to_free in
                  let new_delta = Int64.add delta (Int64.neg size_to_free) in
                  (new_size, new_delta, resource :: rs))
              (target_size, file_size, [])
              resources
          in
          update_cache_size total_delta metadata runtime.cache;
          List.iter
            (fun resource ->
              P.update_cached_resource_state runtime.cache
                CacheData.Resource.State.ToDownload
                resource.CacheData.Resource.id)
            resources_to_free;
          P.delete_files_from_cache runtime.cache resources_to_free |> ignore)
        else update_cache_size file_size metadata runtime.cache)

  let delete_memory_buffers resource =
    Option.may P.remove_memory_buffers resource.CacheData.Resource.remote_id

  let delete_from_context resource =
    delete_memory_buffers resource;
    Option.may P.remove_file_lock resource.CacheData.Resource.remote_id

  let delete_cached_resource runtime resource =
    P.delete_resource runtime.cache resource;
    let total_size = P.delete_files_from_cache runtime.cache [ resource ] in
    Option.may
      (fun metadata ->
        update_cache_size (Int64.neg total_size) metadata runtime.cache)
      runtime.metadata;
    delete_from_context resource

  let delete_cached_resources runtime metadata resources =
    P.delete_resources runtime.cache resources;
    let total_size = P.delete_files_from_cache runtime.cache resources in
    update_cache_size (Int64.neg total_size) metadata runtime.cache;
    List.iter delete_from_context resources

  let update_cache_size_for_documents runtime resource content_path op =
    P.with_metadata_lock (fun () ->
        if
          resource.CacheData.Resource.size = Some 0L
          && P.file_exists content_path
        then
          try
            let stats = P.stat_file content_path in
            let size = stats.Unix.LargeFile.st_size in
            let metadata = metadata runtime in
            let delta = op size in
            update_cache_size delta metadata runtime.cache
          with e -> P.log_exception e)
end
