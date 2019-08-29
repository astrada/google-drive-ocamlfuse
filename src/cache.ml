let create_cache app_dir config =
  let cache_dir = app_dir.AppDir.cache_dir in
  let db_path = Filename.concat cache_dir "cache.db" in
  let busy_timeout = config.Config.sqlite3_busy_timeout in
  let in_memory = config.Config.metadata_memory_cache in
  let autosaving_interval =
    config.Config.metadata_memory_cache_saving_interval in
  { CacheData.cache_dir;
    db_path;
    busy_timeout;
    in_memory;
    autosaving_interval;
  }

module Resource =
struct
  (* Queries *)
  let insert_resource cache resource =
    if cache.CacheData.in_memory then
      MemoryCache.Resource.insert_resource cache resource
    else
      DbCache.Resource.insert_resource cache resource

  let update_resource cache resource =
    if cache.CacheData.in_memory then
      MemoryCache.Resource.update_resource cache resource
    else
      DbCache.Resource.update_resource cache resource

  let update_resource_state cache state id =
    if cache.CacheData.in_memory then
      MemoryCache.Resource.update_resource_state cache state id
    else
      DbCache.Resource.update_resource_state cache state id

  let update_resource_state_and_size cache state size id =
    if cache.CacheData.in_memory then
      MemoryCache.Resource.update_resource_state_and_size cache state size id
    else
      DbCache.Resource.update_resource_state_and_size cache state size id

  let delete_resource cache resource =
    if cache.CacheData.in_memory then
      MemoryCache.Resource.delete_resource cache resource
    else
      DbCache.Resource.delete_resource cache resource

  let delete_not_found_resource_with_path cache path =
    if cache.CacheData.in_memory then
      MemoryCache.Resource.delete_not_found_resource_with_path cache path
    else
      DbCache.Resource.delete_not_found_resource_with_path cache path

  let delete_resources cache resources =
    if cache.CacheData.in_memory then
      MemoryCache.Resource.delete_resources cache resources
    else
      DbCache.Resource.delete_resources cache resources

  let insert_resources cache resources parent_path trashed =
    if cache.CacheData.in_memory then
      MemoryCache.Resource.insert_resources cache resources parent_path trashed
    else
      DbCache.Resource.insert_resources cache resources parent_path trashed

  let invalidate_resources cache ids =
    if cache.CacheData.in_memory then
      MemoryCache.Resource.invalidate_resources cache ids
    else
      DbCache.Resource.invalidate_resources cache ids

  let invalidate_path cache path =
    if cache.CacheData.in_memory then
      MemoryCache.Resource.invalidate_path cache path
    else
      DbCache.Resource.invalidate_path cache path

  let invalidate_all cache =
    if cache.CacheData.in_memory then
      MemoryCache.Resource.invalidate_all cache
    else
      DbCache.Resource.invalidate_all cache

  let invalidate_trash_bin cache =
    if cache.CacheData.in_memory then
      MemoryCache.Resource.invalidate_trash_bin cache
    else
      DbCache.Resource.invalidate_trash_bin cache

  let trash_resources cache resources =
    if cache.CacheData.in_memory then
      MemoryCache.Resource.trash_resources cache resources
    else
      DbCache.Resource.trash_resources cache resources

  let delete_all_with_parent_path cache parent_path trashed =
    if cache.CacheData.in_memory then
      MemoryCache.Resource.delete_all_with_parent_path cache parent_path trashed
    else
      DbCache.Resource.delete_all_with_parent_path cache parent_path trashed

  let trash_all_with_parent_path cache parent_path =
    if cache.CacheData.in_memory then
      MemoryCache.Resource.trash_all_with_parent_path cache parent_path
    else
      DbCache.Resource.trash_all_with_parent_path cache parent_path

  let update_all_timestamps cache last_update =
    if cache.CacheData.in_memory then
      MemoryCache.Resource.update_all_timestamps cache last_update
    else
      DbCache.Resource.update_all_timestamps cache last_update

  let select_resource_with_path cache path trashed =
    if cache.CacheData.in_memory then
      MemoryCache.Resource.select_resource_with_path cache path trashed
    else
      DbCache.Resource.select_resource_with_path cache path trashed

  let select_first_resource_with_remote_id cache remote_id =
    if cache.CacheData.in_memory then
      MemoryCache.Resource.select_first_resource_with_remote_id cache remote_id
    else
      DbCache.Resource.select_first_resource_with_remote_id cache remote_id

  let select_resources_with_remote_id cache remote_id =
    if cache.CacheData.in_memory then
      MemoryCache.Resource.select_resources_with_remote_id cache remote_id
    else
      DbCache.Resource.select_resources_with_remote_id cache remote_id

  let select_resources_with_parent_path cache parent_path trashed =
    if cache.CacheData.in_memory then
      MemoryCache.Resource.select_resources_with_parent_path cache parent_path trashed
    else
      DbCache.Resource.select_resources_with_parent_path cache parent_path trashed

  let select_resources_order_by_last_update cache =
    if cache.CacheData.in_memory then
      MemoryCache.Resource.select_resources_order_by_last_update cache
    else
      DbCache.Resource.select_resources_order_by_last_update cache

  let select_resource_with_id cache id =
    if cache.CacheData.in_memory then
      MemoryCache.Resource.select_resource_with_id cache id
    else
      DbCache.Resource.select_resource_with_id cache id
  (* END Queries *)

end

module Metadata =
struct
  (* Queries *)
  let insert_metadata cache metadata =
    if cache.CacheData.in_memory then
      MemoryCache.Metadata.insert_metadata cache metadata
    else
      DbCache.Metadata.insert_metadata cache metadata

  let select_metadata cache =
    if cache.CacheData.in_memory then
      MemoryCache.Metadata.select_metadata cache
    else
      DbCache.Metadata.select_metadata cache

  let update_cache_size cache delta =
    if cache.CacheData.in_memory then
      MemoryCache.Metadata.update_cache_size cache delta
    else
      DbCache.Metadata.update_cache_size cache delta
  (* END Queries *)

end

module UploadQueue =
struct
  let insert_upload_entry cache upload_entry =
    if cache.CacheData.in_memory then
      MemoryCache.UploadQueue.insert_upload_entry cache upload_entry
    else
      DbCache.UploadQueue.insert_upload_entry cache upload_entry

  let select_next_resource cache =
    if cache.CacheData.in_memory then
      MemoryCache.UploadQueue.select_next_resource cache
    else
      DbCache.UploadQueue.select_next_resource cache

  let select_with_resource_id cache resource_id =
    if cache.CacheData.in_memory then
      MemoryCache.UploadQueue.select_with_resource_id cache resource_id
    else
      DbCache.UploadQueue.select_with_resource_id cache resource_id

  let delete_upload_entry cache upload_entry =
    if cache.CacheData.in_memory then
      MemoryCache.UploadQueue.delete_upload_entry cache upload_entry
    else
      DbCache.UploadQueue.delete_upload_entry cache upload_entry

  let update_entry_state cache state id =
    if cache.CacheData.in_memory then
      MemoryCache.UploadQueue.update_entry_state cache state id
    else
      DbCache.UploadQueue.update_entry_state cache state id

  let count_entries cache =
    if cache.CacheData.in_memory then
      MemoryCache.UploadQueue.count_entries cache
    else
      DbCache.UploadQueue.count_entries cache

end

(* Resource content *)
let get_content_path cache resource =
  Filename.concat cache.CacheData.cache_dir
    (Option.get resource.CacheData.Resource.remote_id)
(* END Resource content *)

let delete_files_from_cache cache resources =
  let remove_file path =
    try
      if Sys.file_exists path then begin
        let stats = Unix.LargeFile.stat path in
        let size = stats.Unix.LargeFile.st_size in
        Sys.remove path;
        size
      end else 0L
    with e -> Utils.log_exception e; 0L
  in
  List.fold_left
    (fun total_size resource ->
       if resource.CacheData.Resource.state = CacheData.Resource.State.NotFound then 0L
       else begin
         let content_path = get_content_path cache resource in
         Utils.log_with_header
           "BEGIN: Removing file (%s: resource %Ld) from cache\n%!"
           content_path resource.CacheData.Resource.id;
         let size = remove_file content_path in
         let new_size = Int64.add total_size size in
         Utils.log_with_header
           "END: Removing file (%s: resource %Ld) from cache\n%!"
           content_path resource.CacheData.Resource.id;
         new_size
       end
    )
    0L
    resources

(* Setup *)
let setup_db cache =
  DbCache.setup_db cache;
  if cache.CacheData.in_memory then
    MemoryCache.setup cache
(* END Setup *)

let clean_up_cache cache =
  if Sys.file_exists cache.CacheData.cache_dir &&
     Sys.is_directory cache.CacheData.cache_dir then begin
    Array.iter
      (fun file ->
         try
           Sys.remove (Filename.concat cache.cache_dir file)
         with e ->
           Utils.log_with_header "Error removing file %s: %s\n%!"
             file (Printexc.to_string e))
      (Sys.readdir cache.cache_dir)
  end

let compute_cache_size cache =
  if Sys.file_exists cache.CacheData.cache_dir &&
     Sys.is_directory cache.CacheData.cache_dir then begin
    Array.fold_left
      (fun size file ->
         try
           let path = Filename.concat cache.cache_dir file in
           if Sys.file_exists path && path <> cache.db_path then begin
             let stats = Unix.LargeFile.stat path in
             let file_size = stats.Unix.LargeFile.st_size in
             Int64.add size file_size
           end else size
         with e -> Utils.log_exception e; size
      )
      0L
      (Sys.readdir cache.CacheData.cache_dir)
  end else 0L

let flush cache =
  if cache.CacheData.in_memory then
    MemoryCache.flush_db cache

