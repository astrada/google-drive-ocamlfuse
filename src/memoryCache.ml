open GapiLens.Infix

type t = {
  metadata : CacheData.Metadata.t option;
  resources : (int64, CacheData.Resource.t) Hashtbl.t;
  last_resource_id : int64;
  upload_queue : (int64, CacheData.UploadEntry.t) Hashtbl.t;
  last_upload_entry_id : int64;
  dirty : bool;
  stop_flush_db : bool;
}

let metadata = {
  GapiLens.get = (fun x -> x.metadata);
  GapiLens.set = (fun v x -> { x with metadata = v })
}
let resources = {
  GapiLens.get = (fun x -> x.resources);
  GapiLens.set = (fun v x -> { x with resources = v })
}
let last_resource_id = {
  GapiLens.get = (fun x -> x.last_resource_id);
  GapiLens.set = (fun v x -> { x with last_resource_id = v })
}
let upload_queue = {
  GapiLens.get = (fun x -> x.upload_queue);
  GapiLens.set = (fun v x -> { x with upload_queue = v })
}
let last_upload_entry_id = {
  GapiLens.get = (fun x -> x.last_upload_entry_id);
  GapiLens.set = (fun v x -> { x with last_upload_entry_id = v })
}
let dirty = {
  GapiLens.get = (fun x -> x.dirty);
  GapiLens.set = (fun v x -> { x with dirty = v })
}
let stop_flush_db = {
  GapiLens.get = (fun x -> x.stop_flush_db);
  GapiLens.set = (fun v x -> { x with stop_flush_db = v })
}

module ConcurrentMemoryCache =
  ConcurrentGlobal.Make(struct type u = t let label = "memory-cache" end)

module Resource =
struct
  let delete_all_with_path d path trashed =
    let resources =
      Hashtbl.fold
        (fun _ r rs ->
           if r.CacheData.Resource.path = path &&
              r.CacheData.Resource.trashed = trashed
           then
             r :: rs
           else
             rs
        )
        d.resources
        [] in
    List.iter
      (fun resource ->
         Hashtbl.remove d.resources resource.CacheData.Resource.id)
      resources

  (* Queries *)
  let insert_resource cache resource =
    let result = ref resource in
    ConcurrentMemoryCache.update
      (fun d ->
         delete_all_with_path d
           resource.CacheData.Resource.path
           resource.CacheData.Resource.trashed;
         let new_id = Int64.succ d.last_resource_id in
         let r = resource |> CacheData.Resource.id ^= new_id in
         Hashtbl.replace d.resources new_id r;
         result := r;
         d |> last_resource_id ^= new_id
           |> dirty ^= true
      );
      !result

  let update_resource cache resource =
    ConcurrentMemoryCache.update
      (fun d ->
         Hashtbl.replace d.resources resource.CacheData.Resource.id resource;
         d |> dirty ^= true
      )

  let update_resource_state cache state id =
    ConcurrentMemoryCache.update
      (fun d ->
         try
           let resource = Hashtbl.find d.resources id in
           let r = resource |> CacheData.Resource.state ^= state in
           Hashtbl.replace d.resources resource.CacheData.Resource.id r;
           d |> dirty ^= true
         with Not_found -> d
      )

  let update_resource_state_and_size cache state size id =
    ConcurrentMemoryCache.update
      (fun d ->
         try
           let resource = Hashtbl.find d.resources id in
           let r = resource
                   |> CacheData.Resource.state ^= state
                   |> CacheData.Resource.size ^= Some size in
           Hashtbl.replace d.resources resource.CacheData.Resource.id r;
           d |> dirty ^= true
         with Not_found -> d
      )

  let delete_resource cache resource =
    ConcurrentMemoryCache.update
      (fun d ->
         Hashtbl.remove d.resources resource.CacheData.Resource.id;
         d |> dirty ^= true
      )

  let delete_not_found_resource_with_path cache path =
    ConcurrentMemoryCache.update
      (fun d ->
         let resources =
           Hashtbl.fold
             (fun _ r rs ->
                if r.CacheData.Resource.state =
                   CacheData.Resource.State.NotFound &&
                   r.CacheData.Resource.path = path then
                  r :: rs
                else
                  rs
             )
             d.resources
             [] in
         List.iter
           (fun resource ->
              Hashtbl.remove d.resources resource.CacheData.Resource.id)
           resources;
         d |> dirty ^= true
      )

  let delete_resources cache resources =
    ConcurrentMemoryCache.update
      (fun d ->
         List.iter
           (fun resource ->
              Hashtbl.remove d.resources resource.CacheData.Resource.id;
           )
           resources;
         d |> dirty ^= true
      )

  let delete_resources_with_parent_path d parent_path trashed =
    let resources =
      Hashtbl.fold
        (fun _ r rs ->
           if r.CacheData.Resource.parent_path = parent_path &&
              r.CacheData.Resource.trashed = Some trashed
           then
             r :: rs
           else
             rs
        )
        d.resources
        [] in
    List.iter
      (fun resource ->
         Hashtbl.remove d.resources resource.CacheData.Resource.id)
      resources

  let insert_resources cache resources parent_path trashed =
    let result = ref [] in
    ConcurrentMemoryCache.update
      (fun d ->
         delete_resources_with_parent_path d parent_path trashed;
         let (rs, lid) =
           List.fold_left
             (fun (rs, last_resource_id) r ->
                let new_id = Int64.succ last_resource_id in
                let r = r |> CacheData.Resource.id ^= new_id in
                Hashtbl.replace d.resources new_id r;
                (r :: rs, new_id)
             )
             ([], d.last_resource_id)
             resources in
         result := rs;
         d |> last_resource_id ^= lid
           |> dirty ^= true
      );
    !result

  let is_invalidable r =
    match r.CacheData.Resource.state with
    | CacheData.Resource.State.ToUpload
    | Uploading
    | NotFound -> false
    | _ -> true

  let invalidate_resource d r =
    let r =
      r |> CacheData.Resource.state ^=
           CacheData.Resource.State.ToDownload in
    Hashtbl.replace d.resources r.CacheData.Resource.id r

  let invalidate_resources cache ids =
    ConcurrentMemoryCache.update
      (fun d ->
         List.iter
           (fun id ->
              try
                let r = Hashtbl.find d.resources id in
                if is_invalidable r then
                  invalidate_resource d r
              with Not_found -> ())
           ids;
         d |> dirty ^= true
      )

  let invalidate_path cache path =
    ConcurrentMemoryCache.update
      (fun d ->
         let resources =
           Hashtbl.fold
             (fun _ r rs ->
                if r.CacheData.Resource.path = path && is_invalidable r then
                  r :: rs
                else
                  rs
             )
             d.resources
             [] in
         List.iter
           (invalidate_resource d)
           resources;
         d |> dirty ^= true
      )

  let invalidate_all cache =
    ConcurrentMemoryCache.update
      (fun d ->
         let resources =
           Hashtbl.fold
             (fun _ r rs ->
                if is_invalidable r then
                  r :: rs
                else
                  rs
             )
             d.resources
             [] in
         List.iter
           (invalidate_resource d)
           resources;
         d |> dirty ^= true
      )

  let invalidate_trash_bin cache =
    ConcurrentMemoryCache.update
      (fun d ->
         let resources =
           Hashtbl.fold
             (fun _ r rs ->
                if r.CacheData.Resource.path = "/" &&
                   r.CacheData.Resource.trashed = Some true &&
                   is_invalidable r then
                  r :: rs
                else
                  rs
             )
             d.resources
             [] in
         List.iter
           (invalidate_resource d)
           resources;
         d |> dirty ^= true
      )

  let trash_resources cache resources =
    ConcurrentMemoryCache.update
      (fun d ->
         List.iter
           (fun r ->
             let r = r |> CacheData.Resource.trashed ^= Some true in
             Hashtbl.replace d.resources r.CacheData.Resource.id r;
           )
           resources;
         d |> dirty ^= true
      )

  let delete_all_with_parent_path cache parent_path trashed =
    ConcurrentMemoryCache.update
      (fun d ->
         let ids =
           Hashtbl.fold
             (fun _ r ids ->
                if r.CacheData.Resource.parent_path = parent_path &&
                   r.CacheData.Resource.trashed = Some trashed
                then
                  r.CacheData.Resource.id :: ids
                else
                  ids
             )
             d.resources
             [] in
         List.iter
           (Hashtbl.remove d.resources)
           ids;
         d |> dirty ^= true
      )

  let trash_all_with_parent_path cache parent_path =
    ConcurrentMemoryCache.update
      (fun d ->
         let resources =
           Hashtbl.fold
             (fun _ r rs ->
                if r.CacheData.Resource.parent_path = parent_path &&
                   not (Option.default false r.CacheData.Resource.trashed)
                then
                  (r |> CacheData.Resource.trashed ^= Some true) :: rs
                else
                  rs
             )
             d.resources
             [] in
         List.iter
           (fun r ->
              Hashtbl.replace d.resources r.CacheData.Resource.id r)
           resources;
         d |> dirty ^= true
      )

  let update_all_timestamps cache last_update =
    ConcurrentMemoryCache.update
      (fun d ->
         Hashtbl.iter
           (fun _ r ->
              let r =
                r |> CacheData.Resource.last_update ^= last_update in
              Hashtbl.replace d.resources r.CacheData.Resource.id r
           )
           d.resources;
         d |> dirty ^= true
      )

  let select_resource_with_path cache path trashed =
    ConcurrentMemoryCache.with_lock
      (fun () ->
         let result = ref None in
         let d = ConcurrentMemoryCache.get_no_lock () in
         begin try
             Hashtbl.iter
               (fun _ r ->
                  if r.CacheData.Resource.path = path &&
                     r.CacheData.Resource.trashed = Some trashed
                  then begin
                    result := Some r;
                    raise Exit
                  end
               )
               d.resources
           with Exit -> ()
         end;
         !result
      )

  let select_first_resource_with_remote_id cache remote_id =
    ConcurrentMemoryCache.with_lock
      (fun () ->
         let result = ref None in
         let d = ConcurrentMemoryCache.get_no_lock () in
         begin try
             Hashtbl.iter
               (fun _ r ->
                  if r.CacheData.Resource.remote_id = Some remote_id then begin
                    result := Some r;
                    raise Exit
                  end
               )
               d.resources
           with Exit -> ()
         end;
         !result
      )

  let select_resources_with_remote_id cache remote_id =
    ConcurrentMemoryCache.with_lock
      (fun () ->
         let result = ref [] in
         let d = ConcurrentMemoryCache.get_no_lock () in
         Hashtbl.iter
           (fun _ r ->
              if r.CacheData.Resource.remote_id = Some remote_id then begin
                result := r :: !result;
              end
           )
           d.resources;
         !result
      )

  let select_resources_with_parent_path cache parent_path trashed =
    ConcurrentMemoryCache.with_lock
      (fun () ->
         let d = ConcurrentMemoryCache.get_no_lock () in
         Hashtbl.fold
           (fun _ r rs ->
              if r.CacheData.Resource.parent_path = parent_path &&
                 r.CacheData.Resource.trashed = Some trashed &&
                 r.CacheData.Resource.state !=
                 CacheData.Resource.State.NotFound
              then
                r :: rs
              else
                rs
           )
           d.resources
           []
      )

  let select_resources_order_by_last_update cache =
    ConcurrentMemoryCache.with_lock
      (fun () ->
         let d = ConcurrentMemoryCache.get_no_lock () in
         let resources =
           Hashtbl.fold
             (fun _ r rs ->
                if Option.default 0L r.CacheData.Resource.size > 0L &&
                   r.CacheData.Resource.state =
                   CacheData.Resource.State.Synchronized
                then
                  r :: rs
                else
                  rs
             )
             d.resources
             [] in
         List.sort
           (fun x y ->
              compare
                x.CacheData.Resource.last_update
                y.CacheData.Resource.last_update
           )
           resources
      )

  let select_resource_with_id cache id =
    ConcurrentMemoryCache.with_lock
      (fun () ->
         let d = ConcurrentMemoryCache.get_no_lock () in
         Utils.safe_find d.resources id
      )

  let select_next_folder_to_fetch cache =
    ConcurrentMemoryCache.with_lock
      (fun () ->
         let d = ConcurrentMemoryCache.get_no_lock () in
         let resources =
           Hashtbl.fold
             (fun _ r rs ->
                if r.CacheData.Resource.mime_type =
                   Some "application/vnd.google-apps.folder" &&
                   r.CacheData.Resource.state =
                   CacheData.Resource.State.ToDownload &&
                   r.CacheData.Resource.trashed = Some false
                then
                  r :: rs
                else
                  rs
             )
             d.resources
             [] in
         if List.length resources = 0 then None
         else
           let sorted =
             List.sort
               (fun x y ->
                  compare
                    x.CacheData.Resource.last_update
                    y.CacheData.Resource.last_update
               )
               resources
           in
           Some (List.hd sorted)
      )

end

module Metadata =
struct
  (* Queries *)
  let insert_metadata cache new_metadata =
    ConcurrentMemoryCache.update
      (fun d ->
         d |> metadata ^= Some new_metadata
           |> dirty ^= true
      )

  let select_metadata cache =
    let data = ConcurrentMemoryCache.get () in
    data.metadata

  let update_cache_size cache delta =
    ConcurrentMemoryCache.update
      (fun d ->
         let updated_metadata =
           Option.map
             (fun m ->
                let cache_size =
                  Int64.add m.CacheData.Metadata.cache_size delta in
                m |> CacheData.Metadata.cache_size ^= cache_size)
             d.metadata in
         d |> metadata ^= updated_metadata
           |> dirty ^= true
      )

end

module UploadQueue =
struct
  let insert_upload_entry cache upload_entry =
    let result = ref upload_entry in
    ConcurrentMemoryCache.update
      (fun d ->
         let new_id = Int64.succ d.last_upload_entry_id in
         let e = upload_entry |> CacheData.UploadEntry.id ^= new_id in
         Hashtbl.replace d.upload_queue new_id e;
         result := e;
         d |> last_upload_entry_id ^= new_id
           |> dirty ^= true
      );
    !result

  let select_next_resource cache =
    ConcurrentMemoryCache.with_lock
      (fun () ->
         let to_upload =
           CacheData.UploadEntry.State.to_string
             CacheData.UploadEntry.State.ToUpload in
         let result = ref None in
         let d = ConcurrentMemoryCache.get_no_lock () in
         begin try
             Hashtbl.iter
               (fun _ e ->
                  if e.CacheData.UploadEntry.state = to_upload then begin
                    result := Some e;
                    raise Exit
                  end
               )
               d.upload_queue
           with Exit -> ()
         end;
         !result
      )

  let select_with_resource_id cache resource_id =
    ConcurrentMemoryCache.with_lock
      (fun () ->
         let result = ref None in
         let d = ConcurrentMemoryCache.get_no_lock () in
         begin try
             Hashtbl.iter
               (fun _ e ->
                  if e.CacheData.UploadEntry.resource_id = resource_id then begin
                    result := Some e;
                    raise Exit
                  end
               )
               d.upload_queue
           with Exit -> ()
         end;
         !result
      )

  let delete_upload_entry cache upload_entry =
    ConcurrentMemoryCache.update
      (fun d ->
         Hashtbl.remove d.upload_queue upload_entry.CacheData.UploadEntry.id;
         d |> dirty ^= true
      )

  let update_entry_state cache state id =
    ConcurrentMemoryCache.update
      (fun d ->
         let upload_entry = Utils.safe_find d.upload_queue id in
         begin match upload_entry with
         | Some e ->
           let updated_entry =
             e |> CacheData.UploadEntry.state ^=
                  (CacheData.UploadEntry.State.to_string state) in
           Hashtbl.replace d.upload_queue id updated_entry
         | None ->
           Utils.log_with_header
             "Cannot find upload entry with id=%Ld.\n%!"
             id
         end;
         d |> dirty ^= true
      )

  let count_entries cache =
    ConcurrentMemoryCache.with_lock
      (fun () ->
         let d = ConcurrentMemoryCache.get_no_lock () in
         Hashtbl.length d.upload_queue
      )

end

let setup cache =
  let metadata = DbCache.Metadata.select_metadata cache in
  let resources = Hashtbl.create 1024 in
  let all_resources = DbCache.Resource.select_all_resources cache in
  let last_resource_id = ref 0L in
  List.iter
    (fun r ->
       if r.CacheData.Resource.id > !last_resource_id then begin
         last_resource_id := r.CacheData.Resource.id
       end;
       Hashtbl.replace resources r.CacheData.Resource.id r)
    all_resources;
  let upload_queue = Hashtbl.create 1024 in
  let all_upload_entries = DbCache.UploadQueue.select_all_entries cache in
  let last_upload_entry_id = ref 0L in
  List.iter
    (fun e ->
       if e.CacheData.UploadEntry.id > !last_upload_entry_id then begin
         last_upload_entry_id := e.CacheData.UploadEntry.id
       end;
       Hashtbl.replace upload_queue e.CacheData.UploadEntry.id e)
    all_upload_entries;
  let data = {
    metadata;
    resources;
    last_resource_id = !last_resource_id;
    upload_queue;
    last_upload_entry_id = !last_upload_entry_id;
    dirty = false;
    stop_flush_db = false;
  } in
  ConcurrentMemoryCache.set data

let flush_db cache =
  let d =
    ConcurrentMemoryCache.with_lock
      (fun () ->
         let d = ConcurrentMemoryCache.get_no_lock () in
         let d' = d |> dirty ^= false in
         ConcurrentMemoryCache.set_no_lock d';
         d
      ) in
  if d.dirty then begin
    Utils.log_with_header "Flushing DB...%!";
    Option.may (fun m -> DbCache.Metadata.insert_metadata cache m) d.metadata;
    let resources =
      Hashtbl.fold
        (fun _ r rs -> r :: rs)
        d.resources
        [] in
    DbCache.Resource.flush_resources cache resources;
    let upload_queue =
      Hashtbl.fold
        (fun _ e es -> e :: es)
        d.upload_queue
        [] in
    DbCache.UploadQueue.flush_upload_queue cache upload_queue;
    Utils.log_message "done\n%!"
  end else ()

let flush_db_thread cache =
  let check () =
    let d = ConcurrentMemoryCache.get () in
    if d.stop_flush_db then raise Exit in
  try
    while true do
      for _ = 1 to cache.CacheData.autosaving_interval do
        check ();
        Thread.delay 1.0;
      done;
      flush_db cache;
    done
  with Exit -> ()

let create_flush_db_thread cache =
  Thread.create flush_db_thread cache

let start_flush_db_thread cache =
  if cache.CacheData.in_memory then
    let thread =
      create_flush_db_thread cache in
    Utils.log_with_header
      "Starting flush DB thread (TID=%d, interval=%ds)\n%!"
      (Thread.id thread)
      cache.CacheData.autosaving_interval;
    Context.update_ctx (Context.flush_db_thread ^= Some thread)
  else ()

let stop_flush_db_thread () =
  ConcurrentMemoryCache.update
    (fun d ->
       d |> stop_flush_db ^= true
    )

