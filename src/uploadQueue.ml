open GapiLens.Infix

type t = {
  stop_async_upload : bool;
  upload_resource_by_id : int64 -> unit;
  thread_pool : ThreadPool.t;
}

let stop_async_upload =
  {
    GapiLens.get = (fun x -> x.stop_async_upload);
    GapiLens.set = (fun v x -> { x with stop_async_upload = v });
  }

let upload_resource_by_id =
  {
    GapiLens.get = (fun x -> x.upload_resource_by_id);
    GapiLens.set = (fun v x -> { x with upload_resource_by_id = v });
  }

let thread_pool =
  {
    GapiLens.get = (fun x -> x.thread_pool);
    GapiLens.set = (fun v x -> { x with thread_pool = v });
  }

module ConcurrentUploadQueue = ConcurrentGlobal.Make (struct
  type u = t

  let label = "upload-queue"
end)

let upload_resource cache =
  let d = ConcurrentUploadQueue.get () in
  let upload = d.upload_resource_by_id in
  let upload_entry = Cache.UploadQueue.select_next_resource cache in
  let do_work e =
    let entry_id = e.CacheData.UploadEntry.id in
    let resource_id = e.CacheData.UploadEntry.resource_id in
    Utils.log_with_header "Uploading queued entry (id=%Ld) resource_id=%Ld.\n%!"
      entry_id resource_id;
    Cache.UploadQueue.update_entry_state cache
      CacheData.UploadEntry.State.Uploading entry_id;
    (try upload resource_id
     with e ->
       Utils.log_with_header "Upload failed for queued entry (id=%Ld).\n%!"
         entry_id;
       Cache.UploadQueue.update_entry_state cache
         CacheData.UploadEntry.State.ToUpload entry_id;
       raise e);
    Utils.log_with_header "Removing queued entry (id=%Ld).\n%!" entry_id;
    Cache.UploadQueue.delete_upload_entry cache e
  in
  match upload_entry with
  | Some e -> ThreadPool.add_work do_work e d.thread_pool
  | None -> ()

let poll_upload_queue cache =
  let check () =
    let d = ConcurrentUploadQueue.get () in
    if d.stop_async_upload then (
      let entries = Cache.UploadQueue.count_entries cache in
      Utils.log_with_header "Waiting for pending uploads (%d)\n%!" entries;
      if entries = 0 then raise Exit)
  in
  try
    while true do
      check ();
      Thread.delay 1.0;
      upload_resource cache
    done
  with Exit ->
    let d = ConcurrentUploadQueue.get () in
    Utils.log_with_header "Waiting for pending upload threads (%d)...%!"
      (ThreadPool.pending_threads d.thread_pool);
    ThreadPool.shutdown d.thread_pool;
    Utils.log_message "done\n%!"

let start_async_upload_thread cache upload_threads upload_resource =
  let data =
    {
      stop_async_upload = false;
      upload_resource_by_id = upload_resource;
      thread_pool = ThreadPool.create ~max_threads:upload_threads ();
    }
  in
  ConcurrentUploadQueue.set data;
  let thread = Thread.create poll_upload_queue cache in
  Utils.log_with_header "Starting async upload thread (TID=%d)\n%!"
    (Thread.id thread);
  Context.update_ctx (Context.async_upload_thread ^= Some thread)

let stop_async_upload_thread () =
  ConcurrentUploadQueue.update (fun q -> q |> stop_async_upload ^= true)

let queue_resource cache config resource =
  let resource_id = resource.CacheData.Resource.id in
  let wait_for_slot () =
    let check () =
      let max_length = config.Config.async_upload_queue_max_length in
      let entries = Cache.UploadQueue.count_entries cache in
      if entries >= max_length then
        Utils.log_with_header
          "Waiting for pending uploads (%d) to get below the limit (%d)\n%!"
          entries max_length
      else (
        Utils.log_with_header "Pending uploads (%d) below the limit (%d)\n%!"
          entries max_length;
        raise Exit)
    in
    try
      while true do
        check ();
        Thread.delay 1.0
      done
    with Exit -> ()
  in
  let queue_r () =
    let upload_entry =
      {
        CacheData.UploadEntry.id = 0L;
        resource_id;
        state = CacheData.UploadEntry.State.(to_string ToUpload);
        last_update = Unix.gettimeofday ();
      }
    in
    let inserted_upload_entry =
      Cache.UploadQueue.insert_upload_entry cache upload_entry
    in
    Utils.log_with_header
      "END: Resource id=%Ld queued for uploading (entry id=%Ld)\n%!" resource_id
      inserted_upload_entry.CacheData.UploadEntry.id
  in
  Utils.log_with_header "BEGIN: Queue resource id=%Ld for uploading\n%!"
    resource_id;
  let upload_entry_with_resource_id =
    Cache.UploadQueue.select_with_resource_id cache resource_id
  in
  match upload_entry_with_resource_id with
  | None ->
      if config.Config.async_upload_queue_max_length > 0 then wait_for_slot ();
      queue_r ()
  | Some e ->
      Utils.log_with_header
        "END: Resource with id=%Ld already queued (entry id=%Ld)\n%!"
        resource_id e.CacheData.UploadEntry.id
