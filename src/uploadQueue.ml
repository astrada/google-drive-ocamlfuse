open GapiLens.Infix

type t = {
  stop_async_upload : bool;
  upload_resource_by_id : int64 -> unit
}
let stop_async_upload = {
  GapiLens.get = (fun x -> x.stop_async_upload);
  GapiLens.set = (fun v x -> { x with stop_async_upload = v })
}
let upload_resource_by_id = {
  GapiLens.get = (fun x -> x.upload_resource_by_id);
  GapiLens.set = (fun v x -> { x with upload_resource_by_id = v })
}

module ConcurrentUploadQueue =
  ConcurrentGlobal.Make(struct type u = t let label = "upload-queue" end)

let upload_resource cache =
  let d = ConcurrentUploadQueue.get () in
  let upload = d.upload_resource_by_id in
  let upload_entry = Cache.UploadQueue.select_next_resource cache in
  match upload_entry with
  | Some e ->
    let entry_id = e.CacheData.UploadEntry.id in
    let resource_id = e.CacheData.UploadEntry.resource_id in
    Utils.log_with_header
      "Uploading queued entry (id=%Ld) resource_id=%Ld.\n%!"
      entry_id
      resource_id;
    Cache.UploadQueue.update_entry_state cache
      CacheData.UploadEntry.State.Uploading entry_id;
    begin try
      upload resource_id;
    with e ->
      Utils.log_with_header
        "Upload failed for queued entry (id=%Ld).\n%!"
        entry_id;
      Cache.UploadQueue.update_entry_state cache
        CacheData.UploadEntry.State.ToUpload entry_id;
      raise e;
    end;
    Utils.log_with_header
      "Removing queued entry (id=%Ld).\n%!"
      entry_id;
    Cache.UploadQueue.delete_upload_entry cache e
  | None -> ()

let poll_upload_queue cache =
  let check () =
    let d = ConcurrentUploadQueue.get () in
    if d.stop_async_upload then raise Exit in
  try
    while true do
      check ();
      Thread.delay 1.0;
      upload_resource cache;
    done
  with Exit -> ()

let start_async_upload_thread cache upload_resource =
  let data = {
    stop_async_upload = false;
    upload_resource_by_id = upload_resource;
  } in
  ConcurrentUploadQueue.set data;
  let thread =
    Thread.create poll_upload_queue cache in
  Utils.log_with_header
    "Starting async upload thread (TID=%d)\n%!"
    (Thread.id thread);
  Context.update_ctx (Context.async_upload_thread ^= Some thread)

let stop_async_upload_thread () =
  ConcurrentUploadQueue.update
    (fun q ->
       q |> stop_async_upload ^= true
    )

let queue_resource cache resource =
  let resource_id = resource.CacheData.Resource.id in
  Utils.log_with_header
    "BEGIN: Queue resource id=%Ld for uploading\n%!"
    resource_id;
  let upload_entry_with_resource_id =
    Cache.UploadQueue.select_with_resource_id cache resource_id in
  match upload_entry_with_resource_id with
  | None ->
    let upload_entry = {
      CacheData.UploadEntry.id = 0L;
      resource_id;
      state = CacheData.UploadEntry.State.(to_string ToUpload);
      last_update = Unix.gettimeofday ();
    } in
    let inserted_upload_entry =
      Cache.UploadQueue.insert_upload_entry cache upload_entry in
    Utils.log_with_header
      "END: Resource id=%Ld queued for uploading (entry id=%Ld)\n%!"
      resource_id
      inserted_upload_entry.CacheData.UploadEntry.id;
  | Some e ->
    Utils.log_with_header
      "END: Resource with id=%Ld already queued (entry id=%Ld)\n%!"
      resource_id
      e.CacheData.UploadEntry.id

