open GapiLens.Infix

type t = {
  stop_async_upload : bool;
}
let stop_async_upload = {
  GapiLens.get = (fun x -> x.stop_async_upload);
  GapiLens.set = (fun v x -> { stop_async_upload = v })
}

module ConcurrentUploadQueue =
  ConcurrentGlobal.Make(struct type u = t let label = "upload-queue" end)

let upload_resource cache =
  ()

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

let start_async_upload_thread cache =
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

