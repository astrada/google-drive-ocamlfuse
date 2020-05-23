open GapiLens.Infix

type t = { stop_folder_fetching : bool; read_dir : string -> unit }

let stop_folder_fetching =
  {
    GapiLens.get = (fun x -> x.stop_folder_fetching);
    GapiLens.set = (fun v x -> { x with stop_folder_fetching = v });
  }

let read_dir =
  {
    GapiLens.get = (fun x -> x.read_dir);
    GapiLens.set = (fun v x -> { x with read_dir = v });
  }

module ConcurrentBackgroundFolderFetching = ConcurrentGlobal.Make (struct
  type u = t

  let label = "background-folder-fetching"
end)

let fetch_next_folder cache =
  let resource = Cache.Resource.select_next_folder_to_fetch cache in
  match resource with
  | None -> ()
  | Some r ->
      let d = ConcurrentBackgroundFolderFetching.get () in
      let path = r.CacheData.Resource.path in
      let remote_id = Option.default "" r.CacheData.Resource.remote_id in
      Utils.log_with_header "BEGIN: Prefetching folder %s (id=%s).\n%!" path
        remote_id;
      d.read_dir path;
      Utils.log_with_header "END: Prefetching folder %s (id=%s).\n%!" path
        remote_id

let folder_fetch cache =
  let check () =
    let d = ConcurrentBackgroundFolderFetching.get () in
    if d.stop_folder_fetching then raise Exit
  in
  try
    while true do
      check ();
      Thread.delay 0.5;
      fetch_next_folder cache
    done
  with Exit -> ()

let start_folder_fetching_thread cache read_dir =
  let data = { stop_folder_fetching = false; read_dir } in
  ConcurrentBackgroundFolderFetching.set data;
  let thread = Thread.create folder_fetch cache in
  Utils.log_with_header
    "Starting background folder fetching thread (TID=%d)\n%!" (Thread.id thread);
  Context.update_ctx (Context.folder_fetching_thread ^= Some thread)

let stop_folder_fetching_thread () =
  ConcurrentBackgroundFolderFetching.update (fun b ->
      b |> stop_folder_fetching ^= true)
