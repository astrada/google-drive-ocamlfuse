open GapiUtils.Infix
open GapiLens.Infix

module ConfigFileStore = KeyValueStore.MakeFileStore(Config)

module StateFileStore = KeyValueStore.MakeFileStore(State)

type t = {
  (* Application paths *)
  app_dir : AppDir.t;
  (* Configuration filesystem storage *)
  config_store : ConfigFileStore.t;
  (* State filesystem storage *)
  state_store : StateFileStore.t;
  (* Gapi configuration *)
  gapi_config : GapiConfig.t;
  (* Sqlite3 cache *)
  cache_table : (int, Cache.t) Hashtbl.t;
  (* CURL global state *)
  curl_state : [`Initialized] GapiCurl.t;
  (* Mountpoint current stats *)
  mountpoint_stats : Unix.LargeFile.stats;
  (* Current metadata *)
  metadata : Cache.Metadata.t option;
}

let app_dir = {
  GapiLens.get = (fun x -> x.app_dir);
  GapiLens.set = (fun v x -> { x with app_dir = v })
}
let config_store = {
  GapiLens.get = (fun x -> x.config_store);
  GapiLens.set = (fun v x -> { x with config_store = v })
}
let state_store = {
  GapiLens.get = (fun x -> x.state_store);
  GapiLens.set = (fun v x -> { x with state_store = v })
}
let gapi_config = {
  GapiLens.get = (fun x -> x.gapi_config);
  GapiLens.set = (fun v x -> { x with gapi_config = v })
}
let cache_table = {
  GapiLens.get = (fun x -> x.cache_table);
  GapiLens.set = (fun v x -> { x with cache_table = v })
}
let curl_state = {
  GapiLens.get = (fun x -> x.curl_state);
  GapiLens.set = (fun v x -> { x with curl_state = v })
}
let mountpoint_stats = {
  GapiLens.get = (fun x -> x.mountpoint_stats);
  GapiLens.set = (fun v x -> { x with mountpoint_stats = v })
}
let metadata = {
  GapiLens.get = (fun x -> x.metadata);
  GapiLens.set = (fun v x -> { x with metadata = v })
}

let config_lens =
  config_store |-- ConfigFileStore.data

let state_lens =
  state_store |-- StateFileStore.data

let request_id_lens =
  state_lens |-- State.auth_request_id

let refresh_token_lens =
  state_lens |-- State.refresh_token

module ConcurrentContext =
  ConcurrentGlobal.Make(struct type u = t let label = "context" end)

let get_ctx = ConcurrentContext.get

let set_ctx = ConcurrentContext.set

let clear_ctx = ConcurrentContext.clear

let save_state_store state_store =
  Utils.log_message "Saving application state in %s..."
    state_store.StateFileStore.path;
  StateFileStore.save state_store;
  Utils.log_message "done\n"

let save_state_from_context context =
  ConcurrentContext.with_lock
    (fun () ->
       save_state_store context.state_store;
       ConcurrentContext.set_no_lock context)

let save_config_store config_store =
  ConcurrentContext.with_lock
    (fun () ->
       Utils.log_message "Saving configuration in %s..."
         config_store.ConfigFileStore.path;
       ConfigFileStore.save config_store;
       Utils.log_message "done\n")

let set_cache cache =
  ConcurrentContext.with_lock
    (fun () ->
       let context = ConcurrentContext.get_no_lock () in
       let key = Thread.self () |> Thread.id in
         Hashtbl.add context.cache_table key cache)

let get_cache () =
  ConcurrentContext.with_lock
    (fun () ->
       let context = ConcurrentContext.get_no_lock () in
       let key = Thread.self () |> Thread.id in
         match Utils.safe_find context.cache_table key with
             None ->
               let cache = Cache.open_db context.app_dir in
                 Hashtbl.add context.cache_table key cache;
                 cache
           | Some cache -> cache)

let close_cache () =
  ConcurrentContext.with_lock
    (fun () ->
       let context = ConcurrentContext.get_no_lock () in
         Hashtbl.iter
           (fun key cache ->
              let res = Cache.close_db cache in
              Utils.log_message "Thread id: %d Sqlite3.close_db: %b\n" key res)
           context.cache_table)

