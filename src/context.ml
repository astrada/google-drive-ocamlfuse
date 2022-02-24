open GapiLens.Infix
module ConfigFileStore = KeyValueStore.MakeFileStore (Config)
module StateFileStore = KeyValueStore.MakeFileStore (State)

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
  cache : CacheData.t;
  (* CURL global state *)
  curl_state : [ `Initialized ] GapiCurl.t;
  (* Mountpoint path *)
  mountpoint_path : string;
  (* Mountpoint current stats *)
  mountpoint_stats : Unix.LargeFile.stats;
  (* Current metadata *)
  metadata : CacheData.Metadata.t option;
  (* Metadata lock *)
  metadata_lock : Mutex.t;
  (* Whether permanently delete files *)
  skip_trash : bool;
  (* Memory buffers *)
  memory_buffers : Buffering.MemoryBuffers.t;
  (* File locks *)
  file_locks : (string, Mutex.t) Hashtbl.t;
  (* Memory buffer eviction thread *)
  buffer_eviction_thread : Thread.t option;
  (* Root folder ID *)
  root_folder_id : string option;
  (* Metadata memory cache saving thread *)
  flush_db_thread : Thread.t option;
  (* Async upload thread *)
  async_upload_thread : Thread.t option;
  (* Background folder fetching thread *)
  folder_fetching_thread : Thread.t option;
  (* OAuth2 verification code *)
  verification_code : string;
}

let app_dir =
  {
    GapiLens.get = (fun x -> x.app_dir);
    GapiLens.set = (fun v x -> { x with app_dir = v });
  }

let config_store =
  {
    GapiLens.get = (fun x -> x.config_store);
    GapiLens.set = (fun v x -> { x with config_store = v });
  }

let state_store =
  {
    GapiLens.get = (fun x -> x.state_store);
    GapiLens.set = (fun v x -> { x with state_store = v });
  }

let gapi_config =
  {
    GapiLens.get = (fun x -> x.gapi_config);
    GapiLens.set = (fun v x -> { x with gapi_config = v });
  }

let cache =
  {
    GapiLens.get = (fun x -> x.cache);
    GapiLens.set = (fun v x -> { x with cache = v });
  }

let curl_state =
  {
    GapiLens.get = (fun x -> x.curl_state);
    GapiLens.set = (fun v x -> { x with curl_state = v });
  }

let mountpoint_path =
  {
    GapiLens.get = (fun x -> x.mountpoint_path);
    GapiLens.set = (fun v x -> { x with mountpoint_path = v });
  }

let mountpoint_stats =
  {
    GapiLens.get = (fun x -> x.mountpoint_stats);
    GapiLens.set = (fun v x -> { x with mountpoint_stats = v });
  }

let metadata =
  {
    GapiLens.get = (fun x -> x.metadata);
    GapiLens.set = (fun v x -> { x with metadata = v });
  }

let metadata_lock =
  {
    GapiLens.get = (fun x -> x.metadata_lock);
    GapiLens.set = (fun v x -> { x with metadata_lock = v });
  }

let skip_trash =
  {
    GapiLens.get = (fun x -> x.skip_trash);
    GapiLens.set = (fun v x -> { x with skip_trash = v });
  }

let memory_buffers =
  {
    GapiLens.get = (fun x -> x.memory_buffers);
    GapiLens.set = (fun v x -> { x with memory_buffers = v });
  }

let file_locks =
  {
    GapiLens.get = (fun x -> x.file_locks);
    GapiLens.set = (fun v x -> { x with file_locks = v });
  }

let buffer_eviction_thread =
  {
    GapiLens.get = (fun x -> x.buffer_eviction_thread);
    GapiLens.set = (fun v x -> { x with buffer_eviction_thread = v });
  }

let root_folder_id =
  {
    GapiLens.get = (fun x -> x.root_folder_id);
    GapiLens.set = (fun v x -> { x with root_folder_id = v });
  }

let flush_db_thread =
  {
    GapiLens.get = (fun x -> x.flush_db_thread);
    GapiLens.set = (fun v x -> { x with flush_db_thread = v });
  }

let async_upload_thread =
  {
    GapiLens.get = (fun x -> x.async_upload_thread);
    GapiLens.set = (fun v x -> { x with async_upload_thread = v });
  }

let folder_fetching_thread =
  {
    GapiLens.get = (fun x -> x.folder_fetching_thread);
    GapiLens.set = (fun v x -> { x with folder_fetching_thread = v });
  }

let verification_code =
  {
    GapiLens.get = (fun x -> x.verification_code);
    GapiLens.set = (fun v x -> { x with verification_code = v });
  }

let config_lens = config_store |-- ConfigFileStore.data
let state_lens = state_store |-- StateFileStore.data
let request_id_lens = state_lens |-- State.auth_request_id
let refresh_token_lens = state_lens |-- State.refresh_token
let saved_version_lens = StateFileStore.data |-- State.saved_version
let metadata_lens = metadata |-- GapiLens.option_get
let metadata_last_update_lens = metadata_lens |-- CacheData.Metadata.last_update

module ConcurrentContext = ConcurrentGlobal.Make (struct
  type u = t

  let label = "context"
end)

let get_ctx = ConcurrentContext.get
let set_ctx = ConcurrentContext.set
let clear_ctx = ConcurrentContext.clear
let update_ctx = ConcurrentContext.update
let with_ctx_lock = ConcurrentContext.with_lock

let save_state_store state_store =
  Utils.log_with_header "BEGIN: Saving application state in %s\n"
    state_store.StateFileStore.path;
  StateFileStore.save state_store;
  Utils.log_with_header "END: Saving application state in %s\n"
    state_store.StateFileStore.path

let save_state_from_context context =
  ConcurrentContext.with_lock (fun () ->
      save_state_store context.state_store;
      ConcurrentContext.set_no_lock context)

let save_config_store config_store =
  ConcurrentContext.with_lock (fun () ->
      Utils.log_with_header "BEGIN: Saving configuration in %s\n"
        config_store.ConfigFileStore.path;
      ConfigFileStore.save config_store;
      Utils.log_with_header "END: Saving configuration in %s\n"
        config_store.ConfigFileStore.path)

let get_cache () = get_ctx () |. cache
