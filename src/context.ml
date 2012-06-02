open GapiLens.Infix

module ConfigFileStore = KeyValueStore.MakeFileStore(Config)

module StateFileStore = KeyValueStore.MakeFileStore(State)

type t = {
  app_dir : AppDir.t;
  config_store : ConfigFileStore.t;
  state_store : StateFileStore.t;
  gapi_config : GapiConfig.t;
  cache : Cache.t;
  curl_state : [`Initialized] GapiCurl.t;
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
let cache = {
  GapiLens.get = (fun x -> x.cache);
  GapiLens.set = (fun v x -> { x with cache = v })
}
let curl_state = {
  GapiLens.get = (fun x -> x.curl_state);
  GapiLens.set = (fun v x -> { x with curl_state = v })
}

let config_lens =
  config_store |-- ConfigFileStore.data

let state_lens =
  state_store |-- StateFileStore.data

let request_id_lens =
  state_lens |-- State.auth_request_id

let refresh_token_lens =
  state_lens |-- State.refresh_token

let ctx : t Global.t = Global.empty "context"

let get_ctx () = Global.get ctx

let save_state_store state_store =
  Utils.log_message "Saving application state in %s..."
    state_store.StateFileStore.path;
  StateFileStore.save state_store;
  Utils.log_message "done\n"

let save_state_from_context context =
  Global.set ctx context;
  save_state_store context.state_store

let save_config_store config_store =
  Utils.log_message "Saving configuration in %s..."
    config_store.ConfigFileStore.path;
  ConfigFileStore.save config_store;
  Utils.log_message "done\n"

