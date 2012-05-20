open GapiLens.Infix

module ConfigFileStore = KeyValueStore.MakeFileStore(Config)

module StateFileStore = KeyValueStore.MakeFileStore(State)

type t = {
  app_dir : AppDir.t;
  config_store : ConfigFileStore.t;
  state_store : StateFileStore.t;
  gapi_config : GapiConfig.t;
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

let request_id_lens =
  state_store |-- StateFileStore.data |-- State.auth_request_id

let refresh_token_lens =
  state_store |-- StateFileStore.data |-- State.refresh_token

