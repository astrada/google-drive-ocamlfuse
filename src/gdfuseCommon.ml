open GapiUtils.Infix
open GapiLens.Infix

let default_fs_label = "default"
let client_id = "564921029129.apps.googleusercontent.com"
let redirect_uri = GaeProxy.gae_proxy_url ^ "/oauth2callback"

type application_params = {
  debug : bool;
  filesystem_label : string;
  client_id : string;
  client_secret : string;
  mountpoint : string;
  clear_cache : bool;
  headless : bool;
  skip_trash : bool;
  base_dir : string;
  multi_threading : bool;
  config_path : string;
  xdg_base_directory : bool;
  browser : string;
  docs_mode : string;
  service_account_credentials_path : string;
  service_account_user_to_impersonate : string;
  log_to : string;
  scope : string;
  redirect_uri : string;
  device : bool;
  port : int;
}

let get_authorization_url request_id =
  GapiOAuth2.authorization_code_url ~redirect_uri ~scope:Oauth2.scope
    ~state:request_id ~response_type:"code" client_id

let rng =
  let open Cryptokit.Random in
  let dev_rng = device_rng "/dev/urandom" in
  string dev_rng 20 |> pseudo_rng

let get_config_store debug config_path =
  Utils.log_with_header "Loading configuration from %s..." config_path;
  let result = ConfigStore.load_or_create ~debug config_path in
  (match result.ConfigStore.load_state with
  | ConfigStore.Created -> Utils.log_message "created.\n%!"
  | ConfigStore.Migrated -> Utils.log_message "migrated.\n%!"
  | ConfigStore.Upgraded -> Utils.log_message "upgraded.\n%!"
  | ConfigStore.Loaded -> Utils.log_message "done\n%!");
  result

let generate_request_id () =
  Cryptokit.Random.string rng 32
  |> Utils.base64_encode
  |> ExtString.String.replace_chars (function
    | '+' -> "-"
    | c -> ExtString.String.of_char c)

let create_empty_state_store app_dir =
  let request_id = generate_request_id () in
  let state =
    State.empty
    |> State.auth_request_id ^= request_id
    |> State.saved_version ^= Config.version
  in
  let state_store =
    { Context.StateFileStore.path = app_dir.AppDir.state_path; data = state }
  in
  Context.save_state_store state_store;
  state_store

let get_state_store app_dir =
  let state_path = app_dir.AppDir.state_path in
  try
    Utils.log_with_header "Loading application state from %s..." state_path;
    let state_store = Context.StateFileStore.load state_path in
    Utils.log_message "done\n%!";
    state_store
  with KeyValueStore.File_not_found ->
    Utils.log_message "not found.\n%!";
    create_empty_state_store app_dir
