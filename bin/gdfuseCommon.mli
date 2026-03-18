val default_fs_label : string

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

val get_authorization_url : string -> string
val get_config_store : bool -> string -> ConfigStore.load_result
val generate_request_id : unit -> string
val create_empty_state_store : AppDir.t -> Context.StateFileStore.t
val get_state_store : AppDir.t -> Context.StateFileStore.t
