type inputs = {
  persisted : Config.t;
  load_state : ConfigStore.load_state;
  cli_client_id : string;
  cli_client_secret : string;
  cli_service_account_credentials_path : string;
  cli_service_account_user_to_impersonate : string;
  cli_log_to : string;
  cli_scope : string;
  cli_redirect_uri : string;
  cli_docs_mode : string;
  cli_port : int;
  device : bool;
  multi_threading : bool;
}

type result = {
  runtime_config : Config.t;
  persisted_config : Config.t;
  should_persist : bool;
  clear_cache : bool;
}

val apply_docs_mode : string -> Config.t -> Config.t
val resolve : inputs -> result
