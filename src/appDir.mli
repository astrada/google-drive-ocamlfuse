type t = {
  config_path : string;
  data_dir : string;
  cache_dir : string;
  log_dir : string;
  state_path : string;
  app_log_path : string;
  curl_log_path : string;
}

val config_path : (t, string) GapiLens.t
val data_dir : (t, string) GapiLens.t
val log_dir : (t, string) GapiLens.t
val state_path : (t, string) GapiLens.t
val app_log_path : (t, string) GapiLens.t
val curl_log_path : (t, string) GapiLens.t
val cache_dir : (t, string) GapiLens.t
val ( // ) : string -> string -> string
val home : string
val default_base_dir : string
val xdg_data_home : string
val xdg_config_home : string
val xdg_cache_home : string
val get_config_path : string -> bool -> string -> string -> string * bool
val create : Config.t -> string -> string -> string -> bool -> t
val create_directories : t -> unit
