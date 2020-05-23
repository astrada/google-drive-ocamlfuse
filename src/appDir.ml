type t = {
  config_path : string;
  data_dir : string;
  cache_dir : string;
  log_dir : string;
  state_path : string;
  app_log_path : string;
  curl_log_path : string;
}

let config_path =
  {
    GapiLens.get = (fun x -> x.config_path);
    GapiLens.set = (fun v x -> { x with config_path = v });
  }

let data_dir =
  {
    GapiLens.get = (fun x -> x.data_dir);
    GapiLens.set = (fun v x -> { x with data_dir = v });
  }

let cache_dir =
  {
    GapiLens.get = (fun x -> x.cache_dir);
    GapiLens.set = (fun v x -> { x with cache_dir = v });
  }

let log_dir =
  {
    GapiLens.get = (fun x -> x.log_dir);
    GapiLens.set = (fun v x -> { x with log_dir = v });
  }

let state_path =
  {
    GapiLens.get = (fun x -> x.state_path);
    GapiLens.set = (fun v x -> { x with state_path = v });
  }

let app_log_path =
  {
    GapiLens.get = (fun x -> x.app_log_path);
    GapiLens.set = (fun v x -> { x with app_log_path = v });
  }

let curl_log_path =
  {
    GapiLens.get = (fun x -> x.curl_log_path);
    GapiLens.set = (fun v x -> { x with curl_log_path = v });
  }

let cache_dir =
  {
    GapiLens.get = (fun x -> x.cache_dir);
    GapiLens.set = (fun v x -> { x with cache_dir = v });
  }

let ( // ) = Filename.concat

let home = Sys.getenv "HOME"

let default_base_dir = home // ".gdfuse"

(* XDG Base Directory *)
let xdg_data_home =
  try Sys.getenv "XDG_DATA_HOME" with Not_found -> home // ".local" // "share"

let xdg_config_home =
  try Sys.getenv "XDG_CONFIG_HOME" with Not_found -> home // ".config"

let xdg_cache_home =
  try Sys.getenv "XDG_CACHE_HOME" with Not_found -> home // ".cache"

(* END XDG Base Directory *)

let get_config_path config_path xdg_base_directory base_dir fs_label =
  let xdg_config_dir = xdg_config_home // "gdfuse" // fs_label in
  let xdg_config_path = xdg_config_dir // "config" in
  if config_path <> "" then (config_path, false)
  else if xdg_base_directory then (
    Utils.safe_makedir xdg_config_dir;
    (xdg_config_path, true) )
  else if Sys.file_exists xdg_config_path then (xdg_config_path, true)
  else
    let base_dir = if base_dir = "" then default_base_dir else base_dir in
    (base_dir // fs_label // "config", false)

let create config config_path base_dir fs_label xdg_base_directory =
  let data_dir =
    if config.Config.data_directory <> "" then config.Config.data_directory
    else if xdg_base_directory then xdg_data_home // "gdfuse" // fs_label
    else
      let base_dir = if base_dir = "" then default_base_dir else base_dir in
      base_dir // fs_label
  in
  let cache_dir =
    if config.Config.cache_directory <> "" then config.Config.cache_directory
    else if xdg_base_directory then xdg_cache_home // "gdfuse" // fs_label
    else data_dir // "cache"
  in
  let log_dir =
    if config.Config.log_directory <> "" then config.Config.log_directory
    else if xdg_base_directory then cache_dir // "log"
    else data_dir
  in
  {
    config_path;
    data_dir;
    cache_dir;
    log_dir;
    state_path = data_dir // "state";
    app_log_path = log_dir // "gdfuse.log";
    curl_log_path = log_dir // "curl.log";
  }

let create_directories app_dir =
  Utils.safe_makedir app_dir.data_dir;
  Utils.safe_makedir app_dir.cache_dir;
  Utils.safe_makedir app_dir.log_dir
