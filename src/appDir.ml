type t = {
  fs_label : string;
  base_dir : string;
  app_dir : string;
  config_path : string;
  state_path : string;
  log_path : string;
  cache_dir : string;
}

let fs_label = {
  GapiLens.get = (fun x -> x.fs_label);
  GapiLens.set = (fun v x -> { x with fs_label = v })
}
let base_dir = {
  GapiLens.get = (fun x -> x.base_dir);
  GapiLens.set = (fun v x -> { x with base_dir = v })
}
let app_dir = {
  GapiLens.get = (fun x -> x.app_dir);
  GapiLens.set = (fun v x -> { x with app_dir = v })
}
let config_path = {
  GapiLens.get = (fun x -> x.config_path);
  GapiLens.set = (fun v x -> { x with config_path = v })
}
let state_path = {
  GapiLens.get = (fun x -> x.state_path);
  GapiLens.set = (fun v x -> { x with state_path = v })
}
let log_path = {
  GapiLens.get = (fun x -> x.log_path);
  GapiLens.set = (fun v x -> { x with log_path = v })
}
let cache_dir = {
  GapiLens.get = (fun x -> x.cache_dir);
  GapiLens.set = (fun v x -> { x with cache_dir = v })
}

let create fs_label =
  let base_dir = Filename.concat (Sys.getenv "HOME") ".gdfuse" in
  let app_dir = Filename.concat base_dir fs_label in
    { fs_label;
      base_dir;
      app_dir;
      config_path = Filename.concat app_dir "config";
      state_path = Filename.concat app_dir "state";
      log_path = Filename.concat app_dir "log";
      cache_dir = Filename.concat app_dir "cache";
    }

let create_directories app_dir =
  let safe_makedir dir =
    if not (Sys.file_exists dir) then
      try
        Unix.mkdir dir 0o700
      with Sys_error _ -> ()
  in
    safe_makedir app_dir.base_dir;
    safe_makedir app_dir.app_dir

