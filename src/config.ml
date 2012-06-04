open GapiUtils.Infix
open GapiLens.Infix

let application_name = "google-drive-ocamlfuse"

type t = {
  (* Debug mode *)
  debug : bool;
  (* Number of seconds metadata should be cached. *)
  metadata_cache_time : int;
  (* Specifies whether local filesystem is mounted read-only. *)
  read_only : bool;
  (* umask mount option *)
  umask : int;
  (* Sqlite3 busy handler timeout in milliseconds *)
  sqlite3_busy_timeout : int;
}

let debug = {
  GapiLens.get = (fun x -> x.debug);
  GapiLens.set = (fun v x -> { x with debug = v })
}
let metadata_cache_time = {
  GapiLens.get = (fun x -> x.metadata_cache_time);
  GapiLens.set = (fun v x -> { x with metadata_cache_time = v })
}
let read_only = {
  GapiLens.get = (fun x -> x.read_only);
  GapiLens.set = (fun v x -> { x with read_only = v })
}
let umask = {
  GapiLens.get = (fun x -> x.umask);
  GapiLens.set = (fun v x -> { x with umask = v })
}
let sqlite3_busy_timeout = {
  GapiLens.get = (fun x -> x.sqlite3_busy_timeout);
  GapiLens.set = (fun v x -> { x with sqlite3_busy_timeout = v })
}

let umask =
  let prev_umask = Unix.umask 0 in
  let _ = Unix.umask prev_umask in
    prev_umask

let default = {
  debug = false;
  metadata_cache_time = 60;
  read_only = true;
  umask;
  sqlite3_busy_timeout = 500;
}

let default_debug = {
  debug = true;
  metadata_cache_time = 60;
  read_only = true;
  umask;
  sqlite3_busy_timeout = 500;
}

let of_table table =
  let get k = Utils.get_from_string_table table k in
    { debug = get "debug" bool_of_string default.debug;
      metadata_cache_time =
        get "metadata_cache_time" int_of_string default.metadata_cache_time;
      read_only = get "read_only" bool_of_string default.read_only;
      umask = get "umask" int_of_string default.umask;
      sqlite3_busy_timeout =
        get "sqlite3_busy_timeout" int_of_string default.sqlite3_busy_timeout;
    }

let to_table data =
  let table = Hashtbl.create 1 in
  let add = Hashtbl.add table in
    add "debug" (data.debug |> string_of_bool);
    add "metadata_cache_time" (data.metadata_cache_time |> string_of_int);
    add "read_only" (data.read_only |> string_of_bool);
    add "umask" (data.umask |> string_of_int);
    add "sqlite3_busy_timeout" (data.sqlite3_busy_timeout |> string_of_int);
    table

let debug_print out_ch start_time curl info_type info =
  let time = Unix.gettimeofday () in
  let timestamp = time -. start_time in
  let nl =
    if String.length info > 0 &&
       info.[String.length info - 1] = '\n' then ""
    else "\n"
  in
    Printf.fprintf out_ch "[%f] curl: %s: %s%s%!"
      timestamp
      (GapiCurl.string_of_curl_info_type info_type)
      info
      nl

let create_gapi_config config app_dir =
  let gapi_config =
    if config.debug then
      let out_ch = open_out (app_dir |. AppDir.log_path) in
      let debug_function = debug_print out_ch (Unix.gettimeofday ()) in
        GapiConfig.default_debug
        |> GapiConfig.debug ^= Some (GapiConfig.Custom debug_function)
    else
      GapiConfig.default
  in
    gapi_config
    |> GapiConfig.application_name ^= application_name
    (* Do not set client_id and client_secret, because authorization is
     * handled by the GAE proxy *)
    |> GapiConfig.auth ^= GapiConfig.OAuth2 { GapiConfig.client_id = "";
                                              client_secret = "" }

