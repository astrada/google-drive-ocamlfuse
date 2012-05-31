open Utils.Infix
open GapiLens.Infix

let application_name = "google-drive-ocamlfuse"

type t = {
  debug : bool;
}

let debug = {
  GapiLens.get = (fun x -> x.debug);
  GapiLens.set = (fun v x -> { debug = v })
}

let of_table table =
  let get = Hashtbl.find table in
    { debug = get "debug" |> bool_of_string;
    }

let to_table data =
  let table = Hashtbl.create 1 in
  let add = Hashtbl.add table in
    add "debug" (data.debug |> string_of_bool);
    table

let default = {
  debug = false;
}

let default_debug = {
  debug = true;
}

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

