open Utils.Infix
open GapiLens.Infix
open GapiLens.StateInfix
open GapiMonad.SessionM
open GdataDocumentsV3Model
open GdataDocumentsV3Service

(* Application files *)
let app_dir = Filename.concat (Sys.getenv "HOME") ".gdfuse"
let config_path = Filename.concat app_dir "config"
let state_path = Filename.concat app_dir "state"
let log_path = Filename.concat app_dir "log"

(* Google Services parameters *)
let application_name = "google-drive-ocamlfuse"
let gae_proxy = "http://localhost:8080"
let client_id = "564921029129.apps.googleusercontent.com"
let redirect_uri = gae_proxy ^ "/oauth2callback"
let scope = [GdataDocumentsV3Service.all_scopes]

let gapi_config = GapiConfig.default_debug
    |> GapiConfig.application_name ^= application_name

(* Authorization *)
let get_authorization_url request_id =
  GapiOAuth2.authorization_code_url
    ~redirect_uri
    ~scope
    ~state:request_id
    ~response_type:"code"
    client_id

let start_browser request_id =
  let url = get_authorization_url request_id in
  let command = Printf.sprintf "xdg-open \"%s\"" url in
  let ch = Unix.open_process_in command in
  let status = Unix.close_process_in ch in
    if status <> (Unix.WEXITED 0) then
      failwith ("Error executing this command: " ^ command)

type auth_data = {
  request_id : string;
  user_id : string;
  access_token : string;
  refresh_token : string;
  date : GapiDate.t;
}

let get_tokens request_id =
  let rid = Netencoding.Url.encode request_id in
  let gettokens_url = gae_proxy ^ "/gettokens?requestid=" ^ rid in
    GapiConversation.with_curl
      gapi_config
      (fun session ->
         let (tokens, _) =
           GapiConversation.request
             GapiCore.HttpMethod.GET
             session
             gettokens_url
             (fun pipe code headers session ->
                let response = GapiConversation.read_all pipe in
                if code <> 200 then begin
                  failwith (Printf.sprintf
                              "Cannot retrieve auth tokens: Server response: %s (code=%d)"
                              response code);
                end else if response = "Not_found" then begin
                  raise Not_found
                end;
                let json = Json_io.json_of_string response in
                let open Json_type.Browse in
                let obj = objekt json in
                let table = make_table obj in
                  { request_id = field table "request_id" |> string;
                    user_id = field table "user_id" |> string;
                    access_token = field table "access_token" |> string;
                    refresh_token = field table "refresh_token" |> string;
                    date = field table "date" |> string |> GapiDate.of_string;
                  }
             )
         in
           tokens)

let start_server_polling request_id =
  let rec loop n =
    if n = 30 then failwith "Cannot retrieve auth tokens: Timeout expired";
    try
      get_tokens request_id
    with Not_found ->
      Unix.sleep 10;
      loop (succ n)
  in
    loop 0
(* END Authorization *)

(* Setup *)
let rng =
  let open Cryptokit.Random in
  let dev_rng = device_rng "/dev/urandom" in
    string dev_rng 20 |> pseudo_rng

let setup_application () =
  print_endline "Setup";
  let request_id = Cryptokit.Random.string rng 32
    |> Base64.str_encode
  in
    start_browser request_id;
    try
      let tokens = start_server_polling request_id in
        Printf.printf "request_id=%s,\nuser_id=%s,\naccess_token=%s\nrefresh_token=%s\ndate=%s\n\n"
          tokens.request_id
          tokens.user_id
          tokens.access_token
          tokens.refresh_token
          (GapiDate.to_string tokens.date)
    with e ->
      prerr_endline "Cannot retrieve auth tokens.";
      Printexc.to_string e |> prerr_endline;
      exit 1
(* END setup *)

(* FUSE bindings *)
let init_filesystem mounpoint =
  print_endline ("Init filesystem " ^ mounpoint)
(* END FUSE bindings *)

(* Main program *)
let () =
  let setup = ref false in
  let mountpoint = ref "" in
  let program = Filename.basename Sys.executable_name in
  let usage =
    Printf.sprintf
      "Usage: %s -setup\n       %s mountpoint"
      program program in
  let arg_specs =
    Arg.align (
      ["-setup",
       Arg.Set setup,
       " Create configuration directory ~/.gdfuse and request oauth2 tokens.";
      ]) in
  let () =
    Arg.parse
      arg_specs
      (fun s -> mountpoint := s)
      usage in
  let () =
    if not !setup && !mountpoint = "" then begin
      prerr_endline "You must specify a mountpoint (or -setup option).";
      prerr_endline usage;
      exit 1
    end
  in
    if !setup then begin
      setup_application ();
    end;
    if !mountpoint <> "" then begin
      init_filesystem !mountpoint;
    end
(* END Main program *)

