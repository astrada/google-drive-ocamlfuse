open Utils.Infix
open GapiLens.Infix
open GapiLens.StateInfix
open GapiMonad.SessionM.Infix
open GdataDocumentsV3Model
open GdataDocumentsV3Service

exception ServerError of string

let verbose = ref false
let default_fs_label = "default"

let gae_proxy = "http://localhost:8080"
let client_id = "564921029129.apps.googleusercontent.com"
let redirect_uri = gae_proxy ^ "/oauth2callback"
let scope = [GdataDocumentsV3Service.all_scopes]

(* Logging *)
let log_message format =
  if !verbose then
    Printf.printf format
  else
    Printf.ifprintf stdout format

(* Authorization *)
let get_authorization_url request_id =
  GapiOAuth2.authorization_code_url
    ~redirect_uri
    ~scope
    ~state:request_id
    ~response_type:"code"
    client_id

let start_browser request_id =
  let start_process browser url =
    let command = Printf.sprintf "%s \"%s\"" browser url in
    let () = log_message "Starting web browser with command: %s..." command in
    let ch = Unix.open_process_in command in
    let status = Unix.close_process_in ch in
      if status = (Unix.WEXITED 0) then begin
        log_message "ok\n%!";
        true
      end else begin
        log_message "fail\n%!";
        false
      end
  in
  let url = get_authorization_url request_id in
  let browsers = ["xdg-open"; "firefox"; "google-chrome"] in
  let status =
    List.fold_left
      (fun result browser ->
         if result then
           result
         else
           start_process browser url)
      false
      browsers
  in
    if not status then
      failwith ("Error opening URL:" ^ url)

let get_tokens context =
  log_message "Getting tokens from GAE proxy...";
  let request_id = context |. Context.request_id_lens in
  let rid = Netencoding.Url.encode request_id in
  let gettokens_url = gae_proxy ^ "/gettokens?requestid=" ^ rid in
    GapiConversation.with_curl
      context.Context.gapi_config
      (fun session ->
         let (tokens, _) =
           GapiConversation.request
             GapiCore.HttpMethod.GET
             session
             gettokens_url
             (fun pipe code headers session ->
                let response = GapiConversation.read_all pipe in
                if code <> 200 then begin
                  log_message "fail\n%!";
                  raise (ServerError (Printf.sprintf
                                        "Server response: %s (code=%d)"
                                        response code));
                end else begin
                  match response with
                      "Not_found" ->
                        log_message "not found, retrying\n%!";
                        raise Not_found
                    | "access_denied"
                    | "ConflictError"
                    | "Exception" as error_code ->
                        log_message "fail (error_code=%s)\n%!" error_code;
                        raise (ServerError ("error_code " ^ error_code))
                    | "Missing_request_id" ->
                        failwith "Bug! Missing_request_id"
                    | _ -> ()
                end;
                log_message "ok\n%!";
                let json = Json_io.json_of_string response in
                let open Json_type.Browse in
                let obj = objekt json in
                let table = make_table obj in
                  context
                  |> Context.state_store
                  ^%= Context.StateFileStore.data ^= {
                    State.auth_request_id =
                      field table "request_id" |> string;
                    auth_request_date =
                      field table "date" |> string |> GapiDate.of_string;
                    user_id = field table "user_id" |> string;
                    refresh_token = field table "refresh_token" |> string;
                    last_access_token = field table "access_token" |> string;
                  }
             )
         in
           tokens)

let start_server_polling context =
  let rec loop n =
    if n = 24 then failwith "Cannot retrieve auth tokens: Timeout expired";
    try
      get_tokens context
    with Not_found ->
      Unix.sleep 5;
      loop (succ n)
  in
    loop 0
(* END Authorization *)

(* Setup *)
let rng =
  let open Cryptokit.Random in
  let dev_rng = device_rng "/dev/urandom" in
    string dev_rng 20 |> pseudo_rng

(* Application configuration *)
let create_default_config_store app_dir =
  let config = Config.default_debug in
  (* Save configuration file *)
  let config_store = {
    Context.ConfigFileStore.path = app_dir |. AppDir.config_path;
    data = config
  }
  in
    log_message "Saving configuration in %s..."
      config_store.Context.ConfigFileStore.path;
    Context.ConfigFileStore.save config_store;
    log_message "done\n";
    config_store

let get_config_store app_dir =
  let config_path = app_dir |. AppDir.config_path in
    try
      log_message "Loading configuration from %s..." config_path;
      let config_store = Context.ConfigFileStore.load config_path in
        log_message "done\n";
        config_store
    with KeyValueStore.File_not_found ->
      log_message "not found.\n";
      create_default_config_store app_dir
(* END Application configuration *)

(* Application state *)
let generate_request_id () =
  Cryptokit.Random.string rng 32 |> Base64.str_encode

let save_state state_store =
  log_message "Saving application state in %s..."
    state_store.Context.StateFileStore.path;
  Context.StateFileStore.save state_store;
  log_message "done\n"

let save_state_from_context context =
  save_state context.Context.state_store

let create_empty_state_store app_dir =
  let request_id = generate_request_id () in
  let state = State.empty |> State.auth_request_id ^= request_id in
  let state_store = {
    Context.StateFileStore.path = app_dir |. AppDir.state_path;
    data = state
  }
  in
    save_state state_store;
    state_store

let get_state_store app_dir =
  let state_path = app_dir |. AppDir.state_path in
    try
      log_message "Loading application state from %s..." state_path;
      let state_store = Context.StateFileStore.load state_path in
        log_message "done\n";
        state_store
    with KeyValueStore.File_not_found ->
      log_message "not found.\n";
      create_empty_state_store app_dir
(* END Application state *)

let setup_application fs_label =
  let get_auth_tokens_from_server context =
    let request_id =
      let rid = context |. Context.request_id_lens in
        if rid = "" then generate_request_id ()
        else rid
    in
    let context = context |> Context.request_id_lens ^= request_id in
      save_state context.Context.state_store;
      try
        start_browser request_id;
        start_server_polling context |> save_state_from_context;
      with
          ServerError e ->
            log_message "Removing invalid request_id=%s\n%!" request_id;
            context |> Context.request_id_lens ^= "" |> save_state_from_context;
            Printf.eprintf "Cannot retrieve auth tokens: %s\n%!" e;
            exit 1
        | e ->
            prerr_endline "Cannot retrieve auth tokens.";
            Printexc.to_string e |> prerr_endline;
            exit 1
  in

  log_message "Setting up %s filesystem...\n" fs_label;
  let app_dir = AppDir.create fs_label in
  let () = AppDir.create_directories app_dir in
  let config_store = get_config_store app_dir in
  let gapi_config =
    Config.create_gapi_config
      (config_store |. Context.ConfigFileStore.data)
      app_dir in
  let state_store = get_state_store app_dir in
  let context = {
    Context.app_dir;
    config_store;
    gapi_config;
    state_store;
  } in
  let refresh_token = context |. Context.refresh_token_lens in
    if refresh_token = "" then
      get_auth_tokens_from_server context
    else
      log_message "Refresh token already present.\n%!"
(* END setup *)

(* FUSE bindings *)
let init_filesystem mounpoint =
  log_message "Init filesystem %s\n" mounpoint
(* END FUSE bindings *)

(* Main program *)
let () =
  let setup = ref false in
  let fs_label = ref "default" in
  let mountpoint = ref "" in
  let program = Filename.basename Sys.executable_name in
  let usage =
    Printf.sprintf
      "Usage: %s [-verbose] [-label fslabel] -setup\n       %s mountpoint"
      program program in
  let arg_specs =
    Arg.align (
      ["-verbose",
       Arg.Set verbose,
       " Enable verbose logging for application setup. Default is false.";
       "-label",
       Arg.Set_string fs_label,
       " Use a specific label to identify the filesystem. \
        Default is \"default\".";
       "-setup",
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
      setup_application !fs_label;
    end;
    if !mountpoint <> "" then begin
      init_filesystem !mountpoint;
    end
(* END Main program *)

