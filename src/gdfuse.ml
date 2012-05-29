open Utils.Infix
open GapiLens.Infix
open GapiLens.StateInfix

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

let gae_proxy_request page context =
  let request_id = context |. Context.request_id_lens in
  let rid = Netencoding.Url.encode request_id in
  let page_url = Printf.sprintf "%s/%s?requestid=%s" gae_proxy page rid in
    GapiConversation.with_curl
      context.Context.gapi_config
      (fun session ->
         let (tokens, _) =
           GapiConversation.request
             GapiCore.HttpMethod.GET
             session
             page_url
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

let get_tokens context =
  log_message "Getting tokens from GAE proxy...";
  gae_proxy_request "gettokens" context

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

let refresh_access_token context =
  log_message "Refreshing access token...";
  gae_proxy_request "refreshtoken" context
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
        let context =
          start_server_polling context
        in
          save_state_from_context context;
          context
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
  let cache = Cache.setup_db app_dir in
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
    cache;
  } in
  let refresh_token = context |. Context.refresh_token_lens in
    if refresh_token = "" then
      get_auth_tokens_from_server context
    else begin
      log_message "Refresh token already present.\n%!";
      context
    end
(* END setup *)

(* FUSE bindings *)
let init_filesystem () =
  log_message "init_filesystem\n%!"

    (* TODO:
let statfs path =
  log_message "statfs %s\n%!" path;
  { Unix_util.f_bsize = 0L;
    f_frsize = 0L;
    f_blocks = 0L;
    f_bfree = 0L;
    f_bavail = 0L;
    f_files = 0L;
    f_ffree = 0L;
    f_favail = 0L;
    f_fsid = 0L;
    f_flag = 0L;
    f_namemax = 0L;
  } *)

let getattr path =
  log_message "getattr %s\n%!" path;
  Unix.LargeFile.lstat path

let readdir path hnd =
  log_message "readdir %s %d\n%!" path hnd;
  "." :: ".." :: (Array.to_list (Sys.readdir path))

let start_filesystem mounpoint fuse_args context =
  log_message "Starting filesystem %s\n%!" mounpoint;
  let fuse_argv =
    Sys.argv.(0) :: (fuse_args @ [mounpoint])
    |> Array.of_list
  in
    Fuse.main fuse_argv {
      Fuse.default_operations with
          Fuse.init = init_filesystem;
          (* statfs; *)
          getattr;
          readdir;
          (*
          opendir = (fun path flags -> Unix.close (Unix.openfile path flags 0);None);
          releasedir = (fun path mode hnd -> ());
          fsyncdir = (fun path ds hnd -> Printf.printf "sync dir\n%!");
          readlink = Unix.readlink;
          utime = Unix.utimes;
          fopen = (fun path flags -> Some (store_descr (Unix.openfile path flags 0)));
          read = xmp_read;
          write = xmp_write;
          mknod = (fun path mode -> close (openfile path [O_CREAT;O_EXCL] mode));
          mkdir = Unix.mkdir;
          unlink = Unix.unlink;
          rmdir = Unix.rmdir;
          symlink = Unix.symlink;
          rename = Unix.rename;
          link = Unix.link;
          chmod = Unix.chmod;
          chown = Unix.chown;
          truncate = Unix.LargeFile.truncate;
          release = (fun path mode hnd -> Unix.close (retrieve_descr hnd));
          flush = (fun path hnd -> ());
          fsync = (fun path ds hnd -> Printf.printf "sync\n%!");
          listxattr = (fun path -> init_attr xattr path;lskeys (Hashtbl.find xattr path));
          getxattr = (fun path attr ->
                        with_xattr_lock (fun () ->
                                           init_attr xattr path;
                                           try
                                             Hashtbl.find (Hashtbl.find xattr path) attr
                                           with Not_found -> raise (Unix.Unix_error (EUNKNOWNERR 61 (* TODO: this is system-dependent *),"getxattr",path)))());
          setxattr = (fun path attr value flag -> (* TODO: This currently ignores flags *)
                        with_xattr_lock (fun () ->
                                           init_attr xattr path;
                                           Hashtbl.replace (Hashtbl.find xattr path) attr value) ());
          removexattr = (fun path attr ->
                           with_xattr_lock (fun () ->
                                              init_attr xattr path;
                                              Hashtbl.remove (Hashtbl.find xattr path) attr) ());
           *)
    }
(* END FUSE bindings *)

(* Main program *)
let () =
  let fs_label = ref "default" in
  let mountpoint = ref "" in
  let fuse_args = ref [] in
  let program = Filename.basename Sys.executable_name in
  let usage =
    Printf.sprintf
      "Usage: %s [-verbose] [-label fslabel] [-f] [-d] [-s] mountpoint"
      program in
  let arg_specs =
    Arg.align (
      ["-verbose",
       Arg.Set verbose,
       " Enable verbose logging for application setup. Default is false.";
       "-label",
       Arg.Set_string fs_label,
       " Use a specific label to identify the filesystem. \
        Default is \"default\".";
       "-f",
       Arg.Unit (fun _ -> fuse_args := "-f" :: !fuse_args),
       " keep the process in foreground.";
       "-d",
       Arg.Unit (fun _ -> fuse_args := "-d" :: !fuse_args),
       " enable FUSE debug output (implies -f).";
       "-s",
       Arg.Unit (fun _ -> fuse_args := "-s" :: !fuse_args),
       " disable multi-threaded operation.";
      ]) in
  let () =
    Arg.parse
      arg_specs
      (fun s -> mountpoint := s)
      usage in
  let () =
    if !mountpoint = "" then begin
      prerr_endline "You must specify a mountpoint.";
      prerr_endline usage;
      exit 1
    end
  in

  let context = setup_application !fs_label in
    at_exit
      (fun () ->
         let res = Cache.close_db context.Context.cache in
           log_message "close_db: %b\n%!" res);
    try
      start_filesystem !mountpoint !fuse_args context
    with e ->
      let error_message = Printexc.to_string e in begin
        Printf.eprintf "Error: %s\nExiting.\n" error_message;
        exit 1
      end
(* END Main program *)

