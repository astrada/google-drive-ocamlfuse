open GapiUtils.Infix
open GapiLens.Infix
open GapiLens.StateInfix

let default_fs_label = "default"

let client_id = "564921029129.apps.googleusercontent.com"
let redirect_uri = GaeProxy.gae_proxy_url ^ "/oauth2callback"

(* Authorization *)
let get_authorization_url request_id =
  GapiOAuth2.authorization_code_url
    ~redirect_uri
    ~scope:Oauth2.scope
    ~state:request_id
    ~response_type:"code"
    client_id
(* END Authorization *)

(* Setup *)
let rng =
  let open Cryptokit.Random in
  let dev_rng = device_rng "/dev/urandom" in
    string dev_rng 20 |> pseudo_rng

(* Application configuration *)
let create_default_config_store debug app_dir =
  let config =
    if debug then Config.default_debug
    else Config.default in
  (* Save configuration file *)
  let config_store = {
    Context.ConfigFileStore.path = app_dir |. AppDir.config_path;
    data = config
  }
  in
    Context.save_config_store config_store;
    config_store

let get_config_store debug app_dir =
  let config_path = app_dir |. AppDir.config_path in
    try
      Utils.log_with_header "Loading configuration from %s..." config_path;
      let config_store = Context.ConfigFileStore.load config_path in
        Utils.log_message "done\n";
        config_store
    with KeyValueStore.File_not_found ->
      Utils.log_message "not found.\n";
      create_default_config_store debug app_dir
(* END Application configuration *)

(* Application state *)
let generate_request_id () =
  Cryptokit.Random.string rng 32 |> Base64.str_encode

let create_empty_state_store app_dir =
  let request_id = generate_request_id () in
  let state = State.empty |> State.auth_request_id ^= request_id in
  let state_store = {
    Context.StateFileStore.path = app_dir |. AppDir.state_path;
    data = state
  }
  in
    Context.save_state_store state_store;
    state_store

let get_state_store app_dir =
  let state_path = app_dir |. AppDir.state_path in
    try
      Utils.log_with_header "Loading application state from %s..." state_path;
      let state_store = Context.StateFileStore.load state_path in
        Utils.log_message "done\n";
        state_store
    with KeyValueStore.File_not_found ->
      Utils.log_message "not found.\n";
      create_empty_state_store app_dir
(* END Application state *)

let setup_application debug fs_label client_id client_secret mountpoint =
  let get_auth_tokens_from_server () =
    let context = Context.get_ctx () in
    let request_id =
      let rid = context |. Context.request_id_lens in
        if rid = "" then generate_request_id ()
        else rid
    in
      context
        |> Context.request_id_lens ^= request_id
        |> Context.save_state_from_context;
      try
        let url = get_authorization_url request_id in
          Utils.start_browser url;
          GaeProxy.start_server_polling ()
      with
          GaeProxy.ServerError e ->
            Utils.log_with_header "Removing invalid request_id=%s\n%!"
              request_id;
            context
              |> Context.request_id_lens ^= ""
              |> Context.save_state_from_context;
            Printf.eprintf "Cannot retrieve auth tokens: %s\n%!" e;
            exit 1
        | e ->
            prerr_endline "Cannot retrieve auth tokens.";
            Printexc.to_string e |> prerr_endline;
            exit 1
  in

  Utils.log_message "Starting application setup (label=%s).\n%!" fs_label;
  let app_dir = AppDir.create fs_label in
  let () = AppDir.create_directories app_dir in
  let app_log_path = app_dir |. AppDir.app_log_path in
  Utils.log_message "Opening log file: %s\n%!" app_log_path;
  let log_channel = open_out app_log_path in
  Utils.log_channel := log_channel;
  Utils.log_with_header "Setting up %s filesystem...\n" fs_label;
  let config_store = get_config_store debug app_dir in
  let current_config = config_store |. Context.ConfigFileStore.data in
  let config =
    { current_config with
          Config.debug;
          client_id;
          client_secret;
    } in
  Context.save_config_store config_store;
  let config_store = config_store
    |> Context.ConfigFileStore.data ^= config in
  let gapi_config = Config.create_gapi_config config app_dir in
  Utils.log_message "Setting up cache db...";
  let cache = Cache.create_cache app_dir config in
  Cache.setup_db cache;
  Utils.log_message "done\n";
  let state_store = get_state_store app_dir in
  Utils.log_message "Setting up CURL...";
  let curl_state = GapiCurl.global_init () in
  Utils.log_message "done\n";
  let context = {
    Context.app_dir;
    config_store;
    gapi_config;
    state_store;
    cache;
    curl_state;
    mountpoint_stats = Unix.LargeFile.stat mountpoint;
    metadata = None;
  } in
  Context.set_ctx context;
  let refresh_token = context |. Context.refresh_token_lens in
    if refresh_token = "" then
      if client_id = "" || client_secret = "" then
        get_auth_tokens_from_server ()
      else
        Oauth2.get_access_token ()
    else
      Utils.log_message "Refresh token already present.\n%!"
(* END setup *)

(* FUSE bindings *)
let init () =
  Utils.log_with_header "init\n%!"

let destroy () =
  Utils.log_with_header "destroy\n%!"

let lookup req inode name =
  Utils.log_with_header "lookup %Ld %s\n%!" inode name;
  Docs.lookup req inode name

let getattr req inode =
  Utils.log_with_header "getattr %Ld\n%!" inode;
  Docs.getattr req inode

let openfile req inode file_info =
  Utils.log_with_header "openfile %Ld %d\n%!" inode file_info.Fuse.fi_flags;
  Docs.openfile req inode file_info

let read req inode size offset file_info =
  Utils.log_with_header "read %Ld %d %Ld %d\n%!"
    inode size offset file_info.Fuse.fi_flags;
  Docs.read req inode size offset file_info

let readdir req inode size offset file_info =
  Utils.log_with_header "readdir %Ld %d %Ld %d\n%!"
    inode size offset file_info.Fuse.fi_flags;
  Docs.readdir req inode size offset file_info

let statfs req inode =
  Utils.log_with_header "statfs %Ld\n%!" inode;
  Docs.statfs req inode

let start_filesystem mountpoint fuse_args =
  if not (Sys.file_exists mountpoint && Sys.is_directory mountpoint) then
    failwith ("Mountpoint " ^ mountpoint ^ " should be an existing directory.");
  Utils.log_with_header "Starting filesystem %s\n%!" mountpoint;
  let fs_ops =
    let default_ops = Fuse.default_ops () in
      { default_ops with
            Fuse.init;
            destroy;
            lookup;
            getattr;
            (* Commented out: causes Segmentation fault
             openfile;
             *)
            read;
            (*
            write = fs_write;
             *)
            readdir;
            (*
            mknod = fs_mknod;
             *)
            statfs;
      } in
  let filesystem =
    Fuse.make mountpoint fuse_args fs_ops in
  let rec loop () =
    Fuse.process filesystem;
    if not (Fuse.session_exited filesystem)
    then loop ()
  in
    loop ()
(* END FUSE bindings *)

(* Main program *)
let () =
  let fs_label = ref "default" in
  let mountpoint = ref "" in
  let fuse_args = ref [] in
  let show_version = ref false in
  let debug = ref false in
  let client_id = ref "" in
  let client_secret = ref "" in
  let program = Filename.basename Sys.executable_name in
  let usage =
    Printf.sprintf
      "Usage: %s [options] [mountpoint]"
      program in
  let arg_specs =
    Arg.align (
      ["-version",
       Arg.Set show_version,
       " show version and exit.";
       "-verbose",
       Arg.Set Utils.verbose,
       " enable verbose logging on stdout. Default is false.";
       "-debug",
       Arg.Unit (fun () ->
                   debug := true;
                   Utils.verbose := true;
                   fuse_args := "-d" :: !fuse_args),
       " enable debug mode (implies -verbose, -d). Default is false.";
       "-label",
       Arg.Set_string fs_label,
       " use a specific label to identify the filesystem. \
        Default is \"default\".";
       "-id",
       Arg.Set_string client_id,
       " provide OAuth2 client ID.";
       "-secret",
       Arg.Set_string client_secret,
       " provide OAuth2 client secret.";
       "-d",
       Arg.Unit (fun _ -> fuse_args := "-d" :: !fuse_args),
       " enable FUSE debug output.";
      ]) in
  let () =
    Arg.parse
      arg_specs
      (fun s -> mountpoint := s)
      usage in
  let quit error_message =
    Printf.eprintf "Error: %s\n" error_message;
    exit 1
  in

    if !show_version then begin
      Printf.printf "google-drive-ocamlfuse, version %s\n\
                     Copyright (C) 2012 Alessandro Strada\n\
                     License MIT\n"
        Config.version;
    end else begin
      try
        if !mountpoint = "" then begin
          setup_application
            !debug !fs_label !client_id !client_secret ".";
        end else begin
          setup_application
            !debug !fs_label !client_id !client_secret !mountpoint;
          at_exit
            (fun () ->
               Utils.log_with_header "Exiting.\n";
               let context = Context.get_ctx () in
               Utils.log_message "CURL cleanup...";
               ignore (GapiCurl.global_cleanup context.Context.curl_state);
               Utils.log_message "done\nClearing context...";
               Context.clear_ctx ();
               Utils.log_message "done\n%!");
          start_filesystem !mountpoint !fuse_args
        end
      with
          Failure error_message -> quit error_message
        | e ->
            let error_message = Printexc.to_string e in
              quit error_message
    end
(* END Main program *)

