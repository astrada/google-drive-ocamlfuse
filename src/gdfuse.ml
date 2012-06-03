open GapiUtils.Infix
open GapiLens.Infix
open GapiLens.StateInfix

let default_fs_label = "default"

let client_id = "564921029129.apps.googleusercontent.com"
let redirect_uri = GaeProxy.gae_proxy_url ^ "/oauth2callback"
let scope = [GdataDocumentsV3Service.all_scopes]

(* Logging *)
let log_message = Utils.log_message

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
        log_message "done\n%!";
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
      log_message "Loading configuration from %s..." config_path;
      let config_store = Context.ConfigFileStore.load config_path in
        log_message "done\n";
        config_store
    with KeyValueStore.File_not_found ->
      log_message "not found.\n";
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
      log_message "Loading application state from %s..." state_path;
      let state_store = Context.StateFileStore.load state_path in
        log_message "done\n";
        state_store
    with KeyValueStore.File_not_found ->
      log_message "not found.\n";
      create_empty_state_store app_dir
(* END Application state *)

let setup_application debug fs_label mountpoint =
  if not (Sys.file_exists mountpoint && Sys.is_directory mountpoint) then
    failwith ("Mountpoint " ^ mountpoint ^ " should be an existing directory.");
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
        start_browser request_id;
        GaeProxy.start_server_polling ()
      with
          GaeProxy.ServerError e ->
            log_message "Removing invalid request_id=%s\n%!" request_id;
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

  log_message "Setting up %s filesystem...\n" fs_label;
  let app_dir = AppDir.create fs_label in
  let () = AppDir.create_directories app_dir in
  log_message "Setting up cache db...";
  let cache = Cache.open_db app_dir in
  Cache.setup_db cache;
  log_message "done\n";
  let config_store = get_config_store debug app_dir in
  let config_store = config_store
    |> Context.ConfigFileStore.data
    ^%= Config.debug ^= debug in
  let gapi_config =
    Config.create_gapi_config
      (config_store |. Context.ConfigFileStore.data)
      app_dir in
  let state_store = get_state_store app_dir in
  log_message "Setting up CURL...";
  let curl_state = GapiCurl.global_init () in
  log_message "done\n";
  let context = {
    Context.app_dir;
    config_store;
    gapi_config;
    state_store;
    cache_table = Hashtbl.create 16;
    curl_state;
    mountpoint_stats = Unix.LargeFile.stat mountpoint;
    metadata = None;
  } in
  Context.set_ctx context;
  Context.set_cache cache;
  let refresh_token = context |. Context.refresh_token_lens in
    if refresh_token = "" then
      get_auth_tokens_from_server ()
    else
      log_message "Refresh token already present.\n%!"
(* END setup *)

(* FUSE bindings *)
let init_filesystem () =
  log_message "init_filesystem\n%!"

let statfs path =
  log_message "statfs %s\n%!" path;
  try
    Docs.statfs ()
  with e ->
    Utils.log_exception e;
    raise (Unix.Unix_error (Unix.EBUSY, "statfs", path))

let getattr path =
  log_message "getattr %s\n%!" path;
  try
    Docs.get_attr path
  with Docs.File_not_found ->
        log_message "File not found %s\n%!" path;
        raise (Unix.Unix_error (Unix.ENOENT, "stat", path))
    | e ->
        Utils.log_exception e;
        raise (Unix.Unix_error (Unix.EBUSY, "stat", path))

let readdir path hnd =
  log_message "readdir %s %d\n%!" path hnd;
  let dir_list =
    try
      Docs.read_dir path
    with e ->
      Utils.log_exception e;
      raise (Unix.Unix_error (Unix.ENOENT, "readdir", path))
  in
    Filename.current_dir_name :: Filename.parent_dir_name :: dir_list

let start_filesystem mountpoint fuse_args =
  log_message "Starting filesystem %s\n%!" mountpoint;
  let fuse_argv =
    Sys.argv.(0) :: (fuse_args @ [mountpoint])
    |> Array.of_list
  in
    Fuse.main fuse_argv {
      Fuse.default_operations with
          Fuse.init = init_filesystem;
          statfs;
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
  let debug = ref false in
  let program = Filename.basename Sys.executable_name in
  let usage =
    Printf.sprintf
      "Usage: %s [-verbose] [-debug] [-label fslabel] [-f] [-d] [-s] mountpoint"
      program in
  let arg_specs =
    Arg.align (
      ["-verbose",
       Arg.Set Utils.verbose,
       " Enable verbose logging on stdout. Default is false.";
       "-debug",
       Arg.Unit (fun () ->
                   debug := true;
                   Utils.verbose := true;
                   fuse_args := "-f" :: !fuse_args),
       " Enable debug mode (implies -verbose, -f). Default is false.";
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
    end in
  let quit error_message =
    Printf.eprintf "Error: %s\n" error_message;
    exit 1
  in

    try
      setup_application !debug !fs_label !mountpoint;
      at_exit
        (fun () ->
           log_message "Exiting.\n";
           let context = Context.get_ctx () in
           log_message "CURL cleanup...";
           ignore (GapiCurl.global_cleanup context.Context.curl_state);
           log_message "done\nClosing cache db...\n";
           Context.close_cache ();
           log_message "done\nClearing context...";
           Context.clear_ctx ();
           log_message "done\n%!");
      start_filesystem !mountpoint !fuse_args
    with
        Failure error_message -> quit error_message
      | e ->
          let error_message = Printexc.to_string e in
            quit error_message
(* END Main program *)

