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
  let state = State.empty
    |> State.auth_request_id ^= request_id
    |> State.saved_version ^= Config.version in
  let state_store = {
    Context.StateFileStore.path = app_dir |. AppDir.state_path;
    data = state
  } in
  Context.save_state_store state_store;
  state_store

let get_state_store app_dir =
  let state_path = app_dir |. AppDir.state_path in
  try
    Utils.log_with_header "Loading application state from %s..." state_path;
    let state_store = Context.StateFileStore.load state_path in
    Utils.log_message "done\n%!";
    state_store
  with KeyValueStore.File_not_found ->
    Utils.log_message "not found.\n%!";
    create_empty_state_store app_dir
(* END Application state *)

type application_params = {
  debug : bool;
  filesystem_label : string;
  client_id : string;
  client_secret : string;
  mountpoint : string;
  clear_cache : bool;
}

let setup_application params =
  let mountpoint = params.mountpoint in
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

  Utils.log_message "Starting application setup (label=%s).\n%!"
    params.filesystem_label;
  let app_dir = AppDir.create params.filesystem_label in
  let () = AppDir.create_directories app_dir in
  let app_log_path = app_dir |. AppDir.app_log_path in
  Utils.log_message "Opening log file: %s\n%!" app_log_path;
  let log_channel = open_out app_log_path in
  Utils.log_channel := log_channel;
  Utils.log_with_header "Setting up %s filesystem...\n%!"
    params.filesystem_label;
  let config_store = get_config_store params.debug app_dir in
  let current_config = config_store |. Context.ConfigFileStore.data in
  let client_id =
    if params.client_id = ""
    then current_config |. Config.client_id
    else params.client_id in
  let client_secret =
    if params.client_secret = ""
    then current_config |. Config.client_secret
    else params.client_secret in
  let config =
    { current_config with
          Config.debug = params.debug;
          client_id;
          client_secret;
    } in
  Context.save_config_store config_store;
  let config_store = config_store
    |> Context.ConfigFileStore.data ^= config in
  let gapi_config = Config.create_gapi_config config app_dir in
  let state_store = get_state_store app_dir in
  let cache = Cache.create_cache app_dir config in
  let saved_version = state_store |. Context.saved_version_lens in
  if params.clear_cache then begin
    Printf.printf "Clearing cache...%!";
    Utils.log_message "Cleaning up cache...%!";
    Cache.clean_up_cache cache;
    Utils.log_message "done\n%!";
    Printf.printf "done\n%!";
  end else if saved_version <> Config.version then begin
    Utils.log_message
      "Version mismatch (saved=%s, current=%s): cleaning up cache...%!"
      saved_version Config.version;
    Cache.clean_up_cache cache;
    let updated_state_store = state_store
      |> Context.saved_version_lens ^= Config.version in
    Context.save_state_store updated_state_store;
    Utils.log_message "done\n%!";
  end;
  Utils.log_message "Setting up cache db...%!";
  Cache.setup_db cache;
  Utils.log_message "done\nSetting up CURL...%!";
  let curl_state = GapiCurl.global_init () in
  Utils.log_message "done\n%!";
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
let handle_exception e label param =
  match e with
      Drive.File_not_found ->
        Utils.log_message "File not found: %s %s\n%!" label param;
        raise (Unix.Unix_error (Unix.ENOENT, label, param))
    | Drive.Permission_denied ->
        Utils.log_message "Permission denied: %s %s\n%!" label param;
        raise (Unix.Unix_error (Unix.EACCES, label, param))
    | Drive.Resource_busy ->
        Utils.log_message "Resource busy: %s %s\n%!" label param;
        raise (Unix.Unix_error (Unix.EBUSY, label, param))
    | e ->
        Utils.log_exception e;
        raise (Unix.Unix_error (Unix.EBUSY, label, param))

let init_filesystem () =
  Utils.log_with_header "init_filesystem\n%!"

let statfs path =
  Utils.log_with_header "statfs %s\n%!" path;
  try
    Drive.statfs ()
  with e ->
    Utils.log_exception e;
    raise (Unix.Unix_error (Unix.EBUSY, "statfs", path))

let getattr path =
  Utils.log_with_header "getattr %s\n%!" path;
  try
    Drive.get_attr path
  with e -> handle_exception e "stat" path

let readdir path hnd =
  Utils.log_with_header "readdir %s %d\n%!" path hnd;
  let dir_list =
    try
      Drive.read_dir path
    with e -> handle_exception e "readdir" path
  in
    Filename.current_dir_name :: Filename.parent_dir_name :: dir_list

let opendir path flags =
  Utils.log_with_header "opendir %s %s\n%!" path (Utils.flags_to_string flags);
  try
    Drive.opendir path flags
  with e -> handle_exception e "opendir" path

let releasedir path flags hnd =
  Utils.log_with_header "releasedir %s %s\n%!"
    path (Utils.flags_to_string flags)

let fsyncdir path ds hnd =
  Utils.log_with_header "fsyncdir %s %b\n%!" path ds

let utime path atime mtime =
  Utils.log_with_header "utime %s %f %f\n%!" path atime mtime;
  try
    Drive.utime path atime mtime
  with e -> handle_exception e "utime" path

let fopen path flags =
  Utils.log_with_header "fopen %s %s\n%!" path (Utils.flags_to_string flags);
  try
    Drive.fopen path flags
  with e -> handle_exception e "fopen" path

let read path buf offset file_descr =
  Utils.log_with_header "read %s buf %Ld %d\n%!" path offset file_descr;
  try
    Drive.read path buf offset file_descr
  with e -> handle_exception e "read" path

let write path buf offset file_descr =
  Utils.log_with_header "write %s buf %Ld %d\n%!" path offset file_descr;
  try
    Drive.write path buf offset file_descr
  with e -> handle_exception e "write" path

let mknod path mode =
  Utils.log_with_header "mknod %s %d\n%!" path mode;
  try
    Drive.mknod path mode
  with e -> handle_exception e "mknod" path

let mkdir path mode =
  Utils.log_with_header "mkdir %s %d\n%!" path mode;
  try
    Drive.mkdir path mode
  with e -> handle_exception e "mkdir" path

let unlink path =
  Utils.log_with_header "unlink %s\n%!" path;
  try
    Drive.unlink path
  with e -> handle_exception e "unlink" path

let rmdir path =
  Utils.log_with_header "rmdir %s\n%!" path;
  try
    Drive.rmdir path
  with e -> handle_exception e "rmdir" path

let rename path new_path =
  Utils.log_with_header "rename %s %s\n%!" path new_path;
  try
    Drive.rename path new_path
  with e -> handle_exception e "rename" path

let truncate path size =
  Utils.log_with_header "truncate %s %Ld\n%!" path size;
  try
    Drive.truncate path size
  with e -> handle_exception e "truncate" path

let release path flags hnd =
  Utils.log_with_header "release %s %s\n%!" path (Utils.flags_to_string flags);
  try
    Drive.release path flags hnd
  with e -> handle_exception e "release" path

let flush path file_descr =
  Utils.log_with_header "flush %s %d\n%!" path file_descr;
  try
    Drive.flush path file_descr
  with e -> handle_exception e "flush" path

let fsync path ds file_descr =
  Utils.log_with_header "fsync %s %b %d\n%!" path ds file_descr;
  try
    Drive.fsync path ds file_descr
  with e -> handle_exception e "fsync" path

let setxattr path name value xflags =
  Utils.log_with_header "setxattr %s %s %s %s\n%!"
    path name value (Utils.xattr_flags_to_string xflags)

let chmod path mode =
  Utils.log_with_header "chmod %s %d\n%!" path mode

let chown path uid gid =
  Utils.log_with_header "chown %s %d %d\n%!" path uid gid

let start_filesystem mountpoint fuse_args =
  Utils.log_with_header "Starting filesystem %s\n%!" mountpoint;
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
          opendir;
          releasedir;
          fsyncdir;
          utime;
          fopen;
          read;
          write;
          mknod;
          mkdir;
          unlink;
          rmdir;
          rename;
          truncate;
          release;
          flush;
          fsync;
          setxattr;
          chmod;
          chown;
    }
(* END FUSE bindings *)

(* Main program *)
let () =
  let fs_label = ref "default" in
  let mountpoint = ref "" in
  let fuse_args = ref ["-s"; "-obig_writes"] in
  let show_version = ref false in
  let debug = ref false in
  let client_id = ref "" in
  let client_secret = ref "" in
  let clear_cache = ref false in
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
                   fuse_args := "-f" :: !fuse_args),
       " enable debug mode (implies -verbose, -f). Default is false.";
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
       "-f",
       Arg.Unit (fun _ -> fuse_args := "-f" :: !fuse_args),
       " keep the process in foreground.";
       "-d",
       Arg.Unit (fun _ -> fuse_args := "-d" :: !fuse_args),
       " enable FUSE debug output (implies -f).";
       "-m",
       Arg.Unit (fun _ -> fuse_args :=
                          List.filter (fun a -> a <> "-s") !fuse_args),
       " enable multi-threaded operation.";
       "-o",
       Arg.String (fun o -> fuse_args := ("-o" ^ o) :: !fuse_args),
       " specify FUSE mount options.";
       "-cc",
       Arg.Set clear_cache,
       " clear cache";
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
                     Copyright (C) 2012-2013 Alessandro Strada\n\
                     License MIT\n"
        Config.version;
    end else begin
      try
        let params = {
          debug = !debug;
          filesystem_label = !fs_label;
          client_id = !client_id;
          client_secret = !client_secret;
          mountpoint = !mountpoint;
          clear_cache = !clear_cache
        } in
        if !mountpoint = "" then begin
          setup_application { params with mountpoint = "." };
        end else begin
          setup_application params;
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

