open GapiUtils.Infix
open GapiLens.Infix

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
let create_default_config_store debug path =
  let data =
    if debug then Config.default_debug
    else Config.default in
  (* Save configuration file *)
  let config_store = {
    Context.ConfigFileStore.path;
    data;
  } in
  let config_dir = Filename.dirname path in
  Utils.safe_makedir config_dir;
  Context.save_config_store config_store;
  config_store

let get_config_store debug config_path =
  try
    Utils.log_with_header "Loading configuration from %s..." config_path;
    let config_store = Context.ConfigFileStore.load config_path in
    Utils.log_message "done\n";
    config_store
  with KeyValueStore.File_not_found ->
    Utils.log_message "not found.\n";
    create_default_config_store debug config_path
(* END Application configuration *)

(* Application state *)
let generate_request_id () =
  Cryptokit.Random.string rng 32
    |> Base64.str_encode
    |> Bytes.to_string
    |> ExtString.String.replace_chars
      (function
        | '+' -> "-"
        | c -> ExtString.String.of_char c)

let create_empty_state_store app_dir =
  let request_id = generate_request_id () in
  let state = State.empty
    |> State.auth_request_id ^= request_id
    |> State.saved_version ^= Config.version in
  let state_store = {
    Context.StateFileStore.path = app_dir.AppDir.state_path;
    data = state
  } in
  Context.save_state_store state_store;
  state_store

let get_state_store app_dir =
  let state_path = app_dir.AppDir.state_path in
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
  headless : bool;
  skip_trash : bool;
  base_dir : string;
  multi_threading : bool;
  config_path : string;
  xdg_base_directory : bool;
  browser : string;
  docs_mode : string;
  service_account_credentials_path : string;
  service_account_user_to_impersonate : string;
  log_to : string;
  scope : string;
  redirect_uri : string;
  device : bool;
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
        Utils.start_browser params.browser url;
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

  Utils.log_message "Starting application setup (label=%s, base_dir=%s).\n%!"
    params.filesystem_label
    params.base_dir;
  let (config_path, xdg_base_directory) =
    AppDir.get_config_path
      params.config_path
      params.xdg_base_directory
      params.base_dir
      params.filesystem_label in
  let config_store =
    get_config_store params.debug config_path in
  let current_config = config_store.Context.ConfigFileStore.data in
  let app_dir =
    AppDir.create
      current_config
      params.config_path
      params.base_dir
      params.filesystem_label
      xdg_base_directory in
  let () = AppDir.create_directories app_dir in
  let app_log_path = app_dir.AppDir.app_log_path in
  let log_to =
    if params.log_to = ""
    then current_config |. Config.log_to
    else params.log_to in
  begin if log_to = "" then
    Utils.log_message "Opening log file: %s\n%!" app_log_path
  else
    Utils.log_message "Opening log file: %s\n%!" log_to
  end;
  let log_channel = Utils.open_log_out_ch log_to app_log_path in
  Utils.log_channel := log_channel;
  Utils.log_with_header "Setting up %s filesystem...\n%!"
    params.filesystem_label;
  let client_id =
    if params.client_id = ""
    then current_config |. Config.client_id
    else params.client_id in
  let client_secret =
    if params.client_secret = ""
    then current_config |. Config.client_secret
    else params.client_secret in
  let service_account_credentials_path =
    if params.service_account_credentials_path = ""
    then current_config |. Config.service_account_credentials_path
    else params.service_account_credentials_path in
  let service_account_user_to_impersonate =
    if params.service_account_user_to_impersonate = ""
    then current_config |. Config.service_account_user_to_impersonate
    else params.service_account_user_to_impersonate in
  let scope =
    if params.device
    then Drive.device_scope
    else if params.scope = ""
    then current_config |. Config.scope
    else params.scope in
  let redirect_uri =
    if params.redirect_uri = ""
    then current_config |. Config.redirect_uri
    else params.redirect_uri in
  let headless = params.headless in
  let device = params.device in
  let sqlite3_busy_timeout =
    (* Previously default timeout was 500ms that's too low for multi-threading.
     * Update default value to 5000ms *)
    if params.multi_threading &&
       current_config.Config.sqlite3_busy_timeout = 500 then 5000
    else current_config.Config.sqlite3_busy_timeout in
  (* Check max_upload_chunk_size *)
  let max_upload_chunk_size = current_config.Config.max_upload_chunk_size in
  if max_upload_chunk_size <= 0 then
    failwith "max_upload_chunk_size should be > 0";
  if current_config.Config.memory_buffer_size < 128 * 1024 then
    failwith "memory_buffer_size should be >= 131072 (128k)";
  if current_config.Config.max_memory_cache_size <
     current_config.Config.memory_buffer_size then
    failwith "max_memory_cache_size should be >= memory_buffer_size";
  Utils.debug_buffers := current_config.Config.debug_buffers;
  let config_without_docs_mode =
    { current_config with
          Config.client_id;
          client_secret;
          sqlite3_busy_timeout;
          service_account_credentials_path;
          service_account_user_to_impersonate;
          log_to;
          scope;
          redirect_uri;
    } in
  let config =
    if params.docs_mode = "libreoffice" then
      { config_without_docs_mode with
        Config.download_docs = true;
        document_format = "odt";
        drawing_format = "png";
        form_format = "zip";
        presentation_format = "odp";
        spreadsheet_format = "ods";
        apps_script_format = "json";
      }
    else if params.docs_mode = "msoffice" then
      { config_without_docs_mode with
        Config.download_docs = true;
        document_format = "docx";
        drawing_format = "png";
        form_format = "zip";
        presentation_format = "pptx";
        spreadsheet_format = "xlsx";
        apps_script_format = "json";
      }
    else if params.docs_mode = "desktop" then
      { config_without_docs_mode with
        Config.download_docs = true;
        document_format = "desktop";
        drawing_format = "desktop";
        form_format = "desktop";
        presentation_format = "desktop";
        spreadsheet_format = "desktop";
        apps_script_format = "desktop";
      }
    else if params.docs_mode = "off" then
      { config_without_docs_mode with
        Config.download_docs = false;
      }
    else if params.docs_mode <> "" then
      failwith ("Unsupported docsmode: " ^ params.docs_mode)
    else config_without_docs_mode
  in
  let config_store = config_store
    |> Context.ConfigFileStore.data ^= config in
  Context.save_config_store config_store;
  Utils.max_retries := config.Config.max_retries;
  let gapi_config =
    let gapi_config =
      Config.create_gapi_config config params.debug
        app_dir.AppDir.curl_log_path log_to in
    if service_account_credentials_path = "" &&
       (client_id = "" || client_secret = "") then
      let oauth2_config =
        match gapi_config |. GapiConfig.auth with
        | GapiConfig.OAuth2 oauth2 ->
            oauth2 |> GapiConfig.refresh_access_token ^= Some (
              fun () ->
                GaeProxy.refresh_access_token ();
                Context.get_ctx ()
                  |. Context.state_lens
                  |. State.last_access_token
            )
        | _ -> assert false in
      gapi_config |> GapiConfig.auth ^= GapiConfig.OAuth2 oauth2_config
    else gapi_config in
  let state_store = get_state_store app_dir in
  let cache = Cache.create_cache app_dir config in
  let saved_version = state_store |. Context.saved_version_lens in
  Utils.log_message "Current version: %s\n%!" Config.version;
  let clear_cache = if config_without_docs_mode <> config
    then begin
      Utils.log_message "Docs mode changed to %s%!\n" params.docs_mode;
      true
    end else begin
      Utils.log_message "Docs mode not changed!\n";
      params.clear_cache
    end in
  if clear_cache then begin
    Printf.printf "Clearing cache...%!";
    Utils.log_message "Cleaning up cache...%!";
    Cache.clean_up_cache cache;
    Utils.log_message "done\n%!";
    Printf.printf "done\n%!";
  end;
  let state_store =
    if saved_version <> Config.version then begin
      Utils.log_message
        "Version mismatch (saved=%s, current=%s)%!\n"
        saved_version Config.version;
      if not clear_cache then begin
        Utils.log_message "Cleaning up cache...%!";
        Cache.clean_up_cache cache;
        Utils.log_message "done%!";
      end;
      Utils.log_message "\n%!";
      let updated_state_store = state_store
        |> Context.saved_version_lens ^= Config.version in
      Context.save_state_store updated_state_store;
      updated_state_store
    end else state_store
  in
  Utils.log_message "Setting up cache db...%!";
  Cache.setup_db cache;
  Utils.log_message "done\nSetting up CURL...%!";
  let curl_state = GapiCurl.global_init () in
  Utils.log_message "done\n%!";
  let memory_buffers =
    Buffering.MemoryBuffers.create
      config.Config.memory_buffer_size
      config.Config.max_memory_cache_size in
  let context = {
    Context.app_dir;
    config_store;
    gapi_config;
    state_store;
    cache;
    curl_state;
    mountpoint_path = mountpoint;
    mountpoint_stats = Unix.LargeFile.stat mountpoint;
    metadata = None;
    metadata_lock = Mutex.create ();
    skip_trash = params.skip_trash;
    memory_buffers;
    file_locks = Hashtbl.create Utils.hashtable_initial_size;
    buffer_eviction_thread = None;
    root_folder_id = None;
    flush_db_thread = None;
    async_upload_thread = None;
    folder_fetching_thread = None;
  } in
  Context.set_ctx context;
  if not (DbCache.check_clean_shutdown cache) then begin
    Utils.log_with_header
      "google-drive-ocamlfuse didn't shut down correctly.%!\n";
    if not clear_cache then begin
      Utils.log_message "Cleaning up cache...%!";
      Cache.clean_up_cache cache;
      Utils.log_message "done\nSetting up cache db...%!";
      Cache.setup_db cache;
      Utils.log_message "done\n...%!";
    end;
  end else begin
    DbCache.reset_clean_shutdown cache;
  end;
  if config.Config.service_account_credentials_path = "" then begin
    let refresh_token = context |. Context.refresh_token_lens in
    if refresh_token = "" then
      if client_id = "" || client_secret = "" then
        if headless then
          failwith ("In headless mode, you should specify a client id and a \
                     client secret")
        else if device then
          failwith ("In device mode, you should specify a client id and a \
                     client secret")
        else
          get_auth_tokens_from_server ()
      else
        Oauth2.get_access_token headless device params.browser
    else
      Utils.log_message "Refresh token already present.\n%!"
  end else begin
    Utils.log_message "Service account credentials JSON path: %s.\n%!"
      config.Config.service_account_credentials_path;
    if config.Config.service_account_user_to_impersonate <> "" then
      Utils.log_message "Impersonating user: %s.\n%!"
        config.Config.service_account_user_to_impersonate;
  end
(* END setup *)

(* FUSE bindings *)
let handle_exception e label param =
  match e with
      Drive.File_not_found ->
        Utils.log_with_header "File not found: %s %s\n%!" label param;
        raise (Unix.Unix_error (Unix.ENOENT, label, param))
    | Drive.Permission_denied ->
        Utils.log_with_header "Permission denied: %s %s\n%!" label param;
        raise (Unix.Unix_error (Unix.EACCES, label, param))
    | Drive.Directory_not_empty ->
        Utils.log_with_header "Directory not empty: %s %s\n%!" label param;
        raise (Unix.Unix_error (Unix.ENOTEMPTY, label, param))
    | Drive.IO_error ->
        Utils.log_with_header "Input/output error: %s %s\n%!" label param;
        raise (Unix.Unix_error (Unix.EIO, label, param))
    | Drive.No_attribute ->
        raise (Unix.Unix_error (Unix.EUNKNOWNERR 61, label, param))
    | Drive.Existing_attribute ->
        raise (Unix.Unix_error (Unix.EEXIST, label, param))
    | Drive.Invalid_operation ->
        raise (Unix.Unix_error (Unix.EINVAL, label, param))
    | Unix.Unix_error _ as e ->
        Utils.log_exception e;
        raise e
    | e ->
        Utils.log_exception e;
        raise (Unix.Unix_error (Unix.EIO, label, param))

let init_filesystem () =
  Utils.log_with_header "init_filesystem\n%!";
  try
    Drive.init_filesystem ()
  with e ->
    Utils.log_exception e;
    handle_exception e "init_filesystem" ""

let statfs path =
  Utils.log_with_header "statfs %s\n%!" path;
  try
    Drive.statfs ()
  with e ->
    Utils.log_exception e;
    handle_exception e "statfs" path

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
  Utils.log_with_header "fsyncdir %s %b %d\n%!" path ds hnd

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
  let buf_len = Bigarray.Array1.dim buf in
  Utils.log_with_header "read %s [%d bytes] %Ld %d\n%!"
    path buf_len offset file_descr;
  try
    let result = Drive.read path buf offset file_descr in
    if !Utils.debug_buffers then begin
      Utils.log_buffer
        (Printf.sprintf "read %s [%d bytes] %Ld %d"
           path buf_len offset file_descr)
        buf result;
    end;
    result
  with e -> handle_exception e "read" path

let write path buf offset file_descr =
  let buf_len = Bigarray.Array1.dim buf in
  Utils.log_with_header "write %s [%d bytes] %Ld %d\n%!"
    path buf_len offset file_descr;
  if !Utils.debug_buffers then begin
    Utils.log_buffer
      (Printf.sprintf "write %s [%d bytes] %Ld %d"
         path buf_len offset file_descr)
      buf buf_len;
  end;
  try
    Drive.write path buf offset file_descr
  with e -> handle_exception e "write" path

let mknod path mode =
  Utils.log_with_header "mknod %s %o\n%!" path mode;
  try
    Drive.mknod path mode
  with e -> handle_exception e "mknod" path

let mkdir path mode =
  Utils.log_with_header "mkdir %s %o\n%!" path mode;
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

let chmod path mode =
  Utils.log_with_header "chmod %s %o\n%!" path mode;
  try
    Drive.chmod path mode
  with e -> handle_exception e "chmod" path

let chown path uid gid =
  Utils.log_with_header "chown %s %d %d\n%!" path uid gid;
  try
    Drive.chown path uid gid
  with e -> handle_exception e "chown" path

let getxattr path name =
  Utils.log_with_header "getxattr %s %s\n%!" path name;
  try
    Drive.get_xattr path name
  with e -> handle_exception e "getxattr" path

let setxattr path name value xflags =
  Utils.log_with_header "setxattr %s %s %s %s\n%!"
    path name value (Utils.xattr_flags_to_string xflags);
  try
    Drive.set_xattr path name value xflags
  with e -> handle_exception e "setxattr" path

let listxattr path =
  Utils.log_with_header "listxattr %s\n%!" path;
  try
    Drive.list_xattr path
  with e -> handle_exception e "listxattr" path

let removexattr path name =
  Utils.log_with_header "removexattr %s %s\n%!" path name;
  try
    Drive.remove_xattr path name
  with e -> handle_exception e "removexattr" path

let readlink path =
  Utils.log_with_header "readlink %s\n%!" path;
  try
    Drive.read_link path
  with e -> handle_exception e "readlink" path

let symlink target linkpath =
  Utils.log_with_header "symlink %s %s\n%!" target linkpath;
  try
    Drive.symlink target linkpath
  with e -> handle_exception e "symlink" target
(* END FUSE bindings *)

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
        chmod;
        chown;
        getxattr;
        setxattr;
        listxattr;
        removexattr;
        readlink;
        symlink;
  }
(* END FUSE bindings *)

(* Main program *)
let () =
  let fs_label = ref "default" in
  let mountpoint = ref "" in
  let fuse_args = ref ["-obig_writes"] in
  let show_version = ref false in
  let debug = ref false in
  let client_id = ref "" in
  let client_secret = ref "" in
  let clear_cache = ref false in
  let headless = ref false in
  let skip_trash = ref false in
  let base_dir = ref "" in
  let multi_threading = ref false in
  let config_path = ref "" in
  let xdg_base_directory = ref false in
  let browser = ref "" in
  let docs_mode = ref "" in
  let service_account_credentials_path = ref "" in
  let service_account_user_to_impersonate = ref "" in
  let log_to = ref "" in
  let scope = ref "" in
  let redirect_uri = ref "" in
  let device = ref false in
  let program = Filename.basename Sys.executable_name in
  let usage =
    Printf.sprintf
      "Usage: %s [options] [mountpoint]"
      program in
  let parse_mount_options opt_string =
    let opts = Str.split (Str.regexp " *, *") opt_string in
    let base_dir_opt =
      List.filter
        (fun o -> ExtString.String.starts_with o "gdfroot") opts in
    List.iter
      (fun o ->
        try
          let (_, bd) = ExtString.String.split o "=" in
          base_dir := bd
        with ExtString.Invalid_string ->
          failwith "Invalid mount option gdfroot")
      base_dir_opt;
    let fuse_mount_opts =
      List.filter
        (fun o -> not (ExtString.String.starts_with o "gdfroot")) opts in
    let fuse_mount_opt_string = String.concat "," fuse_mount_opts in
    fuse_args := ("-o" ^ fuse_mount_opt_string) :: !fuse_args
  in

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
       Arg.Unit (fun _ ->
         multi_threading := true),
       " run in multi-threaded mode (default).";
       "-s",
       Arg.Unit (fun _ ->
         fuse_args := "-s" :: !fuse_args;
         multi_threading := false),
       " run in single-threaded mode.";
       "-o",
       Arg.String parse_mount_options,
       " specify FUSE mount options.";
       "-cc",
       Arg.Set clear_cache,
       " clear cache";
       "-headless",
       Arg.Set headless,
       " enable headless mode. Default is false.";
       "-skiptrash",
       Arg.Set skip_trash,
       " enable permanent deletion mode. Default is false. Activate at your \
        own risk. Files deleted with this option *cannot* be restored.";
       "-config",
       Arg.Set_string config_path,
       " use a custom config file path. \
        Default is \"~/.gdfuse/[label]/config\".";
       "-xdgbd",
       Arg.Set xdg_base_directory,
       " enable XDG Base Directory support. Default is false.";
       "-browser",
       Arg.Set_string browser,
       " starts a specific browser to access authorization page. \
        Default is xdg-open, firefox, google-chrome, chromium-browser, open.";
       "-docsmode",
       Arg.Set_string docs_mode,
       " sets the specified mode for Google Docs (implies -cc, if changed). \
        Supported values are: libreoffice, msoffice, desktop, off. \
        Default is desktop.";
       "-serviceaccountpath",
       Arg.Set_string service_account_credentials_path,
       " sets the path of the JSON file that contains service account \
        credentials.";
       "-serviceaccountuser",
       Arg.Set_string service_account_user_to_impersonate,
       " sets the email of the G Suite user to impersonate.";
       "-log_to",
       Arg.Set_string log_to,
       " logs to 'stdout' ('-'), 'stderr', or an absolute path.";
       "-scope",
       Arg.Set_string scope,
       " sets a custom Drive API scope.";
       "-redirect_uri",
       Arg.Set_string redirect_uri,
       " sets a custom Drive API redirect URI.";
       "-device",
       Arg.Set device,
       " use OAuth2 for devices. Default is false.";
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
                   Copyright (C) 2012-2020 Alessandro Strada\n\
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
        clear_cache = !clear_cache;
        headless = !headless;
        skip_trash = !skip_trash;
        base_dir = !base_dir;
        multi_threading = !multi_threading;
        config_path = !config_path;
        xdg_base_directory = !xdg_base_directory;
        browser = !browser;
        docs_mode = !docs_mode;
        service_account_credentials_path = !service_account_credentials_path;
        service_account_user_to_impersonate =
          !service_account_user_to_impersonate;
        log_to = !log_to;
        scope = !scope;
        redirect_uri = !redirect_uri;
        device = !device;
      } in
      if !mountpoint = "" then begin
        setup_application { params with mountpoint = "." };
      end else begin
        setup_application params;
        at_exit
          (fun () ->
             Utils.log_with_header "Exiting.\n%!";
             let context = Context.get_ctx () in
             begin match context.Context.buffer_eviction_thread with
               | None -> ()
               | Some buffer_eviction_thread -> begin
                   Utils.log_with_header
                     "Stopping buffer eviction thread (TID=%d)...%!"
                     (Thread.id buffer_eviction_thread);
                   Buffering.MemoryBuffers.stop_eviction_thread
                     context.Context.memory_buffers;
                   Thread.join buffer_eviction_thread;
                   Utils.log_message "done\n%!";
                 end
             end;
             begin match context.Context.flush_db_thread with
               | None -> ()
               | Some flush_db_thread -> begin
                   Utils.log_with_header
                     "Stopping flush DB thread (TID=%d)...%!"
                     (Thread.id flush_db_thread);
                   MemoryCache.stop_flush_db_thread ();
                   Thread.join flush_db_thread;
                   Utils.log_message "done\n%!";
                 end
             end;
             begin match context.Context.async_upload_thread with
               | None -> ()
               | Some async_upload_thread -> begin
                   Utils.log_with_header
                     "Stopping async upload thread (TID=%d)\n%!"
                     (Thread.id async_upload_thread);
                   UploadQueue.stop_async_upload_thread ();
                   Thread.join async_upload_thread;
                 end
             end;
             begin match context.Context.folder_fetching_thread with
               | None -> ()
               | Some folder_fetching_thread -> begin
                   Utils.log_with_header
                     "Stopping background folder fetching thread (TID=%d)...%!"
                     (Thread.id folder_fetching_thread);
                   BackgroundFolderFetching.stop_folder_fetching_thread ();
                   Thread.join folder_fetching_thread;
                   Utils.log_message "done\n%!";
                 end
             end;
             Utils.log_with_header "Flushing cache...\n%!";
             Cache.flush context.Context.cache;
             Utils.log_with_header "Storing clean shutdown flag...%!";
             DbCache.set_clean_shutdown context.Context.cache;
             Utils.log_message "done\n%!";
             Utils.log_with_header "CURL cleanup...%!";
             ignore (GapiCurl.global_cleanup context.Context.curl_state);
             Utils.log_message "done\n%!";
             Utils.log_with_header "Clearing context...%!";
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

