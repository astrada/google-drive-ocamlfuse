open GapiUtils.Infix
open GapiLens.Infix

let validate_mountpoint mountpoint =
  if not (Sys.file_exists mountpoint && Sys.is_directory mountpoint) then
    failwith ("Mountpoint " ^ mountpoint ^ " should be an existing directory.")

let get_auth_tokens_from_server params =
  let context = Context.get_ctx () in
  let request_id =
    let rid = context |. Context.request_id_lens in
    if rid = "" then GdfuseCommon.generate_request_id () else rid
  in
  context |> Context.request_id_lens ^= request_id |> Context.save_state_from_context;
  try
    let url = GdfuseCommon.get_authorization_url request_id in
    Utils.start_browser params.GdfuseCommon.browser url;
    GaeProxy.start_server_polling ()
  with
  | GaeProxy.ServerError e ->
      Utils.log_with_header "Removing invalid request_id=%s\n%!" request_id;
      context |> Context.request_id_lens ^= "" |> Context.save_state_from_context;
      Printf.eprintf "Cannot retrieve auth tokens: %s\n%!" e;
      exit 1
  | e ->
      prerr_endline "Cannot retrieve auth tokens.";
      Printexc.to_string e |> prerr_endline;
      exit 1

let resolve_paths_and_logging params =
  Utils.log_message "Starting application setup (label=%s, base_dir=%s).\n%!"
    params.GdfuseCommon.filesystem_label params.base_dir;
  let config_path, xdg_base_directory =
    AppDir.get_config_path params.config_path params.xdg_base_directory
      params.base_dir params.filesystem_label
  in
  let config_store_result =
    GdfuseCommon.get_config_store params.debug config_path
  in
  let config_store = config_store_result.ConfigStore.store in
  let current_config = config_store.Context.ConfigFileStore.data in
  let app_dir =
    AppDir.create current_config params.config_path params.base_dir
      params.filesystem_label xdg_base_directory
  in
  AppDir.create_directories app_dir;
  let app_log_path = app_dir.AppDir.app_log_path in
  let log_to =
    if params.log_to = "" then current_config |. Config.log_to else params.log_to
  in
  if log_to = "" then Utils.log_message "Opening log file: %s\n%!" app_log_path
  else Utils.log_message "Opening log file: %s\n%!" log_to;
  let log_channel = Utils.open_log_out_ch log_to app_log_path in
  Utils.log_channel := log_channel;
  Utils.log_with_header "Setting up %s filesystem...\n%!"
    params.filesystem_label;
  (config_store_result, config_store, current_config, app_dir, log_to)

let resolve_runtime_config params config_store_result config_store current_config =
  let config_resolution =
    ConfigRuntime.resolve
      {
        ConfigRuntime.persisted = current_config;
        load_state = config_store_result.ConfigStore.load_state;
        cli_client_id = params.GdfuseCommon.client_id;
        cli_client_secret = params.client_secret;
        cli_service_account_credentials_path =
          params.service_account_credentials_path;
        cli_service_account_user_to_impersonate =
          params.service_account_user_to_impersonate;
        cli_log_to = params.log_to;
        cli_scope = params.scope;
        cli_redirect_uri = params.redirect_uri;
        cli_docs_mode = params.docs_mode;
        cli_port = params.port;
        device = params.device;
        multi_threading = params.multi_threading;
      }
  in
  let config = config_resolution.ConfigRuntime.runtime_config in
  let persisted_config = config_resolution.ConfigRuntime.persisted_config in
  Utils.debug_buffers := config.Config.debug_buffers;
  let should_persist_config =
    (match config_store_result.ConfigStore.load_state with
    | ConfigStore.Created | ConfigStore.Migrated | ConfigStore.Upgraded -> true
    | ConfigStore.Loaded -> false)
    || config_resolution.ConfigRuntime.should_persist
  in
  if should_persist_config then
    Context.save_config_store
      (config_store |> Context.ConfigFileStore.data ^= persisted_config);
  let runtime_config_store =
    config_store |> Context.ConfigFileStore.data ^= config
  in
  Utils.max_retries := config.Config.max_retries;
  (config_resolution, config, runtime_config_store)

let is_gae_proxy_mode client_id client_secret service_account_credentials_path =
  service_account_credentials_path = ""
  && client_id = GaeProxy.gae_proxy_mode
  && client_secret = GaeProxy.gae_proxy_mode

let gae_proxy_refresh_access_token () =
  GaeProxy.refresh_access_token ();
  Context.get_ctx () |. Context.state_lens |. State.last_access_token

let build_gapi_config params config app_dir log_to =
  let client_id = config.Config.client_id in
  let client_secret = config.Config.client_secret in
  let service_account_credentials_path =
    config.Config.service_account_credentials_path
  in
  let gapi_config =
    Config.create_gapi_config config params.GdfuseCommon.debug
      app_dir.AppDir.curl_log_path log_to
  in
  let gapi_config =
    if
      is_gae_proxy_mode client_id client_secret service_account_credentials_path
    then
      let oauth2_config =
        match gapi_config |. GapiConfig.auth with
        | GapiConfig.OAuth2 oauth2 ->
            oauth2
            |> GapiConfig.refresh_access_token
               ^= Some gae_proxy_refresh_access_token
        | _ -> assert false
      in
      gapi_config |> GapiConfig.auth ^= GapiConfig.OAuth2 oauth2_config
    else gapi_config
  in
  (gapi_config, client_id, client_secret, service_account_credentials_path)

let resolve_clear_cache params config_resolution =
  if config_resolution.ConfigRuntime.clear_cache then (
    Utils.log_message "Docs mode changed to %s%!\n" params.GdfuseCommon.docs_mode;
    true)
  else (
    Utils.log_message "Docs mode not changed!\n";
    params.clear_cache)

let clear_cache_data cache =
  Printf.printf "Clearing cache...%!";
  Utils.log_message "Cleaning up cache...%!";
  Cache.clean_up_cache cache;
  Utils.log_message "done\n%!";
  Printf.printf "done\n%!"

let update_state_for_version_mismatch clear_cache cache state_store saved_version =
  if saved_version <> Config.version then (
    Utils.log_message "Version mismatch (saved=%s, current=%s)%!\n" saved_version
      Config.version;
    if not clear_cache then (
      Utils.log_message "Cleaning up cache...%!";
      Cache.clean_up_cache cache;
      Utils.log_message "done%!");
    Utils.log_message "\n%!";
    let updated_state_store =
      state_store |> Context.saved_version_lens ^= Config.version
    in
    Context.save_state_store updated_state_store;
    updated_state_store)
  else state_store

let load_state_and_cache params config_resolution app_dir config =
  let state_store = GdfuseCommon.get_state_store app_dir in
  let cache = Cache.create_cache app_dir config in
  let saved_version = state_store |. Context.saved_version_lens in
  Utils.log_message "Current version: %s\n%!" Config.version;
  let clear_cache = resolve_clear_cache params config_resolution in
  if clear_cache then clear_cache_data cache;
  let state_store =
    update_state_for_version_mismatch clear_cache cache state_store saved_version
  in
  Utils.log_message "Setting up cache db...%!";
  Cache.setup_db cache;
  Utils.log_message "done\nSetting up CURL...%!";
  let curl_state = GapiCurl.global_init () in
  Utils.log_message "done\n%!";
  let memory_buffers =
    Buffering.MemoryBuffers.create config.Config.memory_buffer_size
      config.Config.max_memory_cache_size
  in
  (state_store, cache, clear_cache, curl_state, memory_buffers)

let build_context params app_dir runtime_config_store gapi_config state_store
    cache curl_state memory_buffers =
  {
    Context.app_dir = app_dir;
    config_store = runtime_config_store;
    gapi_config;
    state_store;
    cache;
    curl_state;
    mountpoint_path = params.GdfuseCommon.mountpoint;
    mountpoint_stats = Unix.LargeFile.stat params.mountpoint;
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
    verification_code = "";
  }

let recover_from_dirty_shutdown clear_cache cache =
  if not (DbCache.check_clean_shutdown cache) then (
    Utils.log_with_header
      "google-drive-ocamlfuse didn't shut down correctly.%!\n";
    if not clear_cache then (
      Utils.log_message "Cleaning up cache...%!";
      Cache.clean_up_cache cache;
      Utils.log_message "done\nSetting up cache db...%!";
      Cache.setup_db cache;
      Utils.log_message "done\n...%!"))
  else DbCache.reset_clean_shutdown cache

let ensure_credentials params context config client_id client_secret =
  if config.Config.service_account_credentials_path = "" then
    let refresh_token = context |. Context.refresh_token_lens in
    if refresh_token = "" then
      if
        is_gae_proxy_mode client_id client_secret
          config.Config.service_account_credentials_path
      then get_auth_tokens_from_server params
      else if client_id = "" || client_secret = "" then
        if params.device then
          failwith
            "In device mode, you should specify a client id (-id) and a client \
             secret (-secret)"
        else
          failwith
            "You should specify a client id (-id) and a client secret (-secret)"
      else Oauth2.get_access_token params.headless params.device params.browser
    else Utils.log_message "Refresh token already present.\n%!"
  else (
    Utils.log_message "Service account credentials JSON path: %s.\n%!"
      config.Config.service_account_credentials_path;
    if config.Config.service_account_user_to_impersonate <> "" then
      Utils.log_message "Impersonating user: %s.\n%!"
        config.Config.service_account_user_to_impersonate)

let validate_initial_access_token config client_id client_secret =
  let refresh_access_token () =
    if
      is_gae_proxy_mode client_id client_secret
        config.Config.service_account_credentials_path
    then gae_proxy_refresh_access_token () |> ignore
    else if config.Config.service_account_credentials_path = "" then
      Oauth2.refresh_access_token () |> ignore
    else ()
  in
  try refresh_access_token () with
  | Oauth2.InvalidRefreshToken ->
      prerr_endline "Invalid refresh token. Quitting.";
      exit 1
  | e ->
      prerr_endline "Cannot refresh access token. Quitting.";
      Printexc.to_string e |> prerr_endline;
      exit 1

let setup_application params =
  validate_mountpoint params.GdfuseCommon.mountpoint;
  let config_store_result, config_store, current_config, app_dir, log_to =
    resolve_paths_and_logging params
  in
  let config_resolution, config, runtime_config_store =
    resolve_runtime_config params config_store_result config_store current_config
  in
  let gapi_config, client_id, client_secret, _service_account_credentials_path =
    build_gapi_config params config app_dir log_to
  in
  let state_store, cache, clear_cache, curl_state, memory_buffers =
    load_state_and_cache params config_resolution app_dir config
  in
  let context =
    build_context params app_dir runtime_config_store gapi_config state_store
      cache curl_state memory_buffers
  in
  Context.set_ctx context;
  recover_from_dirty_shutdown clear_cache cache;
  ensure_credentials params context config client_id client_secret;
  validate_initial_access_token config client_id client_secret
