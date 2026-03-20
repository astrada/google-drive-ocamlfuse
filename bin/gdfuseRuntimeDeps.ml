open GapiLens.Infix

module System = struct
  let open_log_out_ch = Utils.open_log_out_ch
  let start_browser = Utils.start_browser
  let register_exit = at_exit
  let curl_init = GapiCurl.global_init
  let curl_cleanup curl_state = ignore (GapiCurl.global_cleanup curl_state)
  let exit = exit
end

module Auth = struct
  let gae_start_server_polling = GaeProxy.start_server_polling

  let gae_refresh_access_token () =
    GaeProxy.refresh_access_token ();
    Context.get_ctx () |. Context.state_lens |. State.last_access_token

  let get_access_token = Oauth2.get_access_token
  let refresh_access_token = Oauth2.refresh_access_token
end

module Cache = struct
  let create_cache = Cache.create_cache
  let clean_up_cache = Cache.clean_up_cache
  let setup_db = Cache.setup_db
  let check_clean_shutdown = DbCache.check_clean_shutdown
  let reset_clean_shutdown = DbCache.reset_clean_shutdown
  let set_clean_shutdown = DbCache.set_clean_shutdown
  let flush = Cache.flush
end

module Fuse = struct
  let start_filesystem = GdfuseFuse.start_filesystem
end
