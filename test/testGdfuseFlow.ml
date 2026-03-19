open OUnit
open GapiLens.Infix
open TestUtils

exception Exit_called of int

module FakeDeps = struct
  let trace = ref []
  let registered_exit = ref None
  let cache_clean_shutdown = ref true
  let invalid_refresh_token = ref false

  let record event = trace := !trace @ [ event ]

  module System = struct
    let open_log_out_ch _log_to _path =
      record "open_log";
      stdout

    let start_browser browser _url =
      record ("start_browser:" ^ browser)

    let register_exit callback =
      record "register_exit";
      registered_exit := Some callback

    let curl_init () =
      record "curl_init";
      Obj.magic ()

    let curl_cleanup _curl_state = record "curl_cleanup"
    let exit code = raise (Exit_called code)
  end

  module Auth = struct
    let gae_start_server_polling () = record "gae_poll"
    let gae_refresh_access_token () = record "gae_refresh"; "fake-access-token"

    let get_access_token _headless _device _browser =
      record "get_access_token"

    let refresh_access_token () =
      record "refresh_access_token";
      if !invalid_refresh_token then raise Oauth2.InvalidRefreshToken
      else "fake-access-token"
  end

  module Cache = struct
    let create_cache app_dir _config =
      record "create_cache";
      {
        CacheData.cache_dir = app_dir.AppDir.cache_dir;
        db_path = Filename.concat app_dir.cache_dir "test-cache.db";
        busy_timeout = 0;
        in_memory = true;
        autosaving_interval = 0;
      }

    let clean_up_cache _cache = record "clean_up_cache"
    let setup_db _cache = record "setup_db"

    let check_clean_shutdown _cache =
      record "check_clean_shutdown";
      !cache_clean_shutdown

    let reset_clean_shutdown _cache = record "reset_clean_shutdown"
    let set_clean_shutdown _cache = record "set_clean_shutdown"
    let flush _cache = record "flush"
  end

  module Fuse = struct
    let start_filesystem mountpoint _fuse_args =
      record ("start_filesystem:" ^ mountpoint)
  end
end

module Flow = GdfuseFlow.Make (FakeDeps)

let safe_clear_context () =
  try
    ignore (Context.get_ctx ());
    Context.clear_ctx ()
  with _ -> ()

let reset_runtime_globals () =
  safe_clear_context ();
  Utils.verbose := false;
  Utils.debug_buffers := false;
  Utils.max_retries := Config.default.Config.max_retries;
  Utils.log_channel := stdout;
  FakeDeps.trace := [];
  FakeDeps.registered_exit := None;
  FakeDeps.cache_clean_shutdown := true;
  FakeDeps.invalid_refresh_token := false

let with_clean_runtime f =
  Utils.try_finally
    (fun () ->
      reset_runtime_globals ();
      f ())
    reset_runtime_globals

let default_params ~base_dir ~mountpoint =
  {
    GdfuseCommon.debug = false;
    filesystem_label = GdfuseCommon.default_fs_label;
    client_id = "client-id";
    client_secret = "client-secret";
    mountpoint;
    clear_cache = false;
    headless = false;
    skip_trash = false;
    base_dir;
    multi_threading = false;
    config_path = "";
    xdg_base_directory = false;
    browser = "test-browser";
    docs_mode = "";
    service_account_credentials_path = "";
    service_account_user_to_impersonate = "";
    log_to = "stdout";
    scope = "";
    redirect_uri = "";
    device = false;
    port = 8080;
  }

let app_dir_for_params params =
  AppDir.create Config.default params.GdfuseCommon.config_path params.base_dir
    params.filesystem_label false

let write_state app_dir state =
  AppDir.create_directories app_dir;
  Context.StateFileStore.save
    { Context.StateFileStore.path = app_dir.AppDir.state_path; data = state }

let read_state app_dir = Context.StateFileStore.load app_dir.AppDir.state_path

let test_bootstrap_only_does_not_start_fuse () =
  with_temp_dir (fun dir ->
      with_clean_runtime (fun () ->
          let params = default_params ~base_dir:dir ~mountpoint:dir in
          Flow.run_bootstrap_only params;
          assert_bool "FUSE should not start in bootstrap-only mode"
            (List.for_all
               (fun event -> not (String.starts_with ~prefix:"start_filesystem:" event))
               !FakeDeps.trace);
          assert_bool "Expected context to be initialized"
            (try
               ignore (Context.get_ctx ());
               true
             with _ -> false)))

let test_mount_mode_registers_shutdown_and_starts_fuse () =
  with_temp_dir (fun dir ->
      with_clean_runtime (fun () ->
          let params = default_params ~base_dir:dir ~mountpoint:dir in
          Flow.run_mount_mode params [ "-f"; "-obig_writes" ];
          assert_bool "Expected shutdown callback to be registered"
            (Option.is_some !(FakeDeps.registered_exit));
          assert_equal ~printer:(String.concat ";")
            [ "open_log"; "create_cache"; "setup_db"; "curl_init";
              "check_clean_shutdown"; "reset_clean_shutdown";
              "get_access_token"; "refresh_access_token"; "register_exit";
              "start_filesystem:" ^ dir ]
            !FakeDeps.trace))

let test_existing_refresh_token_skips_interactive_auth () =
  with_temp_dir (fun dir ->
      with_clean_runtime (fun () ->
          let params = default_params ~base_dir:dir ~mountpoint:dir in
          let app_dir = app_dir_for_params params in
          let state =
            State.empty
            |> State.refresh_token ^= "refresh-token"
            |> State.saved_version ^= Config.version
          in
          write_state app_dir state;
          Flow.setup_application params;
          assert_bool "Expected refresh-token path to skip interactive auth"
            (not (List.mem "get_access_token" !FakeDeps.trace));
          assert_bool "Expected refresh-token validation"
            (List.mem "refresh_access_token" !FakeDeps.trace)))

let test_docs_mode_triggers_cache_cleanup () =
  with_temp_dir (fun dir ->
      with_clean_runtime (fun () ->
          let params =
            {
              (default_params ~base_dir:dir ~mountpoint:dir) with
              docs_mode = "msoffice";
            }
          in
          Flow.setup_application params;
          assert_bool "Expected cache cleanup for docs-mode change"
            (List.mem "clean_up_cache" !FakeDeps.trace)))

let test_version_mismatch_updates_state_and_cleans_cache () =
  with_temp_dir (fun dir ->
      with_clean_runtime (fun () ->
          let params = default_params ~base_dir:dir ~mountpoint:dir in
          let app_dir = app_dir_for_params params in
          let state =
            State.empty
            |> State.saved_version ^= "0.0.1"
            |> State.refresh_token ^= "refresh-token"
          in
          write_state app_dir state;
          Flow.setup_application params;
          let updated_state = read_state app_dir in
          assert_equal ~printer:(fun x -> x) Config.version
            updated_state.Context.StateFileStore.data.State.saved_version;
          assert_bool "Expected cache cleanup on version mismatch"
            (List.mem "clean_up_cache" !FakeDeps.trace)))

let test_missing_credentials_fails () =
  with_temp_dir (fun dir ->
      with_clean_runtime (fun () ->
          let params =
            {
              (default_params ~base_dir:dir ~mountpoint:dir) with
              client_id = "";
              client_secret = "";
            }
          in
          assert_raises
            (Failure
               "You should specify a client id (-id) and a client secret \
                (-secret)")
            (fun () -> Flow.setup_application params)))

let test_shutdown_flushes_and_cleans_up_in_order () =
  with_temp_dir (fun dir ->
      with_clean_runtime (fun () ->
          let params = default_params ~base_dir:dir ~mountpoint:dir in
          let app_dir = app_dir_for_params params in
          let state =
            State.empty
            |> State.refresh_token ^= "refresh-token"
            |> State.saved_version ^= Config.version
          in
          write_state app_dir state;
          Flow.run_mount_mode params [ "-f"; "-obig_writes" ];
          let shutdown =
            match !(FakeDeps.registered_exit) with
            | Some callback -> callback
            | None -> failwith "Expected shutdown callback"
          in
          FakeDeps.trace := [];
          shutdown ();
          assert_equal ~printer:(String.concat ";")
            [ "flush"; "set_clean_shutdown"; "curl_cleanup" ]
            !FakeDeps.trace;
          assert_bool "Expected context to be cleared after shutdown"
            (try
               ignore (Context.get_ctx ());
               false
             with _ -> true)))

let suite =
  "GdfuseFlow test"
  >::: [
         "test_bootstrap_only_does_not_start_fuse"
         >:: test_bootstrap_only_does_not_start_fuse;
         "test_mount_mode_registers_shutdown_and_starts_fuse"
         >:: test_mount_mode_registers_shutdown_and_starts_fuse;
         "test_existing_refresh_token_skips_interactive_auth"
         >:: test_existing_refresh_token_skips_interactive_auth;
         "test_docs_mode_triggers_cache_cleanup"
         >:: test_docs_mode_triggers_cache_cleanup;
         "test_version_mismatch_updates_state_and_cleans_cache"
         >:: test_version_mismatch_updates_state_and_cleans_cache;
         "test_missing_credentials_fails" >:: test_missing_credentials_fails;
         "test_shutdown_flushes_and_cleans_up_in_order"
         >:: test_shutdown_flushes_and_cleans_up_in_order;
       ]
