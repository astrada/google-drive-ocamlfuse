open OUnit

let default_inputs () =
  {
    ConfigRuntime.persisted = Config.default;
    load_state = ConfigStore.Loaded;
    cli_client_id = "";
    cli_client_secret = "";
    cli_service_account_credentials_path = "";
    cli_service_account_user_to_impersonate = "";
    cli_log_to = "";
    cli_scope = "";
    cli_redirect_uri = "";
    cli_docs_mode = "";
    cli_port = Config.default.Config.oauth2_loopback_port;
    device = false;
    multi_threading = false;
  }

let test_id_and_secret_persist () =
  let defaults = default_inputs () in
  let inputs =
    { defaults with cli_client_id = "new-id"; cli_client_secret = "new-secret" }
  in
  let result = ConfigRuntime.resolve inputs in
  assert_equal
    ~printer:(fun x -> x)
    "new-id" result.ConfigRuntime.persisted_config.Config.client_id;
  assert_equal
    ~printer:(fun x -> x)
    "new-secret" result.ConfigRuntime.persisted_config.Config.client_secret;
  assert_equal ~printer:string_of_bool true result.ConfigRuntime.should_persist

let test_scope_does_not_persist () =
  let defaults = default_inputs () in
  let inputs = { defaults with cli_scope = "custom-scope" } in
  let result = ConfigRuntime.resolve inputs in
  assert_equal
    ~printer:(fun x -> x)
    "" result.ConfigRuntime.persisted_config.Config.scope;
  assert_equal
    ~printer:(fun x -> x)
    "custom-scope" result.ConfigRuntime.runtime_config.Config.scope;
  assert_equal ~printer:string_of_bool false result.ConfigRuntime.should_persist

let test_redirect_uri_does_not_persist () =
  let defaults = default_inputs () in
  let inputs = { defaults with cli_redirect_uri = "http://example" } in
  let result = ConfigRuntime.resolve inputs in
  assert_equal
    ~printer:(fun x -> x)
    "" result.ConfigRuntime.persisted_config.Config.redirect_uri;
  assert_equal
    ~printer:(fun x -> x)
    "http://example" result.ConfigRuntime.runtime_config.Config.redirect_uri;
  assert_equal ~printer:string_of_bool false result.ConfigRuntime.should_persist

let test_port_is_runtime_only () =
  let defaults = default_inputs () in
  let inputs = { defaults with cli_port = 9999 } in
  let result = ConfigRuntime.resolve inputs in
  assert_equal ~printer:string_of_int Config.default.Config.oauth2_loopback_port
    result.ConfigRuntime.persisted_config.Config.oauth2_loopback_port;
  assert_equal ~printer:string_of_int 9999
    result.ConfigRuntime.runtime_config.Config.oauth2_loopback_port

let test_docs_mode_requests_cache_clear () =
  let defaults = default_inputs () in
  let inputs = { defaults with cli_docs_mode = "msoffice" } in
  let result = ConfigRuntime.resolve inputs in
  assert_equal ~printer:string_of_bool true result.ConfigRuntime.clear_cache;
  assert_equal
    ~printer:(fun x -> x)
    "docx" result.ConfigRuntime.runtime_config.Config.document_format

let test_no_changes_do_not_persist () =
  let result = ConfigRuntime.resolve (default_inputs ()) in
  assert_equal ~printer:string_of_bool false result.ConfigRuntime.should_persist;
  assert_equal ~printer:string_of_bool false result.ConfigRuntime.clear_cache

let test_migrated_config_is_not_persisted_again () =
  let defaults = default_inputs () in
  let result =
    ConfigRuntime.resolve { defaults with load_state = ConfigStore.Migrated }
  in
  assert_equal ~printer:string_of_bool false result.ConfigRuntime.should_persist

let test_created_config_is_not_persisted_again () =
  let defaults = default_inputs () in
  let result =
    ConfigRuntime.resolve { defaults with load_state = ConfigStore.Created }
  in
  assert_equal ~printer:string_of_bool false result.ConfigRuntime.should_persist

let test_upgraded_config_is_not_persisted_again () =
  let defaults = default_inputs () in
  let result =
    ConfigRuntime.resolve { defaults with load_state = ConfigStore.Upgraded }
  in
  assert_equal ~printer:string_of_bool false result.ConfigRuntime.should_persist

let suite =
  "ConfigRuntime test"
  >::: [
         "test_id_and_secret_persist" >:: test_id_and_secret_persist;
         "test_scope_does_not_persist" >:: test_scope_does_not_persist;
         "test_redirect_uri_does_not_persist"
         >:: test_redirect_uri_does_not_persist;
         "test_port_is_runtime_only" >:: test_port_is_runtime_only;
         "test_docs_mode_requests_cache_clear"
         >:: test_docs_mode_requests_cache_clear;
         "test_no_changes_do_not_persist" >:: test_no_changes_do_not_persist;
         "test_migrated_config_is_not_persisted_again"
         >:: test_migrated_config_is_not_persisted_again;
         "test_created_config_is_not_persisted_again"
         >:: test_created_config_is_not_persisted_again;
         "test_upgraded_config_is_not_persisted_again"
         >:: test_upgraded_config_is_not_persisted_again;
       ]
