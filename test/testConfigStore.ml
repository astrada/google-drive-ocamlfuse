open OUnit
open TestUtils

let test_create_default_writes_minimal_toml () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "config" in
      let _store = ConfigStore.create_default ~debug:false ~path in
      let contents = read_file path in
      assert_contains "config_version = 1" contents;
      assert_not_contains "[auth]" contents;
      assert_not_contains "read_only" contents;
      assert_not_contains "client_id" contents)

let test_mount_option_is_emitted_in_grouped_toml () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "config" in
      let store = ConfigStore.create_default ~debug:false ~path in
      let updated_store =
        {
          store with
          ConfigStore.data =
            { store.data with Config.delete_forever_in_trash_folder = true };
        }
      in
      ConfigStore.save updated_store;
      let contents = read_file path in
      assert_contains "[mount]" contents;
      assert_contains "delete_forever_in_trash_folder = true" contents)

let test_save_rotates_previous_version_to_bak () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "config" in
      let store = ConfigStore.create_default ~debug:false ~path in
      let original = read_file path in
      let updated_store =
        {
          store with
          ConfigStore.data =
            { store.data with Config.client_id = "rotated-client" };
        }
      in
      ConfigStore.save updated_store;
      let backup_path = path ^ ".bak" in
      assert_bool "Expected backup to exist after save"
        (Sys.file_exists backup_path);
      assert_equal ~printer:(fun x -> x) original (read_file backup_path);
      assert_contains "client_id = \"rotated-client\"" (read_file path))

let test_load_or_create_migrates_legacy_file () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "config" in
      let legacy_contents =
        "metadata_cache_time=61\nread_only=true\nclient_id=test-client\n"
      in
      write_file path legacy_contents;
      let result = ConfigStore.load_or_create ~debug:false path in
      assert_equal ConfigStore.Migrated result.load_state;
      assert_equal ~printer:string_of_int 61
        result.store.data.Config.metadata_cache_time;
      assert_equal ~printer:string_of_bool true
        result.store.data.Config.read_only;
      assert_equal
        ~printer:(fun x -> x)
        "test-client" result.store.data.Config.client_id;
      assert_equal ~printer:(fun x -> x) legacy_contents (read_file path);
      let backup_path = path ^ ".bak" in
      assert_bool "Did not expect backup to exist before an explicit save"
        (not (Sys.file_exists backup_path)))

let test_save_after_migration_writes_toml_and_rotates_legacy_to_bak () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "config" in
      let legacy_contents =
        "metadata_cache_time=61\nread_only=true\nclient_id=test-client\n"
      in
      write_file path legacy_contents;
      let result = ConfigStore.load_or_create ~debug:false path in
      assert_equal ConfigStore.Migrated result.load_state;
      ConfigStore.save result.store;
      let migrated_contents = read_file path in
      assert_contains "config_version = 1" migrated_contents;
      assert_contains "[mount]" migrated_contents;
      assert_contains "metadata_cache_time = 61" migrated_contents;
      assert_contains "read_only = true" migrated_contents;
      assert_contains "[auth]" migrated_contents;
      assert_contains "client_id = \"test-client\"" migrated_contents;
      let backup_path = path ^ ".bak" in
      assert_bool "Expected legacy backup to exist after explicit save"
        (Sys.file_exists backup_path);
      assert_equal ~printer:(fun x -> x) legacy_contents (read_file backup_path))

let test_load_or_create_missing_file_does_not_persist () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "config" in
      let result = ConfigStore.load_or_create ~debug:false path in
      assert_equal ConfigStore.Created result.load_state;
      assert_equal ~printer:string_of_bool false (Sys.file_exists path))

let test_legacy_missing_keys_fall_back_to_defaults () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "config" in
      write_file path "read_only=true\n";
      let result = ConfigStore.load_or_create ~debug:false path in
      assert_equal ConfigStore.Migrated result.load_state;
      assert_equal ~printer:string_of_bool true
        result.store.data.Config.read_only;
      assert_equal ~printer:string_of_int
        Config.default.Config.metadata_cache_time
        result.store.data.Config.metadata_cache_time;
      assert_equal
        ~printer:(fun x -> x)
        Config.default.Config.client_id result.store.data.Config.client_id)

let test_duplicate_legacy_keys_are_rejected () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "config" in
      write_file path "read_only=true\nread_only=false\n";
      assert_raises
        (ConfigStore.Parse_error
           (Printf.sprintf
              "Cannot parse configuration %s: duplicate key 'read_only' at \
               line 2"
              path))
        (fun () -> ignore (ConfigStore.load_or_create ~debug:false path)))

let test_legacy_empty_values_are_allowed () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "config" in
      write_file path "client_id=\nredirect_uri=\n";
      let result = ConfigStore.load_or_create ~debug:false path in
      assert_equal ConfigStore.Migrated result.load_state;
      assert_equal ~printer:(fun x -> x) "" result.store.data.Config.client_id;
      assert_equal
        ~printer:(fun x -> x)
        "" result.store.data.Config.redirect_uri)

let test_unknown_legacy_key_is_rejected () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "config" in
      write_file path "unknown_key=true\n";
      assert_raises
        (ConfigStore.Parse_error
           (Printf.sprintf
              "Cannot parse configuration %s: unknown key 'unknown_key'" path))
        (fun () -> ignore (ConfigStore.load_or_create ~debug:false path)))

let test_malformed_legacy_line_is_rejected () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "config" in
      write_file path "read_only true\n";
      assert_parse_error_contains
        (Printf.sprintf "Cannot parse configuration %s:" path) (fun () ->
          ignore (ConfigStore.load_or_create ~debug:false path)))

let test_existing_toml_with_comments_is_not_rewritten () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "config" in
      let contents =
        "# keep this comment\n\
         config_version = 1\n\n\
         [auth]\n\
         client_id = \"test-client\"\n"
      in
      write_file path contents;
      let result = ConfigStore.load_or_create ~debug:false path in
      assert_equal ConfigStore.Loaded result.load_state;
      assert_equal
        ~printer:(fun x -> x)
        "test-client" result.store.data.Config.client_id;
      assert_equal ~printer:(fun x -> x) contents (read_file path))

let test_grouped_toml_is_loaded () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "config" in
      write_file path
        "config_version = 1\n\n\
         [auth]\n\
         client_id = \"client\"\n\n\
         [mount]\n\
         read_only = true\n";
      let result = ConfigStore.load_or_create ~debug:false path in
      assert_equal ConfigStore.Loaded result.load_state;
      assert_equal
        ~printer:(fun x -> x)
        "client" result.store.data.Config.client_id;
      assert_equal ~printer:string_of_bool true
        result.store.data.Config.read_only)

let test_unversioned_toml_is_upgraded () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "config" in
      let contents = "[auth]\nclient_id = \"client\"\n" in
      write_file path contents;
      let result = ConfigStore.load_or_create ~debug:false path in
      assert_equal ConfigStore.Upgraded result.load_state;
      assert_equal
        ~printer:(fun x -> x)
        "client" result.store.data.Config.client_id;
      assert_equal ~printer:(fun x -> x) contents (read_file path))

let test_future_config_version_is_rejected () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "config" in
      write_file path "config_version = 99\n";
      assert_raises
        (ConfigStore.Parse_error
           (Printf.sprintf
              "Cannot parse configuration %s: unsupported config_version 99"
              path))
        (fun () -> ignore (ConfigStore.load_or_create ~debug:false path)))

let test_unknown_toml_key_is_rejected () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "config" in
      write_file path "config_version = 1\nunknown_key = true\n";
      assert_raises
        (ConfigStore.Parse_error
           (Printf.sprintf
              "Cannot parse configuration %s: unknown key 'unknown_key'" path))
        (fun () -> ignore (ConfigStore.load_or_create ~debug:false path)))

let test_wrong_toml_type_is_rejected () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "config" in
      write_file path "config_version = 1\n\n[mount]\nread_only = [true]\n";
      assert_raises
        (ConfigStore.Parse_error
           (Printf.sprintf
              "Cannot parse configuration %s: unsupported TOML value for key \
               'read_only'"
              path))
        (fun () -> ignore (ConfigStore.load_or_create ~debug:false path)))

let test_invalid_memory_buffer_size_is_rejected () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "config" in
      write_file path "config_version = 1\n\n[io]\nmemory_buffer_size = 1\n";
      assert_raises
        (ConfigStore.Parse_error
           (Printf.sprintf
              "Cannot parse configuration %s: memory_buffer_size should be >= \
               131072 (128k)"
              path))
        (fun () -> ignore (ConfigStore.load_or_create ~debug:false path)))

let test_invalid_max_memory_cache_size_is_rejected () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "config" in
      write_file path
        "config_version = 1\n\n\
         [io]\n\
         memory_buffer_size = 131072\n\
         max_memory_cache_size = 131071\n";
      assert_raises
        (ConfigStore.Parse_error
           (Printf.sprintf
              "Cannot parse configuration %s: max_memory_cache_size should be \
               >= memory_buffer_size"
              path))
        (fun () -> ignore (ConfigStore.load_or_create ~debug:false path)))

let test_invalid_max_upload_chunk_size_is_rejected () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "config" in
      write_file path "config_version = 1\n\n[io]\nmax_upload_chunk_size = 0\n";
      assert_raises
        (ConfigStore.Parse_error
           (Printf.sprintf
              "Cannot parse configuration %s: max_upload_chunk_size should be \
               > 0"
              path))
        (fun () -> ignore (ConfigStore.load_or_create ~debug:false path)))

let test_invalid_umask_representation_is_rejected () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "config" in
      write_file path "config_version = 1\n\n[mount]\numask = \"nope\"\n";
      assert_raises
        (ConfigStore.Parse_error
           (Printf.sprintf "Cannot parse configuration %s: int_of_string" path))
        (fun () -> ignore (ConfigStore.load_or_create ~debug:false path)))

let suite =
  "ConfigStore test"
  >::: [
         "test_create_default_writes_minimal_toml"
         >:: test_create_default_writes_minimal_toml;
         "test_mount_option_is_emitted_in_grouped_toml"
         >:: test_mount_option_is_emitted_in_grouped_toml;
         "test_save_rotates_previous_version_to_bak"
         >:: test_save_rotates_previous_version_to_bak;
         "test_load_or_create_migrates_legacy_file"
         >:: test_load_or_create_migrates_legacy_file;
         "test_save_after_migration_writes_toml_and_rotates_legacy_to_bak"
         >:: test_save_after_migration_writes_toml_and_rotates_legacy_to_bak;
         "test_load_or_create_missing_file_does_not_persist"
         >:: test_load_or_create_missing_file_does_not_persist;
         "test_legacy_missing_keys_fall_back_to_defaults"
         >:: test_legacy_missing_keys_fall_back_to_defaults;
         "test_duplicate_legacy_keys_are_rejected"
         >:: test_duplicate_legacy_keys_are_rejected;
         "test_legacy_empty_values_are_allowed"
         >:: test_legacy_empty_values_are_allowed;
         "test_unknown_legacy_key_is_rejected"
         >:: test_unknown_legacy_key_is_rejected;
         "test_malformed_legacy_line_is_rejected"
         >:: test_malformed_legacy_line_is_rejected;
         "test_existing_toml_with_comments_is_not_rewritten"
         >:: test_existing_toml_with_comments_is_not_rewritten;
         "test_grouped_toml_is_loaded" >:: test_grouped_toml_is_loaded;
         "test_unversioned_toml_is_upgraded"
         >:: test_unversioned_toml_is_upgraded;
         "test_future_config_version_is_rejected"
         >:: test_future_config_version_is_rejected;
         "test_unknown_toml_key_is_rejected"
         >:: test_unknown_toml_key_is_rejected;
         "test_wrong_toml_type_is_rejected" >:: test_wrong_toml_type_is_rejected;
         "test_invalid_memory_buffer_size_is_rejected"
         >:: test_invalid_memory_buffer_size_is_rejected;
         "test_invalid_max_memory_cache_size_is_rejected"
         >:: test_invalid_max_memory_cache_size_is_rejected;
         "test_invalid_max_upload_chunk_size_is_rejected"
         >:: test_invalid_max_upload_chunk_size_is_rejected;
         "test_invalid_umask_representation_is_rejected"
         >:: test_invalid_umask_representation_is_rejected;
       ]
