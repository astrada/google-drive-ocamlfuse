open OUnit

let with_temp_dir f =
  let rec make_dir n =
    let path =
      Filename.concat (Filename.get_temp_dir_name ())
        (Printf.sprintf "gdfuse-configstore-%d-%06d" (Unix.getpid ()) n)
    in
    if Sys.file_exists path then make_dir (n + 1)
    else (
      Unix.mkdir path 0o700;
      path)
  in
  let dir = make_dir 0 in
  let finally () =
    if Sys.file_exists dir then
      Array.iter (fun name -> Sys.remove (Filename.concat dir name)) (Sys.readdir dir);
    if Sys.file_exists dir then Unix.rmdir dir
  in
  Utils.try_finally (fun () -> f dir) finally

let read_file path =
  Utils.with_in_channel path (fun ch ->
      let buffer = Buffer.create 256 in
      (try
         while true do
           Buffer.add_string buffer (input_line ch);
           Buffer.add_char buffer '\n'
         done
       with End_of_file -> ());
      Buffer.contents buffer)

let write_file path content =
  Utils.with_out_channel ~mode:[ Open_creat; Open_trunc; Open_wronly ] path
    (fun ch -> output_string ch content)

let assert_contains needle haystack =
  assert_bool
    (Printf.sprintf "Expected to find %S in:\n%s" needle haystack)
    (try
       ignore (Str.search_forward (Str.regexp_string needle) haystack 0);
       true
     with Not_found -> false)

let assert_not_contains needle haystack =
  assert_bool
    (Printf.sprintf "Expected not to find %S in:\n%s" needle haystack)
    (try
       ignore (Str.search_forward (Str.regexp_string needle) haystack 0);
       false
     with Not_found -> true)

let test_create_default_writes_minimal_toml () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "config" in
      let _store = ConfigStore.create_default ~debug:false ~path in
      let contents = read_file path in
      assert_contains "config_version = 1" contents;
      assert_not_contains "[auth]" contents;
      assert_not_contains "read_only" contents;
      assert_not_contains "client_id" contents)

let test_load_or_create_migrates_legacy_file () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "config" in
      write_file path
        "metadata_cache_time=61\nread_only=true\nclient_id=test-client\n";
      let result = ConfigStore.load_or_create ~debug:false path in
      assert_equal ~printer:string_of_bool false result.created;
      assert_equal ~printer:string_of_bool true result.migrated;
      assert_equal ~printer:string_of_bool false result.upgraded;
      assert_equal ~printer:string_of_int 61 result.store.data.Config.metadata_cache_time;
      assert_equal ~printer:string_of_bool true result.store.data.Config.read_only;
      assert_equal ~printer:(fun x -> x) "test-client"
        result.store.data.Config.client_id;
      let migrated_contents = read_file path in
      assert_contains "config_version = 1" migrated_contents;
      assert_contains "[mount]" migrated_contents;
      assert_contains "metadata_cache_time = 61" migrated_contents;
      assert_contains "read_only = true" migrated_contents;
      assert_contains "[auth]" migrated_contents;
      assert_contains "client_id = \"test-client\"" migrated_contents;
      let backup_path = path ^ ".legacy.bak" in
      assert_bool "Expected legacy backup to exist" (Sys.file_exists backup_path);
      let backup_contents = read_file backup_path in
      assert_contains "client_id=test-client" backup_contents)

let test_duplicate_legacy_keys_are_rejected () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "config" in
      write_file path "read_only=true\nread_only=false\n";
      assert_raises
        (ConfigStore.Parse_error
           (Printf.sprintf
              "Cannot parse configuration %s: duplicate key 'read_only' at line 2"
              path))
        (fun () -> ignore (ConfigStore.load_or_create ~debug:false path)))

let test_existing_toml_with_comments_is_not_rewritten () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "config" in
      let contents =
        "# keep this comment\nconfig_version = 1\n\n[auth]\nclient_id = \"test-client\"\n"
      in
      write_file path contents;
      let result = ConfigStore.load_or_create ~debug:false path in
      assert_equal ~printer:string_of_bool false result.created;
      assert_equal ~printer:string_of_bool false result.migrated;
      assert_equal ~printer:string_of_bool false result.upgraded;
      assert_equal ~printer:(fun x -> x) "test-client"
        result.store.data.Config.client_id;
      assert_equal ~printer:(fun x -> x) contents (read_file path))

let test_grouped_toml_is_loaded () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "config" in
      write_file path
        "config_version = 1\n\n[auth]\nclient_id = \"client\"\n\n[mount]\nread_only = true\n";
      let result = ConfigStore.load_or_create ~debug:false path in
      assert_equal ~printer:string_of_bool false result.upgraded;
      assert_equal ~printer:(fun x -> x) "client" result.store.data.Config.client_id;
      assert_equal ~printer:string_of_bool true result.store.data.Config.read_only)

let test_unversioned_toml_is_upgraded () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "config" in
      write_file path "[auth]\nclient_id = \"client\"\n";
      let result = ConfigStore.load_or_create ~debug:false path in
      assert_equal ~printer:string_of_bool false result.created;
      assert_equal ~printer:string_of_bool false result.migrated;
      assert_equal ~printer:string_of_bool true result.upgraded;
      assert_equal ~printer:(fun x -> x) "client" result.store.data.Config.client_id;
      let contents = read_file path in
      assert_contains "config_version = 1" contents;
      assert_contains "[auth]" contents)

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

let suite =
  "ConfigStore test"
  >::: [
         "test_create_default_writes_minimal_toml"
         >:: test_create_default_writes_minimal_toml;
         "test_load_or_create_migrates_legacy_file"
         >:: test_load_or_create_migrates_legacy_file;
         "test_duplicate_legacy_keys_are_rejected"
         >:: test_duplicate_legacy_keys_are_rejected;
         "test_existing_toml_with_comments_is_not_rewritten"
         >:: test_existing_toml_with_comments_is_not_rewritten;
         "test_grouped_toml_is_loaded" >:: test_grouped_toml_is_loaded;
         "test_unversioned_toml_is_upgraded"
         >:: test_unversioned_toml_is_upgraded;
         "test_future_config_version_is_rejected"
         >:: test_future_config_version_is_rejected;
       ]
