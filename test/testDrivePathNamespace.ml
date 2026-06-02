open OUnit
module Namespace = DrivePathNamespace

let make_config ?(disable_trash = false) ?(lost_and_found = false) () =
  { Config.default with disable_trash; lost_and_found }

let string_of_mapping (path, trashed) = Printf.sprintf "(%S, %b)" path trashed

let assert_path_mapping expected path config =
  assert_equal ~printer:string_of_mapping expected
    (Namespace.get_path_in_cache path config)

let test_constants () =
  assert_equal "/" Namespace.root_directory;
  assert_equal "/.Trash" Namespace.trash_directory;
  assert_equal
    (String.length Namespace.trash_directory)
    Namespace.trash_directory_name_length;
  assert_equal "/.Trash/" Namespace.trash_directory_base_path;
  assert_equal "/lost+found" Namespace.lost_and_found_directory;
  assert_equal "/.shared" Namespace.shared_with_me_directory

let test_trash_mapping_enabled () =
  let config = make_config () in
  assert_path_mapping ("/", false) "/" config;
  assert_path_mapping ("/file.txt", false) "/file.txt" config;
  assert_path_mapping ("/", true) "/.Trash" config;
  assert_path_mapping ("/file.txt", true) "/.Trash/file.txt" config;
  assert_path_mapping ("/dir/file.txt", true) "/.Trash/dir/file.txt" config;
  assert_path_mapping
    ("/.Trashcan/file.txt", false)
    "/.Trashcan/file.txt" config

let test_trash_mapping_disabled () =
  let config = make_config ~disable_trash:true () in
  assert_path_mapping ("/.Trash", false) "/.Trash" config;
  assert_path_mapping ("/.Trash/file.txt", false) "/.Trash/file.txt" config;
  assert_bool "trash root is not considered a nested trash path"
    (not (Namespace.is_in_trash_directory "/.Trash" config));
  assert_bool "nested trash paths are disabled by config"
    (not (Namespace.is_in_trash_directory "/.Trash/file.txt" config))

let test_trash_directory_predicate () =
  let enabled = make_config () in
  let disabled = make_config ~disable_trash:true () in
  assert_bool "nested trash path is detected"
    (Namespace.is_in_trash_directory "/.Trash/file.txt" enabled);
  assert_bool "trash root is not nested"
    (not (Namespace.is_in_trash_directory "/.Trash" enabled));
  assert_bool "trash-like prefix does not match"
    (not (Namespace.is_in_trash_directory "/.Trashcan/file.txt" enabled));
  assert_bool "disabled trash config rejects nested paths"
    (not (Namespace.is_in_trash_directory "/.Trash/file.txt" disabled))

let test_lost_and_found_predicates () =
  let enabled = make_config ~lost_and_found:true () in
  let disabled = make_config ~lost_and_found:false () in
  assert_bool "enabled lost+found root matches"
    (Namespace.is_lost_and_found_root "/lost+found" false enabled);
  assert_bool "disabled lost+found root is hidden"
    (not (Namespace.is_lost_and_found_root "/lost+found" false disabled));
  assert_bool "trashed lost+found root is hidden"
    (not (Namespace.is_lost_and_found_root "/lost+found" true enabled));
  assert_bool "root predicate is exact"
    (not
       (Namespace.is_lost_and_found_root "/lost+found/file.txt" false enabled));
  assert_bool "nested lost+found path matches prefix"
    (Namespace.is_lost_and_found "/lost+found/file.txt" false enabled);
  assert_bool "lost+found prefix is disabled by config"
    (not (Namespace.is_lost_and_found "/lost+found/file.txt" false disabled));
  assert_bool "trashed lost+found prefix is hidden"
    (not (Namespace.is_lost_and_found "/lost+found/file.txt" true enabled))

let test_shared_with_me_predicates () =
  let config = make_config () in
  let other_config = make_config ~lost_and_found:true ~disable_trash:true () in
  assert_bool "shared-with-me root matches"
    (Namespace.is_shared_with_me_root "/.shared" false config);
  assert_bool "trashed shared-with-me root is hidden"
    (not (Namespace.is_shared_with_me_root "/.shared" true config));
  assert_bool "root predicate is exact"
    (not (Namespace.is_shared_with_me_root "/.shared/file.txt" false config));
  assert_bool "nested shared-with-me path matches prefix"
    (Namespace.is_shared_with_me "/.shared/file.txt" false config);
  assert_bool "trashed shared-with-me prefix is hidden"
    (not (Namespace.is_shared_with_me "/.shared/file.txt" true config));
  assert_bool "shared-with-me ignores unrelated config fields"
    (Namespace.is_shared_with_me "/.shared/file.txt" false other_config)

let suite =
  "DrivePathNamespace tests"
  >::: [
         "test_constants" >:: test_constants;
         "test_trash_mapping_enabled" >:: test_trash_mapping_enabled;
         "test_trash_mapping_disabled" >:: test_trash_mapping_disabled;
         "test_trash_directory_predicate" >:: test_trash_directory_predicate;
         "test_lost_and_found_predicates" >:: test_lost_and_found_predicates;
         "test_shared_with_me_predicates" >:: test_shared_with_me_predicates;
       ]
