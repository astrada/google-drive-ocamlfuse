open OUnit
module ResourceKeys = DriveResourceKeys

let make_resource ?remote_id ?resource_key path =
  {
    (DriveResourceMapping.create_resource ~now:(fun () -> 0.) path) with
    CacheData.Resource.remote_id;
    resource_key;
  }

let string_of_headers headers =
  headers
  |> List.map (function
    | GapiCore.Header.KeyValueHeader (name, value) -> name ^ ":" ^ value
    | _ -> "<non-key-value-header>")
  |> String.concat "; "

let assert_no_headers headers =
  assert_equal ~printer:string_of_headers [] headers

let assert_single_header expected_value headers =
  match headers with
  | [ GapiCore.Header.KeyValueHeader (name, value) ] ->
      assert_equal ResourceKeys.resource_keys_header_name name;
      assert_equal expected_value value
  | _ ->
      assert_failure
        (Printf.sprintf "expected one resource-key header, got [%s]"
           (string_of_headers headers))

let test_raw_empty_input_returns_no_headers () =
  assert_no_headers (ResourceKeys.build_resource_keys_header [])

let test_raw_invalid_pairs_return_no_headers () =
  assert_no_headers
    (ResourceKeys.build_resource_keys_header
       [ (Some "rid-a", None); (Some "rid-b", Some ""); (None, Some "key-c") ])

let test_raw_single_valid_pair_emits_header () =
  assert_single_header "rid-a/key-a"
    (ResourceKeys.build_resource_keys_header [ (Some "rid-a", Some "key-a") ])

let test_raw_multiple_valid_pairs_preserve_input_order () =
  assert_single_header "rid-a/key-a,rid-b/key-b,rid-c/key-c"
    (ResourceKeys.build_resource_keys_header
       [
         (Some "rid-a", Some "key-a");
         (Some "rid-b", Some "key-b");
         (Some "rid-c", Some "key-c");
       ])

let test_raw_skips_invalid_pairs_without_reordering () =
  assert_single_header "rid-a/key-a,rid-b/key-b"
    (ResourceKeys.build_resource_keys_header
       [
         (Some "missing-key", None);
         (Some "rid-a", Some "key-a");
         (None, Some "missing-id");
         (Some "empty-key", Some "");
         (Some "rid-b", Some "key-b");
       ])

let test_resource_with_id_and_key_emits_header () =
  let resource =
    make_resource ~remote_id:"rid-a" ~resource_key:"key-a" "/file.txt"
  in
  assert_single_header "rid-a/key-a"
    (ResourceKeys.build_resource_keys_header_from_resource resource)

let test_resource_without_key_emits_no_headers () =
  let resource = make_resource ~remote_id:"rid-a" "/file.txt" in
  assert_no_headers
    (ResourceKeys.build_resource_keys_header_from_resource resource)

let test_resource_without_remote_id_emits_no_headers () =
  let resource = make_resource ~resource_key:"key-a" "/file.txt" in
  assert_no_headers
    (ResourceKeys.build_resource_keys_header_from_resource resource)

let test_resources_preserve_valid_resource_order () =
  let resources =
    [
      make_resource ~remote_id:"rid-a" ~resource_key:"key-a" "/a.txt";
      make_resource ~remote_id:"missing-key" "/ignored-a.txt";
      make_resource ~resource_key:"missing-id" "/ignored-b.txt";
      make_resource ~remote_id:"empty-key" ~resource_key:"" "/ignored-c.txt";
      make_resource ~remote_id:"rid-b" ~resource_key:"key-b" "/b.txt";
    ]
  in
  assert_single_header "rid-a/key-a,rid-b/key-b"
    (ResourceKeys.build_resource_keys_header_from_resources resources)

let suite =
  "DriveResourceKeys tests"
  >::: [
         "test_raw_empty_input_returns_no_headers"
         >:: test_raw_empty_input_returns_no_headers;
         "test_raw_invalid_pairs_return_no_headers"
         >:: test_raw_invalid_pairs_return_no_headers;
         "test_raw_single_valid_pair_emits_header"
         >:: test_raw_single_valid_pair_emits_header;
         "test_raw_multiple_valid_pairs_preserve_input_order"
         >:: test_raw_multiple_valid_pairs_preserve_input_order;
         "test_raw_skips_invalid_pairs_without_reordering"
         >:: test_raw_skips_invalid_pairs_without_reordering;
         "test_resource_with_id_and_key_emits_header"
         >:: test_resource_with_id_and_key_emits_header;
         "test_resource_without_key_emits_no_headers"
         >:: test_resource_without_key_emits_no_headers;
         "test_resource_without_remote_id_emits_no_headers"
         >:: test_resource_without_remote_id_emits_no_headers;
         "test_resources_preserve_valid_resource_order"
         >:: test_resources_preserve_valid_resource_order;
       ]
