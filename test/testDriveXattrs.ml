open OUnit
open GapiMonad
open GapiMonad.SessionM.Infix
module File = GapiDriveV3Model.File

let run_session = DriveTestSupport.run_session

let default_runtime ?(config = Config.default) () =
  DriveTestSupport.base_runtime ~config ()

let string_list_printer values = "[" ^ String.concat "; " values ^ "]"
let sort_strings = List.sort String.compare

module FakePorts = struct
  let resources = Hashtbl.create 32
  let trace = ref []
  let remote_update_calls = ref []

  let reset () =
    Hashtbl.reset resources;
    trace := [];
    remote_update_calls := []

  let record event = trace := !trace @ [ event ]
  let key path trashed = Printf.sprintf "%b:%s" trashed path

  let add_resource resource =
    let trashed = Option.default false resource.CacheData.Resource.trashed in
    Hashtbl.replace resources
      (key resource.CacheData.Resource.path trashed)
      resource

  let find_resource path trashed =
    try Some (Hashtbl.find resources (key path trashed))
    with Not_found -> None

  let max_attribute_length = 126
  let json_length = Drive.json_length

  let get_path_in_cache path config =
    let path_in_cache, trashed = Drive.get_path_in_cache path config in
    record
      (Printf.sprintf "get_path_in_cache:%s:%s:%b" path path_in_cache trashed);
    (path_in_cache, trashed)

  let get_resource path trashed =
    record (Printf.sprintf "get_resource:%s:%b" path trashed);
    match find_resource path trashed with
    | Some resource -> SessionM.return resource
    | None -> Utils.raise_m Drive.File_not_found

  let build_resource_keys_header_from_resource resource =
    let remote_id = Option.default "" resource.CacheData.Resource.remote_id in
    record ("headers:" ^ remote_id);
    Drive.build_resource_keys_header_from_resource resource

  let remote_update ~custom_headers:_ ~fileId file_patch =
    remote_update_calls :=
      !remote_update_calls @ [ (fileId, file_patch.File.appProperties) ];
    record ("remote_update:" ^ fileId);
    SessionM.return { file_patch with File.id = fileId }

  let update_remote_resource runtime path do_remote_update =
    record ("update_remote_resource:" ^ path);
    let path_in_cache, trashed =
      get_path_in_cache path runtime.DriveXattrs.config
    in
    get_resource path_in_cache trashed >>= fun resource ->
    do_remote_update resource >>= fun file_option ->
    (match file_option with
    | None -> record "update_remote_resource:none"
    | Some file -> record ("update_remote_resource:some:" ^ file.File.id));
    SessionM.return ()
end

module Xattrs = DriveXattrs.Make (FakePorts)

let cached_xattrs xattrs =
  xattrs
  |> List.map (fun (name, value) -> ("x-" ^ name, value))
  |> CacheData.Resource.render_xattrs

let make_resource ?(id = 1L) ?(remote_id = "rid-file") ?(trashed = false)
    ?(xattrs = []) path =
  let resource = DriveTestSupport.make_resource ~id ~remote_id ~trashed path in
  { resource with xattrs = cached_xattrs xattrs }

let last_remote_update () =
  match List.rev !FakePorts.remote_update_calls with
  | call :: _ -> call
  | [] -> assert_failure "expected remote update call"

let test_get_xattr_returns_existing_value () =
  FakePorts.reset ();
  FakePorts.add_resource
    (make_resource ~xattrs:[ ("user.color", "blue") ] "/file.txt");
  let value =
    run_session (Xattrs.get_xattr (default_runtime ()) "/file.txt" "user.color")
  in
  assert_equal "blue" value

let test_get_xattr_raises_for_missing_name () =
  FakePorts.reset ();
  FakePorts.add_resource
    (make_resource ~xattrs:[ ("user.color", "blue") ] "/file.txt");
  assert_raises DriveXattrs.No_attribute (fun () ->
      run_session
        (Xattrs.get_xattr (default_runtime ()) "/file.txt" "user.missing"))

let test_list_xattr_returns_names_only () =
  FakePorts.reset ();
  FakePorts.add_resource
    (make_resource
       ~xattrs:[ ("user.color", "blue"); ("user.owner", "alice") ]
       "/file.txt");
  let names =
    run_session (Xattrs.list_xattr (default_runtime ()) "/file.txt")
    |> sort_strings
  in
  assert_equal ~printer:string_list_printer [ "user.color"; "user.owner" ] names

let test_read_uses_normalized_trash_path_for_lookup () =
  FakePorts.reset ();
  FakePorts.add_resource
    (make_resource ~trashed:true ~xattrs:[ ("user.color", "blue") ] "/file.txt");
  let value =
    run_session
      (Xattrs.get_xattr (default_runtime ()) "/.Trash/file.txt" "user.color")
  in
  assert_equal "blue" value;
  assert_bool "expected normalized trashed lookup"
    (List.mem "get_resource:/file.txt:true" !FakePorts.trace)

let test_set_xattr_auto_sends_app_property_patch () =
  FakePorts.reset ();
  FakePorts.add_resource (make_resource "/file.txt");
  run_session
    (Xattrs.set_xattr (default_runtime ()) "/file.txt" "user.color" "blue"
       Fuse.AUTO);
  let file_id, app_properties = last_remote_update () in
  assert_equal "rid-file" file_id;
  assert_equal [ ("x-user.color", "blue") ] app_properties;
  assert_bool "expected update wrapper"
    (List.mem "update_remote_resource:/file.txt" !FakePorts.trace);
  assert_bool "expected resource-key header request"
    (List.mem "headers:rid-file" !FakePorts.trace)

let test_set_xattr_create_raises_when_present () =
  FakePorts.reset ();
  FakePorts.add_resource
    (make_resource ~xattrs:[ ("user.color", "blue") ] "/file.txt");
  assert_raises DriveXattrs.Existing_attribute (fun () ->
      run_session
        (Xattrs.set_xattr (default_runtime ()) "/file.txt" "user.color" "red"
           Fuse.CREATE));
  assert_equal [] !FakePorts.remote_update_calls

let test_set_xattr_create_succeeds_when_absent () =
  FakePorts.reset ();
  FakePorts.add_resource (make_resource "/file.txt");
  run_session
    (Xattrs.set_xattr (default_runtime ()) "/file.txt" "user.color" "blue"
       Fuse.CREATE);
  let _, app_properties = last_remote_update () in
  assert_equal [ ("x-user.color", "blue") ] app_properties

let test_set_xattr_replace_raises_when_absent () =
  FakePorts.reset ();
  FakePorts.add_resource (make_resource "/file.txt");
  assert_raises DriveXattrs.No_attribute (fun () ->
      run_session
        (Xattrs.set_xattr (default_runtime ()) "/file.txt" "user.color" "blue"
           Fuse.REPLACE));
  assert_equal [] !FakePorts.remote_update_calls

let test_set_xattr_replace_succeeds_when_present () =
  FakePorts.reset ();
  FakePorts.add_resource
    (make_resource ~xattrs:[ ("user.color", "blue") ] "/file.txt");
  run_session
    (Xattrs.set_xattr (default_runtime ()) "/file.txt" "user.color" "red"
       Fuse.REPLACE);
  let _, app_properties = last_remote_update () in
  assert_equal [ ("x-user.color", "red") ] app_properties

let test_set_xattr_rejects_over_limit_escaped_length () =
  FakePorts.reset ();
  FakePorts.add_resource (make_resource "/file.txt");
  let value = String.make 70 '"' in
  assert_raises DriveXattrs.Invalid_operation (fun () ->
      run_session
        (Xattrs.set_xattr (default_runtime ()) "/file.txt" "a" value Fuse.AUTO));
  assert_equal [] !FakePorts.remote_update_calls

let test_remove_xattr_sends_no_value_app_property_patch () =
  FakePorts.reset ();
  FakePorts.add_resource
    (make_resource ~xattrs:[ ("user.color", "blue") ] "/file.txt");
  run_session
    (Xattrs.remove_xattr (default_runtime ()) "/file.txt" "user.color");
  let file_id, app_properties = last_remote_update () in
  assert_equal "rid-file" file_id;
  assert_equal [ ("x-user.color", "") ] app_properties

let test_remove_xattr_raises_for_missing_name () =
  FakePorts.reset ();
  FakePorts.add_resource (make_resource "/file.txt");
  assert_raises DriveXattrs.No_attribute (fun () ->
      run_session
        (Xattrs.remove_xattr (default_runtime ()) "/file.txt" "user.color"));
  assert_equal [] !FakePorts.remote_update_calls

let suite =
  "DriveXattrs test"
  >::: [
         "test_get_xattr_returns_existing_value"
         >:: test_get_xattr_returns_existing_value;
         "test_get_xattr_raises_for_missing_name"
         >:: test_get_xattr_raises_for_missing_name;
         "test_list_xattr_returns_names_only"
         >:: test_list_xattr_returns_names_only;
         "test_read_uses_normalized_trash_path_for_lookup"
         >:: test_read_uses_normalized_trash_path_for_lookup;
         "test_set_xattr_auto_sends_app_property_patch"
         >:: test_set_xattr_auto_sends_app_property_patch;
         "test_set_xattr_create_raises_when_present"
         >:: test_set_xattr_create_raises_when_present;
         "test_set_xattr_create_succeeds_when_absent"
         >:: test_set_xattr_create_succeeds_when_absent;
         "test_set_xattr_replace_raises_when_absent"
         >:: test_set_xattr_replace_raises_when_absent;
         "test_set_xattr_replace_succeeds_when_present"
         >:: test_set_xattr_replace_succeeds_when_present;
         "test_set_xattr_rejects_over_limit_escaped_length"
         >:: test_set_xattr_rejects_over_limit_escaped_length;
         "test_remove_xattr_sends_no_value_app_property_patch"
         >:: test_remove_xattr_sends_no_value_app_property_patch;
         "test_remove_xattr_raises_for_missing_name"
         >:: test_remove_xattr_raises_for_missing_name;
       ]
