open OUnit
open GapiMonad
open GapiMonad.SessionM.Infix
module File = GapiDriveV3Model.File

let session =
  {
    GapiConversation.Session.curl = GapiCurl.Initialized;
    config = GapiConfig.default;
    auth = GapiConversation.Session.NoAuth;
    cookies = [];
    etag = "";
  }

let run_session m = fst (m session)

let dummy_cache =
  {
    CacheData.cache_dir = "/tmp";
    db_path = "/tmp/test-cache.db";
    busy_timeout = 0;
    in_memory = true;
    autosaving_interval = 0;
  }

let default_runtime ?(config = Config.default) () =
  { DriveMetadataMutations.cache = dummy_cache; config }

let date_printer = GapiDate.to_string

module FakePorts = struct
  let resource = ref None
  let trace = ref []
  let remote_update_calls = ref []
  let update_file_in_cache_hook = ref None
  let update_file_times_calls = ref []

  let reset () =
    resource := None;
    trace := [];
    remote_update_calls := [];
    update_file_in_cache_hook := None;
    update_file_times_calls := []

  let record event = trace := !trace @ [ event ]
  let set_resource r = resource := Some r

  let build_resource_keys_header_from_resource resource =
    let remote_id = Option.default "" resource.CacheData.Resource.remote_id in
    record ("headers:" ^ remote_id);
    []

  let remote_update ~custom_headers:_ ~fileId file_patch =
    remote_update_calls := !remote_update_calls @ [ (fileId, file_patch) ];
    record ("remote_update:" ^ fileId);
    SessionM.return { file_patch with File.id = fileId }

  let update_remote_resource _runtime path ?update_file_in_cache
      do_remote_update =
    record ("update_remote_resource:" ^ path);
    update_file_in_cache_hook := update_file_in_cache;
    match !resource with
    | None -> assert_failure "expected fake resource"
    | Some resource ->
        do_remote_update resource >>= fun file_option ->
        (match file_option with
        | None -> record "update_remote_resource:none"
        | Some file -> record ("update_remote_resource:some:" ^ file.File.id));
        SessionM.return ()

  let update_file_times content_path atime mtime =
    update_file_times_calls :=
      !update_file_times_calls @ [ (content_path, atime, mtime) ];
    record (Printf.sprintf "utimes:%s:%f:%f" content_path atime mtime)
end

module MetadataMutations = DriveMetadataMutations.Make (FakePorts)

let make_resource ?(id = 1L) ?(remote_id = "rid-file") path =
  let resource = Drive.create_resource path in
  {
    resource with
    id;
    remote_id = Some remote_id;
    name = Some (Filename.basename path);
    mime_type = Some "text/plain";
    trashed = Some false;
    version = Some 1L;
    can_edit = Some true;
    modified_time = Some 0.;
    created_time = Some 0.;
    viewed_by_me_time = Some 0.;
  }

let last_remote_update () =
  match List.rev !FakePorts.remote_update_calls with
  | call :: _ -> call
  | [] -> assert_failure "expected remote update call"

let test_utime_patches_remote_mtime_and_exposes_local_hook () =
  FakePorts.reset ();
  FakePorts.set_resource (make_resource "/file.txt");
  let atime = 10.25 in
  let mtime = 20.5 in
  run_session
    (MetadataMutations.utime (default_runtime ()) "/file.txt" atime mtime);
  let file_id, file_patch = last_remote_update () in
  assert_equal "rid-file" file_id;
  assert_equal ~printer:date_printer (Netdate.create mtime)
    file_patch.File.modifiedTime;
  assert_equal [] file_patch.File.appProperties;
  assert_bool "expected update wrapper"
    (List.mem "update_remote_resource:/file.txt" !FakePorts.trace);
  assert_bool "expected resource-key header request"
    (List.mem "headers:rid-file" !FakePorts.trace);
  assert_bool "expected Some patched file"
    (List.mem "update_remote_resource:some:rid-file" !FakePorts.trace);
  let hook =
    match !FakePorts.update_file_in_cache_hook with
    | Some hook -> hook
    | None -> assert_failure "expected update_file_in_cache hook"
  in
  hook "/tmp/cache-file";
  assert_equal
    [ ("/tmp/cache-file", atime, mtime) ]
    !FakePorts.update_file_times_calls

let test_chmod_sends_mode_app_property_without_masking () =
  FakePorts.reset ();
  FakePorts.set_resource (make_resource "/file.txt");
  run_session
    (MetadataMutations.chmod (default_runtime ()) "/file.txt" 0o104755);
  let file_id, file_patch = last_remote_update () in
  assert_equal "rid-file" file_id;
  assert_equal
    [ CacheData.Resource.mode_to_app_property 0o104755 ]
    file_patch.File.appProperties;
  assert_bool "expected update wrapper"
    (List.mem "update_remote_resource:/file.txt" !FakePorts.trace);
  assert_bool "expected resource-key header request"
    (List.mem "headers:rid-file" !FakePorts.trace)

let test_chown_sends_uid_before_gid_when_both_present () =
  FakePorts.reset ();
  FakePorts.set_resource (make_resource "/file.txt");
  run_session
    (MetadataMutations.chown (default_runtime ()) "/file.txt" 1000 1001);
  let _, file_patch = last_remote_update () in
  assert_equal
    [
      CacheData.Resource.uid_to_app_property "1000";
      CacheData.Resource.gid_to_app_property "1001";
    ]
    file_patch.File.appProperties

let test_chown_omits_uid_for_minus_one () =
  FakePorts.reset ();
  FakePorts.set_resource (make_resource "/file.txt");
  run_session
    (MetadataMutations.chown (default_runtime ()) "/file.txt" (-1) 1001);
  let _, file_patch = last_remote_update () in
  assert_equal
    [ CacheData.Resource.gid_to_app_property "1001" ]
    file_patch.File.appProperties

let test_chown_omits_gid_for_minus_one () =
  FakePorts.reset ();
  FakePorts.set_resource (make_resource "/file.txt");
  run_session
    (MetadataMutations.chown (default_runtime ()) "/file.txt" 1000 (-1));
  let _, file_patch = last_remote_update () in
  assert_equal
    [ CacheData.Resource.uid_to_app_property "1000" ]
    file_patch.File.appProperties

let test_chown_omits_unsigned_32bit_all_ones () =
  FakePorts.reset ();
  FakePorts.set_resource (make_resource "/file.txt");
  if Sys.word_size >= 64 then (
    let unsigned_minus_one = Int64.to_int 4294967295L in
    run_session
      (MetadataMutations.chown (default_runtime ()) "/file.txt"
         unsigned_minus_one 1001);
    let _, file_patch = last_remote_update () in
    assert_equal
      [ CacheData.Resource.gid_to_app_property "1001" ]
      file_patch.File.appProperties)

let test_chown_with_both_sides_omitted_sends_empty_patch () =
  FakePorts.reset ();
  FakePorts.set_resource (make_resource "/file.txt");
  run_session
    (MetadataMutations.chown (default_runtime ()) "/file.txt" (-1) (-1));
  let _, file_patch = last_remote_update () in
  assert_equal [] file_patch.File.appProperties

let suite =
  "DriveMetadataMutations test"
  >::: [
         "test_utime_patches_remote_mtime_and_exposes_local_hook"
         >:: test_utime_patches_remote_mtime_and_exposes_local_hook;
         "test_chmod_sends_mode_app_property_without_masking"
         >:: test_chmod_sends_mode_app_property_without_masking;
         "test_chown_sends_uid_before_gid_when_both_present"
         >:: test_chown_sends_uid_before_gid_when_both_present;
         "test_chown_omits_uid_for_minus_one"
         >:: test_chown_omits_uid_for_minus_one;
         "test_chown_omits_gid_for_minus_one"
         >:: test_chown_omits_gid_for_minus_one;
         "test_chown_omits_unsigned_32bit_all_ones"
         >:: test_chown_omits_unsigned_32bit_all_ones;
         "test_chown_with_both_sides_omitted_sends_empty_patch"
         >:: test_chown_with_both_sides_omitted_sends_empty_patch;
       ]
