open OUnit
module RuntimeServices = DriveRuntimeServices

let make_config ?(async_upload_queue = false) ?(async_upload_threads = 10)
    ?(background_folder_fetching = false) () =
  {
    Config.default with
    async_upload_queue;
    async_upload_threads;
    background_folder_fetching;
  }

let runtime ?(cache = DriveTestSupport.dummy_cache) ?(config = make_config ())
    () =
  { RuntimeServices.cache; config }

module FakePorts = struct
  let trace = ref []
  let captured_upload_callback = ref None
  let captured_read_dir_callback = ref None

  let reset () =
    trace := [];
    captured_upload_callback := None;
    captured_read_dir_callback := None

  let record event = trace := !trace @ [ event ]
  let start_flush_db_thread cache = record ("flush:" ^ cache.CacheData.cache_dir)

  let start_async_upload_thread cache upload_threads upload_resource =
    captured_upload_callback := Some upload_resource;
    record
      (Printf.sprintf "async:%s:%d" cache.CacheData.cache_dir upload_threads)

  let start_folder_fetching_thread cache read_dir =
    captured_read_dir_callback := Some read_dir;
    record ("background:" ^ cache.CacheData.cache_dir)

  let upload_resource_by_id resource_id =
    record (Printf.sprintf "upload:%Ld" resource_id)

  let read_dir path =
    record ("read_dir:" ^ path);
    [ "ignored" ]
end

module Ops = RuntimeServices.Make (FakePorts)

let test_starts_only_flush_when_optional_services_are_disabled () =
  FakePorts.reset ();
  Ops.init_filesystem (runtime ());
  assert_equal [ "flush:/tmp" ] !FakePorts.trace;
  assert_equal None !FakePorts.captured_upload_callback;
  assert_equal None !FakePorts.captured_read_dir_callback

let test_starts_async_upload_with_configured_thread_count () =
  FakePorts.reset ();
  let config =
    make_config ~async_upload_queue:true ~async_upload_threads:4 ()
  in
  Ops.init_filesystem (runtime ~config ());
  assert_equal [ "flush:/tmp"; "async:/tmp:4" ] !FakePorts.trace;
  match !FakePorts.captured_upload_callback with
  | None -> assert_failure "expected upload callback"
  | Some upload ->
      upload 42L;
      assert_equal
        [ "flush:/tmp"; "async:/tmp:4"; "upload:42" ]
        !FakePorts.trace

let test_starts_background_fetching_with_read_dir_callback () =
  FakePorts.reset ();
  let config = make_config ~background_folder_fetching:true () in
  Ops.init_filesystem (runtime ~config ());
  assert_equal [ "flush:/tmp"; "background:/tmp" ] !FakePorts.trace;
  match !FakePorts.captured_read_dir_callback with
  | None -> assert_failure "expected read_dir callback"
  | Some read_dir ->
      read_dir "/folder";
      assert_equal
        [ "flush:/tmp"; "background:/tmp"; "read_dir:/folder" ]
        !FakePorts.trace

let test_starts_enabled_services_in_order () =
  FakePorts.reset ();
  let config =
    make_config ~async_upload_queue:true ~async_upload_threads:3
      ~background_folder_fetching:true ()
  in
  Ops.init_filesystem (runtime ~config ());
  assert_equal
    [ "flush:/tmp"; "async:/tmp:3"; "background:/tmp" ]
    !FakePorts.trace

let suite =
  "DriveRuntimeServices tests"
  >::: [
         "test_starts_only_flush_when_optional_services_are_disabled"
         >:: test_starts_only_flush_when_optional_services_are_disabled;
         "test_starts_async_upload_with_configured_thread_count"
         >:: test_starts_async_upload_with_configured_thread_count;
         "test_starts_background_fetching_with_read_dir_callback"
         >:: test_starts_background_fetching_with_read_dir_callback;
         "test_starts_enabled_services_in_order"
         >:: test_starts_enabled_services_in_order;
       ]
