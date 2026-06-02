open OUnit
open GapiMonad
module Reads = DriveReads

let run_session = DriveTestSupport.run_session

let default_runtime ?(config = Config.default) () =
  DriveTestSupport.base_runtime ~config ()

let buffer length =
  Bigarray.Array1.create Bigarray.char Bigarray.c_layout length

let index_of value values =
  let rec loop i = function
    | [] -> raise Not_found
    | x :: xs -> if x = value then i else loop (i + 1) xs
  in
  loop 0 values

module FakePorts = struct
  let resource_responses = ref []
  let trace = ref []
  let read_local_file_result = ref 0
  let read_ahead_count = ref 2

  let reset () =
    resource_responses := [];
    trace := [];
    read_local_file_result := 0;
    read_ahead_count := 2

  let record event = trace := !trace @ [ event ]
  let set_resources resources = resource_responses := resources

  let get_path_in_cache path config =
    let path_in_cache, trashed = Drive.get_path_in_cache path config in
    record
      (Printf.sprintf "get_path_in_cache:%s:%s:%b" path path_in_cache trashed);
    (path_in_cache, trashed)

  let get_resource path trashed =
    record (Printf.sprintf "get_resource:%s:%b" path trashed);
    match !resource_responses with
    | resource :: rest ->
        resource_responses := rest;
        SessionM.return resource
    | [] -> assert_failure "expected fake resource"

  let stream_resource offset buf resource =
    record
      (Printf.sprintf "stream_direct:%s:%Ld:%d" resource.CacheData.Resource.path
         offset (Bigarray.Array1.dim buf));
    SessionM.return ()

  let stream_resource_to_memory_buffer offset buf resource =
    record
      (Printf.sprintf "stream_memory:%s:%Ld:%d" resource.CacheData.Resource.path
         offset (Bigarray.Array1.dim buf));
    SessionM.return ()

  let stream_resource_to_read_ahead_buffers offset resource =
    record
      (Printf.sprintf "read_ahead_build:%s:%Ld" resource.CacheData.Resource.path
         offset);
    let rec build i requests =
      if i = !read_ahead_count then List.rev requests
      else
        let request =
          record (Printf.sprintf "read_ahead_request_created:%d" i);
          SessionM.return ()
        in
        build (i + 1) (request :: requests)
    in
    SessionM.return (build 0 [])

  let flush_memory_buffers resource =
    record ("flush:" ^ resource.CacheData.Resource.path)

  let ensure_local_content resource =
    record ("ensure:" ^ resource.CacheData.Resource.path);
    SessionM.return ("/tmp/" ^ Option.get resource.CacheData.Resource.remote_id)

  let read_local_file content_path buf offset =
    record
      (Printf.sprintf "read_local:%s:%Ld:%d" content_path offset
         (Bigarray.Array1.dim buf));
    !read_local_file_result

  let enqueue_async_request _request = record "enqueue"
end

module ReadOps = DriveReads.Make (FakePorts)

let make_resource ?(id = 1L) ?(remote_id = "rid-file")
    ?(state = CacheData.Resource.State.Synchronized) ?(size = 0L)
    ?(mime_type = "text/plain") ?(trashed = false) path =
  DriveTestSupport.make_resource ~id ~remote_id ~state ~size ~mime_type ~trashed
    path

let streaming_config ?(memory_buffer_size = 0) ?(read_ahead_buffers = 0) () =
  {
    Config.default with
    Config.stream_large_files = true;
    large_file_threshold_mb = 1;
    memory_buffer_size;
    read_ahead_buffers;
  }

let large_streaming_resource ?(state = CacheData.Resource.State.ToDownload) path
    =
  make_resource ~state ~size:2097152L path

let test_direct_streaming_returns_buffer_length_without_local_work () =
  FakePorts.reset ();
  FakePorts.set_resources
    [
      large_streaming_resource "/file.txt"; large_streaming_resource "/file.txt";
    ];
  let config =
    streaming_config ~memory_buffer_size:0 ~read_ahead_buffers:2 ()
  in
  let buf = buffer 5 in
  let bytes =
    run_session (ReadOps.read (default_runtime ~config ()) "/file.txt" buf 3L)
  in
  assert_equal 5 bytes;
  assert_bool "expected normalized foreground lookup"
    (List.mem "get_resource:/file.txt:false" !FakePorts.trace);
  assert_bool "expected direct streaming"
    (List.mem "stream_direct:/file.txt:3:5" !FakePorts.trace);
  assert_equal 2
    (List.length
       (List.filter (( = ) "get_resource:/file.txt:false") !FakePorts.trace));
  assert_bool "unexpected memory streaming"
    (not
       (List.exists
          (fun event -> event = "stream_memory:/file.txt:3:5")
          !FakePorts.trace));
  assert_bool "unexpected read-ahead build"
    (not
       (List.exists
          (fun event -> String.starts_with ~prefix:"read_ahead_build:" event)
          !FakePorts.trace));
  assert_bool "unexpected flush"
    (not
       (List.exists (fun event -> event = "flush:/file.txt") !FakePorts.trace));
  assert_bool "unexpected local materialization"
    (not
       (List.exists (fun event -> event = "ensure:/file.txt") !FakePorts.trace));
  assert_bool "unexpected read-ahead enqueue"
    (not (List.exists (fun event -> event = "enqueue") !FakePorts.trace))

let test_memory_streaming_builds_and_enqueues_read_ahead_after_foreground () =
  FakePorts.reset ();
  FakePorts.set_resources
    [
      large_streaming_resource "/file.txt"; large_streaming_resource "/file.txt";
    ];
  FakePorts.read_ahead_count := 2;
  let config =
    streaming_config ~memory_buffer_size:1048576 ~read_ahead_buffers:2 ()
  in
  let buf = buffer 7 in
  let bytes =
    run_session (ReadOps.read (default_runtime ~config ()) "/file.txt" buf 4L)
  in
  assert_equal 7 bytes;
  let foreground_index =
    index_of "stream_memory:/file.txt:4:7" !FakePorts.trace
  in
  let read_ahead_index =
    index_of "read_ahead_build:/file.txt:4" !FakePorts.trace
  in
  let enqueue_index = index_of "enqueue" !FakePorts.trace in
  assert_bool "expected read-ahead after foreground"
    (foreground_index < read_ahead_index);
  assert_bool "expected enqueue after read-ahead build"
    (read_ahead_index < enqueue_index);
  assert_equal 2
    (List.length
       (List.filter (( = ) "get_resource:/file.txt:false") !FakePorts.trace));
  assert_equal 2 (List.length (List.filter (( = ) "enqueue") !FakePorts.trace))

let test_memory_streaming_skips_read_ahead_when_disabled () =
  FakePorts.reset ();
  FakePorts.set_resources [ large_streaming_resource "/file.txt" ];
  let config =
    streaming_config ~memory_buffer_size:1048576 ~read_ahead_buffers:0 ()
  in
  let bytes =
    run_session
      (ReadOps.read (default_runtime ~config ()) "/file.txt" (buffer 7) 4L)
  in
  assert_equal 7 bytes;
  assert_bool "expected memory streaming"
    (List.mem "stream_memory:/file.txt:4:7" !FakePorts.trace);
  assert_equal 1
    (List.length
       (List.filter (( = ) "get_resource:/file.txt:false") !FakePorts.trace));
  assert_bool "unexpected read-ahead build"
    (not
       (List.exists
          (fun event -> String.starts_with ~prefix:"read_ahead_build:" event)
          !FakePorts.trace));
  assert_bool "unexpected enqueue" (not (List.mem "enqueue" !FakePorts.trace))

let test_local_file_read_flushes_materializes_and_uses_local_count () =
  FakePorts.reset ();
  FakePorts.set_resources
    [ make_resource "/file.txt"; make_resource "/file.txt" ];
  FakePorts.read_local_file_result := 3;
  let bytes =
    run_session (ReadOps.read (default_runtime ()) "/file.txt" (buffer 9) 6L)
  in
  assert_equal 3 bytes;
  let flush_index = index_of "flush:/file.txt" !FakePorts.trace in
  let ensure_index = index_of "ensure:/file.txt" !FakePorts.trace in
  let read_index = index_of "read_local:/tmp/rid-file:6:9" !FakePorts.trace in
  assert_bool "expected flush before materialization"
    (flush_index < ensure_index);
  assert_bool "expected materialization before local read"
    (ensure_index < read_index);
  assert_bool "unexpected streaming"
    (not
       (List.exists
          (fun event -> String.starts_with ~prefix:"stream_" event)
          !FakePorts.trace));
  assert_bool "unexpected read-ahead enqueue"
    (not (List.mem "enqueue" !FakePorts.trace))

let test_read_uses_normalized_trash_path () =
  FakePorts.reset ();
  FakePorts.set_resources
    [
      make_resource ~trashed:true "/file.txt";
      make_resource ~trashed:true "/file.txt";
    ];
  FakePorts.read_local_file_result := 1;
  let bytes =
    run_session
      (ReadOps.read (default_runtime ()) "/.Trash/file.txt" (buffer 2) 0L)
  in
  assert_equal 1 bytes;
  assert_bool "expected trashed cache path lookup"
    (List.mem "get_resource:/file.txt:true" !FakePorts.trace)

let test_read_ahead_skips_when_second_lookup_no_longer_streams () =
  FakePorts.reset ();
  FakePorts.set_resources
    [
      large_streaming_resource "/file.txt";
      large_streaming_resource ~state:CacheData.Resource.State.Synchronized
        "/file.txt";
    ];
  let config =
    streaming_config ~memory_buffer_size:1048576 ~read_ahead_buffers:2 ()
  in
  let bytes =
    run_session
      (ReadOps.read (default_runtime ~config ()) "/file.txt" (buffer 4) 0L)
  in
  assert_equal 4 bytes;
  assert_bool "expected foreground memory stream"
    (List.mem "stream_memory:/file.txt:0:4" !FakePorts.trace);
  assert_bool "unexpected read-ahead build"
    (not
       (List.exists
          (fun event -> String.starts_with ~prefix:"read_ahead_build:" event)
          !FakePorts.trace));
  assert_bool "unexpected enqueue" (not (List.mem "enqueue" !FakePorts.trace))

let suite =
  "DriveReads test"
  >::: [
         "test_direct_streaming_returns_buffer_length_without_local_work"
         >:: test_direct_streaming_returns_buffer_length_without_local_work;
         "test_memory_streaming_builds_and_enqueues_read_ahead_after_foreground"
         >:: test_memory_streaming_builds_and_enqueues_read_ahead_after_foreground;
         "test_memory_streaming_skips_read_ahead_when_disabled"
         >:: test_memory_streaming_skips_read_ahead_when_disabled;
         "test_local_file_read_flushes_materializes_and_uses_local_count"
         >:: test_local_file_read_flushes_materializes_and_uses_local_count;
         "test_read_uses_normalized_trash_path"
         >:: test_read_uses_normalized_trash_path;
         "test_read_ahead_skips_when_second_lookup_no_longer_streams"
         >:: test_read_ahead_skips_when_second_lookup_no_longer_streams;
       ]
