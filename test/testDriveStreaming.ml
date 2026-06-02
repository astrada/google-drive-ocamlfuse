open OUnit
open GapiMonad
open GapiMonad.SessionM.Infix
module File = GapiDriveV3Model.File
module Streaming = DriveStreaming

exception Abusive_download

let run_session = DriveTestSupport.run_session

let buffer length =
  Bigarray.Array1.create Bigarray.char Bigarray.c_layout length

let memory_buffers () = Buffering.MemoryBuffers.create 8 2

let default_runtime ?(config = Config.default) ?buffer_eviction_thread () =
  {
    Streaming.config;
    memory_buffers = memory_buffers ();
    buffer_eviction_thread;
  }

let media_download ?(range_spec = "") () =
  let destination = GapiMediaResource.ArrayBuffer (buffer 1) in
  { GapiMediaResource.destination; range_spec }

let index_of = DriveTestSupport.Trace.index_of
let assert_before = DriveTestSupport.Trace.assert_before

let header_values headers =
  headers
  |> List.map (function
    | GapiCore.Header.KeyValueHeader (_name, value) -> value
    | _ -> "<non-key-value-header>")
  |> String.concat ";"

module FakePorts = struct
  let trace = ref []
  let media_failure = ref None

  let reset () =
    trace := [];
    media_failure := None

  let record event = trace := !trace @ [ event ]

  let build_resource_keys_header_from_resource resource =
    record ("headers:" ^ resource.CacheData.Resource.path);
    DriveResourceKeys.build_resource_keys_header_from_resource resource

  let get_media ~acknowledge_abuse ~media_download ~custom_headers ~file_id =
    SessionM.return () >>= fun () ->
    record
      (Printf.sprintf "get_media:%s:%b:%s:%s" file_id acknowledge_abuse
         media_download.GapiMediaResource.range_spec
         (header_values custom_headers));
    match (!media_failure, acknowledge_abuse) with
    | Some e, false -> Utils.raise_m e
    | _ -> SessionM.return { File.empty with File.id = file_id }

  let match_service_error reason e =
    record (Printf.sprintf "match:%s:%s" reason (Printexc.to_string e));
    match (reason, e) with
    | "cannotDownloadAbusiveFile", Abusive_download -> true
    | _ -> false

  let handle_default_exceptions e =
    record ("handle:" ^ Printexc.to_string e);
    Utils.raise_m e

  let with_retry_default request =
    record "retry_default";
    request

  let create_eviction_thread _memory_buffers =
    record "create_eviction";
    Thread.self ()

  let set_buffer_eviction_thread _thread = record "set_eviction"

  let read_block remote_id offset size fill ?dest_arr _memory_buffers =
    let dest_length =
      dest_arr |> Option.map Bigarray.Array1.dim |> Option.default (-1)
    in
    record
      (Printf.sprintf "read_block:%s:%Ld:%Ld:%d" remote_id offset size
         dest_length);
    fill 8L (buffer 3)

  let read_ahead count remote_id offset size fill _memory_buffers =
    record
      (Printf.sprintf "read_ahead:%d:%s:%Ld:%Ld" count remote_id offset size);
    let request i =
      SessionM.return () >>= fun () ->
      record (Printf.sprintf "read_ahead_request_run:%d" i);
      fill (Int64.add offset (Int64.of_int (i * 10))) (buffer 2)
    in
    SessionM.return [ request 0; request 1 ]

  let with_resource_retry resource request =
    SessionM.return () >>= fun () ->
    record ("retry:" ^ resource.CacheData.Resource.path);
    request
end

module StreamingOps = Streaming.Make (FakePorts)

let make_resource ?(id = 1L) ?(remote_id = "rid-file") ?resource_key
    ?(size = 100L) path =
  let resource =
    DriveResourceMapping.create_resource ~now:(fun () -> 0.) path
  in
  {
    resource with
    id;
    remote_id = Some remote_id;
    resource_key;
    name = Some (Filename.basename path);
    size = Some size;
    mime_type = Some "text/plain";
    version = Some 1L;
    modified_time = Some 0.;
    created_time = Some 0.;
    viewed_by_me_time = Some 0.;
  }

let assert_raises_failure message f =
  try
    f ();
    assert_failure ("expected Failure " ^ message)
  with Failure actual -> assert_equal message actual

let test_download_media_uses_resource_headers_and_file_id () =
  FakePorts.reset ();
  let resource =
    make_resource ~remote_id:"rid-media" ~resource_key:"key-media" "/file.txt"
  in
  let file =
    run_session
      (StreamingOps.download_media (default_runtime ()) (media_download ())
         resource)
  in
  assert_equal "rid-media" file.File.id;
  assert_equal
    [ "headers:/file.txt"; "get_media:rid-media:false::rid-media/key-media" ]
    !FakePorts.trace

let test_download_media_retries_abusive_file_when_acknowledged () =
  FakePorts.reset ();
  FakePorts.media_failure := Some Abusive_download;
  let config = { Config.default with Config.acknowledge_abuse = true } in
  let resource = make_resource "/file.txt" in
  ignore
    (run_session
       (StreamingOps.download_media
          (default_runtime ~config ())
          (media_download ()) resource));
  assert_before "get_media:rid-file:false::" "retry_default" !FakePorts.trace;
  assert_before "retry_default" "get_media:rid-file:true::" !FakePorts.trace;
  assert_bool "expected abusive error match"
    (List.exists
       (fun event ->
         String.starts_with ~prefix:"match:cannotDownloadAbusiveFile:" event)
       !FakePorts.trace)

let test_download_media_delegates_non_retryable_error () =
  FakePorts.reset ();
  FakePorts.media_failure := Some (Failure "network");
  let config = { Config.default with Config.acknowledge_abuse = true } in
  let resource = make_resource "/file.txt" in
  assert_raises_failure "network" (fun () ->
      ignore
        (run_session
           (StreamingOps.download_media
              (default_runtime ~config ())
              (media_download ()) resource)));
  assert_bool "expected default handler"
    (List.mem "handle:Failure(\"network\")" !FakePorts.trace);
  assert_bool "unexpected abuse retry"
    (not (List.mem "get_media:rid-file:true::" !FakePorts.trace))

let test_stream_resource_builds_byte_range_download () =
  FakePorts.reset ();
  let resource = make_resource "/file.txt" in
  let buf = buffer 4 in
  run_session
    (StreamingOps.stream_resource (default_runtime ()) 10L buf resource);
  let expected_range =
    GapiMediaResource.generate_range_spec [ (Some 10L, Some 13L) ]
  in
  assert_bool "expected ranged media download"
    (List.mem
       (Printf.sprintf "get_media:rid-file:false:%s:" expected_range)
       !FakePorts.trace)

let test_start_buffer_eviction_thread_skips_when_streaming_disabled () =
  FakePorts.reset ();
  StreamingOps.start_buffer_eviction_thread (default_runtime ());
  assert_equal [] !FakePorts.trace

let test_start_buffer_eviction_thread_skips_when_thread_exists () =
  FakePorts.reset ();
  let config = { Config.default with Config.stream_large_files = true } in
  StreamingOps.start_buffer_eviction_thread
    (default_runtime ~config ~buffer_eviction_thread:(Thread.self ()) ());
  assert_equal [] !FakePorts.trace

let test_start_buffer_eviction_thread_creates_and_stores_thread () =
  FakePorts.reset ();
  let config = { Config.default with Config.stream_large_files = true } in
  StreamingOps.start_buffer_eviction_thread (default_runtime ~config ());
  assert_equal [ "create_eviction"; "set_eviction" ] !FakePorts.trace

let test_memory_buffer_streaming_reads_block_and_streams_callback () =
  FakePorts.reset ();
  let config = { Config.default with Config.stream_large_files = true } in
  let resource = make_resource "/file.txt" in
  let dest = buffer 5 in
  run_session
    (StreamingOps.stream_resource_to_memory_buffer
       (default_runtime ~config ())
       4L dest resource);
  let expected_range =
    GapiMediaResource.generate_range_spec [ (Some 8L, Some 10L) ]
  in
  assert_before "create_eviction" "read_block:rid-file:4:100:5" !FakePorts.trace;
  assert_before "read_block:rid-file:4:100:5"
    (Printf.sprintf "get_media:rid-file:false:%s:" expected_range)
    !FakePorts.trace

let test_read_ahead_streaming_wraps_deferred_requests_with_retry () =
  FakePorts.reset ();
  let config = { Config.default with Config.read_ahead_buffers = 2 } in
  let resource = make_resource "/file.txt" in
  let requests =
    run_session
      (StreamingOps.stream_resource_to_read_ahead_buffers
         (default_runtime ~config ())
         12L resource)
  in
  assert_equal 2 (List.length requests);
  assert_bool "retry should be deferred"
    (not (List.mem "retry:/file.txt" !FakePorts.trace));
  run_session (List.hd requests);
  let expected_range =
    GapiMediaResource.generate_range_spec [ (Some 12L, Some 13L) ]
  in
  assert_before "read_ahead:2:rid-file:12:100" "retry:/file.txt"
    !FakePorts.trace;
  assert_before "retry:/file.txt" "read_ahead_request_run:0" !FakePorts.trace;
  assert_before "read_ahead_request_run:0"
    (Printf.sprintf "get_media:rid-file:false:%s:" expected_range)
    !FakePorts.trace

let suite =
  "DriveStreaming tests"
  >::: [
         "test_download_media_uses_resource_headers_and_file_id"
         >:: test_download_media_uses_resource_headers_and_file_id;
         "test_download_media_retries_abusive_file_when_acknowledged"
         >:: test_download_media_retries_abusive_file_when_acknowledged;
         "test_download_media_delegates_non_retryable_error"
         >:: test_download_media_delegates_non_retryable_error;
         "test_stream_resource_builds_byte_range_download"
         >:: test_stream_resource_builds_byte_range_download;
         "test_start_buffer_eviction_thread_skips_when_streaming_disabled"
         >:: test_start_buffer_eviction_thread_skips_when_streaming_disabled;
         "test_start_buffer_eviction_thread_skips_when_thread_exists"
         >:: test_start_buffer_eviction_thread_skips_when_thread_exists;
         "test_start_buffer_eviction_thread_creates_and_stores_thread"
         >:: test_start_buffer_eviction_thread_creates_and_stores_thread;
         "test_memory_buffer_streaming_reads_block_and_streams_callback"
         >:: test_memory_buffer_streaming_reads_block_and_streams_callback;
         "test_read_ahead_streaming_wraps_deferred_requests_with_retry"
         >:: test_read_ahead_streaming_wraps_deferred_requests_with_retry;
       ]
