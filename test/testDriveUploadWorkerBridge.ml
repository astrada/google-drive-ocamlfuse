open OUnit
open GapiMonad
open GapiMonad.SessionM.Infix
module Bridge = DriveUploadWorkerBridge

exception Upload_failed

let run_session = DriveTestSupport.run_session

let runtime ?(cache = DriveTestSupport.dummy_cache) () =
  DriveTestSupport.cache_only_runtime ~cache ()

let string_of_string_list values = String.concat "," values

let make_resource ?(id = 1L) ?(state = CacheData.Resource.State.ToUpload) path
    remote_id =
  let resource = Drive.create_resource path in
  {
    resource with
    id;
    remote_id = Some remote_id;
    name = Some (Filename.basename path);
    mime_type = Some "text/plain";
    size = Some 0L;
    trashed = Some false;
    version = Some 1L;
    can_edit = Some true;
    modified_time = Some 0.;
    created_time = Some 0.;
    viewed_by_me_time = Some 0.;
    state;
  }

module FakePorts = struct
  let resources = Hashtbl.create 32
  let trace = ref []
  let retry_resource = ref None
  let upload_failure = ref None

  let reset () =
    Hashtbl.reset resources;
    trace := [];
    retry_resource := None;
    upload_failure := None

  let record event = trace := !trace @ [ event ]

  let add_resource resource =
    Hashtbl.replace resources resource.CacheData.Resource.id resource

  let flush_memory_buffers resource =
    record ("flush:" ^ resource.CacheData.Resource.path)

  let upload resource =
    SessionM.return () >>= fun () ->
    record ("upload:" ^ resource.CacheData.Resource.path);
    match !upload_failure with
    | None -> SessionM.return ()
    | Some e -> Utils.raise_m e

  let try_with_default request =
    record "try_default";
    request

  let with_resource_retry upload_resource resource =
    record ("retry:" ^ resource.CacheData.Resource.path);
    let resource_for_retry = Option.default resource !retry_resource in
    upload_resource resource_for_retry

  let select_resource_with_id _cache resource_id =
    record (Printf.sprintf "select:%Ld" resource_id);
    try Some (Hashtbl.find resources resource_id) with Not_found -> None

  let run_request request =
    record "run_request";
    run_session request

  let log_missing_resource resource_id =
    record (Printf.sprintf "missing:%Ld" resource_id)
end

module UploadWorkerBridge = Bridge.Make (FakePorts)

let test_upload_resource_with_retry_flushes_and_wraps_upload () =
  FakePorts.reset ();
  let resource = make_resource "/file.txt" "rid-file" in
  let request = UploadWorkerBridge.upload_resource_with_retry resource in
  assert_equal ~printer:string_of_string_list
    [ "flush:/file.txt"; "retry:/file.txt"; "try_default" ]
    !FakePorts.trace;
  run_session request;
  assert_equal ~printer:string_of_string_list
    [ "flush:/file.txt"; "retry:/file.txt"; "try_default"; "upload:/file.txt" ]
    !FakePorts.trace

let test_retry_callback_uploads_retry_supplied_resource () =
  FakePorts.reset ();
  let original = make_resource "/original.txt" "rid-original" in
  let refreshed = make_resource ~id:2L "/refreshed.txt" "rid-refreshed" in
  FakePorts.retry_resource := Some refreshed;
  run_session (UploadWorkerBridge.upload_resource_with_retry original);
  assert_equal ~printer:string_of_string_list
    [
      "flush:/original.txt";
      "retry:/original.txt";
      "try_default";
      "upload:/refreshed.txt";
    ]
    !FakePorts.trace

let test_upload_resource_by_id_reloads_and_runs_request () =
  FakePorts.reset ();
  FakePorts.add_resource (make_resource ~id:42L "/queued.txt" "rid-queued");
  UploadWorkerBridge.upload_resource_by_id (runtime ()) 42L;
  assert_equal ~printer:string_of_string_list
    [
      "select:42";
      "flush:/queued.txt";
      "retry:/queued.txt";
      "try_default";
      "run_request";
      "upload:/queued.txt";
    ]
    !FakePorts.trace

let test_upload_resource_by_id_logs_missing_resource () =
  FakePorts.reset ();
  UploadWorkerBridge.upload_resource_by_id (runtime ()) 404L;
  assert_equal ~printer:string_of_string_list
    [ "select:404"; "missing:404" ]
    !FakePorts.trace

let test_upload_resource_by_id_propagates_upload_failure () =
  FakePorts.reset ();
  FakePorts.add_resource (make_resource ~id:42L "/queued.txt" "rid-queued");
  FakePorts.upload_failure := Some Upload_failed;
  assert_raises Upload_failed (fun () ->
      UploadWorkerBridge.upload_resource_by_id (runtime ()) 42L);
  assert_equal ~printer:string_of_string_list
    [
      "select:42";
      "flush:/queued.txt";
      "retry:/queued.txt";
      "try_default";
      "run_request";
      "upload:/queued.txt";
    ]
    !FakePorts.trace

let suite =
  "DriveUploadWorkerBridge tests"
  >::: [
         "test_upload_resource_with_retry_flushes_and_wraps_upload"
         >:: test_upload_resource_with_retry_flushes_and_wraps_upload;
         "test_retry_callback_uploads_retry_supplied_resource"
         >:: test_retry_callback_uploads_retry_supplied_resource;
         "test_upload_resource_by_id_reloads_and_runs_request"
         >:: test_upload_resource_by_id_reloads_and_runs_request;
         "test_upload_resource_by_id_logs_missing_resource"
         >:: test_upload_resource_by_id_logs_missing_resource;
         "test_upload_resource_by_id_propagates_upload_failure"
         >:: test_upload_resource_by_id_propagates_upload_failure;
       ]
