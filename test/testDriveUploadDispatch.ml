open OUnit
open GapiMonad

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
  { DriveUploadDispatch.cache = dummy_cache; config }

let string_of_string_list values = String.concat "," values

module FakePorts = struct
  let resources = Hashtbl.create 32
  let trace = ref []

  let reset () =
    Hashtbl.reset resources;
    trace := []

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

  let find_by_id id =
    Hashtbl.to_seq_values resources
    |> List.of_seq
    |> List.find_opt (fun resource -> resource.CacheData.Resource.id = id)

  let replace_resource resource =
    let old_keys =
      Hashtbl.to_seq resources |> List.of_seq
      |> List.filter_map (fun (k, existing) ->
          if existing.CacheData.Resource.id = resource.CacheData.Resource.id
          then Some k
          else None)
    in
    List.iter (Hashtbl.remove resources) old_keys;
    add_resource resource

  let get_path_in_cache = Drive.get_path_in_cache

  let lookup_resource _cache path trashed =
    record (Printf.sprintf "lookup:%s:%b" path trashed);
    find_resource path trashed

  let update_cached_resource_state _cache state id =
    record
      (Printf.sprintf "update_state:%Ld:%s" id
         (CacheData.Resource.State.to_string state));
    match find_by_id id with
    | None -> ()
    | Some resource -> replace_resource { resource with state }

  let get_resource path trashed =
    record (Printf.sprintf "get_resource:%s:%b" path trashed);
    match find_resource path trashed with
    | Some resource -> SessionM.return resource
    | None -> Utils.raise_m Drive.File_not_found

  let flush_memory_buffers resource =
    record ("flush:" ^ resource.CacheData.Resource.path)

  let enqueue_async_upload _cache _config resource =
    record ("enqueue:" ^ resource.CacheData.Resource.path)

  let upload_now_with_retry resource =
    record ("upload_now:" ^ resource.CacheData.Resource.path);
    SessionM.return ()
end

module UploadDispatch = DriveUploadDispatch.Make (FakePorts)

let make_resource ?(id = 1L) ?(state = CacheData.Resource.State.Synchronized)
    ?(mime_type = "text/plain") path remote_id =
  let resource = Drive.create_resource path in
  {
    resource with
    id;
    remote_id = Some remote_id;
    name = Some (Filename.basename path);
    mime_type = Some mime_type;
    size = Some 0L;
    trashed = Some false;
    version = Some 1L;
    can_edit = Some true;
    modified_time = Some 0.;
    created_time = Some 0.;
    viewed_by_me_time = Some 0.;
    state;
  }

let test_start_uploading_if_dirty_flips_state_once () =
  FakePorts.reset ();
  FakePorts.add_resource
    (make_resource ~state:CacheData.Resource.State.ToUpload "/file.txt"
       "file-id");
  let runtime = default_runtime () in
  assert_equal true
    (UploadDispatch.start_uploading_if_dirty runtime "/file.txt");
  let resource = Option.get (FakePorts.find_resource "/file.txt" false) in
  assert_equal CacheData.Resource.State.Uploading
    resource.CacheData.Resource.state;
  assert_equal false
    (UploadDispatch.start_uploading_if_dirty runtime "/file.txt");
  assert_equal ~printer:string_of_string_list
    [
      "lookup:/file.txt:false";
      "update_state:1:Uploading";
      "lookup:/file.txt:false";
    ]
    !FakePorts.trace

let test_upload_if_dirty_uses_lookup_before_request_side_work () =
  FakePorts.reset ();
  FakePorts.add_resource
    (make_resource ~state:CacheData.Resource.State.ToUpload "/file.txt"
       "file-id");
  let runtime = default_runtime () in
  let upload_request = UploadDispatch.upload_if_dirty runtime "/file.txt" in
  assert_bool "expected queued upload request" (Option.is_some upload_request);
  assert_equal ~printer:string_of_string_list
    [
      "lookup:/file.txt:false";
      "update_state:1:Uploading";
      "get_resource:/file.txt:false";
    ]
    !FakePorts.trace;
  run_session (Option.get upload_request);
  assert_equal ~printer:string_of_string_list
    [
      "lookup:/file.txt:false";
      "update_state:1:Uploading";
      "get_resource:/file.txt:false";
      "upload_now:/file.txt";
    ]
    !FakePorts.trace

let test_upload_if_dirty_ignores_non_dirty_resources () =
  FakePorts.reset ();
  FakePorts.add_resource (make_resource "/file.txt" "file-id");
  let runtime = default_runtime () in
  let upload_request = UploadDispatch.upload_if_dirty runtime "/file.txt" in
  assert_bool "expected no upload request" (Option.is_none upload_request);
  assert_equal ~printer:string_of_string_list
    [ "lookup:/file.txt:false" ]
    !FakePorts.trace

let test_queue_upload_async_flushes_and_enqueues () =
  FakePorts.reset ();
  let config = { Config.default with async_upload_queue = true } in
  let runtime = default_runtime ~config () in
  let resource = make_resource "/file.txt" "file-id" in
  run_session (UploadDispatch.queue_upload runtime resource);
  assert_equal ~printer:string_of_string_list
    [ "flush:/file.txt"; "enqueue:/file.txt" ]
    !FakePorts.trace

let test_queue_upload_direct_calls_upload_now () =
  FakePorts.reset ();
  let config = { Config.default with async_upload_queue = false } in
  let runtime = default_runtime ~config () in
  let resource = make_resource "/file.txt" "file-id" in
  run_session (UploadDispatch.queue_upload runtime resource);
  assert_equal ~printer:string_of_string_list [ "upload_now:/file.txt" ]
    !FakePorts.trace

let suite =
  "DriveUploadDispatch test"
  >::: [
         "test_start_uploading_if_dirty_flips_state_once"
         >:: test_start_uploading_if_dirty_flips_state_once;
         "test_upload_if_dirty_uses_lookup_before_request_side_work"
         >:: test_upload_if_dirty_uses_lookup_before_request_side_work;
         "test_upload_if_dirty_ignores_non_dirty_resources"
         >:: test_upload_if_dirty_ignores_non_dirty_resources;
         "test_queue_upload_async_flushes_and_enqueues"
         >:: test_queue_upload_async_flushes_and_enqueues;
         "test_queue_upload_direct_calls_upload_now"
         >:: test_queue_upload_direct_calls_upload_now;
       ]
