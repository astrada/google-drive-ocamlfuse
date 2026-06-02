open OUnit
open GapiMonad
module File = GapiDriveV3Model.File
module RemoteUpdates = DriveRemoteUpdates

let run_session = DriveTestSupport.run_session

let default_runtime ?(config = Config.default) () =
  DriveTestSupport.base_runtime ~config ()

let assert_before = DriveTestSupport.Trace.assert_before
let assert_no_event = DriveTestSupport.Trace.assert_no_event

type resource_response = Return of CacheData.Resource.t | Raise of exn

module FakePorts = struct
  let trace = ref []
  let resource_responses = ref []
  let existing_paths = ref []
  let update_from_file_calls = ref []
  let cached_updates = ref []

  let reset () =
    trace := [];
    resource_responses := [];
    existing_paths := [];
    update_from_file_calls := [];
    cached_updates := []

  let record event = trace := !trace @ [ event ]
  let set_resource_responses responses = resource_responses := responses
  let set_existing paths = existing_paths := paths

  let get_path_in_cache path config =
    let path_in_cache, trashed = Drive.get_path_in_cache path config in
    record
      (Printf.sprintf "get_path_in_cache:%s:%s:%b" path path_in_cache trashed);
    (path_in_cache, trashed)

  let get_resource path trashed =
    record (Printf.sprintf "get_resource:%s:%b" path trashed);
    match !resource_responses with
    | response :: rest -> (
        resource_responses := rest;
        match response with
        | Return resource -> SessionM.return resource
        | Raise e -> Utils.raise_m e)
    | [] -> assert_failure "expected fake resource response"

  let get_content_path _cache resource =
    let remote_id =
      Option.default "no-remote-id" resource.CacheData.Resource.remote_id
    in
    let path = "/cache/" ^ remote_id in
    record ("content_path:" ^ path);
    path

  let file_exists path =
    record ("file_exists:" ^ path);
    List.mem path !existing_paths

  let update_resource_from_file resource file =
    update_from_file_calls :=
      !update_from_file_calls
      @ [ (resource.CacheData.Resource.id, file.File.id) ];
    record ("update_from_file:" ^ file.File.id);
    {
      resource with
      CacheData.Resource.remote_id = Some file.File.id;
      name = Some file.File.name;
      mime_type = Some file.File.mimeType;
      size = Some file.File.size;
      version = Some file.File.version;
    }

  let update_cached_resource _cache resource =
    cached_updates := !cached_updates @ [ resource ];
    record
      (Printf.sprintf "update_cache:%s:%s" resource.CacheData.Resource.path
         (Option.default "" resource.CacheData.Resource.remote_id))
end

module UpdateOps = RemoteUpdates.Make (FakePorts)

let make_resource ?(id = 1L) ?(remote_id = Some "rid-file")
    ?(state = CacheData.Resource.State.Synchronized) ?(trashed = false) path =
  let resource = Drive.create_resource path in
  {
    resource with
    id;
    remote_id;
    name = Some (Filename.basename path);
    mime_type = Some "text/plain";
    size = Some 1L;
    trashed = Some trashed;
    version = Some 1L;
    can_edit = Some true;
    modified_time = Some 0.;
    created_time = Some 0.;
    viewed_by_me_time = Some 0.;
    state;
  }

let make_file ?(id = "rid-updated") ?(name = "file.txt")
    ?(mime_type = "text/plain") ?(size = 2L) ?(version = 2L) () =
  { File.empty with id; name; mimeType = mime_type; size; version }

let with_reset f =
  FakePorts.reset ();
  f ()

let remote_returns file resource =
  FakePorts.record ("remote_update:" ^ resource.CacheData.Resource.path);
  SessionM.return (Some file)

let remote_returns_none resource =
  FakePorts.record ("remote_update:" ^ resource.CacheData.Resource.path);
  SessionM.return None

let test_trash_path_lookup_uses_normalized_path () =
  with_reset (fun () ->
      let resource = make_resource ~trashed:true "/file.txt" in
      let file = make_file () in
      FakePorts.set_resource_responses [ Return resource ];
      run_session
        (UpdateOps.update_remote_resource (default_runtime ())
           "/.Trash/file.txt" (remote_returns file));
      assert_bool "expected normalized trash path"
        (List.mem "get_path_in_cache:/.Trash/file.txt:/file.txt:true"
           !FakePorts.trace);
      assert_bool "expected trashed resource lookup"
        (List.mem "get_resource:/file.txt:true" !FakePorts.trace))

let test_read_only_denies_after_path_normalization_before_lookup () =
  with_reset (fun () ->
      let config = { Config.default with read_only = true } in
      assert_raises RemoteUpdates.Permission_denied (fun () ->
          run_session
            (UpdateOps.update_remote_resource (default_runtime ~config ())
               "/.Trash/file.txt" (fun _ ->
                 assert_failure "unexpected remote update")));
      assert_equal
        [ "get_path_in_cache:/.Trash/file.txt:/file.txt:true" ]
        !FakePorts.trace)

let test_resource_lookup_failure_propagates_without_side_effects () =
  with_reset (fun () ->
      let failure = Failure "lookup failed" in
      FakePorts.set_resource_responses [ Raise failure ];
      assert_raises failure (fun () ->
          run_session
            (UpdateOps.update_remote_resource (default_runtime ()) "/file.txt"
               (fun _ -> assert_failure "unexpected remote update")));
      assert_equal
        [
          "get_path_in_cache:/file.txt:/file.txt:false";
          "get_resource:/file.txt:false";
        ]
        !FakePorts.trace;
      assert_equal [] !FakePorts.update_from_file_calls;
      assert_equal [] !FakePorts.cached_updates)

let test_remote_failure_propagates_without_save_purge_or_hook () =
  with_reset (fun () ->
      let failure = Failure "remote failed" in
      let resource = make_resource "/file.txt" in
      FakePorts.set_resource_responses [ Return resource ];
      assert_raises failure (fun () ->
          run_session
            (UpdateOps.update_remote_resource (default_runtime ()) "/file.txt"
               ~update_file_in_cache:(fun path ->
                 FakePorts.record ("hook:" ^ path))
               (fun resource ->
                 FakePorts.record
                   ("remote_update:" ^ resource.CacheData.Resource.path);
                 Utils.raise_m failure)));
      assert_equal
        [
          "get_path_in_cache:/file.txt:/file.txt:false";
          "get_resource:/file.txt:false";
          "remote_update:/file.txt";
        ]
        !FakePorts.trace;
      assert_equal [] !FakePorts.update_from_file_calls;
      assert_equal [] !FakePorts.cached_updates)

let test_default_save_updates_resource_and_cache () =
  with_reset (fun () ->
      let resource = make_resource "/file.txt" in
      let file = make_file ~id:"rid-new" () in
      FakePorts.set_resource_responses [ Return resource ];
      run_session
        (UpdateOps.update_remote_resource (default_runtime ()) "/file.txt"
           (remote_returns file));
      assert_equal [ (1L, "rid-new") ] !FakePorts.update_from_file_calls;
      assert_equal 1 (List.length !FakePorts.cached_updates);
      let updated_resource = List.hd !FakePorts.cached_updates in
      assert_equal (Some "rid-new")
        updated_resource.CacheData.Resource.remote_id;
      assert_before "update_from_file:rid-new" "update_cache:/file.txt:rid-new"
        !FakePorts.trace)

let test_custom_save_overrides_default_save () =
  with_reset (fun () ->
      let resource = make_resource "/file.txt" in
      let file = make_file ~id:"rid-custom" () in
      FakePorts.set_resource_responses [ Return resource ];
      let save_to_db _cache resource file =
        FakePorts.record
          (Printf.sprintf "custom_save:%s:%s" resource.CacheData.Resource.path
             file.File.id)
      in
      run_session
        (UpdateOps.update_remote_resource (default_runtime ()) "/file.txt"
           ~save_to_db (remote_returns file));
      assert_bool "expected custom save"
        (List.mem "custom_save:/file.txt:rid-custom" !FakePorts.trace);
      assert_no_event "update_from_file:" !FakePorts.trace;
      assert_no_event "update_cache:" !FakePorts.trace;
      assert_equal [] !FakePorts.update_from_file_calls;
      assert_equal [] !FakePorts.cached_updates)

let test_update_file_in_cache_runs_before_save_for_synchronized_existing_file ()
    =
  with_reset (fun () ->
      let resource = make_resource "/file.txt" in
      let file = make_file ~id:"rid-new" () in
      FakePorts.set_resource_responses [ Return resource ];
      FakePorts.set_existing [ "/cache/rid-file" ];
      run_session
        (UpdateOps.update_remote_resource (default_runtime ()) "/file.txt"
           ~update_file_in_cache:(fun path -> FakePorts.record ("hook:" ^ path))
           (remote_returns file));
      let trace = !FakePorts.trace in
      assert_bool "expected update hook" (List.mem "hook:/cache/rid-file" trace);
      assert_before "content_path:/cache/rid-file" "file_exists:/cache/rid-file"
        trace;
      assert_before "file_exists:/cache/rid-file" "hook:/cache/rid-file" trace;
      assert_before "hook:/cache/rid-file" "update_from_file:rid-new" trace)

let test_update_file_in_cache_skips_missing_content () =
  with_reset (fun () ->
      let resource = make_resource "/file.txt" in
      let file = make_file () in
      FakePorts.set_resource_responses [ Return resource ];
      run_session
        (UpdateOps.update_remote_resource (default_runtime ()) "/file.txt"
           ~update_file_in_cache:(fun path -> FakePorts.record ("hook:" ^ path))
           (remote_returns file));
      assert_bool "expected existence check"
        (List.mem "file_exists:/cache/rid-file" !FakePorts.trace);
      assert_no_event "hook:" !FakePorts.trace)

let test_update_file_in_cache_skips_non_synchronized_without_content_lookup () =
  [
    CacheData.Resource.State.ToDownload;
    CacheData.Resource.State.Downloading;
    CacheData.Resource.State.ToUpload;
    CacheData.Resource.State.Uploading;
    CacheData.Resource.State.NotFound;
  ]
  |> List.iter (fun state ->
      with_reset (fun () ->
          let resource = make_resource ~state "/file.txt" in
          let file = make_file () in
          FakePorts.set_resource_responses [ Return resource ];
          FakePorts.set_existing [ "/cache/rid-file" ];
          run_session
            (UpdateOps.update_remote_resource (default_runtime ()) "/file.txt"
               ~update_file_in_cache:(fun path ->
                 FakePorts.record ("hook:" ^ path))
               (remote_returns file));
          assert_no_event "content_path:" !FakePorts.trace;
          assert_no_event "file_exists:" !FakePorts.trace;
          assert_no_event "hook:" !FakePorts.trace))

let test_missing_update_hook_skips_content_lookup () =
  with_reset (fun () ->
      let resource = make_resource "/file.txt" in
      let file = make_file () in
      FakePorts.set_resource_responses [ Return resource ];
      FakePorts.set_existing [ "/cache/rid-file" ];
      run_session
        (UpdateOps.update_remote_resource (default_runtime ()) "/file.txt"
           (remote_returns file));
      assert_no_event "content_path:" !FakePorts.trace;
      assert_no_event "file_exists:" !FakePorts.trace)

let test_none_branch_uses_custom_purge_without_save_or_hook () =
  with_reset (fun () ->
      let resource = make_resource "/file.txt" in
      FakePorts.set_resource_responses [ Return resource ];
      let purge_cache _cache resource =
        FakePorts.record ("purge:" ^ resource.CacheData.Resource.path)
      in
      run_session
        (UpdateOps.update_remote_resource (default_runtime ()) "/file.txt"
           ~purge_cache
           ~update_file_in_cache:(fun path -> FakePorts.record ("hook:" ^ path))
           remote_returns_none);
      assert_bool "expected purge" (List.mem "purge:/file.txt" !FakePorts.trace);
      assert_no_event "content_path:" !FakePorts.trace;
      assert_no_event "update_from_file:" !FakePorts.trace;
      assert_no_event "update_cache:" !FakePorts.trace;
      assert_no_event "hook:" !FakePorts.trace)

let test_none_branch_default_purge_is_noop () =
  with_reset (fun () ->
      let resource = make_resource "/file.txt" in
      FakePorts.set_resource_responses [ Return resource ];
      run_session
        (UpdateOps.update_remote_resource (default_runtime ()) "/file.txt"
           remote_returns_none);
      assert_equal
        [
          "get_path_in_cache:/file.txt:/file.txt:false";
          "get_resource:/file.txt:false";
          "remote_update:/file.txt";
        ]
        !FakePorts.trace;
      assert_equal [] !FakePorts.update_from_file_calls;
      assert_equal [] !FakePorts.cached_updates)

let suite =
  "DriveRemoteUpdates test"
  >::: [
         "test_trash_path_lookup_uses_normalized_path"
         >:: test_trash_path_lookup_uses_normalized_path;
         "test_read_only_denies_after_path_normalization_before_lookup"
         >:: test_read_only_denies_after_path_normalization_before_lookup;
         "test_resource_lookup_failure_propagates_without_side_effects"
         >:: test_resource_lookup_failure_propagates_without_side_effects;
         "test_remote_failure_propagates_without_save_purge_or_hook"
         >:: test_remote_failure_propagates_without_save_purge_or_hook;
         "test_default_save_updates_resource_and_cache"
         >:: test_default_save_updates_resource_and_cache;
         "test_custom_save_overrides_default_save"
         >:: test_custom_save_overrides_default_save;
         "test_update_file_in_cache_runs_before_save_for_synchronized_existing_file"
         >:: test_update_file_in_cache_runs_before_save_for_synchronized_existing_file;
         "test_update_file_in_cache_skips_missing_content"
         >:: test_update_file_in_cache_skips_missing_content;
         "test_update_file_in_cache_skips_non_synchronized_without_content_lookup"
         >:: test_update_file_in_cache_skips_non_synchronized_without_content_lookup;
         "test_missing_update_hook_skips_content_lookup"
         >:: test_missing_update_hook_skips_content_lookup;
         "test_none_branch_uses_custom_purge_without_save_or_hook"
         >:: test_none_branch_uses_custom_purge_without_save_or_hook;
         "test_none_branch_default_purge_is_noop"
         >:: test_none_branch_default_purge_is_noop;
       ]
