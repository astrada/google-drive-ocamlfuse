open OUnit
open GapiMonad
module Change = GapiDriveV3Model.Change
module File = GapiDriveV3Model.File
module Refresh = DriveMetadataRefresh

let run_session = DriveTestSupport.run_session

let default_runtime ?(config = Config.default) () =
  DriveTestSupport.base_runtime ~config ()

let make_metadata ?(display_name = "Cached User") ?(start_page_token = "old")
    ?(cache_size = 10L) ?(last_update = 100.0) () =
  {
    CacheData.Metadata.display_name;
    storage_quota_limit = 1000L;
    storage_quota_usage = 100L;
    start_page_token;
    cache_size;
    last_update;
    clean_shutdown = true;
  }

let make_file ?(parents = []) ?(trashed = false) ?(version = 1L) id name =
  {
    File.empty with
    id;
    name;
    mimeType = "text/plain";
    parents;
    trashed;
    version;
    size = 0L;
  }

let make_change ?(removed = false) file =
  {
    Change.empty with
    removed;
    file;
    fileId = file.File.id;
    changeType = "file";
  }

let make_resource ?(id = 1L) ?(state = CacheData.Resource.State.Synchronized)
    ?(mime_type = "text/plain") ?(version = 1L) remote_id path =
  let resource = Drive.create_resource path in
  {
    resource with
    id;
    remote_id = Some remote_id;
    name = Some (Filename.basename path);
    mime_type = Some mime_type;
    trashed = Some false;
    version = Some version;
    size = Some 0L;
    state;
    last_update = 100.0;
  }

let assert_has_event event events =
  assert_bool (Printf.sprintf "expected event %s" event) (List.mem event events)

let assert_no_event = DriveTestSupport.Trace.assert_no_event

module FakePorts = struct
  let trace = ref []
  let context_metadata = ref None
  let db_metadata = ref None
  let metadata_valid = ref true
  let cache_size = ref 42L
  let start_tokens = ref []
  let probe_tokens = ref []
  let listed_changes = ref ([], "next")
  let account_failure = ref None
  let resources_by_remote_id = Hashtbl.create 32
  let inserted_from_files = ref []
  let updated_resources = ref []
  let invalidated_ids = ref []
  let trashed_resources = ref []
  let deleted_resources = ref []

  let reset () =
    trace := [];
    context_metadata := None;
    db_metadata := None;
    metadata_valid := true;
    cache_size := 42L;
    start_tokens := [];
    probe_tokens := [];
    listed_changes := ([], "next");
    account_failure := None;
    Hashtbl.reset resources_by_remote_id;
    inserted_from_files := [];
    updated_resources := [];
    invalidated_ids := [];
    trashed_resources := [];
    deleted_resources := []

  let record event = trace := !trace @ [ event ]

  let add_resource resource =
    match resource.CacheData.Resource.remote_id with
    | None -> ()
    | Some remote_id ->
        let resources =
          match Hashtbl.find_opt resources_by_remote_id remote_id with
          | None -> []
          | Some resources -> resources
        in
        Hashtbl.replace resources_by_remote_id remote_id (resource :: resources)

  let with_metadata_lock f =
    record "lock";
    f ()

  let get_context_metadata () =
    record "get_context";
    !context_metadata

  let set_context_metadata metadata =
    record "set_context";
    context_metadata := metadata

  let select_metadata _cache =
    record "select_metadata";
    !db_metadata

  let insert_metadata _cache metadata =
    record ("insert_metadata:" ^ metadata.CacheData.Metadata.start_page_token);
    db_metadata := Some metadata

  let compute_cache_size _cache =
    record "compute_cache_size";
    !cache_size

  let metadata_is_valid _metadata_cache_time _metadata =
    record "metadata_is_valid";
    !metadata_valid

  let now () = 1234.0

  let run_request request =
    record "run_request";
    run_session request

  let with_default_retry request =
    record "retry";
    request

  let request_account_metadata () =
    record "account";
    match !account_failure with
    | Some e -> Utils.raise_m e
    | None ->
        SessionM.return
          {
            Refresh.display_name = "Remote User";
            storage_quota_limit = 2000L;
            storage_quota_usage = 250L;
          }

  let pop label values =
    match !values with
    | value :: rest ->
        values := rest;
        value
    | [] -> assert_failure ("expected " ^ label)

  let request_new_start_page_token () =
    record "start_token";
    SessionM.return (pop "start token" start_tokens)

  let probe_remaining_changes ~start_page_token =
    record ("probe:" ^ start_page_token);
    SessionM.return (pop "probe token" probe_tokens)

  let list_changes ~start_page_token =
    record ("list_changes:" ^ start_page_token);
    SessionM.return !listed_changes

  let update_all_timestamps _cache last_update =
    record (Printf.sprintf "timestamps:%.0f" last_update)

  let invalidate_all_resources _cache = record "invalidate_all"

  let invalidate_resources _cache ids =
    record ("invalidate_ids:" ^ String.concat "," (List.map Int64.to_string ids));
    invalidated_ids := !invalidated_ids @ ids

  let invalidate_trash_bin _cache = record "invalidate_trash_bin"
  let invalidate_path _cache path = record ("invalidate_path:" ^ path)

  let select_resources_with_remote_id _cache remote_id =
    record ("select_remote:" ^ remote_id);
    match Hashtbl.find_opt resources_by_remote_id remote_id with
    | None -> []
    | Some resources -> resources

  let trash_resources _cache resources =
    record
      ("trash:"
      ^ String.concat ","
          (List.map
             (fun resource -> Int64.to_string resource.CacheData.Resource.id)
             resources));
    trashed_resources := !trashed_resources @ resources

  let delete_cached_resources _metadata _cache resources =
    record
      ("delete:"
      ^ String.concat ","
          (List.map
             (fun resource -> Int64.to_string resource.CacheData.Resource.id)
             resources));
    deleted_resources := !deleted_resources @ resources

  let build_resource_tables parent_path _trashed =
    record ("tables:" ^ parent_path);
    (Hashtbl.create 4, Hashtbl.create 4)

  let get_unique_filename_from_file file _filename_table =
    record ("filename:" ^ file.File.name);
    file.File.name

  let create_resource path =
    record ("create:" ^ path);
    Drive.create_resource path

  let update_resource_from_file resource file =
    record
      (Printf.sprintf "update_from_file:%Ld:%s" resource.CacheData.Resource.id
         file.File.id);
    {
      resource with
      CacheData.Resource.remote_id = Some file.File.id;
      name = Some file.File.name;
      trashed = Some file.File.trashed;
      version = Some file.File.version;
      last_update = 1234.0;
    }

  let insert_resource_from_file _cache resource file =
    record ("insert_from_file:" ^ file.File.id);
    let inserted = update_resource_from_file resource file in
    inserted_from_files := !inserted_from_files @ [ inserted ];
    add_resource inserted;
    inserted

  let update_cached_resource _cache resource =
    record
      ("update_cache:" ^ Option.default "" resource.CacheData.Resource.remote_id);
    updated_resources := !updated_resources @ [ resource ];
    add_resource resource

  let lost_and_found_directory = "/lost+found"
  let shared_with_me_directory = "/.shared"
end

module MetadataRefresh = Refresh.Make (FakePorts)

let get_metadata ?(runtime = default_runtime ()) () =
  MetadataRefresh.get_metadata runtime

let test_valid_context_metadata_returns_immediately () =
  FakePorts.reset ();
  let metadata = make_metadata () in
  FakePorts.context_metadata := Some metadata;
  FakePorts.metadata_valid := true;
  assert_equal metadata (get_metadata ());
  assert_equal [ "lock"; "get_context"; "metadata_is_valid" ] !FakePorts.trace

let test_db_metadata_is_resynced_and_stored_in_context () =
  FakePorts.reset ();
  FakePorts.context_metadata := None;
  FakePorts.db_metadata := Some (make_metadata ~cache_size:1L ());
  FakePorts.cache_size := 77L;
  FakePorts.metadata_valid := true;
  let metadata = get_metadata () in
  assert_equal 77L metadata.CacheData.Metadata.cache_size;
  assert_equal (Some metadata) !FakePorts.context_metadata;
  assert_has_event "compute_cache_size" !FakePorts.trace;
  assert_no_event "run_request" !FakePorts.trace

let test_missing_metadata_refreshes_from_drive () =
  FakePorts.reset ();
  FakePorts.db_metadata := None;
  FakePorts.start_tokens := [ "fresh" ];
  FakePorts.probe_tokens := [ "fresh" ];
  let metadata = get_metadata () in
  assert_equal "Remote User" metadata.CacheData.Metadata.display_name;
  assert_equal "fresh" metadata.CacheData.Metadata.start_page_token;
  assert_equal 0L metadata.CacheData.Metadata.cache_size;
  assert_equal false metadata.CacheData.Metadata.clean_shutdown;
  assert_equal (Some metadata) !FakePorts.context_metadata;
  assert_has_event "retry" !FakePorts.trace;
  assert_has_event "account" !FakePorts.trace;
  assert_has_event "start_token" !FakePorts.trace;
  assert_has_event "timestamps:1234" !FakePorts.trace;
  assert_has_event "insert_metadata:fresh" !FakePorts.trace

let test_stale_metadata_with_no_changes_updates_timestamps () =
  FakePorts.reset ();
  FakePorts.context_metadata := Some (make_metadata ~start_page_token:"old" ());
  FakePorts.metadata_valid := false;
  FakePorts.probe_tokens := [ "old" ];
  let metadata = get_metadata () in
  assert_equal "old" metadata.CacheData.Metadata.start_page_token;
  assert_equal 10L metadata.CacheData.Metadata.cache_size;
  assert_has_event "timestamps:1234" !FakePorts.trace;
  assert_no_event "list_changes:" !FakePorts.trace

let test_over_limit_invalidates_all_and_stores_fresh_token () =
  FakePorts.reset ();
  FakePorts.context_metadata := Some (make_metadata ~start_page_token:"old" ());
  FakePorts.metadata_valid := false;
  FakePorts.probe_tokens := [ "" ];
  FakePorts.start_tokens := [ "fresh" ];
  let metadata = get_metadata () in
  assert_equal "fresh" metadata.CacheData.Metadata.start_page_token;
  assert_has_event "invalidate_all" !FakePorts.trace;
  assert_has_event "start_token" !FakePorts.trace

let test_first_time_normal_branch_skips_full_replay () =
  FakePorts.reset ();
  FakePorts.start_tokens := [ "base" ];
  FakePorts.probe_tokens := [ "next" ];
  let metadata = get_metadata () in
  assert_equal "base" metadata.CacheData.Metadata.start_page_token;
  assert_has_event "timestamps:1234" !FakePorts.trace;
  assert_no_event "list_changes:" !FakePorts.trace

let test_incremental_replay_updates_cache_and_synthetic_views () =
  FakePorts.reset ();
  let config =
    { Config.default with Config.lost_and_found = true; disable_trash = false }
  in
  let parent =
    make_resource ~id:10L ~mime_type:Drive.folder_mime_type "parent" "/docs"
  in
  let existing = make_resource ~id:20L ~version:1L "existing" "/old.txt" in
  let up_to_date = make_resource ~id:21L ~version:3L "same" "/same.txt" in
  let trashed = make_resource ~id:30L "trashed" "/trashed.txt" in
  let removed = make_resource ~id:40L "removed" "/removed.txt" in
  List.iter FakePorts.add_resource
    [ parent; existing; up_to_date; trashed; removed ];
  FakePorts.context_metadata := Some (make_metadata ~start_page_token:"old" ());
  FakePorts.metadata_valid := false;
  FakePorts.probe_tokens := [ "changes" ];
  let new_file = make_file ~parents:[ "parent" ] ~version:1L "new" "new.txt" in
  let update_file = make_file ~version:2L "existing" "old.txt" in
  let same_file = make_file ~version:3L "same" "same.txt" in
  let trash_file = make_file ~trashed:true "trashed" "trashed.txt" in
  let remove_file = make_file "removed" "removed.txt" in
  FakePorts.listed_changes :=
    ( [
        make_change new_file;
        make_change update_file;
        make_change same_file;
        make_change trash_file;
        make_change ~removed:true remove_file;
      ],
      "new-token" );
  let metadata = get_metadata ~runtime:(default_runtime ~config ()) () in
  assert_equal "new-token" metadata.CacheData.Metadata.start_page_token;
  assert_equal 1 (List.length !FakePorts.inserted_from_files);
  assert_equal [ 20L ] !FakePorts.invalidated_ids;
  assert_equal [ 30L ]
    (List.map (fun r -> r.CacheData.Resource.id) !FakePorts.trashed_resources);
  assert_equal [ 40L ]
    (List.map (fun r -> r.CacheData.Resource.id) !FakePorts.deleted_resources);
  assert_has_event "invalidate_trash_bin" !FakePorts.trace;
  assert_has_event "invalidate_path:/lost+found" !FakePorts.trace;
  assert_has_event "invalidate_path:/.shared" !FakePorts.trace

let test_incremental_replay_ignores_unanchored_new_resource () =
  FakePorts.reset ();
  FakePorts.context_metadata := Some (make_metadata ~start_page_token:"old" ());
  FakePorts.metadata_valid := false;
  FakePorts.probe_tokens := [ "changes" ];
  let orphan_file =
    make_file ~parents:[ "missing-parent" ] ~version:1L "new" "new.txt"
  in
  FakePorts.listed_changes := ([ make_change orphan_file ], "new-token");
  ignore (get_metadata ());
  assert_equal [] !FakePorts.inserted_from_files;
  assert_has_event "select_remote:missing-parent" !FakePorts.trace

let test_synthetic_invalidation_respects_config () =
  FakePorts.reset ();
  let config =
    { Config.default with Config.disable_trash = true; lost_and_found = false }
  in
  let existing = make_resource ~id:20L ~version:1L "existing" "/old.txt" in
  FakePorts.add_resource existing;
  FakePorts.context_metadata := Some (make_metadata ~start_page_token:"old" ());
  FakePorts.metadata_valid := false;
  FakePorts.probe_tokens := [ "changes" ];
  FakePorts.listed_changes :=
    ([ make_change (make_file ~version:2L "existing" "old.txt") ], "new-token");
  ignore (get_metadata ~runtime:(default_runtime ~config ()) ());
  assert_no_event "invalidate_trash_bin" !FakePorts.trace;
  assert_no_event "invalidate_path:/lost+found" !FakePorts.trace;
  assert_has_event "invalidate_path:/.shared" !FakePorts.trace

let test_request_exception_propagates () =
  FakePorts.reset ();
  FakePorts.account_failure := Some (Failure "account failed");
  assert_raises (Failure "account failed") (fun () -> ignore (get_metadata ()))

let suite =
  "DriveMetadataRefresh"
  >::: [
         "valid context metadata returns immediately"
         >:: test_valid_context_metadata_returns_immediately;
         "db metadata is resynced and stored in context"
         >:: test_db_metadata_is_resynced_and_stored_in_context;
         "missing metadata refreshes from drive"
         >:: test_missing_metadata_refreshes_from_drive;
         "stale metadata with no changes updates timestamps"
         >:: test_stale_metadata_with_no_changes_updates_timestamps;
         "over limit invalidates all and stores fresh token"
         >:: test_over_limit_invalidates_all_and_stores_fresh_token;
         "first time normal branch skips full replay"
         >:: test_first_time_normal_branch_skips_full_replay;
         "incremental replay updates cache and synthetic views"
         >:: test_incremental_replay_updates_cache_and_synthetic_views;
         "incremental replay ignores unanchored new resource"
         >:: test_incremental_replay_ignores_unanchored_new_resource;
         "synthetic invalidation respects config"
         >:: test_synthetic_invalidation_respects_config;
         "request exception propagates" >:: test_request_exception_propagates;
       ]
