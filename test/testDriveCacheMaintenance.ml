open OUnit
module Maintenance = DriveCacheMaintenance

let make_metadata ?(display_name = "metadata") ?(cache_size = 0L) () =
  {
    CacheData.Metadata.display_name;
    storage_quota_limit = 0L;
    storage_quota_usage = 0L;
    start_page_token = "token";
    cache_size;
    last_update = 0.;
    clean_shutdown = false;
  }

let make_resource ?(id = 1L) ?remote_id ?size path =
  {
    (DriveResourceMapping.create_resource ~now:(fun () -> 0.) path) with
    CacheData.Resource.id;
    remote_id;
    size;
    state = CacheData.Resource.State.Synchronized;
  }

let make_config ?(max_cache_size_mb = 512) () =
  { Config.default with max_cache_size_mb }

let default_runtime ?config ?metadata () =
  {
    Maintenance.cache = DriveTestSupport.dummy_cache;
    config = Option.default Config.default config;
    metadata;
  }

let ids resources =
  resources
  |> List.map (fun resource -> Int64.to_string resource.CacheData.Resource.id)
  |> String.concat ","

let stats_with_size size =
  let stats = Unix.LargeFile.stat "." in
  { stats with Unix.LargeFile.st_size = size }

module FakePorts = struct
  let trace = ref []
  let context_metadata = ref (make_metadata ())
  let ordered_resources = ref []
  let delete_file_size = ref 0L
  let existing_paths = ref []
  let stats_by_path = Hashtbl.create 8
  let stat_failure = ref None

  let reset () =
    trace := [];
    context_metadata := make_metadata ();
    ordered_resources := [];
    delete_file_size := 0L;
    existing_paths := [];
    Hashtbl.reset stats_by_path;
    stat_failure := None

  let record event = trace := !trace @ [ event ]

  let with_metadata_lock f =
    record "lock_begin";
    let result = f () in
    record "lock_end";
    result

  let update_cache_size_in_db _cache delta =
    record (Printf.sprintf "db:%Ld" delta)

  let update_context_metadata f =
    record "context_update";
    context_metadata := f !context_metadata;
    record
      (Printf.sprintf "context_size:%Ld"
         !context_metadata.CacheData.Metadata.cache_size)

  let select_resources_order_by_last_update _cache =
    record "select";
    !ordered_resources

  let update_cached_resource_state _cache state id =
    record
      (Printf.sprintf "state:%Ld:%s" id
         (CacheData.Resource.State.to_string state))

  let delete_files_from_cache _cache resources =
    record ("delete_files:" ^ ids resources);
    !delete_file_size

  let delete_resource _cache resource =
    record (Printf.sprintf "delete_resource:%Ld" resource.CacheData.Resource.id)

  let delete_resources _cache resources =
    record ("delete_resources:" ^ ids resources)

  let remove_memory_buffers remote_id = record ("buffers:" ^ remote_id)
  let remove_file_lock remote_id = record ("file_lock:" ^ remote_id)

  let file_exists path =
    record ("exists:" ^ path);
    List.mem path !existing_paths

  let stat_file path =
    record ("stat:" ^ path);
    match !stat_failure with
    | Some e -> raise e
    | None -> Hashtbl.find stats_by_path path

  let log_exception e = record ("exception:" ^ Printexc.to_string e)
end

module Ops = Maintenance.Make (FakePorts)

let test_update_cache_size_zero_skips_side_effects () =
  FakePorts.reset ();
  Ops.update_cache_size 0L
    (make_metadata ~cache_size:10L ())
    DriveTestSupport.dummy_cache;
  assert_equal [] !FakePorts.trace

let test_update_cache_size_updates_db_then_context () =
  FakePorts.reset ();
  FakePorts.context_metadata :=
    make_metadata ~display_name:"context" ~cache_size:1L ();
  Ops.update_cache_size 25L
    (make_metadata ~display_name:"argument" ~cache_size:100L ())
    DriveTestSupport.dummy_cache;
  assert_equal
    [ "db:25"; "context_update"; "context_size:125" ]
    !FakePorts.trace;
  assert_equal "context"
    !FakePorts.context_metadata.CacheData.Metadata.display_name;
  assert_equal 125L !FakePorts.context_metadata.CacheData.Metadata.cache_size

let test_shrink_under_limit_accounts_for_new_file_size () =
  FakePorts.reset ();
  FakePorts.context_metadata := make_metadata ~cache_size:10L ();
  let runtime =
    default_runtime
      ~config:(make_config ~max_cache_size_mb:1 ())
      ~metadata:(make_metadata ~cache_size:10L ())
      ()
  in
  Ops.shrink_cache runtime ~file_size:5L ();
  assert_equal
    [ "lock_begin"; "db:5"; "context_update"; "context_size:15"; "lock_end" ]
    !FakePorts.trace

let test_shrink_over_limit_marks_and_deletes_selected_resources () =
  FakePorts.reset ();
  FakePorts.context_metadata := make_metadata ~cache_size:100L ();
  let first = make_resource ~id:1L ~size:40L "/first" in
  let second = make_resource ~id:2L ~size:70L "/second" in
  FakePorts.ordered_resources := [ first; second ];
  let runtime =
    default_runtime
      ~config:(make_config ~max_cache_size_mb:0 ())
      ~metadata:(make_metadata ~cache_size:100L ())
      ()
  in
  Ops.shrink_cache runtime ~file_size:10L ();
  assert_equal
    [
      "lock_begin";
      "select";
      "db:-100";
      "context_update";
      "context_size:0";
      "state:2:ToDownload";
      "state:1:ToDownload";
      "delete_files:2,1";
      "lock_end";
    ]
    !FakePorts.trace

let test_shrink_negative_file_size_only_updates_accounting () =
  FakePorts.reset ();
  FakePorts.context_metadata := make_metadata ~cache_size:50L ();
  let runtime =
    default_runtime
      ~config:(make_config ~max_cache_size_mb:1 ())
      ~metadata:(make_metadata ~cache_size:50L ())
      ()
  in
  Ops.shrink_cache runtime ~file_size:(-10L) ();
  assert_equal
    [ "lock_begin"; "db:-10"; "context_update"; "context_size:40"; "lock_end" ]
    !FakePorts.trace

let test_delete_from_context_depends_on_remote_id () =
  FakePorts.reset ();
  Ops.delete_from_context (make_resource "/local");
  assert_equal [] !FakePorts.trace;
  Ops.delete_from_context (make_resource ~remote_id:"rid" "/remote");
  assert_equal [ "buffers:rid"; "file_lock:rid" ] !FakePorts.trace

let test_delete_cached_resource_updates_accounting_when_metadata_exists () =
  FakePorts.reset ();
  FakePorts.context_metadata := make_metadata ~cache_size:100L ();
  FakePorts.delete_file_size := 30L;
  let resource = make_resource ~id:7L ~remote_id:"rid" "/file" in
  let runtime =
    default_runtime ~metadata:(make_metadata ~cache_size:100L ()) ()
  in
  Ops.delete_cached_resource runtime resource;
  assert_equal
    [
      "delete_resource:7";
      "delete_files:7";
      "db:-30";
      "context_update";
      "context_size:70";
      "buffers:rid";
      "file_lock:rid";
    ]
    !FakePorts.trace

let test_delete_cached_resource_without_metadata_skips_accounting () =
  FakePorts.reset ();
  FakePorts.delete_file_size := 30L;
  let resource = make_resource ~id:7L ~remote_id:"rid" "/file" in
  Ops.delete_cached_resource (default_runtime ()) resource;
  assert_equal
    [ "delete_resource:7"; "delete_files:7"; "buffers:rid"; "file_lock:rid" ]
    !FakePorts.trace

let test_delete_cached_resources_updates_accounting_and_cleans_each_resource ()
    =
  FakePorts.reset ();
  FakePorts.context_metadata := make_metadata ~cache_size:100L ();
  FakePorts.delete_file_size := 25L;
  let first = make_resource ~id:1L ~remote_id:"rid-1" "/first" in
  let second = make_resource ~id:2L "/second" in
  let runtime = default_runtime () in
  Ops.delete_cached_resources runtime
    (make_metadata ~cache_size:100L ())
    [ first; second ];
  assert_equal
    [
      "delete_resources:1,2";
      "delete_files:1,2";
      "db:-25";
      "context_update";
      "context_size:75";
      "buffers:rid-1";
      "file_lock:rid-1";
    ]
    !FakePorts.trace

let test_update_cache_size_for_documents_ignores_nonzero_resources () =
  FakePorts.reset ();
  let runtime =
    default_runtime ~metadata:(make_metadata ~cache_size:100L ()) ()
  in
  Ops.update_cache_size_for_documents runtime
    (make_resource ~size:1L "/doc")
    "/cache/doc" Std.identity;
  assert_equal [ "lock_begin"; "lock_end" ] !FakePorts.trace

let test_update_cache_size_for_documents_ignores_missing_files () =
  FakePorts.reset ();
  let runtime =
    default_runtime ~metadata:(make_metadata ~cache_size:100L ()) ()
  in
  Ops.update_cache_size_for_documents runtime
    (make_resource ~size:0L "/doc")
    "/cache/doc" Std.identity;
  assert_equal
    [ "lock_begin"; "exists:/cache/doc"; "lock_end" ]
    !FakePorts.trace

let test_update_cache_size_for_documents_applies_size_operation () =
  FakePorts.reset ();
  FakePorts.context_metadata := make_metadata ~cache_size:100L ();
  FakePorts.existing_paths := [ "/cache/doc" ];
  Hashtbl.add FakePorts.stats_by_path "/cache/doc" (stats_with_size 12L);
  let runtime =
    default_runtime ~metadata:(make_metadata ~cache_size:100L ()) ()
  in
  Ops.update_cache_size_for_documents runtime
    (make_resource ~size:0L "/doc")
    "/cache/doc" Int64.neg;
  assert_equal
    [
      "lock_begin";
      "exists:/cache/doc";
      "stat:/cache/doc";
      "db:-12";
      "context_update";
      "context_size:88";
      "lock_end";
    ]
    !FakePorts.trace

let test_update_cache_size_for_documents_logs_stat_exceptions () =
  FakePorts.reset ();
  FakePorts.existing_paths := [ "/cache/doc" ];
  FakePorts.stat_failure := Some (Failure "stat failed");
  let runtime =
    default_runtime ~metadata:(make_metadata ~cache_size:100L ()) ()
  in
  Ops.update_cache_size_for_documents runtime
    (make_resource ~size:0L "/doc")
    "/cache/doc" Std.identity;
  assert_equal
    [
      "lock_begin";
      "exists:/cache/doc";
      "stat:/cache/doc";
      "exception:Failure(\"stat failed\")";
      "lock_end";
    ]
    !FakePorts.trace

let suite =
  "DriveCacheMaintenance"
  >::: [
         "update_cache_size zero skips side effects"
         >:: test_update_cache_size_zero_skips_side_effects;
         "update_cache_size updates db then context"
         >:: test_update_cache_size_updates_db_then_context;
         "shrink under limit accounts for new file size"
         >:: test_shrink_under_limit_accounts_for_new_file_size;
         "shrink over limit marks and deletes selected resources"
         >:: test_shrink_over_limit_marks_and_deletes_selected_resources;
         "shrink negative file size only updates accounting"
         >:: test_shrink_negative_file_size_only_updates_accounting;
         "delete_from_context depends on remote id"
         >:: test_delete_from_context_depends_on_remote_id;
         "delete_cached_resource updates accounting when metadata exists"
         >:: test_delete_cached_resource_updates_accounting_when_metadata_exists;
         "delete_cached_resource without metadata skips accounting"
         >:: test_delete_cached_resource_without_metadata_skips_accounting;
         "delete_cached_resources updates accounting and cleans each resource"
         >:: test_delete_cached_resources_updates_accounting_and_cleans_each_resource;
         "update_cache_size_for_documents ignores nonzero resources"
         >:: test_update_cache_size_for_documents_ignores_nonzero_resources;
         "update_cache_size_for_documents ignores missing files"
         >:: test_update_cache_size_for_documents_ignores_missing_files;
         "update_cache_size_for_documents applies size operation"
         >:: test_update_cache_size_for_documents_applies_size_operation;
         "update_cache_size_for_documents logs stat exceptions"
         >:: test_update_cache_size_for_documents_logs_stat_exceptions;
       ]
