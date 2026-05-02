open OUnit
open GapiMonad
module File = GapiDriveV3Model.File
module Resolver = DriveResourceResolver

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
  { Resolver.cache = dummy_cache; config }

let make_metadata last_update =
  {
    CacheData.Metadata.display_name = "Test User";
    storage_quota_limit = 0L;
    storage_quota_usage = 0L;
    start_page_token = "token";
    cache_size = 0L;
    last_update;
    clean_shutdown = false;
  }

let make_resource ?(id = 1L) ?remote_id
    ?(state = CacheData.Resource.State.Synchronized) ?(mime_type = "text/plain")
    ?(trashed = false) ?(last_update = 200.0) path =
  let resource = Drive.create_resource path in
  {
    resource with
    id;
    remote_id;
    name = Some (Filename.basename path);
    mime_type = Some mime_type;
    trashed = Some trashed;
    size = Some 0L;
    version = Some 1L;
    state;
    last_update;
  }

let make_file ?(mime_type = "text/plain") ?(trashed = false) ?(size = 0L)
    ?(version = 1L) id name =
  { File.empty with id; name; mimeType = mime_type; trashed; size; version }

let index_of value values =
  let rec loop i = function
    | [] -> raise Not_found
    | x :: xs -> if x = value then i else loop (i + 1) xs
  in
  loop 0 values

let assert_before earlier later events =
  assert_bool
    (Printf.sprintf "expected %s before %s" earlier later)
    (index_of earlier events < index_of later events)

let assert_has_event event events =
  assert_bool (Printf.sprintf "expected event %s" event) (List.mem event events)

let assert_no_event prefix events =
  assert_bool
    (Printf.sprintf "unexpected event with prefix %s" prefix)
    (not (List.exists (String.starts_with ~prefix) events))

module FakePorts = struct
  let root_directory = "/"
  let lost_and_found_directory = "/lost+found"
  let shared_with_me_directory = "/.shared"
  let trace = ref []
  let resources = Hashtbl.create 64
  let find_results = Hashtbl.create 16
  let remote_files = Hashtbl.create 16
  let reload_results = Hashtbl.create 16
  let inserted_resources = ref []
  let inserted_from_files = ref []
  let updated_resources = ref []
  let deleted_resources = ref []
  let metadata_last_update = ref 100.0
  let current_last_update = ref 100.0
  let metadata_failure = ref None
  let find_failure = ref None
  let remote_file_failure = ref None

  let reset () =
    trace := [];
    Hashtbl.reset resources;
    Hashtbl.reset find_results;
    Hashtbl.reset remote_files;
    Hashtbl.reset reload_results;
    inserted_resources := [];
    inserted_from_files := [];
    updated_resources := [];
    deleted_resources := [];
    metadata_last_update := 100.0;
    current_last_update := 100.0;
    metadata_failure := None;
    find_failure := None;
    remote_file_failure := None

  let record event = trace := !trace @ [ event ]
  let key path trashed = Printf.sprintf "%b:%s" trashed path

  let find_key ~parent_folder_id ~name ~trashed =
    Printf.sprintf "%b:%s:%s" trashed parent_folder_id name

  let add_resource resource =
    let trashed = Option.default false resource.CacheData.Resource.trashed in
    Hashtbl.replace resources
      (key resource.CacheData.Resource.path trashed)
      resource

  let set_find_result ~parent_folder_id ~name ~trashed file =
    Hashtbl.replace find_results
      (find_key ~parent_folder_id ~name ~trashed)
      file

  let set_remote_file remote_id file =
    Hashtbl.replace remote_files remote_id file

  let set_reload_result remote_id resource =
    Hashtbl.replace reload_results remote_id resource

  let get_metadata () =
    record "metadata";
    match !metadata_failure with
    | Some e -> raise e
    | None -> make_metadata !metadata_last_update

  let current_metadata_last_update () =
    record "current_metadata";
    !current_last_update

  let get_root_folder_id () =
    record "root_id";
    "root"

  let get_well_known_resource path trashed =
    record (Printf.sprintf "well_known:%s:%b" path trashed);
    make_resource ~remote_id:path ~mime_type:Drive.folder_mime_type ~trashed
      path

  let is_lost_and_found_root path trashed config =
    (not trashed) && config.Config.lost_and_found
    && path = lost_and_found_directory

  let is_shared_with_me_root path trashed _config =
    (not trashed) && path = shared_with_me_directory

  let lookup_resource _cache path trashed =
    record (Printf.sprintf "lookup:%s:%b" path trashed);
    Hashtbl.find_opt resources (key path trashed)

  let create_resource path =
    record ("create:" ^ path);
    Drive.create_resource path

  let insert_resource _cache resource =
    record
      (Printf.sprintf "insert:%s:%s" resource.CacheData.Resource.path
         (CacheData.Resource.State.to_string resource.CacheData.Resource.state));
    inserted_resources := !inserted_resources @ [ resource ];
    add_resource resource;
    resource

  let update_resource_from_file resource file =
    record
      (Printf.sprintf "update_from_file:%Ld:%s" resource.CacheData.Resource.id
         file.File.id);
    {
      resource with
      CacheData.Resource.remote_id = Some file.File.id;
      name = Some file.File.name;
      mime_type = Some file.File.mimeType;
      trashed = Some file.File.trashed;
      size = Some file.File.size;
      version = Some file.File.version;
      last_update = !metadata_last_update;
    }

  let insert_resource_from_file cache resource file =
    record
      (Printf.sprintf "insert_from_file:%s:%s" resource.CacheData.Resource.path
         file.File.id);
    let inserted = update_resource_from_file resource file in
    inserted_from_files := !inserted_from_files @ [ inserted ];
    add_resource inserted;
    ignore cache;
    inserted

  let update_cached_resource _cache resource =
    record
      (Printf.sprintf "update_cache:%Ld:%s" resource.CacheData.Resource.id
         (Option.default "" resource.CacheData.Resource.remote_id));
    updated_resources := !updated_resources @ [ resource ];
    add_resource resource

  let delete_cached_resource resource =
    record ("delete:" ^ resource.CacheData.Resource.path);
    deleted_resources := !deleted_resources @ [ resource ];
    let trashed = Option.default false resource.CacheData.Resource.trashed in
    Hashtbl.remove resources (key resource.CacheData.Resource.path trashed)

  let select_first_resource_with_remote_id _cache remote_id =
    record ("reload:" ^ remote_id);
    Hashtbl.find_opt reload_results remote_id

  let find_file_in_folder ~parent_folder_id ~name ~trashed session =
    record (Printf.sprintf "find:%s:%s:%b" parent_folder_id name trashed);
    match !find_failure with
    | Some e -> Utils.raise_m e session
    | None -> (
        let result =
          Hashtbl.find_opt find_results
            (find_key ~parent_folder_id ~name ~trashed)
        in
        match result with
        | None -> SessionM.return None session
        | Some file -> SessionM.return file session)

  let get_file_by_remote_id remote_id session =
    record ("fetch:" ^ remote_id);
    match !remote_file_failure with
    | Some e -> Utils.raise_m e session
    | None -> (
        match Hashtbl.find_opt remote_files remote_id with
        | None -> assert_failure ("missing fake remote file " ^ remote_id)
        | Some file -> SessionM.return file session)

  let with_default_retry request session =
    record "retry";
    request session
end

module ResolverOps = Resolver.Make (FakePorts)

let get_resource ?(runtime = default_runtime ()) ?(trashed = false) path =
  run_session (ResolverOps.get_resource runtime path trashed)

let get_folder_id ?(runtime = default_runtime ()) ?(trashed = false) path =
  run_session (ResolverOps.get_folder_id runtime path trashed)

let test_check_resource_in_cache_classifies_rows () =
  FakePorts.reset ();
  let runtime = default_runtime () in
  assert_bool "missing resource should not be cached"
    (not (ResolverOps.check_resource_in_cache runtime "/missing.txt" false));
  FakePorts.add_resource
    (make_resource ~last_update:10.0 ~remote_id:"rid-stale" "/stale.txt");
  assert_bool "stale resource should not be cached"
    (not (ResolverOps.check_resource_in_cache runtime "/stale.txt" false));
  FakePorts.add_resource
    (make_resource ~last_update:200.0 ~remote_id:"rid-file" "/file.txt");
  assert_bool "valid file should be cached"
    (ResolverOps.check_resource_in_cache runtime "/file.txt" false);
  FakePorts.add_resource
    (make_resource ~last_update:200.0 ~remote_id:"rid-folder"
       ~mime_type:Drive.folder_mime_type
       ~state:CacheData.Resource.State.ToDownload "/folder");
  assert_bool "unsynchronized folder should not be cached"
    (not (ResolverOps.check_resource_in_cache runtime "/folder" false));
  FakePorts.add_resource
    (make_resource ~last_update:200.0 ~remote_id:"rid-sync"
       ~mime_type:Drive.folder_mime_type
       ~state:CacheData.Resource.State.Synchronized "/synced");
  assert_bool "synchronized folder should be cached"
    (ResolverOps.check_resource_in_cache runtime "/synced" false)

let test_get_folder_id_root_uses_root_id () =
  FakePorts.reset ();
  assert_equal "root" (get_folder_id "/");
  assert_equal [ "root_id" ] !FakePorts.trace

let test_get_folder_id_non_root_uses_resolved_resource () =
  FakePorts.reset ();
  FakePorts.add_resource
    (make_resource ~remote_id:"rid-docs" ~mime_type:Drive.folder_mime_type
       "/docs");
  assert_equal "rid-docs" (get_folder_id "/docs");
  assert_has_event "metadata" !FakePorts.trace;
  assert_has_event "lookup:/docs:false" !FakePorts.trace

let test_well_known_resources_refresh_metadata_first () =
  List.iter
    (fun (path, config, expected_path) ->
      FakePorts.reset ();
      let runtime = default_runtime ~config () in
      let resource = get_resource ~runtime path in
      assert_equal expected_path resource.CacheData.Resource.path;
      assert_before "metadata"
        (Printf.sprintf "well_known:%s:false" expected_path)
        !FakePorts.trace)
    [
      ("/", Config.default, "/");
      ( "/lost+found",
        { Config.default with Config.lost_and_found = true },
        "/lost+found" );
      ("/.shared", Config.default, "/.shared");
    ]

let test_valid_cached_resource_returns_without_server_lookup () =
  FakePorts.reset ();
  let resource = make_resource ~remote_id:"rid-file" "/file.txt" in
  FakePorts.add_resource resource;
  assert_equal resource (get_resource "/file.txt");
  assert_no_event "find:" !FakePorts.trace;
  assert_no_event "retry" !FakePorts.trace

let test_miss_with_valid_synchronized_parent_raises_not_found () =
  FakePorts.reset ();
  FakePorts.add_resource
    (make_resource ~remote_id:"rid-docs" ~mime_type:Drive.folder_mime_type
       ~state:CacheData.Resource.State.Synchronized "/docs");
  assert_raises Resolver.File_not_found (fun () ->
      ignore (get_resource "/docs/missing.txt"));
  assert_no_event "find:" !FakePorts.trace

let test_miss_queries_server_with_root_parent_and_inserts_hit () =
  FakePorts.reset ();
  let file = make_file "rid-child" "child.txt" in
  FakePorts.set_find_result ~parent_folder_id:"root" ~name:"child.txt"
    ~trashed:false (Some file);
  let resource = get_resource "/child.txt" in
  assert_equal (Some "rid-child") resource.CacheData.Resource.remote_id;
  assert_equal [ resource ] !FakePorts.inserted_from_files;
  assert_has_event "find:root:child.txt:false" !FakePorts.trace

let test_nested_miss_resolves_parent_before_server_lookup () =
  FakePorts.reset ();
  FakePorts.current_last_update := 500.0;
  FakePorts.add_resource
    (make_resource ~remote_id:"rid-docs" ~mime_type:Drive.folder_mime_type
       ~state:CacheData.Resource.State.Synchronized "/docs");
  let file = make_file "rid-child" "child.txt" in
  FakePorts.set_find_result ~parent_folder_id:"rid-docs" ~name:"child.txt"
    ~trashed:false (Some file);
  let resource = get_resource "/docs/child.txt" in
  assert_equal (Some "rid-child") resource.CacheData.Resource.remote_id;
  assert_before "lookup:/docs:false" "find:rid-docs:child.txt:false"
    !FakePorts.trace

let test_server_miss_inserts_not_found_and_raises () =
  FakePorts.reset ();
  FakePorts.set_find_result ~parent_folder_id:"root" ~name:"missing.txt"
    ~trashed:false None;
  assert_raises Resolver.File_not_found (fun () ->
      ignore (get_resource "/missing.txt"));
  match !FakePorts.inserted_resources with
  | [ resource ] ->
      assert_equal CacheData.Resource.State.NotFound
        resource.CacheData.Resource.state;
      assert_equal (Some false) resource.CacheData.Resource.trashed
  | _ -> assert_failure "expected one inserted not-found resource"

let test_cached_not_found_raises () =
  FakePorts.reset ();
  FakePorts.add_resource
    (make_resource ~remote_id:"rid-missing"
       ~state:CacheData.Resource.State.NotFound "/missing.txt");
  assert_raises Resolver.File_not_found (fun () ->
      ignore (get_resource "/missing.txt"))

let test_stale_resource_refreshes_by_remote_id_and_updates_reloaded_row () =
  FakePorts.reset ();
  let stale =
    make_resource ~id:1L ~remote_id:"rid-file" ~last_update:10.0 "/file.txt"
  in
  let reloaded =
    make_resource ~id:99L ~remote_id:"rid-file" ~last_update:20.0 "/file.txt"
  in
  let remote_file = make_file "rid-file" "file.txt" in
  FakePorts.add_resource stale;
  FakePorts.set_reload_result "rid-file" reloaded;
  FakePorts.set_remote_file "rid-file" remote_file;
  let resource = get_resource "/file.txt" in
  assert_equal 99L resource.CacheData.Resource.id;
  assert_equal [ resource ] !FakePorts.updated_resources;
  assert_before "retry" "fetch:rid-file" !FakePorts.trace;
  assert_before "fetch:rid-file" "reload:rid-file" !FakePorts.trace;
  assert_before "reload:rid-file" "update_from_file:99:rid-file"
    !FakePorts.trace;
  assert_before "update_from_file:99:rid-file" "update_cache:99:rid-file"
    !FakePorts.trace

let test_stale_resource_refresh_uses_original_when_reload_missing () =
  FakePorts.reset ();
  let stale =
    make_resource ~id:1L ~remote_id:"rid-file" ~last_update:10.0 "/file.txt"
  in
  FakePorts.add_resource stale;
  FakePorts.set_remote_file "rid-file" (make_file "rid-file" "file.txt");
  let resource = get_resource "/file.txt" in
  assert_equal 1L resource.CacheData.Resource.id;
  assert_has_event "update_from_file:1:rid-file" !FakePorts.trace

let test_stale_resource_without_remote_id_deletes_then_misses () =
  FakePorts.reset ();
  let stale = make_resource ~last_update:10.0 "/orphan.txt" in
  let file = make_file "rid-orphan" "orphan.txt" in
  FakePorts.add_resource stale;
  FakePorts.set_find_result ~parent_folder_id:"root" ~name:"orphan.txt"
    ~trashed:false (Some file);
  let resource = get_resource "/orphan.txt" in
  assert_equal (Some "rid-orphan") resource.CacheData.Resource.remote_id;
  assert_equal [ stale ] !FakePorts.deleted_resources;
  assert_before "delete:/orphan.txt" "find:root:orphan.txt:false"
    !FakePorts.trace

let test_metadata_exception_propagates () =
  FakePorts.reset ();
  FakePorts.metadata_failure := Some (Failure "metadata failed");
  assert_raises (Failure "metadata failed") (fun () ->
      ignore
        (run_session
           (ResolverOps.get_resource (default_runtime ()) "/file.txt" false)))

let test_find_exception_propagates () =
  FakePorts.reset ();
  FakePorts.find_failure := Some (Failure "find failed");
  assert_raises (Failure "find failed") (fun () ->
      ignore (get_resource "/file.txt"))

let test_remote_file_exception_propagates () =
  FakePorts.reset ();
  let stale =
    make_resource ~remote_id:"rid-file" ~last_update:10.0 "/file.txt"
  in
  FakePorts.add_resource stale;
  FakePorts.remote_file_failure := Some (Failure "fetch failed");
  assert_raises (Failure "fetch failed") (fun () ->
      ignore (get_resource "/file.txt"))

let suite =
  "DriveResourceResolver"
  >::: [
         "check_resource_in_cache classifies rows"
         >:: test_check_resource_in_cache_classifies_rows;
         "get_folder_id root uses root id"
         >:: test_get_folder_id_root_uses_root_id;
         "get_folder_id non-root uses resolved resource"
         >:: test_get_folder_id_non_root_uses_resolved_resource;
         "well-known resources refresh metadata first"
         >:: test_well_known_resources_refresh_metadata_first;
         "valid cached resource returns without server lookup"
         >:: test_valid_cached_resource_returns_without_server_lookup;
         "miss with valid synchronized parent raises not found"
         >:: test_miss_with_valid_synchronized_parent_raises_not_found;
         "miss queries server with root parent and inserts hit"
         >:: test_miss_queries_server_with_root_parent_and_inserts_hit;
         "nested miss resolves parent before server lookup"
         >:: test_nested_miss_resolves_parent_before_server_lookup;
         "server miss inserts not found and raises"
         >:: test_server_miss_inserts_not_found_and_raises;
         "cached not found raises" >:: test_cached_not_found_raises;
         "stale resource refreshes by remote id and updates reloaded row"
         >:: test_stale_resource_refreshes_by_remote_id_and_updates_reloaded_row;
         "stale resource refresh uses original when reload missing"
         >:: test_stale_resource_refresh_uses_original_when_reload_missing;
         "stale resource without remote id deletes then misses"
         >:: test_stale_resource_without_remote_id_deletes_then_misses;
         "metadata exception propagates" >:: test_metadata_exception_propagates;
         "find exception propagates" >:: test_find_exception_propagates;
         "remote file exception propagates"
         >:: test_remote_file_exception_propagates;
       ]
