open OUnit
open GapiMonad
module File = GapiDriveV3Model.File
module Root = DriveRootResolution

let run_session = DriveTestSupport.run_session

let default_runtime ?(config = Config.default) ?root_folder_id () =
  { Root.cache = DriveTestSupport.dummy_cache; config; root_folder_id }

let make_config ?(scope = "") ?(team_drive_id = "") ?(root_folder = "")
    ?(lost_and_found = false) () =
  { Config.default with scope; team_drive_id; root_folder; lost_and_found }

let make_file ?(mime_type = "text/plain") id name =
  { File.empty with id; name; mimeType = mime_type }

let make_resource ?(id = 1L) ?remote_id ?(trashed = false)
    ?(mime_type = "text/plain") path =
  let resource =
    DriveResourceMapping.create_resource ~now:(fun () -> 0.) path
  in
  {
    resource with
    id;
    remote_id;
    mime_type = Some mime_type;
    size = Some 0L;
    trashed = Some trashed;
  }

let assert_before = DriveTestSupport.Trace.assert_before

module FakePorts = struct
  let trace = ref []
  let find_results = Hashtbl.create 16
  let remote_files = Hashtbl.create 16
  let cached_resources = Hashtbl.create 16
  let stored_root_folder_id = ref None
  let create_folder_id = ref "created-root"
  let find_failure = ref None

  let reset () =
    trace := [];
    Hashtbl.reset find_results;
    Hashtbl.reset remote_files;
    Hashtbl.reset cached_resources;
    stored_root_folder_id := None;
    create_folder_id := "created-root";
    find_failure := None

  let record event = trace := !trace @ [ event ]

  let find_key ~parent_folder_id ~name ~trashed =
    Printf.sprintf "%b:%s:%s" trashed parent_folder_id name

  let cache_key path trashed = Printf.sprintf "%b:%s" trashed path

  let set_find_result ~parent_folder_id ~name ~trashed file =
    Hashtbl.replace find_results
      (find_key ~parent_folder_id ~name ~trashed)
      file

  let set_remote_file file = Hashtbl.replace remote_files file.File.id file

  let set_cached_resource resource =
    let trashed = Option.default false resource.CacheData.Resource.trashed in
    Hashtbl.replace cached_resources
      (cache_key resource.CacheData.Resource.path trashed)
      resource

  let folder_mime_type = DriveResourceMapping.folder_mime_type

  let create_resource path =
    record ("create_resource:" ^ path);
    DriveResourceMapping.create_resource ~now:(fun () -> 0.) path

  let find_file_in_folder ~parent_folder_id ~name ~trashed session =
    record (Printf.sprintf "find:%s:%s:%b" parent_folder_id name trashed);
    match !find_failure with
    | Some e -> Utils.raise_m e session
    | None ->
        (match
           Hashtbl.find_opt find_results
             (find_key ~parent_folder_id ~name ~trashed)
         with
        | None -> assert_failure "missing fake find result"
        | Some result -> SessionM.return result)
          session

  let get_file_by_remote_id remote_id session =
    record ("get:" ^ remote_id);
    match Hashtbl.find_opt remote_files remote_id with
    | Some file -> SessionM.return file session
    | None -> assert_failure ("missing remote file " ^ remote_id)

  let create_folder ~name session =
    record ("create_folder:" ^ name);
    SessionM.return
      (make_file ~mime_type:folder_mime_type !create_folder_id name)
      session

  let run_request request =
    record "run_request";
    run_session request

  let set_context_root_folder_id root_folder_id =
    record ("set_root:" ^ root_folder_id);
    stored_root_folder_id := Some root_folder_id

  let lookup_resource _cache path trashed =
    record (Printf.sprintf "lookup:%s:%b" path trashed);
    Hashtbl.find_opt cached_resources (cache_key path trashed)

  let insert_resource _cache ~label resource =
    record
      (Printf.sprintf "insert:%s:%s" label resource.CacheData.Resource.path);
    let resource = { resource with CacheData.Resource.id = 99L } in
    set_cached_resource resource;
    resource
end

module RootOps = Root.Make (FakePorts)

let test_predicates_match_virtual_roots () =
  let enabled = make_config ~lost_and_found:true () in
  let disabled = make_config ~lost_and_found:false () in
  assert_bool "lost+found enabled"
    (Root.is_lost_and_found_root Root.lost_and_found_directory false enabled);
  assert_bool "lost+found disabled"
    (not
       (Root.is_lost_and_found_root Root.lost_and_found_directory false disabled));
  assert_bool "lost+found trashed"
    (not
       (Root.is_lost_and_found_root Root.lost_and_found_directory true enabled));
  assert_bool "shared"
    (Root.is_shared_with_me_root Root.shared_with_me_directory false enabled);
  assert_bool "shared trashed"
    (not
       (Root.is_shared_with_me_root Root.shared_with_me_directory true enabled))

let test_synthetic_resource_shapes () =
  FakePorts.reset ();
  let root = RootOps.create_root_resource "root-id" true in
  assert_equal Root.root_directory root.CacheData.Resource.path;
  assert_equal (Some "root-id") root.CacheData.Resource.remote_id;
  assert_equal (Some true) root.CacheData.Resource.trashed;
  assert_equal "" root.CacheData.Resource.parent_path;
  assert_equal (Some FakePorts.folder_mime_type)
    root.CacheData.Resource.mime_type;
  let shared =
    RootOps.create_well_known_resource Root.shared_with_me_directory
  in
  assert_equal Root.shared_with_me_directory shared.CacheData.Resource.path;
  assert_equal (Some "") shared.CacheData.Resource.remote_id;
  assert_equal (Some false) shared.CacheData.Resource.trashed;
  assert_equal "" shared.CacheData.Resource.parent_path;
  assert_equal (Some FakePorts.folder_mime_type)
    shared.CacheData.Resource.mime_type

let test_non_device_scope_fetches_default_root () =
  FakePorts.reset ();
  FakePorts.set_remote_file (make_file "root" "My Drive");
  let root_id =
    run_session
      (RootOps.get_root_folder_id_from_server (make_config ~scope:"" ()))
  in
  assert_equal "root" root_id;
  assert_equal [ "get:root" ] !FakePorts.trace

let test_device_scope_reuses_existing_gdfuse_folder () =
  FakePorts.reset ();
  FakePorts.set_find_result ~parent_folder_id:Root.default_root_folder_id
    ~name:Root.device_root_folder ~trashed:false
    (Some (make_file "device-root" Root.device_root_folder));
  let root_id =
    run_session
      (RootOps.get_root_folder_id_from_server
         (make_config ~scope:Root.device_scope ()))
  in
  assert_equal "device-root" root_id;
  assert_equal [ "find:root:gdfuse:false" ] !FakePorts.trace

let test_device_scope_creates_missing_gdfuse_folder () =
  FakePorts.reset ();
  FakePorts.set_find_result ~parent_folder_id:Root.default_root_folder_id
    ~name:Root.device_root_folder ~trashed:false None;
  FakePorts.create_folder_id := "new-device-root";
  let root_id =
    run_session
      (RootOps.get_root_folder_id_from_server
         (make_config ~scope:Root.device_scope ()))
  in
  assert_equal "new-device-root" root_id;
  assert_equal
    [ "find:root:gdfuse:false"; "create_folder:gdfuse" ]
    !FakePorts.trace

let test_configured_root_resolution_variants () =
  FakePorts.reset ();
  FakePorts.set_remote_file (make_file "root" "My Drive");
  assert_equal "root" (run_session (RootOps.get_root_folder_id Config.default));
  FakePorts.reset ();
  assert_equal "team-root"
    (run_session
       (RootOps.get_root_folder_id (make_config ~team_drive_id:"team-root" ())));
  assert_equal [] !FakePorts.trace;
  FakePorts.reset ();
  assert_equal "configured-id"
    (run_session
       (RootOps.get_root_folder_id
          (make_config ~root_folder:"configured-id" ())));
  assert_equal [] !FakePorts.trace

let test_absolute_root_folder_traverses_segments () =
  FakePorts.reset ();
  FakePorts.set_find_result ~parent_folder_id:Root.default_root_folder_id
    ~name:"Top" ~trashed:false
    (Some (make_file "top-id" "Top"));
  FakePorts.set_find_result ~parent_folder_id:"top-id" ~name:"Nested"
    ~trashed:false
    (Some (make_file "nested-id" "Nested"));
  let root_id =
    run_session
      (RootOps.get_root_folder_id (make_config ~root_folder:"/Top/Nested" ()))
  in
  assert_equal "nested-id" root_id;
  assert_equal
    [ "find:root:Top:false"; "find:top-id:Nested:false" ]
    !FakePorts.trace

let test_absolute_root_folder_under_team_drive_starts_from_team_drive () =
  FakePorts.reset ();
  FakePorts.set_find_result ~parent_folder_id:"team-id" ~name:"Top"
    ~trashed:false
    (Some (make_file "top-id" "Top"));
  let root_id =
    run_session
      (RootOps.get_root_folder_id
         (make_config ~team_drive_id:"team-id" ~root_folder:"/Top" ()))
  in
  assert_equal "top-id" root_id;
  assert_equal [ "find:team-id:Top:false" ] !FakePorts.trace

let test_missing_absolute_root_segment_raises () =
  FakePorts.reset ();
  FakePorts.set_find_result ~parent_folder_id:Root.default_root_folder_id
    ~name:"Missing" ~trashed:false None;
  assert_raises (Failure "Invalid root folder in configuration") (fun () ->
      ignore
        (run_session
           (RootOps.get_root_folder_id (make_config ~root_folder:"/Missing" ()))))

let test_context_memoization () =
  FakePorts.reset ();
  let cached_runtime = default_runtime ~root_folder_id:"cached-root" () in
  assert_equal "cached-root"
    (RootOps.get_root_folder_id_from_context cached_runtime);
  assert_equal [] !FakePorts.trace;
  FakePorts.reset ();
  let runtime =
    default_runtime ~config:(make_config ~root_folder:"configured-root" ()) ()
  in
  assert_equal "configured-root"
    (RootOps.get_root_folder_id_from_context runtime);
  assert_equal [ "run_request"; "set_root:configured-root" ] !FakePorts.trace;
  assert_equal (Some "configured-root") !FakePorts.stored_root_folder_id

let test_well_known_resource_cache_hit_resolves_root_first () =
  FakePorts.reset ();
  let cached =
    make_resource ~remote_id:"root-id" ~mime_type:FakePorts.folder_mime_type
      Root.root_directory
  in
  FakePorts.set_cached_resource cached;
  let runtime =
    default_runtime ~config:(make_config ~root_folder:"root-id" ()) ()
  in
  let resource =
    RootOps.get_well_known_resource runtime Root.root_directory false
  in
  assert_equal cached resource;
  assert_before "run_request" "lookup:/:false" !FakePorts.trace;
  assert_equal
    [ "run_request"; "set_root:root-id"; "lookup:/:false" ]
    !FakePorts.trace

let test_well_known_resource_inserts_root_with_trashed_flag () =
  FakePorts.reset ();
  let runtime = default_runtime ~root_folder_id:"root-id" () in
  let resource =
    RootOps.get_well_known_resource runtime Root.root_directory true
  in
  assert_equal 99L resource.CacheData.Resource.id;
  assert_equal (Some "root-id") resource.CacheData.Resource.remote_id;
  assert_equal (Some true) resource.CacheData.Resource.trashed;
  assert_equal
    [ "lookup:/:true"; "create_resource:/"; "insert:root:/" ]
    !FakePorts.trace

let test_well_known_resource_inserts_virtual_roots () =
  FakePorts.reset ();
  let runtime =
    default_runtime
      ~config:(make_config ~lost_and_found:true ())
      ~root_folder_id:"root-id" ()
  in
  let lost =
    RootOps.get_well_known_resource runtime Root.lost_and_found_directory false
  in
  assert_equal Root.lost_and_found_directory lost.CacheData.Resource.path;
  assert_equal (Some "") lost.CacheData.Resource.remote_id;
  let shared =
    RootOps.get_well_known_resource runtime Root.shared_with_me_directory false
  in
  assert_equal Root.shared_with_me_directory shared.CacheData.Resource.path;
  assert_equal (Some "") shared.CacheData.Resource.remote_id;
  assert_equal
    [
      "lookup:/lost+found:false";
      "create_resource:/lost+found";
      "insert:lost+found:/lost+found";
      "lookup:/.shared:false";
      "create_resource:/.shared";
      "insert:shared with me:/.shared";
    ]
    !FakePorts.trace

let test_well_known_resource_rejects_invalid_path () =
  FakePorts.reset ();
  let runtime = default_runtime ~root_folder_id:"root-id" () in
  assert_raises
    (Invalid_argument "Invalid well known path: /invalid trashed=false")
    (fun () ->
      ignore (RootOps.get_well_known_resource runtime "/invalid" false))

let suite =
  "DriveRootResolution"
  >::: [
         "predicates match virtual roots"
         >:: test_predicates_match_virtual_roots;
         "synthetic resource shapes" >:: test_synthetic_resource_shapes;
         "non-device scope fetches default root"
         >:: test_non_device_scope_fetches_default_root;
         "device scope reuses existing gdfuse folder"
         >:: test_device_scope_reuses_existing_gdfuse_folder;
         "device scope creates missing gdfuse folder"
         >:: test_device_scope_creates_missing_gdfuse_folder;
         "configured root resolution variants"
         >:: test_configured_root_resolution_variants;
         "absolute root folder traverses segments"
         >:: test_absolute_root_folder_traverses_segments;
         "absolute root folder under team drive starts from team drive"
         >:: test_absolute_root_folder_under_team_drive_starts_from_team_drive;
         "missing absolute root segment raises"
         >:: test_missing_absolute_root_segment_raises;
         "context memoization" >:: test_context_memoization;
         "well-known resource cache hit resolves root first"
         >:: test_well_known_resource_cache_hit_resolves_root_first;
         "well-known resource inserts root with trashed flag"
         >:: test_well_known_resource_inserts_root_with_trashed_flag;
         "well-known resource inserts virtual roots"
         >:: test_well_known_resource_inserts_virtual_roots;
         "well-known resource rejects invalid path"
         >:: test_well_known_resource_rejects_invalid_path;
       ]
