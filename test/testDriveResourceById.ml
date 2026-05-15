open OUnit
open GapiMonad
module File = GapiDriveV3Model.File
module ResourceById = DriveResourceById

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

let runtime = { ResourceById.cache = dummy_cache }

let make_resource ?(id = 1L) ?remote_id ?(mime_type = "text/plain") path =
  let resource =
    DriveResourceMapping.create_resource ~now:(fun () -> 0.) path
  in
  {
    resource with
    id;
    remote_id;
    name = Some (Filename.basename path);
    mime_type = Some mime_type;
    size = Some 0L;
    version = Some 1L;
    state = CacheData.Resource.State.Synchronized;
  }

let make_file ?(parents = [ "root" ]) ?(shared = false)
    ?(mime_type = "text/plain") id name =
  { File.empty with id; name; mimeType = mime_type; parents; shared }

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

module FakePorts = struct
  let root_directory = "/"
  let shared_with_me_directory = "/.shared"
  let root_folder_id = ref "root"
  let trace = ref []
  let cached_resources = Hashtbl.create 16
  let remote_files = Hashtbl.create 16
  let fetch_failure = ref None

  let reset () =
    root_folder_id := "root";
    trace := [];
    Hashtbl.reset cached_resources;
    Hashtbl.reset remote_files;
    fetch_failure := None

  let record event = trace := !trace @ [ event ]

  let add_cached resource =
    match resource.CacheData.Resource.remote_id with
    | None -> invalid_arg "cached test resource needs a remote id"
    | Some remote_id -> Hashtbl.replace cached_resources remote_id resource

  let add_remote_file file = Hashtbl.replace remote_files file.File.id file

  let get_root_folder_id () =
    record "root_id";
    !root_folder_id

  let get_well_known_resource path trashed =
    record (Printf.sprintf "well_known:%s:%b" path trashed);
    make_resource ~remote_id:!root_folder_id ~mime_type:Drive.folder_mime_type
      path

  let select_first_resource_with_remote_id _cache remote_id =
    record ("cache:" ^ remote_id);
    Hashtbl.find_opt cached_resources remote_id

  let clean_filename name =
    record ("clean:" ^ name);
    DriveResourceMapping.clean_filename name

  let create_resource path =
    record ("create:" ^ path);
    DriveResourceMapping.create_resource ~now:(fun () -> 0.) path

  let update_resource_from_file resource file =
    record
      (Printf.sprintf "update:%s:%s" resource.CacheData.Resource.path
         file.File.id);
    {
      resource with
      CacheData.Resource.remote_id = Some file.File.id;
      name = Some file.File.name;
      mime_type = Some file.File.mimeType;
      size = Some file.File.size;
      version = Some file.File.version;
    }

  let get_file_by_remote_id remote_id session =
    record ("fetch:" ^ remote_id);
    match !fetch_failure with
    | Some e -> Utils.raise_m e session
    | None -> (
        match Hashtbl.find_opt remote_files remote_id with
        | None -> assert_failure ("missing fake remote file " ^ remote_id)
        | Some file -> SessionM.return file session)
end

module ResourceByIdOps = ResourceById.Make (FakePorts)

let get_resource_with_id remote_id =
  run_session (ResourceByIdOps.get_resource_with_id runtime remote_id)

let get_resource_with_id_from_server remote_id =
  run_session (ResourceByIdOps.get_resource_with_id_from_server remote_id)

let test_cached_resource_returns_without_server_lookup () =
  FakePorts.reset ();
  let cached = make_resource ~remote_id:"rid-file" "/cached.txt" in
  FakePorts.add_cached cached;
  assert_equal cached (get_resource_with_id "rid-file");
  assert_equal [ "cache:rid-file" ] !FakePorts.trace

let test_cache_miss_reconstructs_direct_child_under_root () =
  FakePorts.reset ();
  FakePorts.add_remote_file (make_file "rid-child" "child.txt");
  let resource = get_resource_with_id "rid-child" in
  assert_equal "/child.txt" resource.CacheData.Resource.path;
  assert_equal (Some "rid-child") resource.CacheData.Resource.remote_id;
  assert_before "cache:rid-child" "root_id" !FakePorts.trace;
  assert_before "root_id" "fetch:rid-child" !FakePorts.trace;
  assert_before "clean:child.txt" "create:/child.txt" !FakePorts.trace;
  assert_before "create:/child.txt" "update:/child.txt:rid-child"
    !FakePorts.trace

let test_root_remote_id_returns_well_known_root () =
  FakePorts.reset ();
  let resource = get_resource_with_id "root" in
  assert_equal "/" resource.CacheData.Resource.path;
  assert_equal (Some "root") resource.CacheData.Resource.remote_id;
  assert_equal
    [ "cache:root"; "root_id"; "well_known:/:false" ]
    !FakePorts.trace

let test_nested_parent_chain_reconstructs_clean_absolute_path () =
  FakePorts.reset ();
  FakePorts.add_remote_file
    (make_file ~parents:[ "folder-two" ] "rid-child" "child/one.txt");
  FakePorts.add_remote_file
    (make_file ~parents:[ "folder-one" ] "folder-two" "folder\000two");
  FakePorts.add_remote_file (make_file ~parents:[ "root" ] "folder-one" "top");
  let resource = get_resource_with_id "rid-child" in
  assert_equal "/top/folder_two/child_one.txt" resource.CacheData.Resource.path;
  assert_before "fetch:rid-child" "fetch:folder-two" !FakePorts.trace;
  assert_before "fetch:folder-two" "fetch:folder-one" !FakePorts.trace;
  assert_before "clean:child/one.txt" "clean:folder\000two" !FakePorts.trace;
  assert_before "clean:folder\000two" "clean:top" !FakePorts.trace

let test_shared_target_without_parents_uses_shared_with_me_prefix () =
  FakePorts.reset ();
  FakePorts.add_remote_file
    (make_file ~parents:[] ~shared:true "rid-shared" "shared.txt");
  let resource = get_resource_with_id "rid-shared" in
  assert_equal "/.shared/shared.txt" resource.CacheData.Resource.path;
  assert_equal (Some "rid-shared") resource.CacheData.Resource.remote_id

let test_shared_target_with_parents_follows_parent_chain () =
  FakePorts.reset ();
  FakePorts.add_remote_file
    (make_file ~parents:[ "folder-one" ] ~shared:true "rid-shared" "shared.txt");
  FakePorts.add_remote_file
    (make_file ~parents:[ "root" ] "folder-one" "parent");
  let resource = get_resource_with_id "rid-shared" in
  assert_equal "/parent/shared.txt" resource.CacheData.Resource.path

let test_server_result_is_not_inserted_into_cache () =
  FakePorts.reset ();
  FakePorts.add_remote_file (make_file "rid-child" "child.txt");
  ignore (get_resource_with_id "rid-child");
  assert_equal None
    (FakePorts.select_first_resource_with_remote_id dummy_cache "rid-child")

let test_fetch_exception_propagates () =
  FakePorts.reset ();
  FakePorts.fetch_failure := Some (Failure "fetch failed");
  assert_raises (Failure "fetch failed") (fun () ->
      ignore (get_resource_with_id "rid-child"))

let test_no_parent_non_shared_preserves_current_failure () =
  FakePorts.reset ();
  FakePorts.add_remote_file
    (make_file ~parents:[] ~shared:false "rid-orphan" "orphan.txt");
  assert_raises (Failure "hd") (fun () ->
      ignore (get_resource_with_id_from_server "rid-orphan"))

let suite =
  "DriveResourceById"
  >::: [
         "cached resource returns without server lookup"
         >:: test_cached_resource_returns_without_server_lookup;
         "cache miss reconstructs direct child under root"
         >:: test_cache_miss_reconstructs_direct_child_under_root;
         "root remote id returns well-known root"
         >:: test_root_remote_id_returns_well_known_root;
         "nested parent chain reconstructs clean absolute path"
         >:: test_nested_parent_chain_reconstructs_clean_absolute_path;
         "shared target without parents uses shared-with-me prefix"
         >:: test_shared_target_without_parents_uses_shared_with_me_prefix;
         "shared target with parents follows parent chain"
         >:: test_shared_target_with_parents_follows_parent_chain;
         "server result is not inserted into cache"
         >:: test_server_result_is_not_inserted_into_cache;
         "fetch exception propagates" >:: test_fetch_exception_propagates;
         "no-parent non-shared preserves current failure"
         >:: test_no_parent_non_shared_preserves_current_failure;
       ]
