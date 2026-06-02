open OUnit
open GapiMonad
open GapiMonad.SessionM.Infix
module File = GapiDriveV3Model.File

let run_session = DriveTestSupport.run_session

let default_runtime ?(config = Config.default) () =
  DriveTestSupport.base_runtime ~config ()

module FakePorts = struct
  let resources = Hashtbl.create 64
  let query_results = Hashtbl.create 16
  let listed_queries = ref []
  let cache_hits = Hashtbl.create 16

  let reset () =
    Hashtbl.reset resources;
    Hashtbl.reset query_results;
    Hashtbl.reset cache_hits;
    listed_queries := []

  let key path trashed = Printf.sprintf "%b:%s" trashed path

  let add_resource resource =
    let trashed = Option.default false resource.CacheData.Resource.trashed in
    Hashtbl.replace resources
      (key resource.CacheData.Resource.path trashed)
      resource

  let add_query_result query files = Hashtbl.replace query_results query files

  let set_cache_hit path trashed hit =
    Hashtbl.replace cache_hits (key path trashed) hit

  let get_path_in_cache = Drive.get_path_in_cache

  let get_resource path trashed =
    match Hashtbl.find_opt resources (key path trashed) with
    | Some resource -> SessionM.return resource
    | None -> Utils.raise_m Drive.File_not_found

  let get_folder_id path trashed =
    get_resource path trashed >>= fun resource ->
    SessionM.return (Option.get resource.CacheData.Resource.remote_id)

  let is_lost_and_found_root path trashed config =
    if trashed || not config.Config.lost_and_found then false
    else path = "/lost+found"

  let is_shared_with_me_root path trashed _config =
    if trashed then false else path = "/.shared"

  let check_resource_in_cache _cache path trashed =
    match Hashtbl.find_opt cache_hits (key path trashed) with
    | Some hit -> hit
    | None -> false

  let select_resources_with_parent_path _cache parent_path trashed =
    Hashtbl.to_seq_values resources
    |> List.of_seq
    |> List.filter (fun resource ->
        resource.CacheData.Resource.parent_path = parent_path
        && Option.default false resource.CacheData.Resource.trashed = trashed)

  let list_files query =
    listed_queries := !listed_queries @ [ query ];
    SessionM.return
      (match Hashtbl.find_opt query_results query with
      | Some files -> files
      | None -> [])

  let build_resource_tables parent_path trashed =
    let resources =
      select_resources_with_parent_path DriveTestSupport.dummy_cache parent_path
        trashed
    in
    let filename_table = Hashtbl.create 16 in
    let remote_id_table = Hashtbl.create 16 in
    List.iter
      (fun resource ->
        Hashtbl.replace filename_table
          (Filename.basename resource.CacheData.Resource.path)
          0;
        Hashtbl.add remote_id_table
          (Option.get resource.CacheData.Resource.remote_id)
          resource)
      resources;
    (filename_table, remote_id_table)

  let update_resource_from_file resource file =
    let path =
      match resource.CacheData.Resource.name with
      | None -> resource.CacheData.Resource.path
      | Some cached_name ->
          if cached_name = file.File.name then resource.CacheData.Resource.path
          else
            Filename.concat
              (Filename.dirname resource.CacheData.Resource.path)
              file.File.name
    in
    {
      resource with
      CacheData.Resource.remote_id = Some file.File.id;
      name = Some file.File.name;
      mime_type = Some file.File.mimeType;
      trashed = Some file.File.trashed;
      parent_path = Filename.dirname path;
      path;
      size = Some file.File.size;
      version = Some 1L;
    }

  let get_unique_filename_from_file file filename_table =
    let base = file.File.name in
    match Hashtbl.find_opt filename_table base with
    | None ->
        Hashtbl.replace filename_table base 1;
        base
    | Some count ->
        let filename = Printf.sprintf "%s (%d)" base (count + 1) in
        Hashtbl.replace filename_table base (count + 1);
        Hashtbl.replace filename_table filename 1;
        filename

  let create_resource = Drive.create_resource

  let insert_resources _cache new_resources parent_path trashed =
    let old_keys =
      Hashtbl.to_seq resources |> List.of_seq
      |> List.filter_map (fun (k, resource) ->
          if
            resource.CacheData.Resource.parent_path = parent_path
            && Option.default false resource.CacheData.Resource.trashed
               = trashed
          then Some k
          else None)
    in
    List.iter (Hashtbl.remove resources) old_keys;
    List.iter add_resource new_resources;
    new_resources

  let update_cached_resource _cache resource = add_resource resource
  let current_time () = 1234.0
end

module DirectoryReads = DriveDirectoryReads.Make (FakePorts)

let make_resource ?(id = 1L) ?remote_id ?mime_type ?(trashed = false) path =
  let resource = Drive.create_resource path in
  {
    resource with
    id;
    remote_id;
    name = Some (Filename.basename path);
    mime_type;
    trashed = Some trashed;
    size = Some 0L;
    version = Some 1L;
  }

let make_file ?(mime_type = "text/plain") ?(parents = [ "root" ])
    ?(trashed = false) ?(explicitly_trashed = false) id name =
  {
    File.empty with
    id;
    name;
    mimeType = mime_type;
    parents;
    trashed;
    explicitlyTrashed = explicitly_trashed;
    size = 0L;
  }

let sort_strings = List.sort String.compare

let test_cache_hit_returns_cached_child_names () =
  FakePorts.reset ();
  FakePorts.set_cache_hit "/docs" false true;
  FakePorts.add_resource
    (make_resource ~remote_id:"rid-folder" ~mime_type:Drive.folder_mime_type
       "/docs");
  FakePorts.add_resource (make_resource ~remote_id:"rid-a" "/docs/a.txt");
  FakePorts.add_resource (make_resource ~remote_id:"rid-b" "/docs/b.txt");
  let runtime = default_runtime () in
  let names = run_session (DirectoryReads.read_dir runtime "/docs") in
  assert_equal [ "a.txt"; "b.txt" ] (sort_strings names);
  assert_equal [] !FakePorts.listed_queries

let test_lost_and_found_filters_parentless_files () =
  FakePorts.reset ();
  let config = { Config.default with lost_and_found = true } in
  FakePorts.add_resource
    (make_resource ~remote_id:"rid-laf" ~mime_type:Drive.folder_mime_type
       "/lost+found");
  FakePorts.add_query_result "'me' in owners"
    [
      make_file ~parents:[] "rid-a" "a.txt";
      make_file ~parents:[ "root" ] "rid-b" "b.txt";
    ];
  let runtime = default_runtime ~config () in
  let names = run_session (DirectoryReads.read_dir runtime "/lost+found") in
  assert_equal [ "a.txt" ] names;
  assert_equal [ "'me' in owners" ] !FakePorts.listed_queries

let test_shared_root_uses_shared_query () =
  FakePorts.reset ();
  FakePorts.add_resource
    (make_resource ~remote_id:"rid-shared" ~mime_type:Drive.folder_mime_type
       "/.shared");
  FakePorts.add_query_result "sharedWithMe = true"
    [ make_file "rid-s1" "shared.txt" ];
  let runtime = default_runtime () in
  let names = run_session (DirectoryReads.read_dir runtime "/.shared") in
  assert_equal [ "shared.txt" ] names;
  assert_equal [ "sharedWithMe = true" ] !FakePorts.listed_queries

let test_trash_root_includes_explicitly_trashed_files () =
  FakePorts.reset ();
  FakePorts.add_resource
    (make_resource ~remote_id:"root" ~mime_type:Drive.folder_mime_type
       ~trashed:true "/");
  FakePorts.add_query_result "'root' in parents and trashed = true"
    [ make_file ~parents:[ "root" ] ~trashed:true "rid-a" "root-child.txt" ];
  FakePorts.add_query_result "not 'root' in parents and trashed = true"
    [
      make_file ~parents:[ "other" ] ~trashed:true ~explicitly_trashed:true
        "rid-b" "explicit.txt";
      make_file ~parents:[ "other" ] ~trashed:true ~explicitly_trashed:false
        "rid-c" "skip.txt";
    ];
  let runtime = default_runtime () in
  let names = run_session (DirectoryReads.read_dir runtime "/.Trash") in
  assert_equal [ "explicit.txt"; "root-child.txt" ] (sort_strings names)

let test_duplicate_names_are_disambiguated_for_new_resources () =
  FakePorts.reset ();
  FakePorts.add_resource
    (make_resource ~remote_id:"root" ~mime_type:Drive.folder_mime_type "/");
  FakePorts.add_query_result "'root' in parents and trashed = false"
    [ make_file "rid-a" "dup.txt"; make_file "rid-b" "dup.txt" ];
  let runtime = default_runtime () in
  let names = run_session (DirectoryReads.read_dir runtime "/") in
  assert_equal
    [ ".Trash"; ".shared"; "dup.txt"; "dup.txt (2)" ]
    (sort_strings names)

let suite =
  "DriveDirectoryReads test"
  >::: [
         "test_cache_hit_returns_cached_child_names"
         >:: test_cache_hit_returns_cached_child_names;
         "test_lost_and_found_filters_parentless_files"
         >:: test_lost_and_found_filters_parentless_files;
         "test_shared_root_uses_shared_query"
         >:: test_shared_root_uses_shared_query;
         "test_trash_root_includes_explicitly_trashed_files"
         >:: test_trash_root_includes_explicitly_trashed_files;
         "test_duplicate_names_are_disambiguated_for_new_resources"
         >:: test_duplicate_names_are_disambiguated_for_new_resources;
       ]
