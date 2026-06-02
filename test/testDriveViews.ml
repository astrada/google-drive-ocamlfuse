open OUnit
open GapiMonad

let run_session = DriveTestSupport.run_session

let base_mountpoint_stats =
  let stats = Unix.LargeFile.stat "." in
  {
    stats with
    Unix.LargeFile.st_kind = Unix.S_DIR;
    st_perm = 0o777;
    st_uid = 1000;
    st_gid = 1000;
    st_size = 0L;
    st_atime = 10.;
    st_mtime = 20.;
    st_ctime = 20.;
  }

let default_runtime ?(config = Config.default) ?(mountpoint_path = "/mnt/gd") ()
    =
  {
    DriveViews.cache = DriveTestSupport.dummy_cache;
    config;
    mountpoint_path;
    mountpoint_stats = base_mountpoint_stats;
  }

module FakePorts = struct
  type materialize_response = Return of string | Raise_file_not_found

  let resources = Hashtbl.create 32
  let stats_by_path = Hashtbl.create 16
  let updated_resources = ref []
  let resource_with_id_lookups = ref []
  let materialize_response = ref (Return "")

  let reset () =
    updated_resources := [];
    resource_with_id_lookups := [];
    materialize_response := Return "";
    Hashtbl.reset resources;
    Hashtbl.reset stats_by_path

  let key path trashed = Printf.sprintf "%b:%s" trashed path

  let add_resource resource =
    let trashed = Option.default false resource.CacheData.Resource.trashed in
    Hashtbl.replace resources
      (key resource.CacheData.Resource.path trashed)
      resource

  let find_resource path trashed =
    try Some (Hashtbl.find resources (key path trashed))
    with Not_found -> None

  let find_by_remote_id remote_id =
    Hashtbl.to_seq_values resources
    |> List.of_seq
    |> List.find_opt (fun resource ->
        resource.CacheData.Resource.remote_id = Some remote_id)

  let add_stat path stats = Hashtbl.replace stats_by_path path stats
  let get_path_in_cache = Drive.get_path_in_cache

  let get_resource path trashed =
    match find_resource path trashed with
    | Some resource -> SessionM.return resource
    | None -> Utils.raise_m Drive.File_not_found

  let get_resource_with_id remote_id _cache =
    resource_with_id_lookups := !resource_with_id_lookups @ [ remote_id ];
    match find_by_remote_id remote_id with
    | Some resource -> SessionM.return resource
    | None -> Utils.raise_m Drive.File_not_found

  let update_cached_resource _cache resource =
    updated_resources := resource :: !updated_resources;
    add_resource resource

  let materialize_for_stat _resource =
    match !materialize_response with
    | Return path -> SessionM.return path
    | Raise_file_not_found -> Utils.raise_m Drive.File_not_found

  let file_exists path = Hashtbl.mem stats_by_path path
  let stat_file path = Hashtbl.find stats_by_path path

  let is_file_read_only resource =
    not (Option.default true resource.CacheData.Resource.can_edit)

  let is_lost_and_found_root path trashed config =
    if trashed || not config.Config.lost_and_found then false
    else path = "/lost+found"

  let is_shared_with_me_root path trashed _config =
    if trashed then false else path = "/.shared"
end

module Views = DriveViews.Make (FakePorts)

let make_resource ?(id = 1L) ?remote_id ?mime_type ?size ?can_edit
    ?file_mode_bits ?uid ?gid ?link_target ?target_id ?(trashed = false)
    ?(state = CacheData.Resource.State.Synchronized) ?(modified_time = 20.)
    ?(viewed_by_me_time = 10.) path =
  let resource = Drive.create_resource path in
  {
    resource with
    id;
    remote_id;
    name = Some (Filename.basename path);
    mime_type;
    size;
    can_edit;
    trashed = Some trashed;
    file_mode_bits;
    uid;
    gid;
    link_target;
    target_id;
    state;
    modified_time = Some modified_time;
    viewed_by_me_time = Some viewed_by_me_time;
    version = Some 1L;
  }

let test_read_link_shortcut_reconstructs_and_caches_target () =
  FakePorts.reset ();
  let source =
    make_resource ~id:1L ~remote_id:"rid-shortcut"
      ~mime_type:Drive.shortcut_mime_type ~target_id:"rid-target" "/shortcut"
  in
  let target =
    make_resource ~id:2L ~remote_id:"rid-target" ~mime_type:"text/plain"
      "/folder/file.txt"
  in
  FakePorts.add_resource source;
  FakePorts.add_resource target;
  let runtime = default_runtime ~mountpoint_path:"/mnt/gd/" () in
  let link_target = run_session (Views.read_link runtime "/shortcut") in
  assert_equal "/mnt/gd/folder/file.txt" link_target;
  let updated = FakePorts.find_resource "/shortcut" false |> Option.get in
  assert_equal (Some "/mnt/gd/folder/file.txt")
    updated.CacheData.Resource.link_target

let test_read_link_stored_symlink_uses_cached_target () =
  FakePorts.reset ();
  let source =
    make_resource ~remote_id:"rid-link" ~mime_type:"text/plain"
      ~link_target:"../target" "/link"
  in
  FakePorts.add_resource source;
  let runtime = default_runtime () in
  let link_target = run_session (Views.read_link runtime "/link") in
  assert_equal "../target" link_target;
  assert_equal [] !FakePorts.resource_with_id_lookups;
  assert_equal [] !FakePorts.updated_resources

let test_read_link_shortcut_with_cached_target_avoids_extra_lookup () =
  FakePorts.reset ();
  let source =
    make_resource ~remote_id:"rid-shortcut" ~mime_type:Drive.shortcut_mime_type
      ~link_target:"/mnt/gd/already-known" ~target_id:"rid-target" "/shortcut"
  in
  FakePorts.add_resource source;
  let runtime = default_runtime () in
  let link_target = run_session (Views.read_link runtime "/shortcut") in
  assert_equal "/mnt/gd/already-known" link_target;
  assert_equal [] !FakePorts.resource_with_id_lookups;
  assert_equal [] !FakePorts.updated_resources

let test_read_link_non_link_resource_is_invalid () =
  FakePorts.reset ();
  let resource =
    make_resource ~remote_id:"rid-file" ~mime_type:"text/plain" "/file.txt"
  in
  FakePorts.add_resource resource;
  let runtime = default_runtime () in
  assert_raises Drive.Invalid_operation (fun () ->
      run_session (Views.read_link runtime "/file.txt"))

let test_get_attr_root_returns_mountpoint_stats_unchanged () =
  FakePorts.reset ();
  let runtime = default_runtime () in
  let stat = run_session (Views.get_attr runtime "/") in
  assert_equal base_mountpoint_stats.Unix.LargeFile.st_kind
    stat.Unix.LargeFile.st_kind;
  assert_equal base_mountpoint_stats.Unix.LargeFile.st_perm
    stat.Unix.LargeFile.st_perm;
  assert_equal base_mountpoint_stats.Unix.LargeFile.st_uid
    stat.Unix.LargeFile.st_uid;
  assert_equal base_mountpoint_stats.Unix.LargeFile.st_gid
    stat.Unix.LargeFile.st_gid

let test_get_attr_shared_root_is_mountpoint_read_only () =
  FakePorts.reset ();
  let runtime = default_runtime () in
  let stat = run_session (Views.get_attr runtime "/.shared") in
  assert_equal Unix.S_DIR stat.Unix.LargeFile.st_kind;
  assert_equal 0o555 stat.Unix.LargeFile.st_perm

let test_get_attr_lost_and_found_root_uses_mountpoint_stats () =
  FakePorts.reset ();
  let config = { Config.default with lost_and_found = true } in
  let runtime = default_runtime ~config () in
  let stat = run_session (Views.get_attr runtime "/lost+found") in
  assert_equal base_mountpoint_stats.Unix.LargeFile.st_kind
    stat.Unix.LargeFile.st_kind;
  assert_equal base_mountpoint_stats.Unix.LargeFile.st_perm
    stat.Unix.LargeFile.st_perm

let test_get_attr_regular_file_masks_permissions () =
  FakePorts.reset ();
  let config = { Config.default with umask = 0o022 } in
  let resource =
    make_resource ~remote_id:"rid-file" ~mime_type:"text/plain" ~size:123L
      ~can_edit:false ~file_mode_bits:(Int64.of_int 0o100664) "/file.txt"
  in
  FakePorts.add_resource resource;
  let runtime = default_runtime ~config () in
  let stat = run_session (Views.get_attr runtime "/file.txt") in
  assert_equal Unix.S_REG stat.Unix.LargeFile.st_kind;
  assert_equal 0o444 stat.Unix.LargeFile.st_perm;
  assert_equal 123L stat.Unix.LargeFile.st_size;
  assert_equal 1000 stat.Unix.LargeFile.st_uid;
  assert_equal 1000 stat.Unix.LargeFile.st_gid

let test_get_attr_folder_uses_directory_kind_and_fallback_size () =
  FakePorts.reset ();
  let resource =
    make_resource ~remote_id:"rid-folder" ~mime_type:Drive.folder_mime_type
      "/folder"
  in
  FakePorts.add_resource resource;
  let runtime = default_runtime () in
  let stat = run_session (Views.get_attr runtime "/folder") in
  assert_equal Unix.S_DIR stat.Unix.LargeFile.st_kind;
  assert_equal 4096L stat.Unix.LargeFile.st_size

let test_get_attr_shortcut_uses_resolved_target_length () =
  FakePorts.reset ();
  let source =
    make_resource ~id:1L ~remote_id:"rid-shortcut"
      ~mime_type:Drive.shortcut_mime_type ~target_id:"rid-target" "/shortcut"
  in
  let target =
    make_resource ~id:2L ~remote_id:"rid-target" ~mime_type:"text/plain"
      "/folder/file.txt"
  in
  FakePorts.add_resource source;
  FakePorts.add_resource target;
  let runtime = default_runtime ~mountpoint_path:"/mnt/gd/" () in
  let stat = run_session (Views.get_attr runtime "/shortcut") in
  assert_equal Unix.S_LNK stat.Unix.LargeFile.st_kind;
  assert_equal
    (Int64.of_int (String.length "/mnt/gd/folder/file.txt"))
    stat.Unix.LargeFile.st_size

let test_get_attr_stored_symlink_uses_target_length () =
  FakePorts.reset ();
  let resource =
    make_resource ~remote_id:"rid-link" ~mime_type:"text/plain"
      ~link_target:"../target" ~file_mode_bits:(Int64.of_int 0o120777) "/link"
  in
  FakePorts.add_resource resource;
  let runtime = default_runtime () in
  let stat = run_session (Views.get_attr runtime "/link") in
  assert_equal Unix.S_LNK stat.Unix.LargeFile.st_kind;
  assert_equal
    (Int64.of_int (String.length "../target"))
    stat.Unix.LargeFile.st_size;
  assert_equal [] !FakePorts.resource_with_id_lookups

let test_get_attr_document_ignores_file_not_found_from_materialization () =
  FakePorts.reset ();
  FakePorts.materialize_response := Raise_file_not_found;
  let config = { Config.default with download_docs = true } in
  let resource =
    make_resource ~remote_id:"rid-doc"
      ~mime_type:"application/vnd.google-apps.document" ~size:777L "/doc"
  in
  FakePorts.add_resource resource;
  let runtime = default_runtime ~config () in
  let stat = run_session (Views.get_attr runtime "/doc") in
  assert_equal Unix.S_REG stat.Unix.LargeFile.st_kind;
  assert_equal 777L stat.Unix.LargeFile.st_size

let test_get_attr_trash_root_is_mountpoint_read_only () =
  FakePorts.reset ();
  let runtime = default_runtime () in
  let stat = run_session (Views.get_attr runtime "/.Trash") in
  assert_equal Unix.S_DIR stat.Unix.LargeFile.st_kind;
  assert_equal 0o555 stat.Unix.LargeFile.st_perm

let test_opendir_uses_lookup_only () =
  FakePorts.reset ();
  let resource =
    make_resource ~remote_id:"rid-dir" ~mime_type:Drive.folder_mime_type "/dir"
  in
  FakePorts.add_resource resource;
  let runtime = default_runtime () in
  run_session (Views.opendir runtime "/dir")

let test_opendir_missing_path_propagates_lookup_failure () =
  FakePorts.reset ();
  let runtime = default_runtime () in
  assert_raises Drive.File_not_found (fun () ->
      run_session (Views.opendir runtime "/missing"))

let suite =
  "DriveViews test"
  >::: [
         "test_read_link_shortcut_reconstructs_and_caches_target"
         >:: test_read_link_shortcut_reconstructs_and_caches_target;
         "test_read_link_stored_symlink_uses_cached_target"
         >:: test_read_link_stored_symlink_uses_cached_target;
         "test_read_link_shortcut_with_cached_target_avoids_extra_lookup"
         >:: test_read_link_shortcut_with_cached_target_avoids_extra_lookup;
         "test_read_link_non_link_resource_is_invalid"
         >:: test_read_link_non_link_resource_is_invalid;
         "test_get_attr_root_returns_mountpoint_stats_unchanged"
         >:: test_get_attr_root_returns_mountpoint_stats_unchanged;
         "test_get_attr_shared_root_is_mountpoint_read_only"
         >:: test_get_attr_shared_root_is_mountpoint_read_only;
         "test_get_attr_lost_and_found_root_uses_mountpoint_stats"
         >:: test_get_attr_lost_and_found_root_uses_mountpoint_stats;
         "test_get_attr_regular_file_masks_permissions"
         >:: test_get_attr_regular_file_masks_permissions;
         "test_get_attr_folder_uses_directory_kind_and_fallback_size"
         >:: test_get_attr_folder_uses_directory_kind_and_fallback_size;
         "test_get_attr_shortcut_uses_resolved_target_length"
         >:: test_get_attr_shortcut_uses_resolved_target_length;
         "test_get_attr_stored_symlink_uses_target_length"
         >:: test_get_attr_stored_symlink_uses_target_length;
         "test_get_attr_document_ignores_file_not_found_from_materialization"
         >:: test_get_attr_document_ignores_file_not_found_from_materialization;
         "test_get_attr_trash_root_is_mountpoint_read_only"
         >:: test_get_attr_trash_root_is_mountpoint_read_only;
         "test_opendir_uses_lookup_only" >:: test_opendir_uses_lookup_only;
         "test_opendir_missing_path_propagates_lookup_failure"
         >:: test_opendir_missing_path_propagates_lookup_failure;
       ]
