open OUnit
open GapiMonad
module File = GapiDriveV3Model.File

let run_session = DriveTestSupport.run_session

let default_runtime ?(config = Config.default) ?(mountpoint_path = "/mnt/gd")
    ?(skip_trash = false) () =
  {
    DriveMutations.cache = DriveTestSupport.dummy_cache;
    config;
    mountpoint_path;
    skip_trash;
  }

module FakePorts = struct
  let trace = ref []
  let resources = Hashtbl.create 32
  let remote_create_calls = ref []
  let remote_update_calls = ref []
  let remote_delete_calls = ref []
  let remote_move_calls = ref []
  let replace_target_contents_calls = ref []
  let children_present = ref false
  let next_id = ref 1L
  let recompute_path_override = ref None

  let reset () =
    trace := [];
    remote_create_calls := [];
    remote_update_calls := [];
    remote_delete_calls := [];
    remote_move_calls := [];
    replace_target_contents_calls := [];
    children_present := false;
    next_id := 1L;
    recompute_path_override := None;
    Hashtbl.reset resources

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

  let find_by_remote_id remote_id =
    Hashtbl.to_seq_values resources
    |> List.of_seq
    |> List.find_opt (fun resource ->
        resource.CacheData.Resource.remote_id = Some remote_id)

  let server_file_of_resource ?(trashed = false) resource =
    let now = GapiDate.now () in
    {
      File.empty with
      id = Option.get resource.CacheData.Resource.remote_id;
      name = Option.default "" resource.CacheData.Resource.name;
      mimeType = Option.default "" resource.CacheData.Resource.mime_type;
      createdTime = now;
      modifiedTime = now;
      viewedByMeTime = now;
      size = Option.default 0L resource.CacheData.Resource.size;
      trashed;
      explicitlyTrashed = trashed;
      version = Option.default 1L resource.CacheData.Resource.version;
      capabilities = { File.Capabilities.empty with canEdit = true };
      appProperties = [];
    }

  let max_link_target_length = Drive.max_link_target_length
  let json_length = Drive.json_length
  let is_lost_and_found = Drive.is_lost_and_found

  let is_lost_and_found_root path trashed config =
    if trashed || not config.Config.lost_and_found then false
    else path = "/lost+found"

  let get_path_in_cache = Drive.get_path_in_cache
  let is_filesystem_read_only () = false
  let create_resource = Drive.create_resource
  let clean_document_extension name _resource _config = name

  let recompute_path resource name =
    match !recompute_path_override with
    | Some path -> path
    | None -> Filename.concat resource.CacheData.Resource.parent_path name

  let update_resource_from_file ?state ?link_target resource file =
    let link_target =
      match link_target with
      | None -> resource.CacheData.Resource.link_target
      | Some target -> Some target
    in
    let path =
      match resource.CacheData.Resource.name with
      | Some cached_name when cached_name <> file.File.name ->
          Filename.concat resource.CacheData.Resource.parent_path file.File.name
      | _ -> resource.CacheData.Resource.path
    in
    let parent_path = Filename.dirname path in
    let state = Option.default resource.CacheData.Resource.state state in
    {
      resource with
      CacheData.Resource.remote_id = Some file.File.id;
      name = Some file.File.name;
      mime_type = Some file.File.mimeType;
      size = Some file.File.size;
      version = Some file.File.version;
      trashed = Some file.File.trashed;
      path;
      parent_path;
      state;
      link_target;
    }

  let get_resource path trashed =
    match find_resource path trashed with
    | Some resource ->
        record ("get_resource:" ^ path ^ ":" ^ string_of_bool trashed);
        SessionM.return resource
    | None -> Utils.raise_m DriveMutations.File_not_found

  let build_resource_keys_header_from_resource =
    Drive.build_resource_keys_header_from_resource

  let build_resource_keys_header_from_resources _resources = []

  let insert_resource_into_cache ?state ?link_target _cache resource file =
    let resource =
      update_resource_from_file ?state ?link_target resource file
    in
    let inserted = { resource with CacheData.Resource.id = !next_id } in
    next_id := Int64.succ !next_id;
    add_resource inserted;
    record ("insert:" ^ inserted.CacheData.Resource.path);
    inserted

  let update_cached_resource _cache resource =
    let ids_to_remove =
      Hashtbl.to_seq resources |> List.of_seq
      |> List.filter_map (fun (k, existing) ->
          if
            existing.CacheData.Resource.id = resource.CacheData.Resource.id
            || existing.CacheData.Resource.remote_id
               = resource.CacheData.Resource.remote_id
          then Some k
          else None)
    in
    List.iter (Hashtbl.remove resources) ids_to_remove;
    add_resource resource;
    record ("update:" ^ resource.CacheData.Resource.path)

  let delete_cached_resource resource =
    let path = resource.CacheData.Resource.path in
    Hashtbl.remove resources (key path false);
    Hashtbl.remove resources (key path true);
    record ("delete_cached:" ^ path)

  let delete_all_with_parent_path _cache parent_path trashed =
    record (Printf.sprintf "delete_children:%s:%b" parent_path trashed)

  let trash_all_with_parent_path _cache parent_path =
    record ("trash_children:" ^ parent_path)

  let invalidate_trash_bin _cache = record "invalidate_trash_bin"

  let delete_not_found_resource_with_path _cache path =
    record ("delete_not_found:" ^ path)

  let select_first_resource_with_remote_id _cache remote_id =
    find_by_remote_id remote_id

  let remote_create file =
    remote_create_calls := !remote_create_calls @ [ file ];
    record ("remote_create:" ^ file.File.name);
    let now = GapiDate.now () in
    let remote_id = "rid-" ^ string_of_int (List.length !remote_create_calls) in
    let created_file =
      {
        file with
        File.id = remote_id;
        createdTime = now;
        modifiedTime = now;
        viewedByMeTime = now;
        size = 0L;
        trashed = false;
        explicitlyTrashed = false;
        version = 1L;
        capabilities = { File.Capabilities.empty with canEdit = true };
      }
    in
    SessionM.return created_file

  let remote_update ~custom_headers:_ ~fileId file_patch =
    remote_update_calls := !remote_update_calls @ [ (fileId, file_patch) ];
    record ("remote_update:" ^ fileId);
    let file =
      match find_by_remote_id fileId with
      | Some resource ->
          let existing = server_file_of_resource resource in
          {
            existing with
            File.name =
              (if file_patch.File.name = "" then existing.File.name
               else file_patch.File.name);
            mimeType =
              (if file_patch.File.mimeType = "" then existing.File.mimeType
               else file_patch.File.mimeType);
            appProperties =
              (if file_patch.File.appProperties = [] then
                 existing.File.appProperties
               else file_patch.File.appProperties);
            trashed =
              (if file_patch.File.trashed then true else existing.File.trashed);
            explicitlyTrashed = file_patch.File.trashed;
          }
      | None ->
          {
            File.empty with
            id = fileId;
            trashed = file_patch.File.trashed;
            explicitlyTrashed = file_patch.File.trashed;
          }
    in
    SessionM.return file

  let remote_move ~custom_headers:_ ~addParents ~fileId ~removeParents file =
    remote_move_calls :=
      !remote_move_calls @ [ (fileId, addParents, removeParents) ];
    record ("remote_move:" ^ fileId);
    let moved_file =
      match find_by_remote_id fileId with
      | Some resource -> server_file_of_resource resource
      | None -> { file with File.id = fileId }
    in
    SessionM.return moved_file

  let replace_target_contents ~source ~target =
    replace_target_contents_calls :=
      !replace_target_contents_calls
      @ [ (source.CacheData.Resource.path, target.CacheData.Resource.path) ];
    record
      ("replace_target_contents:" ^ source.CacheData.Resource.path ^ ":"
     ^ target.CacheData.Resource.path);
    SessionM.return ()

  let remote_delete ~custom_headers:_ ~fileId =
    remote_delete_calls := !remote_delete_calls @ [ fileId ];
    record ("remote_delete:" ^ fileId);
    SessionM.return ()

  let check_if_empty_remote remote_id is_folder _trashed =
    record (Printf.sprintf "check_empty:%s:%b" remote_id is_folder);
    if is_folder && !children_present then
      raise DriveMutations.Directory_not_empty
    else SessionM.return ()
end

module Mutations = DriveMutations.Make (FakePorts)

let mknod runtime path mode =
  Mutations.create_remote_resource runtime false path mode

let mkdir runtime path mode =
  Mutations.create_remote_resource runtime true path mode

let symlink runtime target linkpath =
  Mutations.create_remote_resource runtime ~link_target:target false linkpath
    0o777

let unlink runtime path = Mutations.delete_remote_resource runtime false path
let rmdir runtime path = Mutations.delete_remote_resource runtime true path

let make_resource ?(trashed = false) ?(mime_type = Drive.folder_mime_type)
    ?(name = "node") path remote_id =
  let resource = Drive.create_resource path in
  {
    resource with
    CacheData.Resource.id = 1L;
    remote_id = Some remote_id;
    name = Some name;
    mime_type = Some mime_type;
    size = Some 0L;
    trashed = Some trashed;
    version = Some 1L;
    can_edit = Some true;
    modified_time = Some 0.;
    created_time = Some 0.;
    viewed_by_me_time = Some 0.;
    state = CacheData.Resource.State.Synchronized;
  }

let with_reset f =
  FakePorts.reset ();
  f ()

let test_create_regular_file_inserts_into_cache () =
  with_reset (fun () ->
      FakePorts.add_resource (make_resource "/" "root");
      let runtime = default_runtime () in
      run_session (mknod runtime "/notes.txt" 0o644);
      assert_equal 1 (List.length !FakePorts.remote_create_calls);
      let created_file = List.hd !FakePorts.remote_create_calls in
      assert_equal "notes.txt" created_file.File.name;
      assert_equal (string_of_int 0o644)
        (List.assoc "mode" created_file.File.appProperties);
      assert_bool "expected inserted resource"
        (Option.is_some (FakePorts.find_resource "/notes.txt" false));
      assert_bool "expected not-found cleanup"
        (List.mem "delete_not_found:/notes.txt" !FakePorts.trace))

let test_create_shortcut_from_relative_target () =
  with_reset (fun () ->
      FakePorts.add_resource (make_resource "/" "root");
      FakePorts.add_resource (make_resource ~name:"links" "/links" "links-id");
      FakePorts.add_resource
        (make_resource ~mime_type:Drive.folder_mime_type ~name:"target"
           "/target" "target-id");
      let runtime = default_runtime () in
      run_session (symlink runtime "../target" "/links/link");
      let created_file = List.hd !FakePorts.remote_create_calls in
      assert_equal Drive.shortcut_mime_type created_file.File.mimeType;
      assert_equal "target-id"
        created_file.File.shortcutDetails.File.ShortcutDetails.targetId)

let test_create_stored_symlink_for_external_target () =
  with_reset (fun () ->
      FakePorts.add_resource (make_resource "/" "root");
      FakePorts.add_resource (make_resource ~name:"links" "/links" "links-id");
      let runtime = default_runtime () in
      run_session (symlink runtime "/tmp/external" "/links/link");
      let created_file = List.hd !FakePorts.remote_create_calls in
      assert_equal "" created_file.File.mimeType;
      assert_equal "/tmp/external"
        (List.assoc "l" created_file.File.appProperties);
      assert_equal (string_of_int 0o120777)
        (List.assoc "mode" created_file.File.appProperties))

let test_create_in_trash_is_denied () =
  with_reset (fun () ->
      let runtime = default_runtime () in
      assert_raises DriveMutations.Permission_denied (fun () ->
          run_session (mknod runtime "/.Trash/file.txt" 0o644)))

let test_create_under_lost_and_found_is_denied () =
  with_reset (fun () ->
      let config = { Config.default with lost_and_found = true } in
      let runtime = default_runtime ~config () in
      assert_raises DriveMutations.Permission_denied (fun () ->
          run_session (mknod runtime "/lost+found/file.txt" 0o644)))

let test_create_shortcut_from_absolute_target_inside_mountpoint () =
  with_reset (fun () ->
      FakePorts.add_resource (make_resource "/" "root");
      FakePorts.add_resource (make_resource ~name:"links" "/links" "links-id");
      FakePorts.add_resource
        (make_resource ~mime_type:Drive.folder_mime_type ~name:"inside"
           "/inside/file" "target-id");
      let runtime = default_runtime ~mountpoint_path:"/mnt/gd/" () in
      run_session (symlink runtime "/mnt/gd/inside/file" "/links/link");
      let created_file = List.hd !FakePorts.remote_create_calls in
      assert_equal Drive.shortcut_mime_type created_file.File.mimeType;
      assert_equal "target-id"
        created_file.File.shortcutDetails.File.ShortcutDetails.targetId)

let test_create_stored_symlink_rejects_oversized_target () =
  with_reset (fun () ->
      FakePorts.add_resource (make_resource "/" "root");
      FakePorts.add_resource (make_resource ~name:"links" "/links" "links-id");
      let runtime = default_runtime () in
      let oversized_target = "/" ^ String.make 256 'a' in
      assert_raises DriveMutations.Invalid_operation (fun () ->
          run_session (symlink runtime oversized_target "/links/link")))

let test_delete_uses_trash_by_default () =
  with_reset (fun () ->
      FakePorts.add_resource
        (make_resource ~mime_type:"text/plain" ~name:"file.txt" "/file.txt"
           "file-id");
      let runtime = default_runtime () in
      run_session (unlink runtime "/file.txt");
      let resource = Option.get (FakePorts.find_resource "/file.txt" true) in
      assert_equal (Some true) resource.CacheData.Resource.trashed;
      assert_equal [ "file-id" ] (List.map fst !FakePorts.remote_update_calls);
      assert_bool "expected trash invalidation"
        (List.mem "invalidate_trash_bin" !FakePorts.trace))

let test_delete_can_skip_trash () =
  with_reset (fun () ->
      FakePorts.add_resource
        (make_resource ~mime_type:"text/plain" ~name:"file.txt" "/file.txt"
           "file-id");
      let runtime = default_runtime ~skip_trash:true () in
      run_session (unlink runtime "/file.txt");
      assert_equal [ "file-id" ] !FakePorts.remote_delete_calls;
      assert_bool "expected cache deletion"
        (Option.is_none (FakePorts.find_resource "/file.txt" false)))

let test_delete_from_trash_folder_can_delete_forever () =
  with_reset (fun () ->
      let config =
        { Config.default with delete_forever_in_trash_folder = true }
      in
      FakePorts.add_resource
        (make_resource ~trashed:true ~mime_type:"text/plain" ~name:"file.txt"
           "/file.txt" "file-id");
      let runtime = default_runtime ~config () in
      run_session (unlink runtime "/.Trash/file.txt");
      assert_equal [ "file-id" ] !FakePorts.remote_delete_calls)

let test_delete_non_empty_folder_is_rejected () =
  with_reset (fun () ->
      FakePorts.children_present := true;
      FakePorts.add_resource (make_resource ~name:"docs" "/docs" "folder-id");
      let runtime = default_runtime () in
      assert_raises DriveMutations.Directory_not_empty (fun () ->
          run_session (rmdir runtime "/docs")))

let test_delete_under_lost_and_found_is_denied () =
  with_reset (fun () ->
      let config = { Config.default with lost_and_found = true } in
      let runtime = default_runtime ~config () in
      assert_raises DriveMutations.Permission_denied (fun () ->
          run_session (unlink runtime "/lost+found/file.txt")))

let test_rename_within_same_parent_updates_name () =
  with_reset (fun () ->
      FakePorts.add_resource (make_resource "/" "root");
      FakePorts.add_resource (make_resource ~name:"docs" "/docs" "docs-id");
      FakePorts.add_resource
        (make_resource ~mime_type:"text/plain" ~name:"old.txt" "/docs/old.txt"
           "file-id");
      let runtime = default_runtime () in
      run_session (Mutations.rename runtime "/docs/old.txt" "/docs/new.txt");
      let resource =
        Option.get (FakePorts.find_resource "/docs/new.txt" false)
      in
      assert_equal (Some "new.txt") resource.CacheData.Resource.name;
      assert_bool "expected old path to disappear"
        (Option.is_none (FakePorts.find_resource "/docs/old.txt" false));
      let file_id, patch = List.hd !FakePorts.remote_update_calls in
      assert_equal "file-id" file_id;
      assert_equal "new.txt" patch.File.name)

let test_rename_across_trash_boundary_is_denied () =
  with_reset (fun () ->
      FakePorts.add_resource
        (make_resource ~mime_type:"text/plain" ~name:"file.txt" "/file.txt"
           "file-id");
      let runtime = default_runtime () in
      assert_raises DriveMutations.Permission_denied (fun () ->
          run_session (Mutations.rename runtime "/file.txt" "/.Trash/file.txt")))

let test_move_between_parents_updates_path () =
  with_reset (fun () ->
      FakePorts.add_resource (make_resource "/" "root");
      FakePorts.add_resource (make_resource ~name:"src" "/src" "src-id");
      FakePorts.add_resource (make_resource ~name:"dst" "/dst" "dst-id");
      FakePorts.add_resource
        (make_resource ~mime_type:"text/plain" ~name:"file.txt" "/src/file.txt"
           "file-id");
      let runtime = default_runtime () in
      run_session (Mutations.rename runtime "/src/file.txt" "/dst/file.txt");
      assert_equal
        [ ("file-id", "dst-id", "src-id") ]
        !FakePorts.remote_move_calls;
      let resource =
        Option.get (FakePorts.find_resource "/dst/file.txt" false)
      in
      assert_equal "/dst" resource.CacheData.Resource.parent_path)

let test_rename_trashes_duplicate_target_first () =
  with_reset (fun () ->
      FakePorts.add_resource (make_resource "/" "root");
      FakePorts.add_resource (make_resource ~name:"docs" "/docs" "docs-id");
      FakePorts.add_resource
        (make_resource ~mime_type:"text/plain" ~name:"source.txt"
           "/docs/source.txt" "source-id");
      FakePorts.add_resource
        (make_resource ~mime_type:"text/plain" ~name:"dest.txt" "/docs/dest.txt"
           "target-id");
      let runtime = default_runtime () in
      run_session (Mutations.rename runtime "/docs/source.txt" "/docs/dest.txt");
      let first_id, first_patch = List.nth !FakePorts.remote_update_calls 0 in
      let second_id, second_patch = List.nth !FakePorts.remote_update_calls 1 in
      assert_equal "target-id" first_id;
      assert_equal true first_patch.File.trashed;
      assert_equal "source-id" second_id;
      assert_equal "dest.txt" second_patch.File.name)

let test_rename_with_mv_keep_target_replaces_content () =
  with_reset (fun () ->
      let config = { Config.default with mv_keep_target = true } in
      FakePorts.add_resource (make_resource "/" "root");
      FakePorts.add_resource (make_resource ~name:"docs" "/docs" "docs-id");
      FakePorts.add_resource
        (make_resource ~mime_type:"text/plain" ~name:"source.txt"
           "/docs/source.txt" "source-id");
      FakePorts.add_resource
        (make_resource ~mime_type:"text/plain" ~name:"dest.txt" "/docs/dest.txt"
           "target-id");
      let runtime = default_runtime ~config () in
      run_session (Mutations.rename runtime "/docs/source.txt" "/docs/dest.txt");
      assert_equal
        [ ("/docs/source.txt", "/docs/dest.txt") ]
        !FakePorts.replace_target_contents_calls)

let test_folder_rename_clears_descendant_cache () =
  with_reset (fun () ->
      FakePorts.add_resource (make_resource "/" "root");
      FakePorts.add_resource
        (make_resource ~name:"folder" "/folder" "folder-id");
      let runtime = default_runtime () in
      run_session (Mutations.rename runtime "/folder" "/renamed");
      assert_bool "expected descendant cleanup"
        (List.mem "delete_children:/folder:false" !FakePorts.trace))

let test_same_name_move_recomputes_visible_path () =
  with_reset (fun () ->
      FakePorts.recompute_path_override := Some "/dst/file (copy).txt";
      FakePorts.add_resource (make_resource "/" "root");
      FakePorts.add_resource (make_resource ~name:"src" "/src" "src-id");
      FakePorts.add_resource (make_resource ~name:"dst" "/dst" "dst-id");
      FakePorts.add_resource
        (make_resource ~mime_type:"text/plain" ~name:"file.txt" "/src/file.txt"
           "file-id");
      let runtime = default_runtime () in
      run_session (Mutations.rename runtime "/src/file.txt" "/dst/file.txt");
      assert_bool "expected requested path to be replaced"
        (Option.is_none (FakePorts.find_resource "/dst/file.txt" false));
      let resource =
        Option.get (FakePorts.find_resource "/dst/file (copy).txt" false)
      in
      assert_equal "/dst" resource.CacheData.Resource.parent_path)

let suite =
  "DriveMutations test"
  >::: [
         "test_create_regular_file_inserts_into_cache"
         >:: test_create_regular_file_inserts_into_cache;
         "test_create_shortcut_from_relative_target"
         >:: test_create_shortcut_from_relative_target;
         "test_create_stored_symlink_for_external_target"
         >:: test_create_stored_symlink_for_external_target;
         "test_create_in_trash_is_denied" >:: test_create_in_trash_is_denied;
         "test_create_under_lost_and_found_is_denied"
         >:: test_create_under_lost_and_found_is_denied;
         "test_create_shortcut_from_absolute_target_inside_mountpoint"
         >:: test_create_shortcut_from_absolute_target_inside_mountpoint;
         "test_create_stored_symlink_rejects_oversized_target"
         >:: test_create_stored_symlink_rejects_oversized_target;
         "test_delete_uses_trash_by_default"
         >:: test_delete_uses_trash_by_default;
         "test_delete_can_skip_trash" >:: test_delete_can_skip_trash;
         "test_delete_from_trash_folder_can_delete_forever"
         >:: test_delete_from_trash_folder_can_delete_forever;
         "test_delete_non_empty_folder_is_rejected"
         >:: test_delete_non_empty_folder_is_rejected;
         "test_delete_under_lost_and_found_is_denied"
         >:: test_delete_under_lost_and_found_is_denied;
         "test_rename_within_same_parent_updates_name"
         >:: test_rename_within_same_parent_updates_name;
         "test_rename_across_trash_boundary_is_denied"
         >:: test_rename_across_trash_boundary_is_denied;
         "test_move_between_parents_updates_path"
         >:: test_move_between_parents_updates_path;
         "test_rename_trashes_duplicate_target_first"
         >:: test_rename_trashes_duplicate_target_first;
         "test_rename_with_mv_keep_target_replaces_content"
         >:: test_rename_with_mv_keep_target_replaces_content;
         "test_folder_rename_clears_descendant_cache"
         >:: test_folder_rename_clears_descendant_cache;
         "test_same_name_move_recomputes_visible_path"
         >:: test_same_name_move_recomputes_visible_path;
       ]
