open OUnit
module File = GapiDriveV3Model.File
module Mapping = DriveResourceMapping

let fixed_time = 1234.5
let fixed_now () = fixed_time

let make_resource ?(id = 1L) ?remote_id ?name ?mime_type ?full_file_extension
    ?size ?trashed ?(state = CacheData.Resource.State.Synchronized) path =
  let resource = Mapping.create_resource ~now:fixed_now path in
  {
    resource with
    id;
    remote_id;
    name;
    mime_type;
    full_file_extension;
    size;
    trashed;
    state;
  }

let make_file ?(id = "rid-file") ?(name = "file.txt")
    ?(mime_type = "text/plain") ?(size = 42L) ?(version = 2L)
    ?(resource_key = "") ?(file_extension = "") ?(full_file_extension = "")
    ?(md5_checksum = "") ?(web_view_link = "") ?(trashed = false)
    ?(can_edit = true) ?(app_properties = []) ?(shortcut_target_id = "")
    ?(shortcut_target_resource_key = "") () =
  {
    File.empty with
    id;
    name;
    mimeType = mime_type;
    size;
    version;
    resourceKey = resource_key;
    fileExtension = file_extension;
    fullFileExtension = full_file_extension;
    md5Checksum = md5_checksum;
    webViewLink = web_view_link;
    trashed;
    capabilities = { File.Capabilities.empty with canEdit = can_edit };
    appProperties = app_properties;
    shortcutDetails =
      {
        File.ShortcutDetails.empty with
        targetId = shortcut_target_id;
        targetResourceKey = shortcut_target_resource_key;
      };
  }

let assert_starts_with prefix value =
  assert_bool
    (Printf.sprintf "expected %S to start with %S" value prefix)
    (String.starts_with ~prefix value)

let assert_ends_with suffix value =
  assert_bool
    (Printf.sprintf "expected %S to end with %S" value suffix)
    (ExtString.String.ends_with value ~suffix)

let update_resource_from_file ?state ?link_target
    ?(recompute_path =
      fun _ _ -> assert_failure "unexpected path recomputation") resource file =
  Mapping.update_resource_from_file ~now:fixed_now ~recompute_path ?state
    ?link_target resource file

let test_create_resource_uses_supplied_clock () =
  let resource = Mapping.create_resource ~now:fixed_now "/dir/file.txt" in
  assert_equal 0L resource.CacheData.Resource.id;
  assert_equal None resource.CacheData.Resource.remote_id;
  assert_equal "/dir" resource.CacheData.Resource.parent_path;
  assert_equal "/dir/file.txt" resource.CacheData.Resource.path;
  assert_equal CacheData.Resource.State.ToDownload
    resource.CacheData.Resource.state;
  assert_equal "" resource.CacheData.Resource.xattrs;
  assert_equal fixed_time resource.CacheData.Resource.last_update

let test_filename_cleanup_and_extension_detection () =
  assert_equal "a_b_c" (Mapping.clean_filename "a/b\000c");
  assert_equal "gz" (Mapping.get_file_extension "archive.tar.gz");
  assert_equal "" (Mapping.get_file_extension "README")

let test_document_extension_rules_use_explicit_config () =
  let document_mime_type = "application/vnd.google-apps.document" in
  let config =
    { Config.default with docs_file_extension = true; document_format = "odt" }
  in
  let document_format config =
    Mapping.get_file_extension_from_mime_type document_mime_type config
  in
  assert_equal "Doc.odt"
    (Mapping.get_filename config "Doc" true document_format);
  assert_equal "Doc.odt"
    (Mapping.get_filename config "Doc.odt" true document_format);
  assert_equal "Doc"
    (Mapping.get_filename
       { config with docs_file_extension = false }
       "Doc" true document_format);
  assert_equal "Doc" (Mapping.get_filename config "Doc" false document_format);
  let desktop_config =
    {
      Config.default with
      document_format = "desktop";
      desktop_entry_as_html = true;
    }
  in
  assert_equal "html"
    (Mapping.get_file_extension_from_mime_type document_mime_type desktop_config);
  assert_equal "Doc.html"
    (Mapping.get_filename desktop_config "Doc" true document_format)

let test_clean_document_extension_only_for_document_resources () =
  let config =
    { Config.default with docs_file_extension = true; document_format = "odt" }
  in
  let document =
    make_resource ~mime_type:"application/vnd.google-apps.document"
      "/docs/Doc.odt"
  in
  let plain = make_resource ~mime_type:"text/plain" "/docs/Doc.odt" in
  assert_equal "Doc"
    (Mapping.clean_document_extension config "Doc.odt" document);
  assert_equal "Doc.odt"
    (Mapping.clean_document_extension config "Doc.odt" plain);
  assert_equal "Doc.odt"
    (Mapping.clean_document_extension
       { config with docs_file_extension = false }
       "Doc.odt" document)

let test_unique_filename_without_collision_uses_clean_name () =
  let filename_table = Hashtbl.create 4 in
  let file = make_file ~id:"rid-a" ~name:"a/b.txt" () in
  let filename =
    Mapping.get_unique_filename_from_file Config.default file filename_table
  in
  assert_equal "a_b.txt" filename;
  assert_bool "expected filename to be recorded"
    (Hashtbl.mem filename_table "a_b.txt")

let test_unique_filename_collision_preserves_extensions () =
  let filename_table = Hashtbl.create 4 in
  Hashtbl.add filename_table "file.txt" 0;
  let file =
    make_file ~id:"rid-a" ~name:"file.txt" ~full_file_extension:"txt" ()
  in
  let filename =
    Mapping.get_unique_filename_from_file Config.default file filename_table
  in
  assert_starts_with "file (" filename;
  assert_ends_with ").txt" filename;
  assert_equal 1 (Hashtbl.find filename_table "file.txt")

let test_unique_filename_collision_preserves_full_file_extension () =
  let filename_table = Hashtbl.create 4 in
  Hashtbl.add filename_table "archive.tar.gz" 0;
  let file =
    make_file ~id:"rid-a" ~name:"archive.tar.gz" ~full_file_extension:"tar.gz"
      ()
  in
  let filename =
    Mapping.get_unique_filename_from_file Config.default file filename_table
  in
  assert_starts_with "archive (" filename;
  assert_ends_with ").tar.gz" filename

let test_unique_filename_collision_handles_extensionless_names () =
  let filename_table = Hashtbl.create 4 in
  Hashtbl.add filename_table "README" 0;
  let file = make_file ~id:"rid-a" ~name:"README" () in
  let filename =
    Mapping.get_unique_filename_from_file Config.default file filename_table
  in
  assert_starts_with "README (" filename;
  assert_bool "expected no file extension" (not (String.contains filename '.'))

let test_build_resource_tables_records_filenames_and_remote_ids () =
  let resource =
    make_resource ~remote_id:"rid-a" ~name:"Drive/Name" ~mime_type:"text/plain"
      "/dir/cached.txt"
  in
  let filename_table, remote_id_table =
    Mapping.build_resource_tables Config.default [ resource ]
  in
  assert_bool "expected cached basename"
    (Hashtbl.mem filename_table "cached.txt");
  assert_bool "expected clean Drive-derived name"
    (Hashtbl.mem filename_table "Drive_Name");
  assert_equal resource (Hashtbl.find remote_id_table "rid-a")

let test_recompute_path_uses_supplied_filename_table () =
  let resource =
    make_resource ~remote_id:"rid-a" ~name:"old.txt" ~mime_type:"text/plain"
      ~full_file_extension:"txt" "/dir/old.txt"
  in
  let filename_table = Hashtbl.create 4 in
  Hashtbl.add filename_table "new.txt" 0;
  let path =
    Mapping.recompute_path Config.default resource "new.txt" filename_table
  in
  assert_starts_with "/dir/new (" path;
  assert_ends_with ").txt" path

let test_update_resource_from_file_maps_standard_metadata () =
  let resource = make_resource "/dir/file.txt" in
  let app_properties =
    [
      ("mode", "33188");
      ("uid", "1000");
      ("gid", "1001");
      ("l", "/target");
      ("x-user.color", "blue");
    ]
  in
  let file =
    make_file ~id:"rid-a" ~name:"file.txt" ~size:99L ~version:7L
      ~resource_key:"resource-key" ~file_extension:"txt"
      ~full_file_extension:"txt" ~md5_checksum:"md5"
      ~web_view_link:"https://drive/view" ~trashed:true ~app_properties ()
  in
  let updated = update_resource_from_file resource file in
  assert_equal (Some "rid-a") updated.CacheData.Resource.remote_id;
  assert_equal (Some "file.txt") updated.CacheData.Resource.name;
  assert_equal (Some "text/plain") updated.CacheData.Resource.mime_type;
  assert_equal (Some 99L) updated.CacheData.Resource.size;
  assert_equal (Some true) updated.CacheData.Resource.can_edit;
  assert_equal (Some true) updated.CacheData.Resource.trashed;
  assert_equal (Some "resource-key") updated.CacheData.Resource.resource_key;
  assert_equal (Some 33188L) updated.CacheData.Resource.file_mode_bits;
  assert_equal (Some 1000L) updated.CacheData.Resource.uid;
  assert_equal (Some 1001L) updated.CacheData.Resource.gid;
  assert_equal (Some "/target") updated.CacheData.Resource.link_target;
  assert_equal
    (CacheData.Resource.render_xattrs [ ("x-user.color", "blue") ])
    updated.CacheData.Resource.xattrs;
  assert_equal fixed_time updated.CacheData.Resource.last_update;
  assert_equal "/dir/file.txt" updated.CacheData.Resource.path;
  assert_equal "/dir" updated.CacheData.Resource.parent_path

let test_update_resource_from_file_preserves_size_for_upload_states () =
  let resource =
    make_resource ~size:7L ~state:CacheData.Resource.State.ToUpload
      "/dir/file.txt"
  in
  let file = make_file ~size:99L () in
  let updated =
    update_resource_from_file ~state:CacheData.Resource.State.Uploading resource
      file
  in
  assert_equal (Some 7L) updated.CacheData.Resource.size;
  assert_equal CacheData.Resource.State.Uploading
    updated.CacheData.Resource.state

let test_update_resource_from_file_maps_shortcut_targets () =
  let resource = make_resource "/shortcut" in
  let file =
    make_file ~mime_type:Mapping.shortcut_mime_type
      ~shortcut_target_id:"target-id"
      ~shortcut_target_resource_key:"target-resource-key"
      ~app_properties:[ ("l", "/ignored") ]
      ()
  in
  let updated =
    update_resource_from_file ~link_target:"/target/path" resource file
  in
  assert_equal (Some "target-id") updated.CacheData.Resource.target_id;
  assert_equal (Some "target-resource-key")
    updated.CacheData.Resource.target_resource_key;
  assert_equal (Some "/target/path") updated.CacheData.Resource.link_target

let test_update_resource_from_file_recomputes_path_only_on_name_change () =
  let calls = ref [] in
  let recompute_path resource name =
    calls := (resource.CacheData.Resource.path, name) :: !calls;
    "/dir/renamed.txt"
  in
  let resource = make_resource ~name:"old.txt" "/dir/old.txt" in
  let file = make_file ~name:"new.txt" () in
  let updated = update_resource_from_file ~recompute_path resource file in
  assert_equal "/dir/renamed.txt" updated.CacheData.Resource.path;
  assert_equal "/dir" updated.CacheData.Resource.parent_path;
  assert_equal [ ("/dir/old.txt", "new.txt") ] !calls;
  calls := [];
  let file = make_file ~name:"new.txt" () in
  let resource = { updated with CacheData.Resource.name = Some "new.txt" } in
  let updated = update_resource_from_file ~recompute_path resource file in
  assert_equal "/dir/renamed.txt" updated.CacheData.Resource.path;
  assert_equal [] !calls

let suite =
  "DriveResourceMapping test"
  >::: [
         "test_create_resource_uses_supplied_clock"
         >:: test_create_resource_uses_supplied_clock;
         "test_filename_cleanup_and_extension_detection"
         >:: test_filename_cleanup_and_extension_detection;
         "test_document_extension_rules_use_explicit_config"
         >:: test_document_extension_rules_use_explicit_config;
         "test_clean_document_extension_only_for_document_resources"
         >:: test_clean_document_extension_only_for_document_resources;
         "test_unique_filename_without_collision_uses_clean_name"
         >:: test_unique_filename_without_collision_uses_clean_name;
         "test_unique_filename_collision_preserves_extensions"
         >:: test_unique_filename_collision_preserves_extensions;
         "test_unique_filename_collision_preserves_full_file_extension"
         >:: test_unique_filename_collision_preserves_full_file_extension;
         "test_unique_filename_collision_handles_extensionless_names"
         >:: test_unique_filename_collision_handles_extensionless_names;
         "test_build_resource_tables_records_filenames_and_remote_ids"
         >:: test_build_resource_tables_records_filenames_and_remote_ids;
         "test_recompute_path_uses_supplied_filename_table"
         >:: test_recompute_path_uses_supplied_filename_table;
         "test_update_resource_from_file_maps_standard_metadata"
         >:: test_update_resource_from_file_maps_standard_metadata;
         "test_update_resource_from_file_preserves_size_for_upload_states"
         >:: test_update_resource_from_file_preserves_size_for_upload_states;
         "test_update_resource_from_file_maps_shortcut_targets"
         >:: test_update_resource_from_file_maps_shortcut_targets;
         "test_update_resource_from_file_recomputes_path_only_on_name_change"
         >:: test_update_resource_from_file_recomputes_path_only_on_name_change;
       ]
