open OUnit
open GapiMonad
module File = GapiDriveV3Model.File
module Uploads = DriveUploads

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
  { Uploads.cache = dummy_cache; config }

let fixed_now = Netdate.create 1234.0
let assert_before = DriveTestSupport.Trace.assert_before
let assert_no_event = DriveTestSupport.Trace.assert_no_event

module FakePorts = struct
  let trace = ref []
  let media_lengths = ref []
  let detected_content_type = ref "application/octet-stream"
  let media_failure = ref None
  let remote_update_failure = ref None
  let update_cached_failure = ref None
  let remote_file = ref File.empty
  let reloaded_resource = ref None
  let remote_updates = ref []
  let update_from_file_calls = ref []
  let cached_updates = ref []

  let reset () =
    trace := [];
    media_lengths := [];
    detected_content_type := "application/octet-stream";
    media_failure := None;
    remote_update_failure := None;
    update_cached_failure := None;
    remote_file := File.empty;
    reloaded_resource := None;
    remote_updates := [];
    update_from_file_calls := [];
    cached_updates := []

  let record event = trace := !trace @ [ event ]
  let set_media_lengths lengths = media_lengths := lengths

  let set_detected_content_type content_type =
    detected_content_type := content_type

  let set_remote_file file = remote_file := file
  let set_reloaded_resource resource = reloaded_resource := resource

  let get_content_path _cache resource =
    let remote_id =
      Option.default "no-remote-id" resource.CacheData.Resource.remote_id
    in
    let path = "/cache/" ^ remote_id in
    record ("content_path:" ^ path);
    path

  let pop_media_length () =
    match !media_lengths with
    | length :: rest ->
        media_lengths := rest;
        length
    | [] -> assert_failure "expected fake media length"

  let create_file_resource ?content_type path =
    match !media_failure with
    | Some e ->
        record ("create_media_failure:" ^ path);
        raise e
    | None ->
        let length = pop_media_length () in
        let requested_content_type =
          match content_type with
          | None -> "<detect>"
          | Some content_type -> content_type
        in
        let resolved_content_type =
          match content_type with
          | None -> !detected_content_type
          | Some content_type -> content_type
        in
        record
          (Printf.sprintf "create_media:%s:%s:%Ld" requested_content_type path
             length);
        {
          GapiMediaResource.source = GapiMediaResource.File path;
          name = Filename.basename path;
          content_type = resolved_content_type;
          content_length = length;
        }

  let media_content_type media =
    record ("media_type:" ^ media.GapiMediaResource.content_type);
    media.GapiMediaResource.content_type

  let media_content_length media =
    record
      (Printf.sprintf "media_length:%Ld" media.GapiMediaResource.content_length);
    media.GapiMediaResource.content_length

  let update_cached_resource_state_and_size _cache state size id =
    record
      (Printf.sprintf "state_size:%Ld:%s:%Ld" id
         (CacheData.Resource.State.to_string state)
         size)

  let build_resource_keys_header_from_resource resource =
    record ("headers:" ^ Option.default "" resource.CacheData.Resource.remote_id);
    []

  let now () =
    record "now";
    fixed_now

  let media_label = function
    | None -> "none"
    | Some media ->
        Printf.sprintf "some:%s:%Ld" media.GapiMediaResource.content_type
          media.GapiMediaResource.content_length

  let remote_update ~media_source ~custom_headers:_ ~fileId file_patch =
    remote_updates := !remote_updates @ [ (fileId, media_source, file_patch) ];
    record
      (Printf.sprintf "remote_update:%s:%s" fileId (media_label media_source));
    match !remote_update_failure with
    | Some e -> Utils.raise_m e
    | None -> SessionM.return !remote_file

  let update_resource_from_file ?state resource file =
    let state_label =
      match state with
      | None -> "none"
      | Some state -> CacheData.Resource.State.to_string state
    in
    update_from_file_calls :=
      !update_from_file_calls
      @ [
          ( resource.CacheData.Resource.id,
            resource.CacheData.Resource.state,
            state );
        ];
    record
      (Printf.sprintf "update_from_file:%Ld:%s:%s"
         resource.CacheData.Resource.id
         (CacheData.Resource.State.to_string resource.CacheData.Resource.state)
         state_label);
    {
      resource with
      CacheData.Resource.remote_id = Some file.File.id;
      name = Some file.File.name;
      mime_type = Some file.File.mimeType;
      size = Some file.File.size;
      version = Some file.File.version;
      state = Option.default resource.CacheData.Resource.state state;
    }

  let select_first_resource_with_remote_id _cache remote_id =
    record ("reload:" ^ remote_id);
    !reloaded_resource

  let update_cached_resource _cache resource =
    record
      (Printf.sprintf "update_cache:%Ld:%s:%s" resource.CacheData.Resource.id
         (CacheData.Resource.State.to_string resource.CacheData.Resource.state)
         (Option.default "" resource.CacheData.Resource.remote_id));
    match !update_cached_failure with
    | Some e -> raise e
    | None -> cached_updates := !cached_updates @ [ resource ]

  let shrink_cache () = record "shrink"
end

module UploadOps = Uploads.Make (FakePorts)

let make_resource ?(id = 1L) ?(remote_id = "rid-file")
    ?(state = CacheData.Resource.State.Uploading) ?(mime_type = "text/plain")
    ?(size = 1L) path =
  DriveTestSupport.make_resource ~id ~remote_id ~state ~mime_type ~size path

let make_file ?(id = "rid-file") ?(name = "file.txt")
    ?(mime_type = "text/plain") ?(size = 42L) ?(version = 2L) () =
  DriveTestSupport.make_file ~id ~name ~mime_type ~size ~version ()

let with_reset f =
  FakePorts.reset ();
  FakePorts.set_remote_file (make_file ());
  f ()

let upload ?config resource =
  run_session (UploadOps.upload (default_runtime ?config ()) resource)

let last_remote_update () =
  match List.rev !FakePorts.remote_updates with
  | call :: _ -> call
  | [] -> assert_failure "expected remote update"

let last_cached_update () =
  match List.rev !FakePorts.cached_updates with
  | resource :: _ -> resource
  | [] -> assert_failure "expected cached update"

let test_editable_document_uses_configured_format_without_preliminary_media () =
  with_reset (fun () ->
      let document_mime_type = "application/vnd.google-apps.document" in
      let config =
        {
          Config.default with
          editable_docs = true;
          document_format = "odt";
          autodetect_mime = false;
        }
      in
      let resource = make_resource ~mime_type:document_mime_type "/doc" in
      let expected_content_type =
        CacheData.Resource.mime_type_of_format "odt"
      in
      FakePorts.set_media_lengths [ 42L ];
      upload ~config resource;
      assert_no_event "create_media:<detect>" !FakePorts.trace;
      let _file_id, media_source, _patch = last_remote_update () in
      let media = Option.get media_source in
      assert_equal expected_content_type media.GapiMediaResource.content_type)

let test_autodetect_mime_uses_empty_content_type () =
  with_reset (fun () ->
      let config = { Config.default with autodetect_mime = true } in
      let resource = make_resource "/file.txt" in
      FakePorts.set_media_lengths [ 42L ];
      upload ~config resource;
      let _file_id, media_source, _patch = last_remote_update () in
      let media = Option.get media_source in
      assert_equal "" media.GapiMediaResource.content_type)

let test_cached_mime_preferred_when_autodetect_disabled () =
  with_reset (fun () ->
      let config = { Config.default with autodetect_mime = false } in
      let resource = make_resource ~mime_type:"text/markdown" "/file.md" in
      FakePorts.set_detected_content_type "application/octet-stream";
      FakePorts.set_media_lengths [ 10L; 42L ];
      upload ~config resource;
      assert_bool "expected preliminary media detection"
        (List.mem "create_media:<detect>:/cache/rid-file:10" !FakePorts.trace);
      let _file_id, media_source, _patch = last_remote_update () in
      let media = Option.get media_source in
      assert_equal "text/markdown" media.GapiMediaResource.content_type)

let test_detected_mime_used_when_cached_mime_empty () =
  with_reset (fun () ->
      let config = { Config.default with autodetect_mime = false } in
      let resource = make_resource ~mime_type:"" "/file.bin" in
      FakePorts.set_detected_content_type "image/png";
      FakePorts.set_media_lengths [ 10L; 42L ];
      upload ~config resource;
      let _file_id, media_source, _patch = last_remote_update () in
      let media = Option.get media_source in
      assert_equal "image/png" media.GapiMediaResource.content_type)

let test_zero_byte_upload_omits_media_source_and_updates_size () =
  with_reset (fun () ->
      let resource = make_resource "/empty.txt" in
      FakePorts.set_media_lengths [ 0L ];
      upload resource;
      let trace = !FakePorts.trace in
      assert_before "state_size:1:Uploading:0" "remote_update:rid-file:none"
        trace;
      let file_id, media_source, patch = last_remote_update () in
      assert_equal "rid-file" file_id;
      assert_equal None media_source;
      assert_equal ~printer:string_of_float 1234.
        (Netdate.since_epoch patch.File.modifiedTime))

let test_upload_reloads_by_returned_remote_id_and_synchronizes_uploading () =
  with_reset (fun () ->
      let resource = make_resource "/file.txt" in
      FakePorts.set_remote_file (make_file ~id:"rid-returned" ());
      FakePorts.set_media_lengths [ 42L ];
      upload resource;
      assert_bool "expected reload by returned id"
        (List.mem "reload:rid-returned" !FakePorts.trace);
      assert_equal 2 (List.length !FakePorts.update_from_file_calls);
      let updated = last_cached_update () in
      assert_equal (Some "rid-returned") updated.CacheData.Resource.remote_id;
      assert_equal CacheData.Resource.State.Synchronized
        updated.CacheData.Resource.state)

let test_upload_preserves_reloaded_to_upload_state () =
  with_reset (fun () ->
      let resource = make_resource "/file.txt" in
      let reloaded =
        make_resource ~id:99L ~state:CacheData.Resource.State.ToUpload
          "/file.txt"
      in
      FakePorts.set_reloaded_resource (Some reloaded);
      FakePorts.set_media_lengths [ 42L ];
      upload resource;
      let updated = last_cached_update () in
      assert_equal 99L updated.CacheData.Resource.id;
      assert_equal CacheData.Resource.State.ToUpload
        updated.CacheData.Resource.state)

let test_upload_orders_final_cache_update_before_shrink () =
  with_reset (fun () ->
      let resource = make_resource "/file.txt" in
      FakePorts.set_media_lengths [ 42L ];
      upload resource;
      assert_before "update_cache:1:Synchronized:rid-file" "shrink"
        !FakePorts.trace)

let test_media_failure_propagates_without_side_effects () =
  with_reset (fun () ->
      let failure = Failure "media failed" in
      let resource = make_resource "/file.txt" in
      FakePorts.media_failure := Some failure;
      assert_raises failure (fun () -> upload resource);
      assert_no_event "state_size:" !FakePorts.trace;
      assert_no_event "remote_update:" !FakePorts.trace;
      assert_no_event "update_cache:" !FakePorts.trace;
      assert_no_event "shrink" !FakePorts.trace)

let test_remote_failure_propagates_after_state_size_update_without_final_save ()
    =
  with_reset (fun () ->
      let failure = Failure "remote failed" in
      let resource = make_resource "/file.txt" in
      FakePorts.set_media_lengths [ 42L ];
      FakePorts.remote_update_failure := Some failure;
      assert_raises failure (fun () -> upload resource);
      assert_bool "expected early state/size update"
        (List.mem "state_size:1:Uploading:42" !FakePorts.trace);
      assert_no_event "update_cache:" !FakePorts.trace;
      assert_no_event "shrink" !FakePorts.trace)

let test_final_cache_update_failure_propagates_without_shrink () =
  with_reset (fun () ->
      let failure = Failure "cache failed" in
      let resource = make_resource "/file.txt" in
      FakePorts.set_media_lengths [ 42L ];
      FakePorts.update_cached_failure := Some failure;
      assert_raises failure (fun () -> upload resource);
      assert_bool "expected final update attempt"
        (List.mem "update_cache:1:Synchronized:rid-file" !FakePorts.trace);
      assert_no_event "shrink" !FakePorts.trace)

let suite =
  "DriveUploads test"
  >::: [
         "test_editable_document_uses_configured_format_without_preliminary_media"
         >:: test_editable_document_uses_configured_format_without_preliminary_media;
         "test_autodetect_mime_uses_empty_content_type"
         >:: test_autodetect_mime_uses_empty_content_type;
         "test_cached_mime_preferred_when_autodetect_disabled"
         >:: test_cached_mime_preferred_when_autodetect_disabled;
         "test_detected_mime_used_when_cached_mime_empty"
         >:: test_detected_mime_used_when_cached_mime_empty;
         "test_zero_byte_upload_omits_media_source_and_updates_size"
         >:: test_zero_byte_upload_omits_media_source_and_updates_size;
         "test_upload_reloads_by_returned_remote_id_and_synchronizes_uploading"
         >:: test_upload_reloads_by_returned_remote_id_and_synchronizes_uploading;
         "test_upload_preserves_reloaded_to_upload_state"
         >:: test_upload_preserves_reloaded_to_upload_state;
         "test_upload_orders_final_cache_update_before_shrink"
         >:: test_upload_orders_final_cache_update_before_shrink;
         "test_media_failure_propagates_without_side_effects"
         >:: test_media_failure_propagates_without_side_effects;
         "test_remote_failure_propagates_after_state_size_update_without_final_save"
         >:: test_remote_failure_propagates_after_state_size_update_without_final_save;
         "test_final_cache_update_failure_propagates_without_shrink"
         >:: test_final_cache_update_failure_propagates_without_shrink;
       ]
