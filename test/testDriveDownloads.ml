open OUnit
open GapiMonad
open GapiMonad.SessionM.Infix
module Downloads = DriveDownloads

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
  { Downloads.cache = dummy_cache; config }

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
  let select_responses = ref []
  let md5_results = ref []
  let existing_paths = ref []
  let trace = ref []
  let media_failure = ref None
  let export_link_failure = ref None
  let export_failure = ref None

  let reset () =
    select_responses := [];
    md5_results := [];
    existing_paths := [];
    trace := [];
    media_failure := None;
    export_link_failure := None;
    export_failure := None

  let record event = trace := !trace @ [ event ]
  let set_select responses = select_responses := responses
  let set_md5 results = md5_results := results
  let set_existing paths = existing_paths := paths

  let get_content_path _cache resource =
    let remote_id =
      Option.default "no-remote-id" resource.CacheData.Resource.remote_id
    in
    let path = "/cache/" ^ remote_id in
    record ("content_path:" ^ path);
    path

  let select_first_resource_with_remote_id _cache remote_id =
    record ("select:" ^ remote_id);
    match !select_responses with
    | response :: rest ->
        select_responses := rest;
        response
    | [] -> assert_failure "expected fake selected resource"

  let file_exists path =
    record ("file_exists:" ^ path);
    List.mem path !existing_paths

  let check_md5_checksum resource _cache =
    record ("md5:" ^ resource.CacheData.Resource.path);
    match !md5_results with
    | result :: rest ->
        md5_results := rest;
        result
    | [] -> assert_failure "expected fake md5 result"

  let update_cached_resource_state _cache state id =
    record
      (Printf.sprintf "state:%Ld:%s" id
         (CacheData.Resource.State.to_string state))

  let cache_size_op_name op =
    let sample = 10L in
    if op sample = Int64.neg sample then "neg"
    else if op sample = sample then "identity"
    else "other"

  let update_cache_size_for_documents _cache _resource content_path op =
    record
      (Printf.sprintf "doc_size:%s:%s" content_path (cache_size_op_name op))

  let shrink_cache ?file_size () =
    record (Printf.sprintf "shrink:%Ld" (Option.default (-1L) file_size))

  let with_resource_lock resource request =
    SessionM.return () >>= fun () ->
    record ("lock_begin:" ^ resource.CacheData.Resource.path);
    request >>= fun () ->
    record ("lock_end:" ^ resource.CacheData.Resource.path);
    SessionM.return ()

  let create_desktop_entry resource content_path _config =
    record
      (Printf.sprintf "create_desktop:%s:%s" resource.CacheData.Resource.path
         content_path)

  let create_html_with_redirect resource content_path _config =
    record
      (Printf.sprintf "create_html:%s:%s" resource.CacheData.Resource.path
         content_path)

  let download_export_link_to_file content_path export_link =
    record
      (Printf.sprintf "download_export_link:%s:%s" content_path export_link);
    match !export_link_failure with
    | None -> SessionM.return ()
    | Some e -> Utils.raise_m e

  let export_document_to_file content_path ~file_id ~mime_type =
    record
      (Printf.sprintf "export_document:%s:%s:%s" content_path file_id mime_type);
    match !export_failure with
    | None -> SessionM.return ()
    | Some e -> Utils.raise_m e

  let download_media_to_file content_path resource =
    record
      (Printf.sprintf "download_media:%s:%s" content_path
         resource.CacheData.Resource.path);
    match !media_failure with
    | None -> SessionM.return ()
    | Some e -> Utils.raise_m e

  let create_empty_file content_path = record ("create_empty:" ^ content_path)
  let wait_exponential_backoff n = record (Printf.sprintf "wait:%d" n)

  let handle_download_exception e =
    record ("handle:" ^ Printexc.to_string e);
    Utils.raise_m e
end

module DownloadOps = DriveDownloads.Make (FakePorts)

let make_resource ?(id = 1L) ?(remote_id = Some "rid-file")
    ?(state = CacheData.Resource.State.Synchronized) ?size
    ?(mime_type = "text/plain") ?export_links path =
  let resource = Drive.create_resource path in
  {
    resource with
    id;
    remote_id;
    name = Some (Filename.basename path);
    mime_type = Some mime_type;
    size;
    export_links;
    version = Some 1L;
    modified_time = Some 0.;
    created_time = Some 0.;
    viewed_by_me_time = Some 0.;
    state;
  }

let download ?config resource =
  run_session
    (DownloadOps.download_resource (default_runtime ?config ()) resource)

let content_path = "/cache/rid-file"
let document_mime_type = "application/vnd.google-apps.document"
let odt_mime_type = CacheData.Resource.mime_type_of_format "odt"

let document_config ?(document_format = "odt") () =
  { Config.default with Config.document_format }

let test_synchronized_existing_content_reuses_file () =
  FakePorts.reset ();
  let resource = make_resource "/file.txt" in
  FakePorts.set_select [ Some resource ];
  FakePorts.set_existing [ content_path ];
  assert_equal content_path (download resource);
  assert_equal
    [
      "content_path:/cache/rid-file";
      "select:rid-file";
      "file_exists:/cache/rid-file";
    ]
    !FakePorts.trace

let test_dirty_existing_content_reuses_file () =
  List.iter
    (fun state ->
      FakePorts.reset ();
      let resource = make_resource ~state "/file.txt" in
      FakePorts.set_select [ Some resource ];
      FakePorts.set_existing [ content_path ];
      assert_equal content_path (download resource);
      assert_bool "unexpected materialization"
        (not
           (List.exists
              (fun event -> String.starts_with ~prefix:"lock_begin:" event)
              !FakePorts.trace)))
    [ CacheData.Resource.State.ToUpload; CacheData.Resource.State.Uploading ]

let test_missing_content_runs_locked_media_materialization () =
  FakePorts.reset ();
  let resource = make_resource ~size:42L "/file.txt" in
  FakePorts.set_select [ Some resource ];
  assert_equal content_path (download resource);
  let trace = !FakePorts.trace in
  assert_before "file_exists:/cache/rid-file" "lock_begin:/file.txt" trace;
  assert_before "lock_begin:/file.txt" "shrink:42" trace;
  assert_before "shrink:42" "state:1:Downloading" trace;
  assert_before "state:1:Downloading" "download_media:/cache/rid-file:/file.txt"
    trace;
  assert_before "download_media:/cache/rid-file:/file.txt"
    "state:1:Synchronized" trace;
  assert_before "state:1:Synchronized" "lock_end:/file.txt" trace

let test_to_download_md5_match_marks_synchronized_without_materializing () =
  FakePorts.reset ();
  let resource =
    make_resource ~state:CacheData.Resource.State.ToDownload "/file.txt"
  in
  FakePorts.set_select [ Some resource ];
  FakePorts.set_md5 [ true ];
  assert_equal content_path (download resource);
  assert_bool "expected md5 check" (List.mem "md5:/file.txt" !FakePorts.trace);
  assert_bool "expected synchronized state"
    (List.mem "state:1:Synchronized" !FakePorts.trace);
  assert_bool "unexpected materialization"
    (not (List.mem "lock_begin:/file.txt" !FakePorts.trace))

let test_to_download_md5_mismatch_materializes () =
  FakePorts.reset ();
  let resource =
    make_resource ~state:CacheData.Resource.State.ToDownload ~size:42L
      "/file.txt"
  in
  FakePorts.set_select [ Some resource ];
  FakePorts.set_md5 [ false ];
  assert_equal content_path (download resource);
  let trace = !FakePorts.trace in
  assert_before "md5:/file.txt" "lock_begin:/file.txt" trace;
  assert_bool "expected media materialization"
    (List.mem "download_media:/cache/rid-file:/file.txt" trace)

let test_downloading_waits_with_capped_backoff_then_reuses_file () =
  FakePorts.reset ();
  let downloading =
    make_resource ~state:CacheData.Resource.State.Downloading "/file.txt"
  in
  let synchronized = make_resource "/file.txt" in
  FakePorts.set_select
    (List.init 8 (fun _ -> Some downloading) @ [ Some synchronized ]);
  FakePorts.set_existing [ content_path ];
  assert_equal content_path (download downloading);
  let waits =
    List.filter_map
      (fun event ->
        if String.starts_with ~prefix:"wait:" event then
          Some (int_of_string (String.sub event 5 (String.length event - 5)))
        else None)
      !FakePorts.trace
  in
  assert_equal [ 0; 1; 2; 3; 4; 5; 6; 6 ] waits;
  assert_bool "unexpected materialization"
    (not (List.mem "lock_begin:/file.txt" !FakePorts.trace))

let test_downloading_stuck_threshold_materializes () =
  FakePorts.reset ();
  let resource =
    make_resource ~state:CacheData.Resource.State.Downloading ~size:42L
      "/file.txt"
  in
  FakePorts.set_select (List.init 302 (fun _ -> Some resource));
  FakePorts.set_md5 [ false ];
  assert_equal content_path (download resource);
  let wait_count =
    List.length
      (List.filter
         (fun event -> String.starts_with ~prefix:"wait:" event)
         !FakePorts.trace)
  in
  assert_equal 301 wait_count;
  assert_bool "expected stuck materialization"
    (List.mem "download_media:/cache/rid-file:/file.txt" !FakePorts.trace)

let test_not_found_raises_without_materializing () =
  FakePorts.reset ();
  let resource = make_resource "/file.txt" in
  FakePorts.set_select [ None ];
  assert_raises Downloads.File_not_found (fun () -> download resource);
  assert_bool "unexpected materialization"
    (not (List.mem "lock_begin:/file.txt" !FakePorts.trace))

let test_desktop_document_creates_desktop_entry_without_downloading_state () =
  FakePorts.reset ();
  let resource = make_resource ~mime_type:document_mime_type "/doc" in
  FakePorts.set_select [ Some resource ];
  assert_equal content_path (download resource);
  let trace = !FakePorts.trace in
  assert_before "shrink:0" "doc_size:/cache/rid-file:neg" trace;
  assert_before "doc_size:/cache/rid-file:neg"
    "create_desktop:/doc:/cache/rid-file" trace;
  assert_before "create_desktop:/doc:/cache/rid-file"
    "doc_size:/cache/rid-file:identity" trace;
  assert_bool "desktop documents should not enter Downloading state"
    (not (List.mem "state:1:Downloading" trace));
  assert_bool "expected final synchronized state"
    (List.mem "state:1:Synchronized" trace)

let test_desktop_document_can_create_html_redirect () =
  FakePorts.reset ();
  let config = { Config.default with Config.desktop_entry_as_html = true } in
  let resource = make_resource ~mime_type:document_mime_type "/doc" in
  FakePorts.set_select [ Some resource ];
  assert_equal content_path (download ~config resource);
  assert_bool "expected html redirect"
    (List.mem "create_html:/doc:/cache/rid-file" !FakePorts.trace)

let test_exportable_document_uses_cached_export_link () =
  FakePorts.reset ();
  let config = document_config () in
  let export_links =
    CacheData.Resource.serialize_export_links
      [ (odt_mime_type, "https://example/export") ]
  in
  let resource =
    make_resource ~mime_type:document_mime_type ~export_links "/doc"
  in
  FakePorts.set_select [ Some resource ];
  assert_equal content_path (download ~config resource);
  assert_bool "expected export link download"
    (List.mem "download_export_link:/cache/rid-file:https://example/export"
       !FakePorts.trace)

let test_exportable_document_falls_back_to_export_call () =
  FakePorts.reset ();
  let config = document_config () in
  let resource = make_resource ~mime_type:document_mime_type "/doc" in
  FakePorts.set_select [ Some resource ];
  assert_equal content_path (download ~config resource);
  assert_bool "expected document export"
    (List.mem
       (Printf.sprintf "export_document:/cache/rid-file:rid-file:%s"
          odt_mime_type)
       !FakePorts.trace)

let test_zero_byte_file_creates_empty_file () =
  FakePorts.reset ();
  let resource = make_resource ~size:0L "/empty.txt" in
  FakePorts.set_select [ Some resource ];
  assert_equal content_path (download resource);
  assert_bool "expected empty file creation"
    (List.mem "create_empty:/cache/rid-file" !FakePorts.trace);
  assert_bool "unexpected media download"
    (not
       (List.exists
          (fun event -> String.starts_with ~prefix:"download_media:" event)
          !FakePorts.trace))

let test_media_failure_restores_to_download_before_handling_exception () =
  FakePorts.reset ();
  let resource = make_resource ~size:42L "/file.txt" in
  let failure = Failure "media failed" in
  FakePorts.set_select [ Some resource ];
  FakePorts.media_failure := Some failure;
  assert_raises failure (fun () -> download resource);
  let trace = !FakePorts.trace in
  assert_before "state:1:Downloading" "download_media:/cache/rid-file:/file.txt"
    trace;
  assert_before "download_media:/cache/rid-file:/file.txt" "state:1:ToDownload"
    trace;
  assert_before "state:1:ToDownload" "handle:Failure(\"media failed\")" trace

let test_remote_id_none_uses_original_resource_without_reload () =
  FakePorts.reset ();
  let resource =
    make_resource ~remote_id:None ~state:CacheData.Resource.State.ToDownload
      "/file.txt"
  in
  FakePorts.set_md5 [ true ];
  assert_equal "/cache/no-remote-id" (download resource);
  assert_bool "unexpected reload by remote id"
    (not
       (List.exists
          (fun event -> String.starts_with ~prefix:"select:" event)
          !FakePorts.trace));
  assert_bool "expected synchronized state"
    (List.mem "state:1:Synchronized" !FakePorts.trace)

let suite =
  "DriveDownloads test"
  >::: [
         "test_synchronized_existing_content_reuses_file"
         >:: test_synchronized_existing_content_reuses_file;
         "test_dirty_existing_content_reuses_file"
         >:: test_dirty_existing_content_reuses_file;
         "test_missing_content_runs_locked_media_materialization"
         >:: test_missing_content_runs_locked_media_materialization;
         "test_to_download_md5_match_marks_synchronized_without_materializing"
         >:: test_to_download_md5_match_marks_synchronized_without_materializing;
         "test_to_download_md5_mismatch_materializes"
         >:: test_to_download_md5_mismatch_materializes;
         "test_downloading_waits_with_capped_backoff_then_reuses_file"
         >:: test_downloading_waits_with_capped_backoff_then_reuses_file;
         "test_downloading_stuck_threshold_materializes"
         >:: test_downloading_stuck_threshold_materializes;
         "test_not_found_raises_without_materializing"
         >:: test_not_found_raises_without_materializing;
         "test_desktop_document_creates_desktop_entry_without_downloading_state"
         >:: test_desktop_document_creates_desktop_entry_without_downloading_state;
         "test_desktop_document_can_create_html_redirect"
         >:: test_desktop_document_can_create_html_redirect;
         "test_exportable_document_uses_cached_export_link"
         >:: test_exportable_document_uses_cached_export_link;
         "test_exportable_document_falls_back_to_export_call"
         >:: test_exportable_document_falls_back_to_export_call;
         "test_zero_byte_file_creates_empty_file"
         >:: test_zero_byte_file_creates_empty_file;
         "test_media_failure_restores_to_download_before_handling_exception"
         >:: test_media_failure_restores_to_download_before_handling_exception;
         "test_remote_id_none_uses_original_resource_without_reload"
         >:: test_remote_id_none_uses_original_resource_without_reload;
       ]
