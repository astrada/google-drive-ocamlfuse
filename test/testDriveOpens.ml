open OUnit
open GapiMonad
module Opens = DriveOpens

let session =
  {
    GapiConversation.Session.curl = GapiCurl.Initialized;
    config = GapiConfig.default;
    auth = GapiConversation.Session.NoAuth;
    cookies = [];
    etag = "";
  }

let run_session m = fst (m session)
let default_runtime ?(config = Config.default) () = { Opens.config }

module FakePorts = struct
  type resource_response =
    | Return of CacheData.Resource.t
    | Raise_file_not_found

  let resource_responses = ref []
  let trace = ref []

  let reset () =
    resource_responses := [];
    trace := []

  let record event = trace := !trace @ [ event ]

  let set_resources resources =
    resource_responses := List.map (fun r -> Return r) resources

  let set_responses responses = resource_responses := responses

  let get_path_in_cache path config =
    let path_in_cache, trashed = Drive.get_path_in_cache path config in
    record
      (Printf.sprintf "get_path_in_cache:%s:%s:%b" path path_in_cache trashed);
    (path_in_cache, trashed)

  let get_resource path trashed =
    record (Printf.sprintf "get_resource:%s:%b" path trashed);
    match !resource_responses with
    | Return resource :: rest ->
        resource_responses := rest;
        SessionM.return resource
    | Raise_file_not_found :: rest ->
        resource_responses := rest;
        Utils.raise_m DriveMutations.File_not_found
    | [] -> assert_failure "expected fake resource"
end

module OpenOps = DriveOpens.Make (FakePorts)

let make_resource ?(id = 1L) ?(remote_id = "rid-file") ?(can_edit = true)
    ?(mime_type = "text/plain") ?size ?(trashed = false) path =
  let resource = Drive.create_resource path in
  {
    resource with
    id;
    remote_id = Some remote_id;
    name = Some (Filename.basename path);
    mime_type = Some mime_type;
    size;
    can_edit = Some can_edit;
    trashed = Some trashed;
    version = Some 1L;
    modified_time = Some 0.;
    created_time = Some 0.;
    viewed_by_me_time = Some 0.;
    state = CacheData.Resource.State.Synchronized;
  }

let document_mime_type = "application/vnd.google-apps.document"

let editable_docs_config ?(document_format = "odt") () =
  { Config.default with Config.editable_docs = true; document_format }

let large_file_read_only_config =
  {
    Config.default with
    Config.stream_large_files = true;
    large_file_threshold_mb = 1;
    large_file_read_only = true;
  }

let assert_no_get_resource () =
  assert_bool "unexpected resource lookup"
    (not
       (List.exists
          (fun event -> String.starts_with ~prefix:"get_resource:" event)
          !FakePorts.trace))

let test_read_only_open_resolves_resource_and_succeeds () =
  FakePorts.reset ();
  FakePorts.set_resources [ make_resource "/file.txt" ];
  run_session (OpenOps.fopen (default_runtime ()) "/file.txt" [ Unix.O_RDONLY ]);
  assert_bool "expected normalized lookup"
    (List.mem "get_path_in_cache:/file.txt:/file.txt:false" !FakePorts.trace);
  assert_equal
    [ "get_resource:/file.txt:false" ]
    (List.filter
       (fun event -> String.starts_with ~prefix:"get_resource:" event)
       !FakePorts.trace)

let test_write_open_on_read_only_filesystem_denies_before_lookup () =
  FakePorts.reset ();
  FakePorts.set_resources [ make_resource "/file.txt" ];
  let config = { Config.default with Config.read_only = true } in
  assert_raises Opens.Permission_denied (fun () ->
      run_session
        (OpenOps.fopen
           (default_runtime ~config ())
           "/file.txt" [ Unix.O_WRONLY ]));
  assert_bool "expected path normalization before global denial"
    (List.mem "get_path_in_cache:/file.txt:/file.txt:false" !FakePorts.trace);
  assert_no_get_resource ()

let test_read_only_open_on_read_only_filesystem_still_resolves () =
  FakePorts.reset ();
  FakePorts.set_resources [ make_resource "/file.txt" ];
  let config = { Config.default with Config.read_only = true } in
  run_session
    (OpenOps.fopen (default_runtime ~config ()) "/file.txt" [ Unix.O_RDONLY ]);
  assert_equal
    [ "get_resource:/file.txt:false" ]
    (List.filter
       (fun event -> String.starts_with ~prefix:"get_resource:" event)
       !FakePorts.trace)

let test_write_open_denies_read_only_resource () =
  FakePorts.reset ();
  FakePorts.set_resources [ make_resource ~can_edit:false "/file.txt" ];
  assert_raises Opens.Permission_denied (fun () ->
      run_session
        (OpenOps.fopen (default_runtime ()) "/file.txt" [ Unix.O_WRONLY ]));
  assert_equal
    [ "get_resource:/file.txt:false" ]
    (List.filter
       (fun event -> String.starts_with ~prefix:"get_resource:" event)
       !FakePorts.trace)

let test_read_only_open_allows_read_only_resource () =
  FakePorts.reset ();
  FakePorts.set_resources [ make_resource ~can_edit:false "/file.txt" ];
  run_session (OpenOps.fopen (default_runtime ()) "/file.txt" [ Unix.O_RDONLY ]);
  assert_equal
    [ "get_resource:/file.txt:false" ]
    (List.filter
       (fun event -> String.starts_with ~prefix:"get_resource:" event)
       !FakePorts.trace)

let test_write_open_allows_editable_file () =
  FakePorts.reset ();
  FakePorts.set_resources [ make_resource "/file.txt" ];
  run_session (OpenOps.fopen (default_runtime ()) "/file.txt" [ Unix.O_RDWR ]);
  assert_equal
    [ "get_resource:/file.txt:false" ]
    (List.filter
       (fun event -> String.starts_with ~prefix:"get_resource:" event)
       !FakePorts.trace)

let test_trash_path_lookup_uses_trashed_flag () =
  FakePorts.reset ();
  FakePorts.set_resources [ make_resource ~trashed:true "/file.txt" ];
  run_session
    (OpenOps.fopen (default_runtime ()) "/.Trash/file.txt" [ Unix.O_RDONLY ]);
  assert_bool "expected trashed cache path"
    (List.mem "get_path_in_cache:/.Trash/file.txt:/file.txt:true"
       !FakePorts.trace);
  assert_equal
    [ "get_resource:/file.txt:true" ]
    (List.filter
       (fun event -> String.starts_with ~prefix:"get_resource:" event)
       !FakePorts.trace)

let test_mixed_read_only_flag_preserves_current_semantics () =
  FakePorts.reset ();
  FakePorts.set_resources [ make_resource ~can_edit:false "/file.txt" ];
  let config = { Config.default with Config.read_only = true } in
  run_session
    (OpenOps.fopen
       (default_runtime ~config ())
       "/file.txt"
       [ Unix.O_RDONLY; Unix.O_WRONLY ]);
  assert_equal
    [ "get_resource:/file.txt:false" ]
    (List.filter
       (fun event -> String.starts_with ~prefix:"get_resource:" event)
       !FakePorts.trace)

let test_document_write_denied_when_editable_docs_disabled () =
  FakePorts.reset ();
  FakePorts.set_resources [ make_resource ~mime_type:document_mime_type "/doc" ];
  assert_raises Opens.Permission_denied (fun () ->
      run_session (OpenOps.fopen (default_runtime ()) "/doc" [ Unix.O_RDWR ]))

let test_document_write_denied_for_desktop_format () =
  FakePorts.reset ();
  FakePorts.set_resources [ make_resource ~mime_type:document_mime_type "/doc" ];
  let config = editable_docs_config ~document_format:"desktop" () in
  assert_raises Opens.Permission_denied (fun () ->
      run_session
        (OpenOps.fopen (default_runtime ~config ()) "/doc" [ Unix.O_RDWR ]))

let test_document_write_allowed_for_editable_format () =
  FakePorts.reset ();
  FakePorts.set_resources [ make_resource ~mime_type:document_mime_type "/doc" ];
  let config = editable_docs_config ~document_format:"odt" () in
  run_session
    (OpenOps.fopen (default_runtime ~config ()) "/doc" [ Unix.O_RDWR ]);
  assert_equal
    [ "get_resource:/doc:false" ]
    (List.filter
       (fun event -> String.starts_with ~prefix:"get_resource:" event)
       !FakePorts.trace)

let test_large_file_write_denied_when_large_file_read_only () =
  FakePorts.reset ();
  FakePorts.set_resources [ make_resource ~size:2097152L "/large.bin" ];
  assert_raises Opens.Permission_denied (fun () ->
      run_session
        (OpenOps.fopen
           (default_runtime ~config:large_file_read_only_config ())
           "/large.bin" [ Unix.O_RDWR ]))

let test_missing_resource_propagates_lookup_exception () =
  FakePorts.reset ();
  FakePorts.set_responses [ FakePorts.Raise_file_not_found ];
  assert_raises DriveMutations.File_not_found (fun () ->
      run_session
        (OpenOps.fopen (default_runtime ()) "/missing.txt" [ Unix.O_RDONLY ]));
  assert_equal
    [ "get_resource:/missing.txt:false" ]
    (List.filter
       (fun event -> String.starts_with ~prefix:"get_resource:" event)
       !FakePorts.trace)

let suite =
  "DriveOpens test"
  >::: [
         "test_read_only_open_resolves_resource_and_succeeds"
         >:: test_read_only_open_resolves_resource_and_succeeds;
         "test_write_open_on_read_only_filesystem_denies_before_lookup"
         >:: test_write_open_on_read_only_filesystem_denies_before_lookup;
         "test_read_only_open_on_read_only_filesystem_still_resolves"
         >:: test_read_only_open_on_read_only_filesystem_still_resolves;
         "test_write_open_denies_read_only_resource"
         >:: test_write_open_denies_read_only_resource;
         "test_read_only_open_allows_read_only_resource"
         >:: test_read_only_open_allows_read_only_resource;
         "test_write_open_allows_editable_file"
         >:: test_write_open_allows_editable_file;
         "test_trash_path_lookup_uses_trashed_flag"
         >:: test_trash_path_lookup_uses_trashed_flag;
         "test_mixed_read_only_flag_preserves_current_semantics"
         >:: test_mixed_read_only_flag_preserves_current_semantics;
         "test_document_write_denied_when_editable_docs_disabled"
         >:: test_document_write_denied_when_editable_docs_disabled;
         "test_document_write_denied_for_desktop_format"
         >:: test_document_write_denied_for_desktop_format;
         "test_document_write_allowed_for_editable_format"
         >:: test_document_write_allowed_for_editable_format;
         "test_large_file_write_denied_when_large_file_read_only"
         >:: test_large_file_write_denied_when_large_file_read_only;
         "test_missing_resource_propagates_lookup_exception"
         >:: test_missing_resource_propagates_lookup_exception;
       ]
