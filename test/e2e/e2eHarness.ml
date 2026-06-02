open GapiLens.Infix
module File = GapiDriveV3Model.File

exception Error of string

type local_paths = {
  root : string;
  data_dir : string;
  cache_dir : string;
  log_dir : string;
  mountpoint : string;
  config_path : string;
  state_path : string;
  app_log_path : string;
  stdout_path : string;
  stderr_path : string;
}

type google_native_fixture = {
  folder_name : string;
  folder_id : string;
  document_name : string;
  document_entry_name : string;
  document_id : string;
  shortcut_target_name : string;
  shortcut_target_content : string;
  shortcut_target_id : string;
  shortcut_name : string;
  shortcut_id : string;
}

type google_doc_export_fixture = {
  export_folder_name : string;
  export_folder_id : string;
  export_document_name : string;
  export_document_entry_name : string;
  export_document_id : string;
}

type t = {
  run_id : string;
  profile_name : string;
  docs_mode : string option;
  config_path : string;
  config : E2eConfig.t;
  settings : E2eSettings.t;
  drive : E2eDrive.t;
  safe_parent_id : string;
  run_root_id : string;
  gdfuse_exe : string;
  label : string;
  paths : local_paths;
  mutable mount : E2eMount.t option;
  mutable google_native_fixture : google_native_fixture option;
  mutable google_doc_export_fixture : google_doc_export_fixture option;
}

module StateStore = KeyValueStore.MakeFileStore (State)

let env name = try Some (Sys.getenv name) with Not_found -> None

let find_build_marker path =
  let marker = Filename.dir_sep ^ "_build" ^ Filename.dir_sep in
  let marker_len = String.length marker in
  let rec loop index =
    if index + marker_len > String.length path then None
    else if String.sub path index marker_len = marker then Some index
    else loop (index + 1)
  in
  loop 0

let rec find_workspace_root_from path =
  let dune_project = Filename.concat path "dune-project" in
  if Sys.file_exists dune_project then path
  else
    let parent = Filename.dirname path in
    if parent = path then raise (Error "cannot locate repository root")
    else find_workspace_root_from parent

let workspace_root () =
  let cwd = Sys.getcwd () in
  match find_build_marker cwd with
  | Some index -> String.sub cwd 0 index
  | None -> find_workspace_root_from cwd

let default_config_path root = Filename.concat root "test/e2e/config.json"

let default_gdfuse_exe root =
  Filename.concat root "_build/default/bin/gdfuse.exe"

let config_path root =
  match env "GDFUSE_E2E_CONFIG" with
  | Some path -> path
  | None -> default_config_path root

let gdfuse_exe root =
  match env "GDFUSE_E2E_GDFUSE_EXE" with
  | Some path -> path
  | None -> default_gdfuse_exe root

let ensure_executable path =
  if not (Sys.file_exists path) then
    raise
      (Error
         (Printf.sprintf
            "google-drive-ocamlfuse executable not found at %s; run dune build \
             @install first"
            path))
  else
    try Unix.access path [ Unix.X_OK ]
    with Unix.Unix_error _ ->
      raise
        (Error
           (Printf.sprintf
              "google-drive-ocamlfuse executable is not executable: %s" path))

let make_run_id () =
  let millis = Int64.of_float (Unix.gettimeofday () *. 1000.0) in
  Printf.sprintf "%d-%Ld" (Unix.getpid ()) millis

let make_profile_run_id profile_name =
  let run_id = make_run_id () in
  if profile_name = "default" then run_id else profile_name ^ "-" ^ run_id

let docs_mode_description = function None -> "default" | Some mode -> mode
let mkdir path = if not (Sys.file_exists path) then Unix.mkdir path 0o700

let rec remove_path path =
  if Sys.file_exists path then
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR ->
        Sys.readdir path
        |> Array.iter (fun name -> remove_path (Filename.concat path name));
        Unix.rmdir path
    | _ -> Sys.remove path

let make_local_paths run_id =
  let root =
    Filename.concat
      (Filename.get_temp_dir_name ())
      ("google-drive-ocamlfuse-e2e-" ^ run_id)
  in
  mkdir root;
  let data_dir = Filename.concat root "data" in
  let cache_dir = Filename.concat root "cache" in
  let log_dir = Filename.concat root "log" in
  let mountpoint = Filename.concat root "mnt" in
  List.iter mkdir [ data_dir; cache_dir; log_dir; mountpoint ];
  {
    root;
    data_dir;
    cache_dir;
    log_dir;
    mountpoint;
    config_path = Filename.concat root "config";
    state_path = Filename.concat data_dir "state";
    app_log_path = Filename.concat log_dir "gdfuse.log";
    stdout_path = Filename.concat log_dir "gdfuse.stdout";
    stderr_path = Filename.concat log_dir "gdfuse.stderr";
  }

let write_profile config run_root_id paths =
  let gdfuse_config =
    {
      Config.default with
      client_id = config.E2eConfig.client_id;
      client_secret = config.client_secret;
      root_folder = run_root_id;
      data_directory = paths.data_dir;
      cache_directory = paths.cache_dir;
      log_directory = paths.log_dir;
      log_to = paths.app_log_path;
    }
  in
  ConfigStore.save
    { ConfigStore.path = paths.config_path; data = gdfuse_config };
  let state =
    State.empty
    |> State.refresh_token ^= config.refresh_token
    |> State.saved_version ^= Config.version
  in
  StateStore.save { StateStore.path = paths.state_path; data = state }

let print_run_summary run =
  Printf.printf
    "e2e run id: %s\n\
     e2e profile: %s\n\
     e2e docs mode: %s\n\
     e2e config: %s\n\
     e2e settings: %s\n\
     e2e test folder path: %s\n\
     e2e safe parent id: %s\n\
     e2e run root id: %s\n\
     e2e local root: %s\n\
     e2e mountpoint: %s\n\
     e2e app log: %s\n\
     e2e stdout: %s\n\
     e2e stderr: %s\n\
     %!"
    run.run_id run.profile_name
    (docs_mode_description run.docs_mode)
    run.config_path
    (E2eSettings.describe run.settings)
    run.config.E2eConfig.test_folder_path run.safe_parent_id run.run_root_id
    run.paths.root run.paths.mountpoint run.paths.app_log_path
    run.paths.stdout_path run.paths.stderr_path

let load_common_environment () =
  let root = workspace_root () in
  let config_path = config_path root in
  let gdfuse_exe = gdfuse_exe root in
  let settings = E2eSettings.load () in
  ensure_executable gdfuse_exe;
  let unmount_helper = E2eMount.require_unmount_helper () in
  let config = E2eConfig.load config_path in
  Printf.printf "Loaded e2e config: %s\n%!" (E2eConfig.describe config);
  Printf.printf "Loaded e2e settings: %s\n%!" (E2eSettings.describe settings);
  Printf.printf "Using FUSE unmount helper: %s\n%!" unmount_helper;
  (config_path, gdfuse_exe, settings, config)

let preflight () =
  let config_path, gdfuse_exe, settings, config = load_common_environment () in
  let run_id = "preflight-" ^ make_run_id () in
  let paths = make_local_paths run_id in
  let cleanup () = remove_path paths.root in
  try
    let drive = E2eDrive.create config in
    E2eDrive.preflight drive;
    let safe_parent_id =
      E2eDrive.resolve_or_create_path drive config.E2eConfig.test_folder_path
    in
    Printf.printf
      "e2e preflight ok\n\
       e2e config: %s\n\
       e2e executable: %s\n\
       e2e settings: %s\n\
       e2e test folder path: %s\n\
       e2e safe parent id: %s\n\
       e2e temporary local root probe: %s\n\
       %!"
      config_path gdfuse_exe
      (E2eSettings.describe settings)
      config.E2eConfig.test_folder_path safe_parent_id paths.root;
    cleanup ()
  with e ->
    cleanup ();
    raise e

let setup ?(profile_name = "default") ?docs_mode () =
  let config_path, gdfuse_exe, settings, config = load_common_environment () in
  let drive = E2eDrive.create config in
  E2eDrive.preflight drive;
  let run_id = make_profile_run_id profile_name in
  let paths = make_local_paths run_id in
  let run_root_id = ref None in
  try
    let safe_parent_id =
      E2eDrive.resolve_or_create_path drive config.E2eConfig.test_folder_path
    in
    let run_root = E2eDrive.create_run_root drive ~safe_parent_id ~run_id in
    run_root_id := Some run_root.GapiDriveV3Model.File.id;
    write_profile config run_root.File.id paths;
    let run =
      {
        run_id;
        profile_name;
        docs_mode;
        config_path;
        config;
        settings;
        drive;
        safe_parent_id;
        run_root_id = run_root.File.id;
        gdfuse_exe;
        label = "e2e-" ^ run_id;
        paths;
        mount = None;
        google_native_fixture = None;
        google_doc_export_fixture = None;
      }
    in
    print_run_summary run;
    run
  with e ->
    (match !run_root_id with
    | None -> ()
    | Some file_id -> ( try E2eDrive.trash_file drive ~file_id with _ -> ()));
    raise e

let start_mount run =
  match run.mount with
  | Some _ -> ()
  | None ->
      let mount =
        E2eMount.start ~docs_mode:run.docs_mode
          ~timeout:run.settings.mount_timeout_seconds ~gdfuse_exe:run.gdfuse_exe
          ~label:run.label ~config_path:run.paths.config_path
          ~mountpoint:run.paths.mountpoint ~stdout_path:run.paths.stdout_path
          ~stderr_path:run.paths.stderr_path
      in
      run.mount <- Some mount

let stop_mount run =
  match run.mount with
  | None -> ()
  | Some mount ->
      run.mount <- None;
      E2eMount.stop ~timeout:run.settings.unmount_timeout_seconds mount

let remount run =
  stop_mount run;
  start_mount run

let wait_remote_child run ~parent_id ~name =
  let deadline =
    Unix.gettimeofday () +. run.settings.E2eSettings.fs_timeout_seconds
  in
  let rec loop () =
    match E2eDrive.find_child run.drive ~parent_id ~name ~trashed:false with
    | Some file -> file
    | None ->
        if Unix.gettimeofday () >= deadline then
          raise
            (Error
               (Printf.sprintf
                  "Timed out waiting for Drive child %S under parent %s before \
                   mounting"
                  name parent_id))
        else (
          Thread.delay 0.5;
          loop ())
  in
  loop ()

let create_google_native_fixture run =
  match run.mount with
  | Some _ ->
      raise
        (Error
           "Google-native e2e fixtures must be created before the mount starts")
  | None ->
      let folder_name = "test-google-native-fixtures" in
      let document_name = "Milestone 5 Doc" in
      let document_entry_name = document_name ^ ".desktop" in
      let shortcut_target_name = "shortcut-target.txt" in
      let shortcut_target_content = "shortcut target content\n" in
      let shortcut_name = "target-shortcut" in
      let folder =
        E2eDrive.create_folder run.drive ~parent_id:run.run_root_id
          ~name:folder_name
      in
      let document =
        E2eDrive.create_google_document run.drive ~parent_id:folder.File.id
          ~name:document_name
      in
      let shortcut_target =
        E2eDrive.create_text_file run.drive ~parent_id:folder.File.id
          ~name:shortcut_target_name ~content:shortcut_target_content
      in
      let shortcut =
        E2eDrive.create_shortcut run.drive ~parent_id:folder.File.id
          ~name:shortcut_name ~target_id:shortcut_target.File.id
      in
      ignore
        (wait_remote_child run ~parent_id:folder.File.id ~name:document_name);
      ignore
        (wait_remote_child run ~parent_id:folder.File.id
           ~name:shortcut_target_name);
      ignore
        (wait_remote_child run ~parent_id:folder.File.id ~name:shortcut_name);
      let fixture =
        {
          folder_name;
          folder_id = folder.File.id;
          document_name;
          document_entry_name;
          document_id = document.File.id;
          shortcut_target_name;
          shortcut_target_content;
          shortcut_target_id = shortcut_target.File.id;
          shortcut_name;
          shortcut_id = shortcut.File.id;
        }
      in
      Printf.printf
        "e2e google-native fixture: folder=%s document=%s target=%s shortcut=%s\n\
         %!"
        fixture.folder_id fixture.document_id fixture.shortcut_target_id
        fixture.shortcut_id;
      fixture

let ensure_google_native_fixture run =
  match run.google_native_fixture with
  | Some fixture -> fixture
  | None ->
      let fixture = create_google_native_fixture run in
      run.google_native_fixture <- Some fixture;
      fixture

let google_native_fixture run =
  match run.google_native_fixture with
  | Some fixture -> fixture
  | None ->
      raise
        (Error
           "Google-native e2e fixture was requested before it was initialized")

let create_google_doc_export_fixture run ~folder_name ~document_name
    ~document_entry_name =
  match run.mount with
  | Some _ ->
      raise
        (Error
           "Google Docs export e2e fixtures must be created before the mount \
            starts")
  | None ->
      let folder =
        E2eDrive.create_folder run.drive ~parent_id:run.run_root_id
          ~name:folder_name
      in
      let document =
        E2eDrive.create_google_document run.drive ~parent_id:folder.File.id
          ~name:document_name
      in
      ignore
        (wait_remote_child run ~parent_id:folder.File.id ~name:document_name);
      let fixture =
        {
          export_folder_name = folder_name;
          export_folder_id = folder.File.id;
          export_document_name = document_name;
          export_document_entry_name = document_entry_name;
          export_document_id = document.File.id;
        }
      in
      Printf.printf
        "e2e google-doc export fixture: folder=%s document=%s expected_entry=%s\n\
         %!"
        fixture.export_folder_id fixture.export_document_id
        fixture.export_document_entry_name;
      fixture

let ensure_msoffice_fixture run =
  match run.google_doc_export_fixture with
  | Some fixture -> fixture
  | None ->
      let fixture =
        create_google_doc_export_fixture run
          ~folder_name:"test-google-doc-msoffice-fixtures"
          ~document_name:"Milestone 6 Doc"
          ~document_entry_name:"Milestone 6 Doc.docx"
      in
      run.google_doc_export_fixture <- Some fixture;
      fixture

let google_doc_export_fixture run =
  match run.google_doc_export_fixture with
  | Some fixture -> fixture
  | None ->
      raise
        (Error
           "Google Docs export e2e fixture was requested before it was \
            initialized")

let cleanup_remote run =
  try
    let summary =
      E2eDrive.visible_children_summary run.drive ~parent_id:run.run_root_id
    in
    Printf.printf "e2e run root before trash: %s\n%!" summary;
    E2eDrive.trash_file run.drive ~file_id:run.run_root_id;
    Printf.printf "Trashed e2e run root: %s\n%!" run.run_root_id
  with e ->
    raise
      (Error
         (Printf.sprintf
            "failed to trash e2e run root %s; inspect and clean it manually: %s"
            run.run_root_id (Printexc.to_string e)))

let teardown ~keep_local run =
  let cleanup_errors = ref [] in
  let capture label f =
    try
      f ();
      Printf.printf "e2e cleanup %s: ok\n%!" label
    with e ->
      Printf.eprintf "e2e cleanup %s: failed: %s\n%!" label
        (Printexc.to_string e);
      cleanup_errors := (label, Printexc.to_string e) :: !cleanup_errors
  in
  capture "unmount" (fun () -> stop_mount run);
  capture "remote cleanup" (fun () -> cleanup_remote run);
  if keep_local then
    Printf.printf "Keeping e2e local root for debugging: %s\n%!" run.paths.root
  else capture "local cleanup" (fun () -> remove_path run.paths.root);
  match List.rev !cleanup_errors with
  | [] -> ()
  | errors ->
      let details =
        errors
        |> List.map (fun (label, message) ->
            Printf.sprintf "%s: %s" label message)
        |> String.concat "\n"
      in
      raise (Error ("e2e cleanup failed:\n" ^ details))

let print_file_excerpt label path lines =
  let excerpt = E2eMount.tail_file ~lines path in
  if excerpt = "" then Printf.eprintf "%s: %s (empty or missing)\n%!" label path
  else Printf.eprintf "%s: %s\n%s\n%!" label path excerpt

let print_failure_diagnostics run ~case ~exn =
  Printf.eprintf
    "\n\
     e2e failure diagnostics\n\
     case: %s\n\
     exception: %s\n\
     run id: %s\n\
     profile: %s\n\
     docs mode: %s\n\
     run root id: %s\n\
     local root: %s\n\
     mountpoint: %s\n\
     mount status: %s\n\
     app log: %s\n\
     stdout: %s\n\
     stderr: %s\n\
     %!"
    case (Printexc.to_string exn) run.run_id run.profile_name
    (docs_mode_description run.docs_mode)
    run.run_root_id run.paths.root run.paths.mountpoint
    (if E2eMount.is_mountpoint run.paths.mountpoint then "mounted"
     else "not mounted")
    run.paths.app_log_path run.paths.stdout_path run.paths.stderr_path;
  print_file_excerpt "last app log lines" run.paths.app_log_path
    run.settings.log_excerpt_lines;
  print_file_excerpt "last stdout lines" run.paths.stdout_path
    run.settings.log_excerpt_lines;
  print_file_excerpt "last stderr lines" run.paths.stderr_path
    run.settings.log_excerpt_lines

let with_run f =
  let run = setup () in
  let result =
    try Ok (f run) with e -> Error (e, Printexc.get_raw_backtrace ())
  in
  let keep_local =
    run.settings.E2eSettings.keep_local
    || match result with Ok _ -> false | Error _ -> true
  in
  let cleanup_result =
    try Ok (teardown ~keep_local run)
    with e -> Error (e, Printexc.get_raw_backtrace ())
  in
  match (result, cleanup_result) with
  | Ok value, Ok () -> value
  | Error (e, backtrace), Ok () -> Printexc.raise_with_backtrace e backtrace
  | Ok _, Error (cleanup, backtrace) ->
      Printexc.raise_with_backtrace cleanup backtrace
  | Error (e, backtrace), Error (cleanup, _) ->
      Printf.eprintf "Additional cleanup failure: %s\n%!"
        (Printexc.to_string cleanup);
      Printexc.raise_with_backtrace e backtrace
