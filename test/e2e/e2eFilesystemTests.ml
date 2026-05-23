open OUnit

let path run relative = Filename.concat run.E2eHarness.paths.mountpoint relative
let run_ref = ref None
let run_failed = ref false
let cleanup_registered = ref false

let write_file path content =
  Utils.with_out_channel ~mode:[ Open_creat; Open_trunc; Open_wronly ] path
    (fun ch -> output_string ch content)

let read_file path =
  let ch = open_in_bin path in
  try
    let length = in_channel_length ch in
    let content = really_input_string ch length in
    close_in ch;
    content
  with e ->
    close_in_noerr ch;
    raise e

let assert_file_content expected path =
  assert_equal ~printer:(fun s -> s) expected (read_file path)

let wait_until run description observe =
  let timeout = run.E2eHarness.settings.E2eSettings.fs_timeout_seconds in
  let deadline = Unix.gettimeofday () +. timeout in
  let last = ref "" in
  let rec loop () =
    let ok, observed = observe () in
    last := observed;
    if ok then ()
    else if Unix.gettimeofday () >= deadline then
      assert_failure
        (Printf.sprintf
           "Timed out after %.1fs waiting for %s. Last observed: %s" timeout
           description !last)
    else (
      Thread.delay 0.25;
      loop ())
  in
  loop ()

let wait_path_exists run path =
  wait_until run (Printf.sprintf "%s to exist" path) (fun () ->
      let exists = Sys.file_exists path in
      (exists, if exists then "exists" else "missing"))

let wait_path_absent run path =
  wait_until run (Printf.sprintf "%s to disappear" path) (fun () ->
      let exists = Sys.file_exists path in
      (not exists, if exists then "exists" else "missing"))

let wait_file_content run path expected =
  wait_until run (Printf.sprintf "%s contents to match" path) (fun () ->
      if not (Sys.file_exists path) then (false, "missing")
      else
        let actual = read_file path in
        ( actual = expected,
          Printf.sprintf "content=%S length=%d" actual (String.length actual) ))

let wait_file_size run path expected =
  wait_until run (Printf.sprintf "%s size to be %Ld" path expected) (fun () ->
      if not (Sys.file_exists path) then (false, "missing")
      else
        let actual = (Unix.LargeFile.stat path).Unix.LargeFile.st_size in
        (actual = expected, Printf.sprintf "size=%Ld" actual))

let wait_dir_contains run dir name =
  wait_until run (Printf.sprintf "%s listing to contain %s" dir name) (fun () ->
      if not (Sys.file_exists dir) then (false, "directory missing")
      else
        let entries = Sys.readdir dir |> Array.to_list in
        (List.mem name entries, "entries=[" ^ String.concat "," entries ^ "]"))

let wait_dir_omits run dir name =
  wait_until run (Printf.sprintf "%s listing to omit %s" dir name) (fun () ->
      if not (Sys.file_exists dir) then (false, "directory missing")
      else
        let entries = Sys.readdir dir |> Array.to_list in
        ( not (List.mem name entries),
          "entries=[" ^ String.concat "," entries ^ "]" ))

let register_cleanup () =
  if not !cleanup_registered then (
    cleanup_registered := true;
    at_exit (fun () ->
        match !run_ref with
        | None -> ()
        | Some run -> (
            let keep_local =
              run.E2eHarness.settings.E2eSettings.keep_local || !run_failed
            in
            try E2eHarness.teardown ~keep_local run
            with e ->
              Printf.eprintf "e2e cleanup failed at process exit: %s\n%!"
                (Printexc.to_string e))))

let get_run () =
  match !run_ref with
  | Some run -> run
  | None ->
      register_cleanup ();
      let run = E2eHarness.setup () in
      E2eHarness.start_mount run;
      run_ref := Some run;
      run

let case_dir run name =
  let dir = path run name in
  if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
  wait_path_exists run dir;
  dir

let with_case name f _ =
  let run = get_run () in
  try f run (case_dir run name)
  with e ->
    run_failed := true;
    E2eHarness.print_failure_diagnostics run ~case:name ~exn:e;
    raise e

let test_mount_root_listing run _dir =
  assert_bool "mountpoint should be a directory"
    (Sys.is_directory run.E2eHarness.paths.mountpoint);
  ignore (Sys.readdir run.E2eHarness.paths.mountpoint)

let test_create_write_remount_read run dir =
  let file_path = Filename.concat dir "hello.txt" in
  write_file file_path "hello from e2e\n";
  E2eHarness.remount run;
  wait_file_content run file_path "hello from e2e\n"

let test_create_remove_directory run dir =
  let nested = Filename.concat dir "created-dir" in
  Unix.mkdir nested 0o755;
  wait_path_exists run nested;
  Unix.rmdir nested;
  wait_path_absent run nested;
  wait_dir_omits run dir "created-dir"

let test_rename_file run dir =
  let source = Filename.concat dir "rename-source.txt" in
  let target = Filename.concat dir "rename-target.txt" in
  write_file source "rename me\n";
  wait_dir_contains run dir "rename-source.txt";
  Sys.rename source target;
  wait_path_absent run source;
  wait_file_content run target "rename me\n"

let test_move_file_between_directories run dir =
  let from_dir = Filename.concat dir "from" in
  let to_dir = Filename.concat dir "to" in
  Unix.mkdir from_dir 0o755;
  Unix.mkdir to_dir 0o755;
  let source = Filename.concat from_dir "move.txt" in
  let target = Filename.concat to_dir "move.txt" in
  write_file source "move me\n";
  wait_dir_contains run from_dir "move.txt";
  Sys.rename source target;
  wait_path_absent run source;
  wait_file_content run target "move me\n"

let test_truncate_remount_read run dir =
  let file_path = Filename.concat dir "truncate.txt" in
  write_file file_path "abcdef\n";
  Unix.truncate file_path 3;
  E2eHarness.remount run;
  wait_file_size run file_path 3L;
  wait_file_content run file_path "abc"

let test_delete_remount_absent run dir =
  let file_path = Filename.concat dir "delete.txt" in
  write_file file_path "delete me\n";
  wait_dir_contains run dir "delete.txt";
  Sys.remove file_path;
  E2eHarness.remount run;
  wait_path_absent run file_path;
  wait_dir_omits run dir "delete.txt"

let suite =
  "google-drive-ocamlfuse e2e"
  >::: [
         "mount root listing"
         >:: with_case "test-mount-root-listing" test_mount_root_listing;
         "create write remount read"
         >:: with_case "test-create-write-remount-read"
               test_create_write_remount_read;
         "create remove directory"
         >:: with_case "test-create-remove-directory"
               test_create_remove_directory;
         "rename file" >:: with_case "test-rename-file" test_rename_file;
         "move file between directories"
         >:: with_case "test-move-file-between-directories"
               test_move_file_between_directories;
         "truncate remount read"
         >:: with_case "test-truncate-remount-read" test_truncate_remount_read;
         "delete remount absent"
         >:: with_case "test-delete-remount-absent" test_delete_remount_absent;
       ]
