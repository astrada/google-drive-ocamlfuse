open OUnit

let path run relative = Filename.concat run.E2eHarness.paths.mountpoint relative
let run_ref = ref None
let run_failed = ref false
let cleanup_registered = ref false

let write_all fd content =
  let bytes = Bytes.of_string content in
  let rec loop offset remaining =
    if remaining > 0 then
      let written = Unix.write fd bytes offset remaining in
      loop (offset + written) (remaining - written)
  in
  loop 0 (Bytes.length bytes)

let write_file path content =
  let ch = open_out_bin path in
  try
    output_string ch content;
    close_out ch
  with e ->
    close_out_noerr ch;
    raise e

let append_file path content =
  let ch = open_out_gen [ Open_wronly; Open_append; Open_creat ] 0o644 path in
  try
    output_string ch content;
    close_out ch
  with e ->
    close_out_noerr ch;
    raise e

let write_file_at path offset content =
  let fd = Unix.openfile path [ Unix.O_RDWR ] 0 in
  try
    ignore (Unix.LargeFile.lseek fd offset Unix.SEEK_SET);
    write_all fd content;
    Unix.close fd
  with e ->
    Unix.close fd;
    raise e

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

let deterministic_content length =
  String.init length (fun index ->
      Char.chr (((index * 37) + (index / 17) + 11) land 0xff))

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
          Printf.sprintf "length=%d md5=%s expected_length=%d expected_md5=%s"
            (String.length actual)
            (Digest.string actual |> Digest.to_hex)
            (String.length expected)
            (Digest.string expected |> Digest.to_hex) ))

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

let sorted_entries dir = Sys.readdir dir |> Array.to_list |> List.sort compare

let wait_dir_contains_all run dir names =
  wait_until run
    (Printf.sprintf "%s listing to contain [%s]" dir (String.concat "," names))
    (fun () ->
      if not (Sys.file_exists dir) then (false, "directory missing")
      else
        let entries = sorted_entries dir in
        let missing =
          List.filter (fun name -> not (List.mem name entries)) names
        in
        ( missing = [],
          Printf.sprintf "entries=[%s]; missing=[%s]"
            (String.concat "," entries)
            (String.concat "," missing) ))

let wait_dir_omits_all run dir names =
  wait_until run
    (Printf.sprintf "%s listing to omit [%s]" dir (String.concat "," names))
    (fun () ->
      if not (Sys.file_exists dir) then (false, "directory missing")
      else
        let entries = sorted_entries dir in
        let present = List.filter (fun name -> List.mem name entries) names in
        ( present = [],
          Printf.sprintf "entries=[%s]; present=[%s]"
            (String.concat "," entries)
            (String.concat "," present) ))

let wait_remote_child run ~parent_id ~name ~trashed =
  let result = ref None in
  wait_until run
    (Printf.sprintf "Drive child %S under %s with trashed=%b" name parent_id
       trashed) (fun () ->
      match
        E2eDrive.find_child run.E2eHarness.drive ~parent_id ~name ~trashed
      with
      | None -> (false, "not found")
      | Some file ->
          result := Some file;
          (true, "found id=" ^ file.GapiDriveV3Model.File.id));
  match !result with
  | Some file -> file
  | None ->
      assert_failure "unreachable: remote child wait succeeded without file"

let wait_remote_file_trashed run ~file_id =
  wait_until run (Printf.sprintf "Drive file %s to be trashed" file_id)
    (fun () ->
      let file = E2eDrive.get_file run.E2eHarness.drive ~file_id in
      ( file.GapiDriveV3Model.File.trashed,
        Printf.sprintf "trashed=%b" file.GapiDriveV3Model.File.trashed ))

let remount_and_assert run f =
  E2eHarness.remount run;
  f ()

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

let test_overwrite_shorter_remount_read run dir =
  let file_path = Filename.concat dir "overwrite-shorter.txt" in
  write_file file_path "abcdefg";
  wait_file_content run file_path "abcdefg";
  write_file file_path "xy";
  wait_file_size run file_path 2L;
  wait_file_content run file_path "xy";
  remount_and_assert run (fun () ->
      wait_file_size run file_path 2L;
      wait_file_content run file_path "xy")

let test_overwrite_longer_remount_read run dir =
  let file_path = Filename.concat dir "overwrite-longer.txt" in
  write_file file_path "abc";
  wait_file_content run file_path "abc";
  write_file file_path "abcdefghijklmnopqrstuvwxyz";
  wait_file_size run file_path 26L;
  wait_file_content run file_path "abcdefghijklmnopqrstuvwxyz";
  remount_and_assert run (fun () ->
      wait_file_size run file_path 26L;
      wait_file_content run file_path "abcdefghijklmnopqrstuvwxyz")

let test_append_remount_read run dir =
  let file_path = Filename.concat dir "append.txt" in
  write_file file_path "alpha";
  append_file file_path "-beta";
  wait_file_size run file_path 10L;
  wait_file_content run file_path "alpha-beta";
  remount_and_assert run (fun () ->
      wait_file_size run file_path 10L;
      wait_file_content run file_path "alpha-beta")

let test_partial_overwrite_remount_read run dir =
  let file_path = Filename.concat dir "partial-overwrite.txt" in
  write_file file_path "abcdefghij";
  write_file_at file_path 3L "XYZ";
  wait_file_size run file_path 10L;
  wait_file_content run file_path "abcXYZghij";
  remount_and_assert run (fun () ->
      wait_file_size run file_path 10L;
      wait_file_content run file_path "abcXYZghij")

let test_listing_cache_coherence run dir =
  let write name content = write_file (Filename.concat dir name) content in
  write "alpha.txt" "alpha";
  write "bravo.txt" "bravo";
  write "charlie.txt" "charlie";
  wait_dir_contains_all run dir [ "alpha.txt"; "bravo.txt"; "charlie.txt" ];
  Sys.rename (Filename.concat dir "bravo.txt") (Filename.concat dir "delta.txt");
  wait_dir_contains_all run dir [ "alpha.txt"; "charlie.txt"; "delta.txt" ];
  wait_dir_omits run dir "bravo.txt";
  let source_dir = Filename.concat dir "source" in
  let dest_dir = Filename.concat dir "dest" in
  Unix.mkdir source_dir 0o755;
  Unix.mkdir dest_dir 0o755;
  write_file (Filename.concat source_dir "moved.txt") "moved";
  wait_dir_contains run source_dir "moved.txt";
  Sys.rename
    (Filename.concat source_dir "moved.txt")
    (Filename.concat dest_dir "moved.txt");
  wait_dir_omits run source_dir "moved.txt";
  wait_dir_contains run dest_dir "moved.txt";
  Sys.remove (Filename.concat dir "charlie.txt");
  wait_dir_omits run dir "charlie.txt";
  E2eHarness.remount run;
  wait_dir_contains_all run dir [ "alpha.txt"; "delta.txt"; "source"; "dest" ];
  wait_dir_omits_all run dir [ "bravo.txt"; "charlie.txt" ];
  wait_dir_contains run dest_dir "moved.txt";
  wait_dir_omits run source_dir "moved.txt"

let test_delete_trashes_remote_file run dir =
  let case_name = Filename.basename dir in
  let remote_dir =
    wait_remote_child run ~parent_id:run.E2eHarness.run_root_id ~name:case_name
      ~trashed:false
  in
  let file_path = Filename.concat dir "trash-me.txt" in
  write_file file_path "trash me";
  remount_and_assert run (fun () -> wait_file_content run file_path "trash me");
  let remote_file =
    wait_remote_child run ~parent_id:remote_dir.GapiDriveV3Model.File.id
      ~name:"trash-me.txt" ~trashed:false
  in
  Sys.remove file_path;
  E2eHarness.remount run;
  wait_path_absent run file_path;
  wait_dir_omits run dir "trash-me.txt";
  wait_remote_file_trashed run ~file_id:remote_file.GapiDriveV3Model.File.id

let test_moderate_size_file_remount_read run dir =
  let file_path = Filename.concat dir "moderate.bin" in
  let initial = deterministic_content (1024 * 1024) in
  write_file file_path initial;
  remount_and_assert run (fun () ->
      wait_file_size run file_path (Int64.of_int (String.length initial));
      wait_file_content run file_path initial);
  let replacement = deterministic_content 4096 in
  let offset = 512 * 1024 in
  write_file_at file_path (Int64.of_int offset) replacement;
  let expected =
    String.sub initial 0 offset
    ^ replacement
    ^ String.sub initial
        (offset + String.length replacement)
        (String.length initial - offset - String.length replacement)
  in
  wait_file_size run file_path (Int64.of_int (String.length expected));
  remount_and_assert run (fun () ->
      wait_file_size run file_path (Int64.of_int (String.length expected));
      wait_file_content run file_path expected)

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
         "overwrite shorter remount read"
         >:: with_case "test-overwrite-shorter-remount-read"
               test_overwrite_shorter_remount_read;
         "overwrite longer remount read"
         >:: with_case "test-overwrite-longer-remount-read"
               test_overwrite_longer_remount_read;
         "append remount read"
         >:: with_case "test-append-remount-read" test_append_remount_read;
         "partial overwrite remount read"
         >:: with_case "test-partial-overwrite-remount-read"
               test_partial_overwrite_remount_read;
         "listing cache coherence"
         >:: with_case "test-listing-cache-coherence"
               test_listing_cache_coherence;
         "delete trashes remote file"
         >:: with_case "test-delete-trashes-remote-file"
               test_delete_trashes_remote_file;
         "moderate size file remount read"
         >:: with_case "test-moderate-size-file-remount-read"
               test_moderate_size_file_remount_read;
       ]
