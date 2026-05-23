open OUnit
module File = GapiDriveV3Model.File

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

let wait_file_perm run path expected =
  wait_until run (Printf.sprintf "%s mode to be %03o" path expected) (fun () ->
      if not (Sys.file_exists path) then (false, "missing")
      else
        let actual =
          (Unix.LargeFile.stat path).Unix.LargeFile.st_perm land 0o7777
        in
        (actual = expected, Printf.sprintf "mode=%03o" actual))

let wait_file_mtime run path ~expected ~tolerance =
  wait_until run
    (Printf.sprintf "%s mtime to be %.3f +/- %.3fs" path expected tolerance)
    (fun () ->
      if not (Sys.file_exists path) then (false, "missing")
      else
        let actual = (Unix.LargeFile.stat path).Unix.LargeFile.st_mtime in
        let delta = abs_float (actual -. expected) in
        ( delta <= tolerance,
          Printf.sprintf "mtime=%.3f expected=%.3f delta=%.3f" actual expected
            delta ))

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

let app_property file name =
  try Some (List.assoc name file.File.appProperties) with Not_found -> None

let wait_remote_mode_app_property run ~file_id expected =
  wait_until run
    (Printf.sprintf "Drive file %s mode appProperty to be %03o" file_id expected)
    (fun () ->
      let file = E2eDrive.get_file run.E2eHarness.drive ~file_id in
      match app_property file "mode" with
      | None -> (false, "missing")
      | Some value -> (
          try
            let actual = int_of_string value land 0o7777 in
            ( actual = expected,
              Printf.sprintf "mode_app_property=%s parsed_mode=%03o" value
                actual )
          with Failure _ ->
            ( false,
              Printf.sprintf "mode_app_property=%S is not an integer" value )))

let wait_remote_app_property run ~file_id ~name ~value =
  wait_until run
    (Printf.sprintf "Drive file %s appProperty %s to be %S" file_id name value)
    (fun () ->
      let file = E2eDrive.get_file run.E2eHarness.drive ~file_id in
      let actual = app_property file name in
      ( actual = Some value,
        Printf.sprintf "appProperty=%s"
          (match actual with None -> "<missing>" | Some value -> value) ))

let wait_remote_mtime run ~file_id ~expected ~tolerance =
  wait_until run
    (Printf.sprintf "Drive file %s modifiedTime to be %.3f +/- %.3fs" file_id
       expected tolerance) (fun () ->
      let file = E2eDrive.get_file run.E2eHarness.drive ~file_id in
      let actual = Netdate.since_epoch file.File.modifiedTime in
      let delta = abs_float (actual -. expected) in
      ( delta <= tolerance,
        Printf.sprintf "remote_mtime=%.3f expected=%.3f delta=%.3f" actual
          expected delta ))

let skip_xattr_unsupported operation e =
  if E2eXattr.is_unsupported e then (
    let message =
      Printf.sprintf "xattr %s is unsupported by this environment: %s" operation
        (Printexc.to_string e)
    in
    skip_if true message;
    assert false)
  else raise e

let set_xattr_or_skip path name value =
  try E2eXattr.set path name value with e -> skip_xattr_unsupported "set" e

let remove_xattr_or_skip path name =
  try E2eXattr.remove path name with e -> skip_xattr_unsupported "remove" e

let get_xattr_or_skip path name =
  try E2eXattr.get path name with e -> skip_xattr_unsupported "get" e

let list_xattr_or_skip path =
  try E2eXattr.list path with e -> skip_xattr_unsupported "list" e

let wait_xattr_value run path name expected =
  wait_until run (Printf.sprintf "%s xattr %s to be %S" path name expected)
    (fun () ->
      try
        let actual = get_xattr_or_skip path name in
        (actual = expected, Printf.sprintf "value=%S" actual)
      with Not_found -> (false, "missing"))

let wait_xattr_absent run path name =
  wait_until run (Printf.sprintf "%s xattr %s to be absent" path name)
    (fun () ->
      try
        let actual = get_xattr_or_skip path name in
        (false, Printf.sprintf "value=%S" actual)
      with Not_found -> (true, "missing"))

let wait_xattr_list_contains run path name =
  wait_until run (Printf.sprintf "%s xattr list to contain %s" path name)
    (fun () ->
      let names = list_xattr_or_skip path in
      (List.mem name names, "names=[" ^ String.concat "," names ^ "]"))

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
  try f run (case_dir run name) with
  | OUnitTest.Skip _ as e -> raise e
  | e ->
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

let test_chmod_remount_stat run dir =
  let case_name = Filename.basename dir in
  let file_path = Filename.concat dir "mode.txt" in
  write_file file_path "mode metadata\n";
  remount_and_assert run (fun () ->
      wait_file_content run file_path "mode metadata\n");
  let remote_dir =
    wait_remote_child run ~parent_id:run.E2eHarness.run_root_id ~name:case_name
      ~trashed:false
  in
  let remote_file =
    wait_remote_child run ~parent_id:remote_dir.File.id ~name:"mode.txt"
      ~trashed:false
  in
  let expected_perm = 0o600 in
  Unix.chmod file_path expected_perm;
  wait_file_perm run file_path expected_perm;
  wait_remote_mode_app_property run ~file_id:remote_file.File.id expected_perm;
  remount_and_assert run (fun () ->
      wait_file_perm run file_path expected_perm;
      wait_file_content run file_path "mode metadata\n")

let test_utime_remount_stat run dir =
  let case_name = Filename.basename dir in
  let file_path = Filename.concat dir "mtime.txt" in
  write_file file_path "mtime metadata\n";
  remount_and_assert run (fun () ->
      wait_file_content run file_path "mtime metadata\n");
  let remote_dir =
    wait_remote_child run ~parent_id:run.E2eHarness.run_root_id ~name:case_name
      ~trashed:false
  in
  let remote_file =
    wait_remote_child run ~parent_id:remote_dir.File.id ~name:"mtime.txt"
      ~trashed:false
  in
  let expected_mtime = 1_700_000_000.0 in
  let tolerance = 1.0 in
  Unix.utimes file_path expected_mtime expected_mtime;
  wait_file_mtime run file_path ~expected:expected_mtime ~tolerance;
  wait_remote_mtime run ~file_id:remote_file.File.id ~expected:expected_mtime
    ~tolerance;
  remount_and_assert run (fun () ->
      wait_file_mtime run file_path ~expected:expected_mtime ~tolerance;
      wait_file_content run file_path "mtime metadata\n")

let test_xattr_remount_roundtrip run dir =
  let case_name = Filename.basename dir in
  let file_path = Filename.concat dir "xattr.txt" in
  write_file file_path "xattr metadata\n";
  remount_and_assert run (fun () ->
      wait_file_content run file_path "xattr metadata\n");
  let remote_dir =
    wait_remote_child run ~parent_id:run.E2eHarness.run_root_id ~name:case_name
      ~trashed:false
  in
  let remote_file =
    wait_remote_child run ~parent_id:remote_dir.File.id ~name:"xattr.txt"
      ~trashed:false
  in
  let name = "user.gdfuse_e2e" in
  let value = "metadata-roundtrip" in
  set_xattr_or_skip file_path name value;
  wait_xattr_value run file_path name value;
  wait_xattr_list_contains run file_path name;
  wait_remote_app_property run ~file_id:remote_file.File.id ~name:("x-" ^ name)
    ~value;
  remount_and_assert run (fun () ->
      wait_xattr_value run file_path name value;
      wait_xattr_list_contains run file_path name);
  remove_xattr_or_skip file_path name;
  wait_xattr_absent run file_path name;
  remount_and_assert run (fun () -> wait_xattr_absent run file_path name)

type case = {
  label : string;
  directory : string;
  test : E2eHarness.t -> string -> unit;
}

let contains ~needle haystack =
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  if needle_length = 0 then true
  else if needle_length > haystack_length then false
  else
    let rec loop index =
      if index > haystack_length - needle_length then false
      else if String.sub haystack index needle_length = needle then true
      else loop (index + 1)
    in
    loop 0

let cases =
  [
    {
      label = "mount root listing";
      directory = "test-mount-root-listing";
      test = test_mount_root_listing;
    };
    {
      label = "create write remount read";
      directory = "test-create-write-remount-read";
      test = test_create_write_remount_read;
    };
    {
      label = "create remove directory";
      directory = "test-create-remove-directory";
      test = test_create_remove_directory;
    };
    {
      label = "rename file";
      directory = "test-rename-file";
      test = test_rename_file;
    };
    {
      label = "move file between directories";
      directory = "test-move-file-between-directories";
      test = test_move_file_between_directories;
    };
    {
      label = "truncate remount read";
      directory = "test-truncate-remount-read";
      test = test_truncate_remount_read;
    };
    {
      label = "delete remount absent";
      directory = "test-delete-remount-absent";
      test = test_delete_remount_absent;
    };
    {
      label = "overwrite shorter remount read";
      directory = "test-overwrite-shorter-remount-read";
      test = test_overwrite_shorter_remount_read;
    };
    {
      label = "overwrite longer remount read";
      directory = "test-overwrite-longer-remount-read";
      test = test_overwrite_longer_remount_read;
    };
    {
      label = "append remount read";
      directory = "test-append-remount-read";
      test = test_append_remount_read;
    };
    {
      label = "partial overwrite remount read";
      directory = "test-partial-overwrite-remount-read";
      test = test_partial_overwrite_remount_read;
    };
    {
      label = "listing cache coherence";
      directory = "test-listing-cache-coherence";
      test = test_listing_cache_coherence;
    };
    {
      label = "delete trashes remote file";
      directory = "test-delete-trashes-remote-file";
      test = test_delete_trashes_remote_file;
    };
    {
      label = "moderate size file remount read";
      directory = "test-moderate-size-file-remount-read";
      test = test_moderate_size_file_remount_read;
    };
    {
      label = "chmod remount stat";
      directory = "test-chmod-remount-stat";
      test = test_chmod_remount_stat;
    };
    {
      label = "utime remount stat";
      directory = "test-utime-remount-stat";
      test = test_utime_remount_stat;
    };
    {
      label = "xattr remount roundtrip";
      directory = "test-xattr-remount-roundtrip";
      test = test_xattr_remount_roundtrip;
    };
  ]

let case_summary { label; directory; _ } =
  Printf.sprintf "%s (%s)" label directory

let available_cases_text () =
  cases |> List.map case_summary
  |> List.map (Printf.sprintf "  - %s")
  |> String.concat "\n"

let matches_filter filter { label; directory; _ } =
  contains ~needle:filter label || contains ~needle:filter directory

let selected_cases = function
  | None -> cases
  | Some filter -> List.filter (matches_filter filter) cases

let suite ?only () =
  let selected = selected_cases only in
  if selected = [] then
    raise
      (E2eHarness.Error
         (Printf.sprintf "no e2e cases matched %S. Available cases:\n%s"
            (match only with None -> "" | Some filter -> filter)
            (available_cases_text ())));
  "google-drive-ocamlfuse e2e"
  >::: List.map
         (fun case -> case.label >:: with_case case.directory case.test)
         selected
