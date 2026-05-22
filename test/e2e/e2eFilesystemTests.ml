open OUnit

let path run relative = Filename.concat run.E2eHarness.paths.mountpoint relative

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

let assert_not_exists message path =
  assert_bool message (not (Sys.file_exists path))

let test_smoke_lifecycle _ =
  E2eHarness.with_run (fun run ->
      E2eHarness.start_mount run;
      ignore (Sys.readdir run.E2eHarness.paths.mountpoint);

      let hello_path = path run "hello.txt" in
      write_file hello_path "hello from e2e\n";
      E2eHarness.remount run;
      assert_file_content "hello from e2e\n" hello_path;

      let dir_path = path run "dir" in
      Unix.mkdir dir_path 0o755;
      assert_bool "created directory should exist" (Sys.is_directory dir_path);
      Unix.rmdir dir_path;
      assert_not_exists "removed directory should disappear" dir_path;

      let rename_source = path run "rename-source.txt" in
      let rename_target = path run "rename-target.txt" in
      write_file rename_source "rename me\n";
      Sys.rename rename_source rename_target;
      assert_not_exists "rename source should disappear" rename_source;
      assert_file_content "rename me\n" rename_target;

      let from_dir = path run "from" in
      let to_dir = path run "to" in
      Unix.mkdir from_dir 0o755;
      Unix.mkdir to_dir 0o755;
      let move_source = Filename.concat from_dir "move.txt" in
      let move_target = Filename.concat to_dir "move.txt" in
      write_file move_source "move me\n";
      Sys.rename move_source move_target;
      assert_not_exists "move source should disappear" move_source;
      assert_file_content "move me\n" move_target;

      let truncate_path = path run "truncate.txt" in
      write_file truncate_path "abcdef\n";
      Unix.truncate truncate_path 3;
      E2eHarness.remount run;
      assert_file_content "abc" truncate_path;

      let delete_path = path run "delete.txt" in
      write_file delete_path "delete me\n";
      Sys.remove delete_path;
      assert_not_exists "deleted file should disappear" delete_path;

      E2eHarness.remount run;
      assert_file_content "hello from e2e\n" hello_path;
      assert_file_content "rename me\n" rename_target;
      assert_file_content "move me\n" move_target;
      assert_file_content "abc" truncate_path;
      assert_not_exists "deleted file should stay deleted after remount"
        delete_path)

let suite =
  "google-drive-ocamlfuse e2e"
  >::: [ "smoke lifecycle" >:: test_smoke_lifecycle ]
