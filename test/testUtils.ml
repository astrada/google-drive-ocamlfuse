open OUnit

let with_temp_dir f =
  let rec make_dir n =
    let path =
      Filename.concat
        (Filename.get_temp_dir_name ())
        (Printf.sprintf "gdfuse-configstore-%d-%06d" (Unix.getpid ()) n)
    in
    if Sys.file_exists path then make_dir (n + 1)
    else (
      Unix.mkdir path 0o700;
      path)
  in
  let dir = make_dir 0 in
  let finally () =
    if Sys.file_exists dir then
      Array.iter
        (fun name -> Sys.remove (Filename.concat dir name))
        (Sys.readdir dir);
    if Sys.file_exists dir then Unix.rmdir dir
  in
  Utils.try_finally (fun () -> f dir) finally

let read_file path =
  Utils.with_in_channel path (fun ch ->
      let buffer = Buffer.create 256 in
      (try
         while true do
           Buffer.add_string buffer (input_line ch);
           Buffer.add_char buffer '\n'
         done
       with End_of_file -> ());
      Buffer.contents buffer)

let write_file path content =
  Utils.with_out_channel ~mode:[ Open_creat; Open_trunc; Open_wronly ] path
    (fun ch -> output_string ch content)

let assert_contains needle haystack =
  assert_bool
    (Printf.sprintf "Expected to find %S in:\n%s" needle haystack)
    (try
       ignore (Str.search_forward (Str.regexp_string needle) haystack 0);
       true
     with Not_found -> false)

let assert_not_contains needle haystack =
  assert_bool
    (Printf.sprintf "Expected not to find %S in:\n%s" needle haystack)
    (try
       ignore (Str.search_forward (Str.regexp_string needle) haystack 0);
       false
     with Not_found -> true)

let assert_parse_error_contains needle f =
  try
    f ();
    assert_failure (Printf.sprintf "Expected Parse_error containing %S" needle)
  with
  | ConfigStore.Parse_error message -> assert_contains needle message
  | exn -> raise exn

let test_normlize_absolute_path () =
  let normalized =
    Utils.normalize_absolute_path "/home/test/../tmp/./gdrive/test123/../test"
  in
  assert_equal ~printer:(fun x -> x) "/home/tmp/gdrive/test" normalized

let test_normlize_absolute_path_2 () =
  let normalized = Utils.normalize_absolute_path "/home/..//.///./test" in
  assert_equal ~printer:(fun x -> x) "/test" normalized

let test_normlize_absolute_path_3 () =
  let normalized = Utils.normalize_absolute_path "/home/..//.///./test/" in
  assert_equal ~printer:(fun x -> x) "/test" normalized

let suite =
  "Utils test"
  >::: [
         "test_normlize_absolute_path" >:: test_normlize_absolute_path;
         "test_normlize_absolute_path 2" >:: test_normlize_absolute_path_2;
         "test_normlize_absolute_path 3" >:: test_normlize_absolute_path_3;
       ]
