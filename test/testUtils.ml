open OUnit

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
