let _ =
  let suite =
    TestBuffering.suite in
  OUnit.run_test_tt_main suite

