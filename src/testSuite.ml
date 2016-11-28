open OUnit

let _ =
  let suite =
    "google-drive-ocamlfuse test suite" >:::
    [TestBuffering.suite;
     TestThreadPool.suite
    ] in
  OUnit.run_test_tt_main suite

