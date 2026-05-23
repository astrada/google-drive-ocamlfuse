open OUnit

let classify_failure = function
  | E2eSettings.Error message -> "configuration error: " ^ message
  | E2eConfig.Error message -> "configuration error: " ^ message
  | E2eDrive.Error message -> "environment/preflight error: " ^ message
  | E2eMount.Error message -> "mount lifecycle error: " ^ message
  | E2eHarness.Error message -> "harness cleanup error: " ^ message
  | e -> Printexc.to_string e

let wrap_test_case test =
  test
  |> test_decorate (fun f context ->
      try f context
      with
      | ( E2eSettings.Error _ | E2eConfig.Error _ | E2eDrive.Error _
        | E2eMount.Error _ | E2eHarness.Error _ ) as e
      ->
        assert_failure (classify_failure e))

let run_preflight () =
  try
    E2eHarness.preflight ();
    exit 0
  with e ->
    prerr_endline (classify_failure e);
    exit 1

let _ =
  Printexc.record_backtrace true;
  if Array.exists (( = ) "--preflight") Sys.argv then run_preflight ()
  else
    let suite = wrap_test_case E2eFilesystemTests.suite in
    OUnit.run_test_tt_main suite
