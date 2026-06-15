open OUnit
open TestUtils

exception Exit_called of int

module FakeDeps = struct
  let stdout_messages = ref []
  let stderr_messages = ref []
  let events = ref []
  let mount_calls = ref []
  let bootstrap_calls = ref []
  let mount_exception = ref None
  let bootstrap_exception = ref None
  let record event = events := !events @ [ event ]

  module Flow = struct
    let run_bootstrap_only params =
      record "bootstrap";
      bootstrap_calls := !bootstrap_calls @ [ params ];
      match !bootstrap_exception with Some exn -> raise exn | None -> ()

    let run_mount_mode params fuse_args =
      record "mount";
      mount_calls := !mount_calls @ [ (params, fuse_args) ];
      match !mount_exception with Some exn -> raise exn | None -> ()
  end

  let print_stdout message = stdout_messages := !stdout_messages @ [ message ]
  let print_stderr message = stderr_messages := !stderr_messages @ [ message ]
  let exit code = raise (Exit_called code)
end

module App = GdfuseApp.Make (FakeDeps)

let reset_cli_globals () = Utils.verbose := false

let reset_fake_deps () =
  FakeDeps.stdout_messages := [];
  FakeDeps.stderr_messages := [];
  FakeDeps.events := [];
  FakeDeps.mount_calls := [];
  FakeDeps.bootstrap_calls := [];
  FakeDeps.mount_exception := None;
  FakeDeps.bootstrap_exception := None

let with_clean_runtime f =
  Utils.try_finally
    (fun () ->
      reset_cli_globals ();
      reset_fake_deps ();
      f ())
    (fun () ->
      reset_fake_deps ();
      reset_cli_globals ())

let expect_exit expected_code f =
  try
    f ();
    assert_failure
      (Printf.sprintf "Expected Exit_called %d, but no exit occurred"
         expected_code)
  with Exit_called code -> assert_equal expected_code code

let only_message = function
  | [ message ] -> message
  | messages ->
      assert_failure
        (Printf.sprintf "Expected exactly one message, got %d"
           (List.length messages))

let test_help_prints_to_stdout_and_exits_cleanly () =
  with_clean_runtime (fun () ->
      expect_exit 0 (fun () -> App.run_argv [| "gdfuse"; "-help" |]);
      let stdout_message = only_message !FakeDeps.stdout_messages in
      assert_contains "Usage: gdfuse" stdout_message;
      assert_equal ~printer:string_of_int 0
        (List.length !FakeDeps.stderr_messages);
      assert_equal ~printer:string_of_int 0 (List.length !FakeDeps.events))

let test_double_dash_help_prints_to_stdout_and_exits_cleanly () =
  with_clean_runtime (fun () ->
      expect_exit 0 (fun () -> App.run_argv [| "gdfuse"; "--help" |]);
      let stdout_message = only_message !FakeDeps.stdout_messages in
      assert_contains "Usage: gdfuse" stdout_message;
      assert_equal ~printer:string_of_int 0
        (List.length !FakeDeps.stderr_messages);
      assert_equal ~printer:string_of_int 0 (List.length !FakeDeps.events))

let test_invalid_option_prints_arg_error_without_running_flow () =
  with_clean_runtime (fun () ->
      expect_exit 2 (fun () ->
          App.run_argv [| "gdfuse"; "--definitely-not-a-real-flag" |]);
      let stderr_message = only_message !FakeDeps.stderr_messages in
      assert_contains "unknown option '--definitely-not-a-real-flag'"
        stderr_message;
      assert_contains "Usage: gdfuse" stderr_message;
      assert_not_contains "Error:" stderr_message;
      assert_equal ~printer:string_of_int 0
        (List.length !FakeDeps.stdout_messages);
      assert_equal ~printer:string_of_int 0 (List.length !FakeDeps.events))

let test_version_prints_and_exits_without_running_flow () =
  with_clean_runtime (fun () ->
      expect_exit 0 (fun () -> App.run_argv [| "gdfuse"; "-version" |]);
      let stdout_message = only_message !FakeDeps.stdout_messages in
      assert_equal ~printer:(fun x -> x) GdfuseCli.version_text stdout_message;
      assert_equal ~printer:string_of_int 0
        (List.length !FakeDeps.stderr_messages);
      assert_equal ~printer:string_of_int 0 (List.length !FakeDeps.events))

let test_mount_invocation_runs_mount_mode () =
  with_clean_runtime (fun () ->
      App.run_argv [| "gdfuse"; "/tmp/mnt" |];
      assert_equal ~printer:(String.concat ";") [ "mount" ] !FakeDeps.events;
      match !FakeDeps.mount_calls with
      | [ (params, fuse_args) ] ->
          assert_equal
            ~printer:(fun x -> x)
            "/tmp/mnt" params.GdfuseCommon.mountpoint;
          assert_equal ~printer:(String.concat ",") [ "-f" ] fuse_args
      | _ -> assert_failure "Expected exactly one mount invocation")

let test_no_mount_invocation_runs_bootstrap_only () =
  with_clean_runtime (fun () ->
      App.run_argv [| "gdfuse" |];
      assert_equal ~printer:(String.concat ";") [ "bootstrap" ] !FakeDeps.events;
      match !FakeDeps.bootstrap_calls with
      | [ params ] ->
          assert_equal ~printer:(fun x -> x) "" params.GdfuseCommon.mountpoint
      | _ -> assert_failure "Expected exactly one bootstrap invocation")

let test_runtime_failure_keeps_clean_error_format () =
  with_clean_runtime (fun () ->
      FakeDeps.mount_exception := Some (Failure "boom");
      expect_exit 1 (fun () -> App.run_argv [| "gdfuse"; "/tmp/mnt" |]);
      let stderr_message = only_message !FakeDeps.stderr_messages in
      assert_equal ~printer:(fun x -> x) "Error: boom\n" stderr_message;
      assert_equal ~printer:(String.concat ";") [ "mount" ] !FakeDeps.events)

let suite =
  "GdfuseApp test"
  >::: [
         "test_help_prints_to_stdout_and_exits_cleanly"
         >:: test_help_prints_to_stdout_and_exits_cleanly;
         "test_double_dash_help_prints_to_stdout_and_exits_cleanly"
         >:: test_double_dash_help_prints_to_stdout_and_exits_cleanly;
         "test_invalid_option_prints_arg_error_without_running_flow"
         >:: test_invalid_option_prints_arg_error_without_running_flow;
         "test_version_prints_and_exits_without_running_flow"
         >:: test_version_prints_and_exits_without_running_flow;
         "test_mount_invocation_runs_mount_mode"
         >:: test_mount_invocation_runs_mount_mode;
         "test_no_mount_invocation_runs_bootstrap_only"
         >:: test_no_mount_invocation_runs_bootstrap_only;
         "test_runtime_failure_keeps_clean_error_format"
         >:: test_runtime_failure_keeps_clean_error_format;
       ]
