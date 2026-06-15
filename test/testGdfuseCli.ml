open OUnit
open TestUtils

let reset_cli_globals () = Utils.verbose := false

let with_clean_cli_globals f =
  Utils.try_finally
    (fun () ->
      reset_cli_globals ();
      f ())
    reset_cli_globals

let assert_parsed = function
  | GdfuseCli.Parsed parsed -> parsed
  | GdfuseCli.Show_version ->
      assert_failure "Expected Parsed outcome, got Show_version"
  | GdfuseCli.Help _ -> assert_failure "Expected Parsed outcome, got Help"
  | GdfuseCli.Error _ -> assert_failure "Expected Parsed outcome, got Error"

let assert_help_contains needle = function
  | GdfuseCli.Help message -> assert_contains needle message
  | GdfuseCli.Parsed _ -> assert_failure "Expected Help outcome, got Parsed"
  | GdfuseCli.Show_version ->
      assert_failure "Expected Help outcome, got Show_version"
  | GdfuseCli.Error _ -> assert_failure "Expected Help outcome, got Error"

let assert_error_contains needle = function
  | GdfuseCli.Error message -> assert_contains needle message
  | GdfuseCli.Parsed _ -> assert_failure "Expected Error outcome, got Parsed"
  | GdfuseCli.Show_version ->
      assert_failure "Expected Error outcome, got Show_version"
  | GdfuseCli.Help _ -> assert_failure "Expected Error outcome, got Help"

let test_parse_argv_forces_foreground () =
  with_clean_cli_globals (fun () ->
      let parsed =
        assert_parsed (GdfuseCli.parse_argv [| "gdfuse"; "/tmp/mnt" |])
      in
      assert_bool "Expected mountpoint to be detected" parsed.mount_requested;
      assert_equal ~printer:(String.concat ",") [ "-f" ] parsed.fuse_args)

let test_parse_argv_debug_sets_verbose_without_extra_f_flag () =
  with_clean_cli_globals (fun () ->
      let parsed =
        assert_parsed
          (GdfuseCli.parse_argv [| "gdfuse"; "-debug"; "/tmp/mnt" |])
      in
      assert_equal ~printer:(String.concat ",") [ "-f" ] parsed.fuse_args;
      assert_equal ~printer:string_of_bool true !Utils.verbose;
      assert_equal ~printer:string_of_bool true parsed.params.GdfuseCommon.debug)

let test_parse_argv_mount_options_extract_gdfroot () =
  with_clean_cli_globals (fun () ->
      let parsed =
        assert_parsed
          (GdfuseCli.parse_argv
             [| "gdfuse"; "-o"; "allow_other,gdfroot=/tmp/root"; "/tmp/mnt" |])
      in
      assert_equal
        ~printer:(fun x -> x)
        "/tmp/root" parsed.params.GdfuseCommon.base_dir;
      assert_equal ~printer:(String.concat ",") [ "-oallow_other"; "-f" ]
        parsed.fuse_args)

let test_parse_argv_help_returns_help_outcome () =
  with_clean_cli_globals (fun () ->
      GdfuseCli.parse_argv [| "gdfuse"; "-help" |]
      |> assert_help_contains "Usage: gdfuse")

let test_parse_argv_double_dash_help_returns_help_outcome () =
  with_clean_cli_globals (fun () ->
      GdfuseCli.parse_argv [| "gdfuse"; "--help" |]
      |> assert_help_contains "Usage: gdfuse")

let test_parse_argv_version_returns_show_version () =
  with_clean_cli_globals (fun () ->
      match GdfuseCli.parse_argv [| "gdfuse"; "-version" |] with
      | GdfuseCli.Show_version -> ()
      | GdfuseCli.Parsed _ ->
          assert_failure "Expected Show_version outcome, got Parsed"
      | GdfuseCli.Help _ ->
          assert_failure "Expected Show_version outcome, got Help"
      | GdfuseCli.Error _ ->
          assert_failure "Expected Show_version outcome, got Error")

let test_parse_argv_unexpected_option_returns_error_outcome () =
  with_clean_cli_globals (fun () ->
      GdfuseCli.parse_argv [| "gdfuse"; "--definitely-not-a-real-flag" |]
      |> assert_error_contains "unknown option '--definitely-not-a-real-flag'")

let test_parse_argv_invalid_gdfroot_returns_error_outcome () =
  with_clean_cli_globals (fun () ->
      GdfuseCli.parse_argv [| "gdfuse"; "-o"; "gdfroot"; "/tmp/mnt" |]
      |> assert_error_contains "Invalid mount option gdfroot")

let test_parse_argv_d_adds_fuse_debug_flag () =
  with_clean_cli_globals (fun () ->
      let parsed =
        assert_parsed (GdfuseCli.parse_argv [| "gdfuse"; "-d"; "/tmp/mnt" |])
      in
      assert_equal ~printer:(String.concat ",") [ "-d"; "-f" ] parsed.fuse_args)

let test_parse_argv_s_forces_single_threaded_mode () =
  with_clean_cli_globals (fun () ->
      let parsed =
        assert_parsed (GdfuseCli.parse_argv [| "gdfuse"; "-s"; "/tmp/mnt" |])
      in
      assert_equal ~printer:(String.concat ",") [ "-s"; "-f" ] parsed.fuse_args;
      assert_equal ~printer:string_of_bool false
        parsed.params.GdfuseCommon.multi_threading)

let test_parse_argv_m_enables_multithreading_without_changing_fuse_args () =
  with_clean_cli_globals (fun () ->
      let parsed =
        assert_parsed (GdfuseCli.parse_argv [| "gdfuse"; "-m"; "/tmp/mnt" |])
      in
      assert_equal ~printer:(String.concat ",") [ "-f" ] parsed.fuse_args;
      assert_equal ~printer:string_of_bool true
        parsed.params.GdfuseCommon.multi_threading)

let test_parse_argv_without_mountpoint_keeps_bootstrap_shape () =
  with_clean_cli_globals (fun () ->
      let parsed = assert_parsed (GdfuseCli.parse_argv [| "gdfuse" |]) in
      assert_equal ~printer:string_of_bool false parsed.mount_requested;
      assert_equal
        ~printer:(fun x -> x)
        "" parsed.params.GdfuseCommon.mountpoint)

let suite =
  "GdfuseCli test"
  >::: [
         "test_parse_argv_forces_foreground"
         >:: test_parse_argv_forces_foreground;
         "test_parse_argv_debug_sets_verbose_without_extra_f_flag"
         >:: test_parse_argv_debug_sets_verbose_without_extra_f_flag;
         "test_parse_argv_mount_options_extract_gdfroot"
         >:: test_parse_argv_mount_options_extract_gdfroot;
         "test_parse_argv_help_returns_help_outcome"
         >:: test_parse_argv_help_returns_help_outcome;
         "test_parse_argv_double_dash_help_returns_help_outcome"
         >:: test_parse_argv_double_dash_help_returns_help_outcome;
         "test_parse_argv_version_returns_show_version"
         >:: test_parse_argv_version_returns_show_version;
         "test_parse_argv_unexpected_option_returns_error_outcome"
         >:: test_parse_argv_unexpected_option_returns_error_outcome;
         "test_parse_argv_invalid_gdfroot_returns_error_outcome"
         >:: test_parse_argv_invalid_gdfroot_returns_error_outcome;
         "test_parse_argv_d_adds_fuse_debug_flag"
         >:: test_parse_argv_d_adds_fuse_debug_flag;
         "test_parse_argv_s_forces_single_threaded_mode"
         >:: test_parse_argv_s_forces_single_threaded_mode;
         "test_parse_argv_m_enables_multithreading_without_changing_fuse_args"
         >:: test_parse_argv_m_enables_multithreading_without_changing_fuse_args;
         "test_parse_argv_without_mountpoint_keeps_bootstrap_shape"
         >:: test_parse_argv_without_mountpoint_keeps_bootstrap_shape;
       ]
