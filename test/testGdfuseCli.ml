open OUnit

let reset_cli_globals () =
  Utils.verbose := false

let with_clean_cli_globals f =
  Utils.try_finally
    (fun () ->
      reset_cli_globals ();
      f ())
    reset_cli_globals

let test_parse_argv_forces_foreground () =
  with_clean_cli_globals (fun () ->
      let parsed = GdfuseCli.parse_argv [| "gdfuse"; "/tmp/mnt" |] in
      assert_bool "Expected mountpoint to be detected" parsed.mount_requested;
      assert_equal ~printer:(String.concat ",") [ "-f"; "-obig_writes" ]
        parsed.fuse_args)

let test_parse_argv_debug_sets_verbose_without_extra_f_flag () =
  with_clean_cli_globals (fun () ->
      let parsed =
        GdfuseCli.parse_argv [| "gdfuse"; "-debug"; "/tmp/mnt" |]
      in
      assert_equal ~printer:(String.concat ",") [ "-f"; "-obig_writes" ]
        parsed.fuse_args;
      assert_equal ~printer:string_of_bool true !Utils.verbose;
      assert_equal ~printer:string_of_bool true parsed.params.GdfuseCommon.debug)

let test_parse_argv_mount_options_extract_gdfroot () =
  with_clean_cli_globals (fun () ->
      let parsed =
        GdfuseCli.parse_argv
          [| "gdfuse"; "-o"; "allow_other,gdfroot=/tmp/root"; "/tmp/mnt" |]
      in
      assert_equal ~printer:(fun x -> x) "/tmp/root"
        parsed.params.GdfuseCommon.base_dir;
      assert_equal ~printer:(String.concat ",")
        [ "-oallow_other"; "-f"; "-obig_writes" ]
        parsed.fuse_args)

let suite =
  "GdfuseCli test"
  >::: [
         "test_parse_argv_forces_foreground"
         >:: test_parse_argv_forces_foreground;
         "test_parse_argv_debug_sets_verbose_without_extra_f_flag"
         >:: test_parse_argv_debug_sets_verbose_without_extra_f_flag;
         "test_parse_argv_mount_options_extract_gdfroot"
         >:: test_parse_argv_mount_options_extract_gdfroot;
       ]
