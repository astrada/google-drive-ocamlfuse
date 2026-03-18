let quit_with_error error_message =
  Printf.eprintf "Error: %s\n" error_message;
  exit 1

let run_bootstrap_only params =
  GdfuseSetup.setup_application { params with GdfuseCommon.mountpoint = "." }

let run_mount_mode params fuse_args =
  GdfuseSetup.setup_application params;
  at_exit GdfuseShutdown.shutdown;
  GdfuseFuse.start_filesystem params.GdfuseCommon.mountpoint fuse_args

let () =
  let parsed = GdfuseCli.parse () in
  if parsed.GdfuseCli.show_version then GdfuseCli.print_version ()
  else
    try
      if parsed.mount_requested then
        run_mount_mode parsed.params parsed.fuse_args
      else run_bootstrap_only parsed.params
    with
    | Failure error_message -> quit_with_error error_message
    | e -> Printexc.to_string e |> quit_with_error
