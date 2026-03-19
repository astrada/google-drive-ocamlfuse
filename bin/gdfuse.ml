module Flow = GdfuseFlow.Make (GdfuseRuntimeDeps)

let quit_with_error error_message =
  Printf.eprintf "Error: %s\n" error_message;
  exit 1

let () =
  let parsed = GdfuseCli.parse () in
  if parsed.GdfuseCli.show_version then GdfuseCli.print_version ()
  else
    try
      if parsed.mount_requested then
        Flow.run_mount_mode parsed.params parsed.fuse_args
      else Flow.run_bootstrap_only parsed.params
    with
    | Failure error_message -> quit_with_error error_message
    | e -> Printexc.to_string e |> quit_with_error
