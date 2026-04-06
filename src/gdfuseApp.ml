module type FLOW = sig
  val run_bootstrap_only : GdfuseCommon.application_params -> unit
  val run_mount_mode : GdfuseCommon.application_params -> string list -> unit
end

module type DEPS = sig
  module Flow : FLOW

  val print_stdout : string -> unit
  val print_stderr : string -> unit
  val exit : int -> 'a
end

module Make (D : DEPS) = struct
  let ensure_trailing_newline message =
    if message = "" || message.[String.length message - 1] = '\n' then message
    else message ^ "\n"

  let print_stdout message = ensure_trailing_newline message |> D.print_stdout
  let print_stderr message = ensure_trailing_newline message |> D.print_stderr

  let quit_with_error error_message =
    Printf.sprintf "Error: %s\n" error_message |> D.print_stderr;
    D.exit 1

  let run_parsed parsed =
    try
      if parsed.GdfuseCli.mount_requested then
        D.Flow.run_mount_mode parsed.params parsed.fuse_args
      else D.Flow.run_bootstrap_only parsed.params
    with
    | Failure error_message -> quit_with_error error_message
    | e -> Printexc.to_string e |> quit_with_error

  let run_argv argv =
    match GdfuseCli.parse_argv argv with
    | GdfuseCli.Parsed parsed -> run_parsed parsed
    | GdfuseCli.Show_version ->
        D.print_stdout GdfuseCli.version_text;
        D.exit 0
    | GdfuseCli.Help message ->
        print_stdout message;
        D.exit 0
    | GdfuseCli.Error message ->
        print_stderr message;
        D.exit 2

  let run () = run_argv Sys.argv
end
