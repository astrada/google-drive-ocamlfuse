module Flow = GdfuseFlow.Make (GdfuseRuntimeDeps)
module App =
  GdfuseApp.Make (struct
    module Flow = Flow

    let print_stdout message =
      output_string stdout message;
      flush stdout

    let print_stderr message =
      output_string stderr message;
      flush stderr

    let exit = exit
  end)

let () = App.run ()
