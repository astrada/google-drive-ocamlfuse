open Printf

let () =
  let dir = Sys.argv.(1) in
  (try Sys.chdir dir
   with _ -> eprintf "Cannot change directory to %s\n%!" dir);
  exit (Sys.command "ocaml setup.ml -uninstall")
