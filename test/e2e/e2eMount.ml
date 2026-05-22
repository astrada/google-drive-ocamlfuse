exception Error of string

type t = {
  pid : int;
  mountpoint : string;
  stdout_path : string;
  stderr_path : string;
  mutable exit_status : Unix.process_status option;
}

let read_file path =
  if not (Sys.file_exists path) then ""
  else
    Utils.with_in_channel path (fun ch ->
        let buffer = Buffer.create 1024 in
        (try
           while true do
             Buffer.add_string buffer (input_line ch);
             Buffer.add_char buffer '\n'
           done
         with End_of_file -> ());
        Buffer.contents buffer)

let status_to_string = function
  | Unix.WEXITED code -> Printf.sprintf "exited with code %d" code
  | Unix.WSIGNALED signal -> Printf.sprintf "terminated by signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "stopped by signal %d" signal

let reap_if_exited process =
  match process.exit_status with
  | Some status -> Some status
  | None -> (
      match Unix.waitpid [ Unix.WNOHANG ] process.pid with
      | 0, _ -> None
      | _, status ->
          process.exit_status <- Some status;
          Some status)

let process_output process =
  Printf.sprintf "stdout:\n%s\nstderr:\n%s"
    (read_file process.stdout_path)
    (read_file process.stderr_path)

let is_mountpoint path =
  if Sys.file_exists "/proc/self/mounts" then (
    let mounted = ref false in
    Utils.with_in_channel "/proc/self/mounts" (fun ch ->
        try
          while not !mounted do
            let line = input_line ch in
            match Str.split (Str.regexp "[ \t]+") line with
            | _source :: mountpoint :: _ when mountpoint = path ->
                mounted := true
            | _ -> ()
          done
        with End_of_file -> ());
    !mounted)
  else false

let wait_until ?(timeout = 30.0) ?(interval = 0.25) predicate =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec loop () =
    if predicate () then true
    else if Unix.gettimeofday () >= deadline then false
    else (
      Thread.delay interval;
      loop ())
  in
  loop ()

let start ~gdfuse_exe ~label ~config_path ~mountpoint ~stdout_path ~stderr_path
    =
  let stdin_fd = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
  let stdout_fd =
    Unix.openfile stdout_path
      [ Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY ]
      0o600
  in
  let stderr_fd =
    Unix.openfile stderr_path
      [ Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY ]
      0o600
  in
  let args =
    [| gdfuse_exe; "-label"; label; "-config"; config_path; mountpoint |]
  in
  let pid =
    try Unix.create_process gdfuse_exe args stdin_fd stdout_fd stderr_fd
    with e ->
      Unix.close stdin_fd;
      Unix.close stdout_fd;
      Unix.close stderr_fd;
      raise e
  in
  Unix.close stdin_fd;
  Unix.close stdout_fd;
  Unix.close stderr_fd;
  let process =
    { pid; mountpoint; stdout_path; stderr_path; exit_status = None }
  in
  let mounted =
    wait_until (fun () ->
        match reap_if_exited process with
        | Some _ -> false
        | None -> is_mountpoint mountpoint)
  in
  if mounted then process
  else
    let detail =
      match reap_if_exited process with
      | Some status ->
          Printf.sprintf "process %s\n%s" (status_to_string status)
            (process_output process)
      | None ->
          Printf.sprintf "mountpoint did not appear within timeout\n%s"
            (process_output process)
    in
    raise (Error ("mount startup failed: " ^ detail))

let run_unmount_helper command args =
  let devnull_r = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
  let devnull_w = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  let argv = Array.of_list (command :: args) in
  try
    let pid = Unix.create_process command argv devnull_r devnull_w devnull_w in
    Unix.close devnull_r;
    Unix.close devnull_w;
    match snd (Unix.waitpid [] pid) with Unix.WEXITED 0 -> true | _ -> false
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
      Unix.close devnull_r;
      Unix.close devnull_w;
      false
  | e ->
      Unix.close devnull_r;
      Unix.close devnull_w;
      raise e

let unmount mountpoint =
  run_unmount_helper "fusermount3" [ "-u"; mountpoint ]
  || run_unmount_helper "fusermount" [ "-u"; mountpoint ]
  || run_unmount_helper "umount" [ mountpoint ]

let stop process =
  let unmount_ok =
    if is_mountpoint process.mountpoint then unmount process.mountpoint
    else true
  in
  let unmounted =
    if unmount_ok then
      wait_until ~timeout:10.0 (fun () ->
          not (is_mountpoint process.mountpoint))
    else false
  in
  let exited =
    wait_until ~timeout:10.0 (fun () -> Option.is_some (reap_if_exited process))
  in
  if not exited then (
    (try Unix.kill process.pid Sys.sigterm with Unix.Unix_error _ -> ());
    ignore
      (wait_until ~timeout:5.0 (fun () ->
           Option.is_some (reap_if_exited process))));
  if not unmounted then
    raise
      (Error
         (Printf.sprintf "failed to unmount %s\n%s" process.mountpoint
            (process_output process)))
