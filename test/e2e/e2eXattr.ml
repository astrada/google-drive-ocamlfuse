let unsupported_message = "xattrs are not supported by this environment"
let missing_python_message = "python3 is required for e2e xattr tests"
let env name = try Some (Sys.getenv name) with Not_found -> None

let python () =
  match env "GDFUSE_E2E_PYTHON" with Some path -> path | None -> "python3"

let script =
  {|
import errno
import os
import sys

def has_errno(error, name):
    return error.errno == getattr(errno, name, None)

op = sys.argv[1]
path = sys.argv[2]
name = sys.argv[3] if len(sys.argv) > 3 else None

try:
    if op == "set":
        os.setxattr(path, name, sys.argv[4].encode("utf-8"))
    elif op == "get":
        sys.stdout.buffer.write(os.getxattr(path, name))
    elif op == "list":
        for attr_name in os.listxattr(path):
            print(attr_name)
    elif op == "remove":
        os.removexattr(path, name)
    else:
        print("unknown xattr operation: %s" % op, file=sys.stderr)
        sys.exit(1)
except AttributeError:
    print("python os module does not expose xattr calls", file=sys.stderr)
    sys.exit(2)
except OSError as error:
    if has_errno(error, "ENODATA") or has_errno(error, "ENOATTR"):
        sys.exit(4)
    if has_errno(error, "ENOTSUP") or has_errno(error, "EOPNOTSUPP") or has_errno(error, "ENOSYS"):
        print(error, file=sys.stderr)
        sys.exit(2)
    print("%s: %s" % (error.errno, error.strerror), file=sys.stderr)
    sys.exit(3)
|}

let read_all ch =
  let buffer = Buffer.create 128 in
  let chunk = Bytes.create 4096 in
  let rec loop () =
    match input ch chunk 0 (Bytes.length chunk) with
    | 0 -> Buffer.contents buffer
    | count ->
        Buffer.add_subbytes buffer chunk 0 count;
        loop ()
  in
  loop ()

let run operation args =
  let executable = python () in
  let process_args =
    Array.of_list (executable :: "-c" :: script :: operation :: args)
  in
  try
    let stdout_ch, stdin_ch, stderr_ch =
      Unix.open_process_args_full executable process_args (Unix.environment ())
    in
    let stdout = read_all stdout_ch in
    let stderr = read_all stderr_ch in
    match Unix.close_process_full (stdout_ch, stdin_ch, stderr_ch) with
    | Unix.WEXITED 0 -> stdout
    | Unix.WEXITED 2 -> raise (Failure unsupported_message)
    | Unix.WEXITED 4 -> raise Not_found
    | Unix.WEXITED code ->
        raise
          (Failure
             (Printf.sprintf "xattr %s failed with exit code %d: %s" operation
                code stderr))
    | Unix.WSIGNALED signal ->
        raise
          (Failure
             (Printf.sprintf "xattr %s was killed by signal %d" operation signal))
    | Unix.WSTOPPED signal ->
        raise
          (Failure
             (Printf.sprintf "xattr %s was stopped by signal %d" operation
                signal))
  with Unix.Unix_error (Unix.ENOENT, _, _) ->
    raise (Failure missing_python_message)

let set path name value = ignore (run "set" [ path; name; value ])
let get path name = run "get" [ path; name ]

let list path =
  run "list" [ path ] |> String.split_on_char '\n'
  |> List.filter (fun name -> name <> "")

let remove path name = ignore (run "remove" [ path; name ])

let is_unsupported = function
  | Failure message
    when message = unsupported_message || message = missing_python_message ->
      true
  | Unix.Unix_error ((Unix.EOPNOTSUPP | Unix.ENOSYS), _, _) -> true
  | _ -> false
