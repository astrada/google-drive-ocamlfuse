open GapiUtils.Infix

let try_finally f finally =
  try
    let result = f () in
      finally ();
      result
  with e ->
    finally ();
    raise e

let with_in_channel path f =
  let ch = open_in path in
    try_finally
      (fun () -> f ch)
      (fun () -> close_in ch)

let get_thread_id () =
  Thread.self () |> Thread.id

(* Logging *)
let start_time = Unix.gettimeofday ()
let verbose = ref false
let log_channel = ref stdout

let log_message format =
  if !verbose then
    Printf.fprintf !log_channel format
  else
    Printf.ifprintf !log_channel format

let log_with_header format =
  if !verbose then begin
    let elapsed = Unix.gettimeofday () -. start_time in
    let thread_id = Thread.id (Thread.self ()) in
    Printf.fprintf !log_channel "[%f] TID=%d: " elapsed thread_id;
    Printf.fprintf !log_channel format
  end else
    Printf.ifprintf !log_channel format

let log_exception e =
  let message = Printexc.to_string e in
  let backtrace = Printexc.get_backtrace () in
    log_message "Exception:%s\n" message;
    log_message "Backtrace:%s\n%!" backtrace

(* Hashtbl *)
let safe_find table key =
  try
    let v = Hashtbl.find table key in
      Some v
  with Not_found -> None

let get_from_string_table table (key : string) conv default =
  safe_find table key |> Option.map_default conv default

(* Unix *)
let flags_to_string flags =
  let flag_descriptions =
    List.map
      (function
         | Unix.O_RDONLY -> "O_RDONLY"
         | Unix.O_WRONLY -> "O_WRONLY"
         | Unix.O_RDWR -> "O_RDWR"
         | Unix.O_NONBLOCK -> "O_NONBLOCK"
         | Unix.O_APPEND -> "O_APPEND"
         | Unix.O_CREAT -> "O_CREAT"
         | Unix.O_TRUNC -> "O_TRUNC"
         | Unix.O_EXCL -> "O_EXCL"
         | Unix.O_NOCTTY -> "O_NOCTTY"
         | Unix.O_DSYNC -> "O_DSYNC"
         | Unix.O_SYNC -> "O_SYNC"
         | Unix.O_RSYNC -> "O_RSYNC")
      flags
  in
    String.concat "," flag_descriptions

