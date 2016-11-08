open GapiUtils.Infix
open GapiLens.Infix

(* Globals *)
let start_time = Unix.gettimeofday ()
let verbose = ref false
let log_channel = ref stdout
let max_retries = ref 10

(* Threads *)
let get_thread_id () =
  Thread.id (Thread.self ())

(* try/finally *)
let try_finally f finally =
  try
    let result = f () in
    finally ();
    result
  with e ->
    begin try
      finally ()
    with e' ->
      let message = Printexc.to_string e' in
      let backtrace = Printexc.get_backtrace () in
      let elapsed = Unix.gettimeofday () -. start_time in
      let thread_id = get_thread_id () in
      Printf.fprintf !log_channel
        "[%f] TID=%d: Error in finally block:\nException:%s\nBacktrace:%s\n%!"
        elapsed thread_id message backtrace
    end;
    raise e

(* Locks *)
let with_lock m f =
  try_finally
    (fun () ->
      Mutex.lock m;
      f ()
    )
    (fun () -> Mutex.unlock m)

(* Channels *)
let with_in_channel path f =
  let ch = open_in path in
  try_finally
    (fun () -> f ch)
    (fun () -> close_in ch)

let with_out_channel ?(mode = [Open_creat; Open_wronly]) path f =
  let ch = open_out_gen mode 0o600 path in
  try_finally
    (fun () -> f ch)
    (fun () -> close_out ch)

(* Logging *)
let log_message format =
  if !verbose then
    Printf.fprintf !log_channel format
  else
    Printf.ifprintf !log_channel format

let log_with_header format =
  if !verbose then begin
    let elapsed = Unix.gettimeofday () -. start_time in
    let thread_id = get_thread_id () in
    Printf.fprintf !log_channel "[%f] TID=%d: " elapsed thread_id;
    Printf.fprintf !log_channel format
  end else
    Printf.ifprintf !log_channel format

let log_exception e =
  let message = Printexc.to_string e in
  let backtrace = Printexc.get_backtrace () in
  log_with_header "Exception:%s\n" message;
  log_message "Backtrace:%s\n%!" backtrace

(* Hashtbl *)
let safe_find table key =
  try
    let v = Hashtbl.find table key in
    Some v
  with Not_found -> None

let get_from_string_table table (key : string) conv default =
  safe_find table key
    |> Option.map_default (conv -| ExtString.String.strip) default

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
         | Unix.O_RSYNC -> "O_RSYNC"
         (* Only in OCaml 4
         | Unix.O_SHARE_DELETE -> "O_SHARE_DELETE" *)
         | _ -> "_")
      flags
  in
  String.concat "," flag_descriptions

let xattr_flags_to_string = function
    Fuse.AUTO -> "AUTO"
  | Fuse.CREATE -> "CREATE"
  | Fuse.REPLACE -> "REPLACE"

(* Browser *)
let start_browser url =
  let start_process browser =
    let command = Printf.sprintf "%s \"%s\"" browser url in
    log_with_header "BEGIN: Starting web browser with command: %s\n" command;
    let ch = Unix.open_process_in command in
    let status = Unix.close_process_in ch in
    if status = (Unix.WEXITED 0) then begin
      log_with_header "END: Starting web browser with command: %s\n" command;
      true
    end else begin
      log_with_header "FAIL: Starting web browser with command: %s\n" command;
      false
    end
  in
  let browsers = ["xdg-open"; "firefox"; "google-chrome"; "open"] in
  let status =
    List.fold_left
      (fun result browser ->
         if result then
           result
         else
           start_process browser)
      false
      browsers
  in
  if not status then
    failwith ("Error opening URL:" ^ url)

(* Retry *)
let with_retry ?(filter_exception = fun _ -> true) f label =
  let rec loop n =
    try
      f ()
    with e when filter_exception e ->
      if n >= !max_retries then begin
        log_with_header "Error during %s after %d attempts: %s\n%!"
          label !max_retries (Printexc.to_string e);
        raise e
      end else begin
        let n' = n + 1 in
        log_with_header "Retrying (%d/%d) %s after exception: %s\n%!"
          n' !max_retries label (Printexc.to_string e);
        GapiUtils.wait_exponential_backoff n;
        loop n'
      end
  in
  loop 0

