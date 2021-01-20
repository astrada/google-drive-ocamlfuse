open GapiUtils.Infix
open GapiMonad
open GapiMonad.SessionM.Infix

exception Temporary_error

(* Globals *)
let start_time = Unix.gettimeofday ()

let verbose = ref false

let debug_buffers = ref false

let log_channel = ref stdout

let log_mutex = Mutex.create ()

let max_retries = ref 10

let mb = 1048576L

let hashtable_initial_size = 64

(* Threads *)
let get_thread_id () = Thread.id (Thread.self ())

(* try/finally *)
let try_finally f finally =
  try
    let result = f () in
    finally ();
    result
  with e ->
    ( try finally ()
      with e' ->
        let message = Printexc.to_string e' in
        let backtrace = Printexc.get_backtrace () in
        let elapsed = Unix.gettimeofday () -. start_time in
        let thread_id = get_thread_id () in
        Printf.fprintf !log_channel
          "[%f] TID=%d: Error in finally block:\nException:%s\nBacktrace:%s\n%!"
          elapsed thread_id message backtrace );
    raise e

(* Channels *)
let with_in_channel path f =
  let ch = open_in path in
  try_finally (fun () -> f ch) (fun () -> close_in ch)

let with_out_channel ?(mode = [ Open_creat; Open_wronly ]) path f =
  let ch = open_out_gen mode 0o600 path in
  try_finally (fun () -> f ch) (fun () -> close_out ch)

(* Logging *)
let open_log_out_ch log_to path =
  match log_to with
  | "-" | "stdout" -> stdout
  | "stderr" -> stderr
  | "" -> open_out path
  | p when String.index p '/' = 0 -> open_out p
  | l ->
      failwith
        ( "Invalid log_to value: " ^ l
        ^ " should be 'stdout', 'stderr', or an absolute path" )

let log_message format =
  if !verbose then (
    if !debug_buffers then Mutex.lock log_mutex;
    let result = Printf.fprintf !log_channel format in
    if !debug_buffers then Mutex.unlock log_mutex;
    result )
  else Printf.ifprintf !log_channel format

let log_with_header format =
  if !verbose then (
    if !debug_buffers then Mutex.lock log_mutex;
    let elapsed = Unix.gettimeofday () -. start_time in
    let thread_id = get_thread_id () in
    Printf.fprintf !log_channel "[%f] TID=%d: " elapsed thread_id;
    let result = Printf.fprintf !log_channel format in
    if !debug_buffers then Mutex.unlock log_mutex;
    result )
  else Printf.ifprintf !log_channel format

let log_exception e =
  let message = Printexc.to_string e in
  let backtrace = Printexc.get_backtrace () in
  log_with_header "Exception:%s\n" message;
  log_message "Backtrace:%s\n%!" backtrace

let log_buffer message buffer length =
  let b = Buffer.create (length * 2) in
  let bytes =
    Netsys_mem.bytes_of_memory (Bigarray.Array1.sub buffer 0 length)
  in
  Bytes.iter
    (fun c -> Buffer.add_string b (Printf.sprintf "%2.2x" (Char.code c)))
    bytes;
  log_with_header "%s: buffer content=%s\n%!" message (Buffer.contents b)

(* Locks *)
let with_lock m f =
  try_finally
    (fun () ->
      Mutex.lock m;
      f ())
    (fun () -> Mutex.unlock m)

(* Monadic combinators *)

(* Used to do a try/with on a monadic f: state parameter s is eta-expanded,
 * otherwise the try/with will be ignored because f is only partially applied
 *)
let try_with_m f handle_exception s = try f s with e -> handle_exception e s

let raise_m e _ = raise e

let try_finally_m f finally =
  try_with_m
    ( f >>= fun result ->
      finally >>= fun _ -> SessionM.return result )
    (fun e -> finally >>= fun _ -> raise_m e)

let lock m s =
  Mutex.lock m;
  ((), s)

let unlock m s =
  Mutex.unlock m;
  ((), s)

let with_lock_m m f =
  try_finally_m
    ( lock m >>= fun () ->
      f >>= fun result -> SessionM.return result )
    (unlock m)

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
  | Fuse.AUTO -> "AUTO"
  | Fuse.CREATE -> "CREATE"
  | Fuse.REPLACE -> "REPLACE"

(* Browser *)
let start_browser browser url =
  let start_process browser =
    let command = Printf.sprintf "%s \"%s\"" browser url in
    log_with_header "BEGIN: Starting web browser with command: %s\n" command;
    let ch = Unix.open_process_in command in
    let status = Unix.close_process_in ch in
    if status = Unix.WEXITED 0 then (
      log_with_header "END: Starting web browser with command: %s\n" command;
      true )
    else (
      log_with_header "FAIL: Starting web browser with command: %s\n" command;
      false )
  in
  let browsers =
    if browser <> "" then [ browser ]
    else [ "xdg-open"; "firefox"; "google-chrome"; "chromium-browser"; "open" ]
  in
  let status =
    List.fold_left
      (fun result browser -> if result then result else start_process browser)
      false browsers
  in
  if not status then failwith ("Error opening URL:" ^ url)

(* Retry *)
let with_retry ?(filter_exception = fun _ -> true) f label =
  let rec loop n =
    try f ()
    with e when filter_exception e ->
      if n >= !max_retries then (
        log_with_header "Error during %s after %d attempts: %s\n%!" label
          !max_retries (Printexc.to_string e);
        raise e )
      else
        let n' = n + 1 in
        log_with_header "Retrying (%d/%d) %s after exception: %s\n%!" n'
          !max_retries label (Printexc.to_string e);
        GapiUtils.wait_exponential_backoff n;
        loop n'
  in
  loop 0

let safe_makedir dir =
  let mkd ds =
    let d = String.concat Filename.dir_sep ds in
    if not (Sys.file_exists d) then
      try Unix.mkdir d 0o700 with Sys_error _ -> ()
  in
  let ds = Str.split (Str.regexp (Str.quote Filename.dir_sep)) dir in
  List.fold_left
    (fun xs x ->
      let xs' = xs @ [ x ] in
      mkd xs';
      xs')
    [ "" ] ds
  |> ignore

let file_copy source_path target_path =
  let copy_buffer_size = 8192 in
  let copy_buffer = Bytes.create copy_buffer_size in
  with_in_channel source_path (fun source_ch ->
      with_out_channel ~mode:[ Open_creat; Open_trunc; Open_wronly ] target_path
        (fun target_ch ->
          let rec copy_loop () =
            match input source_ch copy_buffer 0 copy_buffer_size with
            | 0 -> ()
            | r ->
                ignore (output target_ch copy_buffer 0 r);
                copy_loop ()
          in
          copy_loop ()))

(* Base64 *)
let base64_encode s =
  let base64 = Cryptokit.Base64.encode_compact () in
  Cryptokit.transform_string base64 s

