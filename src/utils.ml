open GapiUtils.Infix

let verbose = ref false

(* Logging *)
let log_message format =
  if !verbose then
    Printf.printf format
  else
    Printf.ifprintf stdout format

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

