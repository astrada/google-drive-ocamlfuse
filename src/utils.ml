module Infix =
struct
  let (|>) x f = f x

end

let verbose = ref false

(* Logging *)
let log_message format =
  if !verbose then
    Printf.printf format
  else
    Printf.ifprintf stdout format

