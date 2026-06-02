external set : string -> string -> string -> unit = "caml_e2e_setxattr"
external get : string -> string -> string = "caml_e2e_getxattr"
external list : string -> string list = "caml_e2e_listxattr"
external remove : string -> string -> unit = "caml_e2e_removexattr"

let unsupported_message = "xattrs are not supported by this environment"

let is_unsupported = function
  | Failure message when message = unsupported_message -> true
  | Unix.Unix_error ((Unix.EOPNOTSUPP | Unix.ENOSYS), _, _) -> true
  | _ -> false
