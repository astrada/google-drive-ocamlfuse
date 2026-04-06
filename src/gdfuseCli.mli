type parsed = {
  mount_requested : bool;
  params : GdfuseCommon.application_params;
  fuse_args : string list;
}

type parse_result =
  | Parsed of parsed
  | Show_version
  | Help of string
  | Error of string

val version_text : string
val print_version : unit -> unit
val parse_argv : string array -> parse_result
val parse : unit -> parse_result
