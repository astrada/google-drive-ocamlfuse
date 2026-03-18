type parsed = {
  show_version : bool;
  mount_requested : bool;
  params : GdfuseCommon.application_params;
  fuse_args : string list;
}

val print_version : unit -> unit
val parse : unit -> parsed
