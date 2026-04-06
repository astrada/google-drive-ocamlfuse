module type FLOW = sig
  val run_bootstrap_only : GdfuseCommon.application_params -> unit
  val run_mount_mode : GdfuseCommon.application_params -> string list -> unit
end

module type DEPS = sig
  module Flow : FLOW

  val print_stdout : string -> unit
  val print_stderr : string -> unit
  val exit : int -> 'a
end

module Make (D : DEPS) : sig
  val run_argv : string array -> unit
  val run : unit -> unit
end
