exception File_not_found
exception IO_error
exception Invalid_operation
exception Permission_denied

module type PORTS = sig
  val max_retries : unit -> int
  val wait_exponential_backoff : int -> unit
end

module Make (P : PORTS) : sig
  val match_service_error : string -> exn -> bool
  val handle_default_exceptions : exn -> 'a GapiMonad.SessionM.m
  val try_with_default : 'a GapiMonad.SessionM.m -> 'a GapiMonad.SessionM.m
  val with_retry_default : 'a GapiMonad.SessionM.m -> 'a GapiMonad.SessionM.m
end

val match_service_error : string -> exn -> bool
val handle_default_exceptions : exn -> 'a GapiMonad.SessionM.m
val try_with_default : 'a GapiMonad.SessionM.m -> 'a GapiMonad.SessionM.m
val with_retry_default : 'a GapiMonad.SessionM.m -> 'a GapiMonad.SessionM.m
