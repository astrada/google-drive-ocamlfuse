exception InvalidRefreshToken

val scope : string list

val do_request :
  (GapiConversation.Session.t -> 'a * GapiConversation.Session.t) ->
  'a * GapiConversation.Session.t

val get_access_token : bool -> bool -> string -> unit
val refresh_access_token : unit -> string
