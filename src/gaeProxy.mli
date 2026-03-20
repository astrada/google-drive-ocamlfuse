exception ServerError of string

val gae_proxy_url : string
val gae_proxy_mode : string
val gae_proxy_request : string -> string -> (string, Yojson.Safe.t) Hashtbl.t
val get_string_field : (string, [> `String of 'a ]) Hashtbl.t -> string -> 'a
val get_tokens : unit -> unit
val start_server_polling : unit -> unit
val refresh_access_token : unit -> unit
