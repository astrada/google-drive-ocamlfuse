type t = { path : string; data : Config.t }
type load_state = Loaded | Created | Migrated | Upgraded
type load_result = { store : t; load_state : load_state }

exception File_not_found
exception Parse_error of string

val path : (t, string) GapiLens.t
val data : (t, Config.t) GapiLens.t
val save : t -> unit
val create_default : debug:bool -> path:string -> t
val load : string -> t
val load_or_create : debug:bool -> string -> load_result
