exception Temporary_error

val start_time : float
val verbose : bool ref
val debug_buffers : bool ref
val log_channel : out_channel ref
val log_mutex : Mutex.t
val max_retries : int ref
val mb : int64
val hashtable_initial_size : int
val get_thread_id : unit -> int
val try_finally : (unit -> 'a) -> (unit -> unit) -> 'a
val with_in_channel : string -> (in_channel -> 'a) -> 'a

val with_out_channel :
  ?mode:open_flag list -> string -> (out_channel -> 'a) -> 'a

val open_log_out_ch : string -> string -> out_channel
val log_message : ('a, out_channel, unit) format -> 'a
val log_with_header : ('a, out_channel, unit) format -> 'a
val log_exception : exn -> unit

val log_buffer :
  string ->
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  int ->
  unit

val with_lock : Mutex.t -> (unit -> 'a) -> 'a
val try_with_m : ('a -> 'b) -> (exn -> 'a -> 'b) -> 'a -> 'b
val raise_m : exn -> 'a -> 'b

val try_finally_m :
  'a GapiMonad.SessionM.m ->
  'b GapiMonad.SessionM.m ->
  GapiConversation.Session.t ->
  'a * GapiConversation.Session.t

val lock : Mutex.t -> 'a -> unit * 'a
val unlock : Mutex.t -> 'a -> unit * 'a

val with_lock_m :
  Mutex.t ->
  'a GapiMonad.SessionM.m ->
  GapiConversation.Session.t ->
  'a * GapiConversation.Session.t

val safe_find : ('a, 'b) Hashtbl.t -> 'a -> 'b option

val get_from_string_table :
  (string, string) Hashtbl.t -> string -> (string -> 'a) -> 'a -> 'a

val flags_to_string : Unix.open_flag list -> string
val xattr_flags_to_string : Fuse.xattr_flags -> string
val start_browser : string -> string -> unit
val with_retry : ?filter_exception:(exn -> bool) -> (unit -> 'a) -> string -> 'a
val safe_makedir : string -> unit
val file_copy : string -> string -> unit
val base64_encode : string -> string
val normalize_absolute_path : string -> string
