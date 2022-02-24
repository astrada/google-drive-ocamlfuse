type t

val create :
  ?max_threads:int -> ?mutex:Mutex.t -> ?condition:Condition.t -> unit -> t

val add_work : ('a -> 'b) -> 'a -> t -> unit
val pending_threads : t -> int
val shutdown : t -> unit
