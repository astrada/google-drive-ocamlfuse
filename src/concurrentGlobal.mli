module Make : (M : sig
                 type u

                 val label : string
               end)
  -> sig
  type t = M.u

  val global : t Global.t
  val mutex : Mutex.t
  val with_lock : (unit -> 'a) -> 'a
  val get_no_lock : unit -> t
  val set_no_lock : t -> unit
  val get : unit -> t
  val set : t -> unit
  val clear : unit -> unit
  val update : (t -> t) -> unit
end
