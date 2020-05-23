module Make (M : sig
  type u

  val label : string
end) =
struct
  type t = M.u

  let global : t Global.t = Global.empty M.label

  let mutex = Mutex.create ()

  let with_lock f = Utils.with_lock mutex f

  let get_no_lock () = Global.get global

  let set_no_lock value = Global.set global value

  let get () = with_lock get_no_lock

  let set value = with_lock (fun () -> set_no_lock value)

  let clear () = with_lock (fun () -> Global.undef global)

  let update f =
    with_lock (fun () ->
        let value = get_no_lock () in
        let updated_value = f value in
        set_no_lock updated_value)
end
