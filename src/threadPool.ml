type t = {
  max_threads : int;
  mutex : Mutex.t;
  condition : Condition.t;
  table : (int, Thread.t) Hashtbl.t;
}

let create ?(max_threads = 128) ?mutex ?condition () =
  {
    max_threads;
    mutex = (match mutex with None -> Mutex.create () | Some m -> m);
    condition =
      (match condition with None -> Condition.create () | Some c -> c);
    table = Hashtbl.create max_threads;
  }

let signal_work_done thread_id pool =
  Utils.log_with_header "Thread id=%d from thread pool completed\n%!" thread_id;
  Utils.with_lock pool.mutex (fun () ->
      Hashtbl.remove pool.table thread_id;
      Condition.signal pool.condition)

let add_work f x pool =
  Utils.with_lock pool.mutex (fun () ->
      while Hashtbl.length pool.table >= pool.max_threads do
        Condition.wait pool.condition pool.mutex
      done;
      let f' x =
        let thread = Thread.self () in
        let thread_id = Thread.id thread in
        Utils.log_with_header "Spawning new thread id=%d from thread pool\n%!"
          thread_id;
        let _ = f x in
        signal_work_done thread_id pool
      in
      let thread = Thread.create f' x in
      let thread_id = Thread.id thread in
      Hashtbl.add pool.table thread_id thread)

let pending_threads pool = Hashtbl.length pool.table
let shutdown pool = Hashtbl.iter (fun _ thread -> Thread.join thread) pool.table
