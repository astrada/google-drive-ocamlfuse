open OUnit

let test_sequential () =
  let flag = ref false in
  let thread_pool = ThreadPool.create ~max_threads:1 () in
  ThreadPool.add_work
    (fun () ->
      Thread.delay 0.1;
      flag := true)
    () thread_pool;
  ThreadPool.add_work (fun () -> flag := not !flag) () thread_pool;
  ThreadPool.shutdown thread_pool;
  assert_equal ~printer:string_of_bool false !flag

let rec wait_until predicate attempts =
  if predicate () then ()
  else if attempts = 0 then assert_failure "Timed out waiting for condition"
  else (
    Thread.delay 0.01;
    wait_until predicate (attempts - 1))

let test_failed_worker_releases_slot () =
  let thread_pool = ThreadPool.create ~max_threads:1 () in
  let started_mutex = Mutex.create () in
  let started_condition = Condition.create () in
  let started = ref false in
  ThreadPool.add_work
    (fun () ->
      Utils.with_lock started_mutex (fun () ->
          started := true;
          Condition.signal started_condition);
      failwith "expected worker failure")
    () thread_pool;
  Utils.with_lock started_mutex (fun () ->
      while not !started do
        Condition.wait started_condition started_mutex
      done);
  wait_until (fun () -> ThreadPool.pending_threads thread_pool = 0) 50;
  ThreadPool.shutdown thread_pool;
  assert_equal 0 (ThreadPool.pending_threads thread_pool)

let suite =
  "Thread pool test"
  >::: [
         "test_sequential" >:: test_sequential;
         "test_failed_worker_releases_slot" >:: test_failed_worker_releases_slot;
       ]
