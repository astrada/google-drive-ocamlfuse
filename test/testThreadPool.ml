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

let suite = "Thread pool test" >::: [ "test_sequential" >:: test_sequential ]
