open OUnit

open GapiMonad
open GapiMonad.SessionM.Infix

let test_with_lock_m () =
  let counter1 = ref 0 in
  let counter2 = ref 0 in
  let x = ref false in
  let mutex = Mutex.create () in
  let switch _ =
    Utils.with_lock_m mutex
      (if !x = false then begin
         x := true;
         counter1 := !counter1 + 1;
       end else begin
         counter2 := !counter2 + 1;
       end;
       SessionM.return ()
      )
  in
  let session = {
    GapiConversation.Session.curl = GapiCurl.Initialized;
    config = GapiConfig.default;
    auth = GapiConversation.Session.NoAuth;
    cookies = [];
    etag = "";
  } in
  let tq = Queue.create () in
  for i = 1 to 10 do
    let t = Thread.create switch session in
    Queue.push t tq;
  done;
  Queue.iter (fun t -> Thread.join t) tq;
  assert_equal
    ~printer:string_of_bool
    true
    !x;
  assert_equal
    ~printer:string_of_int
    1
    !counter1;
  assert_equal
    ~printer:string_of_int
    9
    !counter2

let suite = "Buffering test" >:::
            ["test_with_lock_m" >:: test_with_lock_m;
            ]

