open OUnit

open GapiMonad
open GapiMonad.SessionM.Infix

let print_array arr =
  let len = Bigarray.Array1.dim arr in
  let r = String.make len ' ' in
  for i = 0 to (len - 1) do
    r.[i] <- Bigarray.Array1.get arr i
  done;
  r

let session = {
  GapiConversation.Session.curl = GapiCurl.Initialized;
  config = GapiConfig.default;
  auth = GapiConversation.Session.NoAuth;
  cookies = [];
  etag = "";
}

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

let test_read_block () =
  let remote_id = "test" in
  let resource_size = 24L in
  let block_size = 16 in
  let stream_block_size = 8 in

  let fill_array offset arr =
    begin if offset = 0L then begin
        Bigarray.Array1.fill arr 'a';
        Bigarray.Array1.set arr stream_block_size 'c';
      end else
        Bigarray.Array1.fill arr 'b'
    end;
    SessionM.return ()
  in

  let memory_buffers = Buffering.MemoryBuffers.create block_size in
  let destination =
    Bigarray.Array1.create
      Bigarray.char Bigarray.c_layout (Int64.to_int resource_size) in
  let init_subs i =
    Bigarray.Array1.sub
      destination (i * stream_block_size) stream_block_size in
  let dest_arrs = Array.init 3 init_subs in
  let stream buffer offset =
    Buffering.MemoryBuffers.read_block
      remote_id offset resource_size
      (fun start_pos block_buffer -> fill_array start_pos block_buffer)
      buffer memory_buffers
  in
  for i = 0 to ((Int64.to_int resource_size) / stream_block_size - 1) do
    let offset = Int64.of_int (i * stream_block_size) in
    stream dest_arrs.(i) offset session |> ignore;
    let result =
      let arr =
        Bigarray.Array1.create
          Bigarray.char Bigarray.c_layout stream_block_size in
      begin if i = 2 then
          Bigarray.Array1.fill arr 'b'
        else begin
          Bigarray.Array1.fill arr 'a';
          if i = 1 then
            Bigarray.Array1.set arr 0 'c';
        end
      end;
      arr
    in
    assert_equal
      ~printer:print_array
      result
      dest_arrs.(i);
  done

let suite = "Buffering test" >:::
            ["test_with_lock_m" >:: test_with_lock_m;
             "test_read_block" >:: test_read_block;
            ]

