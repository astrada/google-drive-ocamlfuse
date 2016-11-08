open OUnit

let print_array arr =
  let len = Bigarray.Array1.dim arr in
  let r = String.make len ' ' in
  for i = 0 to (len - 1) do
    r.[i] <- Bigarray.Array1.get arr i
  done;
  r

let create_source () =
  let src = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 15 in
  Bigarray.Array1.fill src 'a';
  src

let create_destination () =
  let dest = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 3 in
  Bigarray.Array1.fill dest 'b';
  dest

let create_result () =
  let dest = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 3 in
  Bigarray.Array1.fill dest 'a';
  Bigarray.Array1.set dest 1 'b';
  dest

let create_block () =
  let b = Buffering.Block.create 15 in
  let b = Buffering.Block.fill (create_source ()) 3L b in
  { b with Buffering.Block.ranges = [
          { Buffering.Block.start_pos = 3L;
            end_pos = 8L;
            buf_offset = 0
          };
          { Buffering.Block.start_pos = 8L;
            end_pos = 13L;
            buf_offset = 5
          };
          { Buffering.Block.start_pos = 13L;
            end_pos = 18L;
            buf_offset = 10
          }]}

let test_blit_to_arr_first_range () =
  let b = create_block () in
  Bigarray.Array1.set b.Buffering.Block.buffer 1 'b';
  let dest = create_destination () in
  Buffering.Block.blit_to_arr dest 3L b;
  assert_equal
    ~printer:print_array
    (create_result ())
    dest

let test_blit_to_arr_second_range () =
  let b = create_block () in
  Bigarray.Array1.set b.Buffering.Block.buffer 7 'b';
  let dest = create_destination () in
  Buffering.Block.blit_to_arr dest 9L b;
  assert_equal
    ~printer:print_array
    (create_result ())
    dest

let test_blit_to_arr_second_and_third_range () =
  let b = create_block () in
  Bigarray.Array1.set b.Buffering.Block.buffer 10 'b';
  let dest = create_destination () in
  Buffering.Block.blit_to_arr dest 12L b;
  assert_equal
    ~printer:print_array
    (create_result ())
    dest

let test_buffering () =
  (*Utils.verbose := true;*)
  let block_size = 25 in
  let stream_buffer_size = 5 in
  let n = block_size / stream_buffer_size in
  let path = "test" in
  let buffers = Buffering.Buffers.create () in
  let download () =
    Buffering.Buffers.add path block_size buffers |> ignore;
    let arr =
      Bigarray.Array1.create Bigarray.char Bigarray.c_layout block_size in
    for i = 0 to (n - 1) do
      let sub_arr =
        Bigarray.Array1.sub arr (i * stream_buffer_size) stream_buffer_size in
      Bigarray.Array1.fill sub_arr (Char.chr (Char.code '0' + i));
    done;
    Thread.delay 0.5;
    Buffering.Buffers.fill_block path arr 0L buffers |> ignore;
  in
  let create_arr _ =
    Bigarray.Array1.create
      Bigarray.char Bigarray.c_layout stream_buffer_size in
  let dest_arrs = Array.init n create_arr in
  let stream i =
    let offset = Int64.of_int (i * stream_buffer_size) in
    Utils.with_retry
      ~filter_exception:(function
          | Buffering.Buffer_underrun -> true
          | Not_found -> download (); true
          | _ -> false)
      (fun () ->
         Buffering.Buffers.blit_buffer_to_arr
           path dest_arrs.(i) offset buffers |> ignore)
      "stream"
  in
  let tq = Queue.create () in
  for i = 0 to n - 1 do
     Queue.add (Thread.create stream i) tq;
  done;
  Queue.iter Thread.join tq;
  for i = 0 to n - 1 do
    let result =
      Bigarray.Array1.create
        Bigarray.char Bigarray.c_layout stream_buffer_size in
    Bigarray.Array1.fill result (Char.chr (Char.code '0' + i));
    assert_equal
      ~printer:print_array
      result
      dest_arrs.(i);
  done

let suite = "Buffering test" >:::
  ["test_blit_to_arr_first_range" >:: test_blit_to_arr_first_range;
   "test_blit_to_arr_second_range" >:: test_blit_to_arr_second_range;
   "test_blit_to_arr_second_and_third_range" >::
     test_blit_to_arr_second_and_third_range;
   "test_buffering" >:: test_buffering
  ]

