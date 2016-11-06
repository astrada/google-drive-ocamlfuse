open OUnit

let print_array arr =
  let len = Bigarray.Array1.dim arr in
  let r = String.make len ' ' in
  for i = 0 to (len - 1) do
    r.[i] <- Bigarray.Array1.get arr i
  done;
  r

let create_buffer () =
  let b = Buffering.Buffer.create 15 in
  let b = {
    b with Buffering.Buffer.ranges = [
      { Buffering.Buffer.start_pos = 0L;
        end_pos = 5L;
        buf_offset = 0
      };
      { Buffering.Buffer.start_pos = 5L;
        end_pos = 10L;
        buf_offset = 5
      };
      { Buffering.Buffer.start_pos = 10L;
        end_pos = 15L;
        buf_offset = 10
      }]} in
  Bigarray.Array1.fill b.Buffering.Buffer.arr 'a';
  b

let create_destination () =
  let dest = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 3 in
  Bigarray.Array1.fill dest 'b';
  dest

let create_result () =
  let dest = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 3 in
  Bigarray.Array1.fill dest 'a';
  Bigarray.Array1.set dest 1 'b';
  dest

let test_blit_to_arr_first_range () =
  let b = create_buffer () in
  Bigarray.Array1.set b.Buffering.Buffer.arr 1 'b';
  let dest = create_destination () in
  Buffering.Buffer.blit_to_arr dest 0L b;
  assert_equal
    ~printer:print_array
    (create_result ())
    dest

let test_blit_to_arr_second_range () =
  let b = create_buffer () in
  Bigarray.Array1.set b.Buffering.Buffer.arr 7 'b';
  let dest = create_destination () in
  Buffering.Buffer.blit_to_arr dest 6L b;
  assert_equal
    ~printer:print_array
    (create_result ())
    dest

let test_blit_to_arr_second_and_third_range () =
  let b = create_buffer () in
  Bigarray.Array1.set b.Buffering.Buffer.arr 10 'b';
  let dest = create_destination () in
  Buffering.Buffer.blit_to_arr dest 9L b;
  assert_equal
    ~printer:print_array
    (create_result ())
    dest

let suite = "Buffering test" >:::
  ["test_blit_to_arr_first_range" >:: test_blit_to_arr_first_range;
   "test_blit_to_arr_second_range" >:: test_blit_to_arr_second_range;
   "test_blit_to_arr_second_and_third_range" >::
     test_blit_to_arr_second_and_third_range;
  ]

