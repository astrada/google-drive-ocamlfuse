module Buffer = struct

  type range = {
    start_pos : int64;
    end_pos : int64;
    buf_offset : int;
  }

  let check_range offset range =
    offset >= range.start_pos && offset < range.end_pos

  let get_range offset ranges =
    let ranges =
      List.filter (check_range offset) ranges in
    if List.length ranges > 1 then
      raise (Invalid_argument "get_arr: overlapping ranges")
    else if List.length ranges = 0 then
      raise (Invalid_argument "get_arr: range not found");
    List.hd ranges

  type t = {
    arr : (char,
           Bigarray.int8_unsigned_elt,
           Bigarray.c_layout) Bigarray.Array1.t;
    ranges : range list;
  }

  let create size = {
    arr = Bigarray.Array1.create Bigarray.char Bigarray.c_layout size;
    ranges = [];
  }

  let is_in_range offset buffer =
    List.exists (check_range offset) buffer.ranges

  let blit_to_arr dest_arr offset buffer =
    let rec loop off dest_sub_arr =
      let dest_len = Bigarray.Array1.dim dest_sub_arr in
      let range = get_range off buffer.ranges in
      if Int64.to_int (Int64.sub range.end_pos off) >= dest_len then begin
        let buf_off =
          Int64.to_int (Int64.sub off range.start_pos) + range.buf_offset in
        let src_arr =
          Bigarray.Array1.sub buffer.arr buf_off dest_len in
        Bigarray.Array1.blit src_arr dest_sub_arr
      end else begin
        let src_len = Int64.sub range.end_pos off |> Int64.to_int in
        let buf_off =
          Int64.to_int (Int64.sub off range.start_pos) + range.buf_offset in
        let src_arr =
          Bigarray.Array1.sub buffer.arr buf_off src_len in
        let dest_left_sub_arr =
          Bigarray.Array1.sub dest_sub_arr 0 src_len in
        Bigarray.Array1.blit src_arr dest_left_sub_arr;
        let dest_right_sub_arr =
          Bigarray.Array1.sub dest_sub_arr src_len (dest_len - src_len) in
        loop range.end_pos dest_right_sub_arr
      end
    in
    loop offset dest_arr

end

