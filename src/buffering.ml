exception Buffer_underrun

module Block =
struct
  type range = {
    start_pos : int64;
    end_pos : int64;
    buf_offset : int;
  }

  let get_range_length range =
    Int64.sub range.end_pos range.start_pos |> Int64.to_int

  let check_range offset range =
    offset >= range.start_pos && offset < range.end_pos

  let get_range offset ranges =
    let ranges =
      List.filter (check_range offset) ranges in
    if List.length ranges > 1 then
      raise (Invalid_argument "get_arr: overlapping ranges")
    else if List.length ranges = 0 then
      raise Not_found;
    List.hd ranges

  type state =
      Empty
    | Writing
    | Clean
    | Dirty

  type t = {
    buffer : (char,
              Bigarray.int8_unsigned_elt,
              Bigarray.c_layout) Bigarray.Array1.t;
    ranges : range list;
    state : state;
    last_update : float;
    mutex : Mutex.t;
  }

  let create size = {
    buffer = Bigarray.Array1.create Bigarray.char Bigarray.c_layout size;
    ranges = [];
    state = Empty;
    last_update = Unix.gettimeofday ();
    mutex = Mutex.create ();
  }

  let is_in_range offset block =
    List.exists (check_range offset) block.ranges

  let blit_to_arr dest_arr offset block =
    if block.state = Empty then raise Buffer_underrun;
    let rec loop off dest_sub_arr =
      let dest_len = Bigarray.Array1.dim dest_sub_arr in
      let range = get_range off block.ranges in
      if Int64.to_int (Int64.sub range.end_pos off) >= dest_len then begin
        let buf_off =
          Int64.to_int (Int64.sub off range.start_pos) + range.buf_offset in
        let src_arr =
          Bigarray.Array1.sub block.buffer buf_off dest_len in
        Bigarray.Array1.blit src_arr dest_sub_arr
      end else begin
        let src_len = Int64.sub range.end_pos off |> Int64.to_int in
        let buf_off =
          Int64.to_int (Int64.sub off range.start_pos) + range.buf_offset in
        let src_arr =
          Bigarray.Array1.sub block.buffer buf_off src_len in
        let dest_left_sub_arr =
          Bigarray.Array1.sub dest_sub_arr 0 src_len in
        Bigarray.Array1.blit src_arr dest_left_sub_arr;
        let dest_right_sub_arr =
          Bigarray.Array1.sub dest_sub_arr src_len (dest_len - src_len) in
        loop range.end_pos dest_right_sub_arr
      end
    in
    Utils.with_lock block.mutex
      (fun () -> loop offset dest_arr)

  let write src_arr offset block =
    let len = Bigarray.Array1.dim src_arr in
    let buf_offset =
      match block.ranges with
      | [] -> 0
      | last :: _ -> last.buf_offset + (get_range_length last) in
    let range = {
      start_pos = offset;
      end_pos = Int64.add offset (Int64.of_int len);
      buf_offset;
    } in
    let dest_arr = Bigarray.Array1.sub block.buffer range.buf_offset len in
    Utils.with_lock block.mutex
      (fun () -> Bigarray.Array1.blit src_arr dest_arr);
    { block with
      ranges = range :: block.ranges;
      state = Dirty;
      last_update = Unix.gettimeofday ();
    }

  let fill src_arr start_pos block =
    let len = Bigarray.Array1.dim src_arr in
    let range = {
      start_pos;
      end_pos = Int64.add start_pos (Int64.of_int len);
      buf_offset = 0;
    } in
    Utils.with_lock block.mutex
      (fun () -> Bigarray.Array1.blit src_arr block.buffer);
    { block with
      ranges = [range];
      state = Clean;
      last_update = Unix.gettimeofday ();
    }

end

module FileBlocks =
struct
  type t = {
    blocks : (int, Block.t) Hashtbl.t;
    block_size : int;
    mutex : Mutex.t;
  }

  let create ?(n = 16) block_size = {
    blocks = Hashtbl.create n;
    block_size;
    mutex = Mutex.create ();
  }

  let get_block_index start_pos file_blocks =
    Int64.div start_pos
      (Int64.of_int file_blocks.block_size) |> Int64.to_int

  let get_block_by_index block_index file_blocks =
    Utils.with_lock file_blocks.mutex
      (fun () ->
         match Utils.safe_find file_blocks.blocks block_index with
         | None ->
           let b = Block.create file_blocks.block_size in
           Hashtbl.add file_blocks.blocks block_index b;
           b
         | Some b -> b
      )

  let replace_block block_index block file_blocks =
    Utils.with_lock file_blocks.mutex
      (fun () ->
         Hashtbl.replace file_blocks.blocks block_index block
      )

  let get_block start_pos length file_blocks =
    let block_index = get_block_index start_pos file_blocks in
    let block = get_block_by_index block_index file_blocks in
    let block_start_pos =
      Int64.mul
        (Int64.of_int block_index)
        (Int64.of_int file_blocks.block_size) in
    let start_writing b =
      let b = {
        block with
        Block.state = Block.Writing;
      } in
      replace_block block_index b file_blocks;
      block_start_pos
    in
    let end_writing b =
      let range = {
        Block.start_pos = block_start_pos;
        end_pos = Int64.add start_pos (Int64.of_int length);
        buf_offset = 0;
      } in
      let b = {
        block with
        Block.state = Block.Clean;
        ranges = [range];
      } in
      replace_block block_index b file_blocks;
      b
    in
    (block, start_writing, end_writing)

  let write src_array start_pos file_blocks =
    let block_index = get_block_index start_pos file_blocks in
    let block = get_block_by_index block_index file_blocks in
    let block = Block.write src_array start_pos block in
    replace_block block_index block file_blocks;
    file_blocks

  let fill_block src_array start_pos file_blocks =
    let block_index = get_block_index start_pos file_blocks in
    let block = get_block_by_index block_index file_blocks in
    let block = Block.fill src_array start_pos block in
    replace_block block_index block file_blocks;
    file_blocks

  let blit_to_arr dest_arr start_pos file_blocks =
    let block_index = get_block_index start_pos file_blocks in
    let block = get_block_by_index block_index file_blocks in
    Block.blit_to_arr dest_arr start_pos block

end

module MemoryBuffers =
struct
  type t = {
    table : (string, FileBlocks.t) Hashtbl.t;
    mutex : Mutex.t;
  }

  let create ?(n = 16) () = {
    table = Hashtbl.create n;
    mutex = Mutex.create ();
  }

  let add_no_lock path block_size buffers =
    let file_blocks = FileBlocks.create block_size in
    Hashtbl.add buffers.table path file_blocks;
    file_blocks

  let add path block_size buffers =
    Utils.with_lock buffers.mutex
      (fun () -> add_no_lock path block_size buffers)

  let get_block path offset block_size buffers =
    let file_blocks =
      Utils.with_lock buffers.mutex
        (fun () ->
           match Utils.safe_find buffers.table path with
           | None -> add_no_lock path block_size buffers
           | Some fb -> fb
        ) in
    FileBlocks.get_block offset block_size file_blocks

  let write_to_buffer path src_arr offset buffers =
    let file_blocks =
      Utils.with_lock buffers.mutex
        (fun () -> Hashtbl.find buffers.table path) in
    FileBlocks.write src_arr offset file_blocks |> ignore;
    buffers

  let fill_block path src_array start_pos buffers =
    let file_blocks =
      Utils.with_lock buffers.mutex
        (fun () -> Hashtbl.find buffers.table path) in
    FileBlocks.fill_block src_array start_pos file_blocks |> ignore;
    buffers

  let blit_buffer_to_arr path dest_arr offset buffers =
    let file_blocks =
      Utils.with_lock buffers.mutex
        (fun () -> Hashtbl.find buffers.table path) in
    FileBlocks.blit_to_arr dest_arr offset file_blocks
    
  let remove_buffer path buffers =
    Utils.with_lock buffers.mutex
      (fun () -> Hashtbl.remove buffers.table path);
    buffers

end

