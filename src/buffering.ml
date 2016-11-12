open GapiMonad
open GapiMonad.SessionM.Infix

exception Buffer_underrun

module Block =
struct
  type state =
      Empty
    | Full

  type t = {
    buffer : (char,
              Bigarray.int8_unsigned_elt,
              Bigarray.c_layout) Bigarray.Array1.t;
    start_pos : int64;
    end_pos : int64;
    state : state;
    last_update : float;
    mutex : Mutex.t;
  }

  let create offset size = {
    buffer = Bigarray.Array1.create Bigarray.char Bigarray.c_layout size;
    start_pos = offset;
    end_pos = Int64.add offset (Int64.of_int size);
    state = Empty;
    last_update = Unix.gettimeofday ();
    mutex = Mutex.create ();
  }

  let blit_to_arr dest_arr offset block =
    if block.state = Empty then raise Buffer_underrun;
    let dest_len = Bigarray.Array1.dim dest_arr in
    let src_off = Int64.to_int (Int64.sub offset block.start_pos) in
    let src_arr = Bigarray.Array1.sub block.buffer src_off dest_len in
    Utils.with_lock block.mutex
      (fun () -> Bigarray.Array1.blit src_arr dest_arr)

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
           let start_pos =
             Int64.mul
               (Int64.of_int block_index)
               (Int64.of_int file_blocks.block_size) in
           let b = Block.create start_pos file_blocks.block_size in
           Hashtbl.add file_blocks.blocks block_index b;
           b
         | Some b -> b
      )

  let replace_block block_index block file_blocks =
    Utils.with_lock file_blocks.mutex
      (fun () ->
         Hashtbl.replace file_blocks.blocks block_index block
      )

  let fill_if_empty start_pos fill_array file_blocks =
    let block_index = get_block_index start_pos file_blocks in
    let block = get_block_by_index block_index file_blocks in
    let get_block_m s =
      let block = get_block_by_index block_index file_blocks in
      (block, s) in
    Utils.with_lock_m block.Block.mutex
      (get_block_m >>= fun block ->
       if block.Block.state = Block.Empty then begin
         fill_array block.Block.start_pos block.Block.buffer >>= fun () ->
         let block = {
           block with
           Block.state = Block.Full;
         } in
         replace_block block_index block file_blocks;
         SessionM.return block
       end else begin
         SessionM.return block
       end
      )

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

  let get_file_blocks path block_size buffers =
    let add path block_size buffers =
      let file_blocks = FileBlocks.create block_size in
      Hashtbl.add buffers.table path file_blocks;
      file_blocks
    in
    Utils.with_lock buffers.mutex
      (fun () ->
         match Utils.safe_find buffers.table path with
         | None -> add path block_size buffers
         | Some fb -> fb
      )

  let get_and_fill_block path offset block_size fill_array buffers =
    let file_blocks = get_file_blocks path block_size buffers in
    FileBlocks.fill_if_empty offset fill_array file_blocks

  let remove_buffer path buffers =
    Utils.with_lock buffers.mutex
      (fun () -> Hashtbl.remove buffers.table path);
    buffers

end

