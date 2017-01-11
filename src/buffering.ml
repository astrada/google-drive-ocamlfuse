open GapiUtils.Infix
open GapiMonad
open GapiMonad.SessionM.Infix

module Block =
struct
  type state =
      Empty
    | Full

  type t = {
    buffer : BufferPool.Buffer.t;
    sub_array : (char,
                 Bigarray.int8_unsigned_elt,
                 Bigarray.c_layout) Bigarray.Array1.t;
    start_pos : int64;
    size : int;
    state : state;
    last_access : float;
  }

  let create offset size buffer_pool =
    let buffer = BufferPool.acquire_buffer buffer_pool in
    let sub_array = Bigarray.Array1.sub buffer.BufferPool.Buffer.arr 0 size in
    { buffer;
      sub_array;
      start_pos = offset;
      size;
      state = Empty;
      last_access = Unix.gettimeofday ();
    }

  let blit_to_arr dest_arr offset block =
    if block.state = Empty then invalid_arg "blit_to_arr";
    let dest_len = Bigarray.Array1.dim dest_arr in
    let src_off = Int64.to_int (Int64.sub offset block.start_pos) in
    let src_len = block.size - src_off in
    let len = min src_len dest_len in
    let src_arr =
      try
        Bigarray.Array1.sub block.sub_array src_off len
      with (Invalid_argument _) as e -> begin
        Utils.log_with_header
          "Invalid source array (src_off=%d, len=%d, block size=%d, \
           buffer id=%d)\n%!"
          src_off len
          (Bigarray.Array1.dim block.sub_array)
          block.buffer.BufferPool.Buffer.id;
        raise e
      end
    in
    let dest_arr =
      try
        Bigarray.Array1.sub dest_arr 0 len
      with (Invalid_argument _) as e -> begin
        Utils.log_with_header
          "Invalid destination array (len=%d, dest_len=%d)\n%!"
          len dest_len;
        raise e
      end
    in
    Bigarray.Array1.blit src_arr dest_arr

end

module MemoryBuffers =
struct
  type t = {
    blocks : (string * int, Block.t) Hashtbl.t;
    files : (string, int list) Hashtbl.t;
    block_size : int;
    mutex : Mutex.t;
    buffer_pool : BufferPool.t;
  }

  let create ?(n = Utils.hashtable_initial_size) block_size pool_size = {
    blocks = Hashtbl.create n;
    files = Hashtbl.create n;
    block_size;
    mutex = Mutex.create ();
    buffer_pool = BufferPool.create ~pool_size ~buffer_size:block_size;
  }

  let get_block_index start_pos buffers =
    Int64.div start_pos
      (Int64.of_int buffers.block_size) |> Int64.to_int

  let get_block_start_pos block_index buffers =
    Int64.mul
      (Int64.of_int block_index)
      (Int64.of_int buffers.block_size)

  let release_lru_buffer_if_needed buffers =
    let remove_block ((remote_id, block_index) as key) =
      let block = Hashtbl.find buffers.blocks key in
      Utils.log_with_header
        "Releasing memory buffer (remote id=%s, index=%d, buffer id=%d)\n%!"
        remote_id block_index block.Block.buffer.BufferPool.Buffer.id;
      Hashtbl.remove buffers.blocks key;
      BufferPool.release_buffer block.Block.buffer buffers.buffer_pool
    in
    let get_total_size_and_lru_key () =
      Hashtbl.fold
        (fun k v (total, lru) ->
           let total = total + v.Block.size in
           let lru =
             if lru = ("", 0) then k
             else
               let b = Hashtbl.find buffers.blocks lru in
               if (v.Block.last_access < b.Block.last_access) then k
               else lru in
           (total, lru)
        )
        buffers.blocks
        (0, ("", 0))
    in
    if Hashtbl.length buffers.blocks > 0 then begin
      let free_buffers =
        BufferPool.free_buffers buffers.buffer_pool in
      Utils.log_with_header
        "Buffer pool free buffers: %d\n%!"
        free_buffers;
      if free_buffers = 0 then begin
        let (total_size, lru_key) = get_total_size_and_lru_key () in
        Utils.log_with_header
          "Memory cache size: %d\n%!"
          total_size;
        remove_block lru_key;
      end
    end

  let read_block
      remote_id offset resource_size fill_array ?dest_arr buffers =
    let get_block block_index = 
      Utils.with_lock buffers.mutex
        (fun () ->
           match Utils.safe_find buffers.blocks (remote_id, block_index) with
           | None ->
             let start_pos = get_block_start_pos block_index buffers in
             release_lru_buffer_if_needed buffers;
             Utils.log_with_header
               "BEGIN: Acquiring memory buffer (remote id=%s, index=%d)\n%!"
               remote_id block_index;
             let b =
               let size =
                 (Int64.to_int
                    (min
                       (Int64.of_int buffers.block_size)
                       (Int64.sub resource_size start_pos))) in
               Block.create start_pos size buffers.buffer_pool in
             Hashtbl.add buffers.blocks (remote_id, block_index) b;
             begin match Utils.safe_find buffers.files remote_id with
               | None ->
                 Hashtbl.add buffers.files remote_id [block_index]
               | Some bs ->
                 Hashtbl.replace buffers.files remote_id (block_index :: bs)
             end;
             Utils.log_with_header
               "END: Acquiring memory buffer (remote id=%s, index=%d, \
                size=%d, buffer id=%d)\n%!"
               remote_id block_index b.Block.size
               b.Block.buffer.BufferPool.Buffer.id;
             b
           | Some b -> b
        )
    in
    let get_block_m block_index s =
      let block = get_block block_index in
      (block, s)
    in
    let replace_block block_index block =
      Utils.with_lock buffers.mutex
        (fun () ->
           Hashtbl.replace buffers.blocks (remote_id, block_index) block
        )
    in
    let fill_and_blit block_index src_offset dest_arr =
      let block = get_block block_index in
      Utils.with_lock_m block.Block.buffer.BufferPool.Buffer.mutex
        (get_block_m block_index >>= fun block ->
         begin if block.Block.state = Block.Empty then begin
           fill_array
             block.Block.start_pos
             block.Block.sub_array >>= fun () ->
           let block =
             { block with
               Block.state = Block.Full;
             } in
           SessionM.return block
         end else
           SessionM.return block
         end >>= fun block ->
         let block = {
           block with
           Block.last_access = Unix.gettimeofday ();
         } in
         replace_block block_index block;
         Option.may
           (fun arr -> Block.blit_to_arr arr src_offset block)
           dest_arr;
         SessionM.return ()
        )
    in
    let start_block_index = get_block_index offset buffers in
    let dest_arr_size = Option.map_default Bigarray.Array1.dim 0 dest_arr in
    let end_pos_dest_arr = Int64.add offset (Int64.of_int dest_arr_size) in 
    let end_pos = min resource_size end_pos_dest_arr in
    let end_block_index = get_block_index end_pos buffers in
    if start_block_index < 0 || offset < 0L then begin
      Utils.log_with_header
        "Invalid starting block (remote id=%s, start_block_index=%d, \
         offset=%Ld, resource_size=%Ld, dest_arr_size=%s)\n%!"
        remote_id start_block_index offset resource_size
        (Option.map_default
           (fun _ -> string_of_int dest_arr_size)
           "N/A"
           dest_arr);
      invalid_arg "fill_and_blit"
    end;
    fill_and_blit start_block_index offset dest_arr >>= fun () ->
    if end_block_index <> start_block_index then begin
      let src_offset = get_block_start_pos end_block_index buffers in
      let dest_len = Int64.to_int (Int64.sub end_pos src_offset) in
      if dest_len > 0 then begin
        let delta_end_pos =
          Int64.to_int (Int64.sub end_pos_dest_arr end_pos) in
        let dest_offset = dest_arr_size - dest_len - delta_end_pos in
        let dest_arr =
          Option.map
            (fun arr ->
              try
                Bigarray.Array1.sub arr dest_offset dest_len
              with (Invalid_argument _) as e -> begin
                Utils.log_with_header
                  "Invalid ending block array (dest_offset=%d, dest_len=%d, \
                   dim=%d)\n%!"
                  dest_offset dest_len (Bigarray.Array1.dim arr);
                raise e
              end
            )
            dest_arr in
        if end_block_index < 0 || src_offset < 0L then begin
          Utils.log_with_header
            "Invalid ending block (remote id=%s, end_block_index=%d, \
             src_offset=%Ld, dest_len=%d, resource_size=%Ld)\n%!"
            remote_id end_block_index src_offset dest_len resource_size;
          invalid_arg "fill_and_blit"
        end;
        fill_and_blit end_block_index src_offset dest_arr
      end else SessionM.return ()
    end else SessionM.return ()

  let read_ahead read_ahead_buffers
      remote_id offset resource_size fill_array buffers =
    let block_index = get_block_index offset buffers in
    let rec loop accu requested_buffer_counter =
      let requested_block_index = block_index + requested_buffer_counter in
      let requested_block_start_pos =
        get_block_start_pos requested_block_index buffers in
      if requested_buffer_counter = 0 then SessionM.return accu
      else if requested_block_start_pos >= resource_size then
        loop accu (requested_buffer_counter - 1)
      else begin
        let read_m =
          SessionM.return () >>= fun () ->
          Utils.log_with_header
            "BEGIN: Read ahead resource (remote id=%s, offset=%Ld, \
             requested_block_index=%d, \
             requested_buffer_counter=%d)\n%!"
            remote_id requested_block_start_pos
            requested_block_index requested_buffer_counter;
          read_block
            remote_id requested_block_start_pos
            resource_size fill_array buffers >>= fun () ->
          Utils.log_with_header
            "END: Read ahead resource (remote id=%s, offset=%Ld, \
             requested_block_index=%d, \
             requested_buffer_counter=%d)\n%!"
            remote_id requested_block_start_pos
            requested_block_index requested_buffer_counter;
          SessionM.return ()
        in
        let accu =
          Utils.with_lock buffers.mutex
            (fun () ->
               match Utils.safe_find
                       buffers.blocks (remote_id, requested_block_index) with
               | None -> read_m :: accu
               | Some _ -> accu
            ) in
        loop accu (requested_buffer_counter - 1)
      end
    in
    loop [] read_ahead_buffers

  let remove_buffers remote_id buffers =
    Utils.with_lock buffers.mutex
      (fun () ->
         Utils.log_with_header
           "BEGIN: Releasing memory buffers (remote id=%s)\n%!"
           remote_id;
         begin match Utils.safe_find buffers.files remote_id with
           | None ->
             Utils.log_with_header
               "END: Releasing no memory buffers (remote id=%s)\n%!"
               remote_id
           | Some bs ->
             let ids =
               List.map
                 (fun block_index ->
                    let block =
                      Hashtbl.find buffers.blocks (remote_id, block_index) in
                    string_of_int block.Block.buffer.BufferPool.Buffer.id
                 )
                 bs in
             List.iter
               (fun block_index ->
                  let block =
                    Hashtbl.find buffers.blocks (remote_id, block_index) in
                  Hashtbl.remove buffers.blocks (remote_id, block_index);
                  BufferPool.release_buffer
                    block.Block.buffer buffers.buffer_pool;
               )
               bs;
             Hashtbl.remove buffers.files remote_id;
             Utils.log_with_header
               "END: Releasing %d memory buffers (remote id=%s, \
                buffer ids=%s)\n%!"
               (List.length bs) remote_id (String.concat ";" ids)
         end
      )

end

