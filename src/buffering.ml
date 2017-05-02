open GapiUtils.Infix
open GapiMonad
open GapiMonad.SessionM.Infix

module Block =
struct
  type state =
      Empty
    | Writing
    | Full
    | Error of exn

  let state_to_string = function
    | Empty -> "Empty"
    | Writing -> "Writing"
    | Full -> "Full"
    | Error e -> Printf.sprintf "Error(%s)" (Printexc.to_string e)

  type t = {
    buffer : BufferPool.Buffer.t;
    sub_array : (char,
                 Bigarray.int8_unsigned_elt,
                 Bigarray.c_layout) Bigarray.Array1.t;
    start_pos : int64;
    size : int;
    mutable state : state;
    mutable last_access : float;
  }

  let create offset size mutex condition buffer_pool =
    let buffer = BufferPool.acquire_buffer mutex condition buffer_pool in
    let sub_array = Bigarray.Array1.sub buffer.BufferPool.Buffer.arr 0 size in
    { buffer;
      sub_array;
      start_pos = offset;
      size;
      state = Empty;
      last_access = Unix.gettimeofday ();
    }

  let blit_to_arr dest_arr offset block =
    begin match block.state with
      | Empty
      | Writing
      | Error _ ->
        invalid_arg
          (Printf.sprintf
             "blit_to_arr (block state=%s)" (state_to_string block.state));
      | Full -> ()
    end;
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
    condition : Condition.t;
    buffer_pool : BufferPool.t;
    mutable stop_eviction_thread : bool;
  }

  let create ?(n = Utils.hashtable_initial_size) block_size pool_size = {
    blocks = Hashtbl.create n;
    files = Hashtbl.create n;
    block_size;
    mutex = Mutex.create ();
    condition = Condition.create ();
    buffer_pool = BufferPool.create ~pool_size ~buffer_size:block_size;
    stop_eviction_thread = false;
  }

  let get_block_index start_pos buffers =
    Int64.div start_pos
      (Int64.of_int buffers.block_size) |> Int64.to_int

  let get_block_start_pos block_index buffers =
    Int64.mul
      (Int64.of_int block_index)
      (Int64.of_int buffers.block_size)

  let remove_block
      expected_state ((remote_id, block_index) as key) block buffers =
    if block.Block.state = expected_state then begin
      Utils.log_with_header
        "Releasing memory buffer (remote id=%s, index=%d, buffer id=%d, \
         state=%s)\n%!"
        remote_id block_index
        block.Block.buffer.BufferPool.Buffer.id
        (Block.state_to_string block.Block.state);
      Hashtbl.remove buffers.blocks key;
      BufferPool.release_buffer
        block.Block.buffer buffers.condition buffers.buffer_pool
    end else
      Utils.log_with_header
        "Cannot release memory buffer (remote id=%s, index=%d, \
         buffer id=%d, state=%s, expected state=%s)\n%!"
        remote_id block_index
        block.Block.buffer.BufferPool.Buffer.id
        (Block.state_to_string block.Block.state)
        (Block.state_to_string expected_state)

  let remove_full_block = remove_block Block.Full

  let remove_partial_block = remove_block Block.Writing

  let release_lru_buffer_if_needed check_condition buffers =
    let null_key = ("", 0) in
    let get_total_size_and_lru_block () =
      Hashtbl.fold
        (fun k v (total, lru_key, lru_block) ->
           let total = total + v.Block.size in
           let (lru_key, lru_block) =
             if v.Block.state = Block.Full then
               match lru_block with
               | None -> (k, Some v)
               | Some b ->
                 if v.Block.last_access < b.Block.last_access then (k, Some v)
                 else (lru_key, lru_block)
             else (lru_key, lru_block) in
           (total, lru_key, lru_block)
        )
        buffers.blocks
        (0, null_key, None)
    in
    if Hashtbl.length buffers.blocks > 0 then begin
      let free_buffers =
        BufferPool.free_buffers buffers.buffer_pool in
      let pending_requests =
        BufferPool.pending_requests buffers.buffer_pool in
      if check_condition free_buffers pending_requests then begin
        Utils.log_with_header
          "Buffer pool stats: free buffers=%d, pending requests=%d\n%!"
          free_buffers
          pending_requests;
        let (total_size, lru_key, lru_block) =
          get_total_size_and_lru_block () in
        Utils.log_with_header
          "Memory cache size: %d\n%!"
          total_size;
        match lru_block with
        | None ->
          Utils.log_with_header "Cannot find a full buffer to remove\n%!";
        | Some b ->
          remove_full_block lru_key b buffers
      end
    end

  let release_lru_buffer_if_no_free_buffer_left buffers =
    release_lru_buffer_if_needed
      (fun free_buffers _ -> free_buffers = 0)
      buffers

  let release_lru_buffer_if_request_blocked buffers =
    release_lru_buffer_if_needed
      (fun free_buffers pending_requests ->
         free_buffers = 0 && pending_requests > 0)
      buffers

  let read_block
      remote_id offset resource_size fill_array ?dest_arr buffers =
    let get_block block_index =
      match Utils.safe_find buffers.blocks (remote_id, block_index) with
      | None ->
        let start_pos = get_block_start_pos block_index buffers in
        release_lru_buffer_if_no_free_buffer_left buffers;
        Utils.log_with_header
          "BEGIN: Acquiring memory buffer (remote id=%s, index=%d)\n%!"
          remote_id block_index;
        let b =
          let size =
            (Int64.to_int
               (min
                  (Int64.of_int buffers.block_size)
                  (Int64.sub resource_size start_pos))) in
          Block.create start_pos size
            buffers.mutex buffers.condition buffers.buffer_pool in
        Hashtbl.replace buffers.blocks (remote_id, block_index) b;
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
      | Some b ->
        Utils.log_with_header
          "Got cached memory buffer (remote id=%s, index=%d, \
           size=%d, buffer id=%d)\n%!"
          remote_id block_index b.Block.size
          b.Block.buffer.BufferPool.Buffer.id;
        b
    in
    let get_block_m block_index s =
      let block = get_block block_index in
      (block, s)
    in
    let wait_for_full_block block =
      while block.Block.state = Block.Writing do
        Utils.log_with_header
          "Waiting for streaming completion (buffer id=%d, state=%s)\n%!"
          block.Block.buffer.BufferPool.Buffer.id
          (Block.state_to_string block.Block.state);
        Condition.wait
          block.Block.buffer.BufferPool.Buffer.condition
          block.Block.buffer.BufferPool.Buffer.mutex;
      done;
      begin match block.Block.state with
        | Block.Error e ->
          Utils.log_with_header
            "Streaming error (buffer id=%d, state=%s)\n%!"
            block.Block.buffer.BufferPool.Buffer.id
            (Block.state_to_string block.Block.state);
          raise e
        | _ ->
          Utils.log_with_header
            "Streaming completed (buffer id=%d, state=%s)\n%!"
            block.Block.buffer.BufferPool.Buffer.id
            (Block.state_to_string block.Block.state);
      end
    in
    let fill_and_blit block_index src_offset dest_arr =
      Utils.with_lock_m buffers.mutex
        (get_block_m block_index >>= fun block ->
         SessionM.return (block, block.Block.state)) >>= fun (block, state) ->
      begin match state with
        | Block.Empty
        | Block.Error _ -> begin
            (* Switch from global lock to block lock to allow concurrent
             * streaming. *)
            Utils.with_lock_m block.Block.buffer.BufferPool.Buffer.mutex
              (SessionM.return () >>= fun () ->
               begin match block.Block.state with
                 | Block.Empty
                 | Block.Error _ -> begin
                     block.Block.state <- Block.Writing;
                     Utils.try_with_m
                       (fill_array
                          block.Block.start_pos
                          block.Block.sub_array)
                       (fun e ->
                          remove_partial_block
                            (remote_id, block_index) block buffers;
                          block.Block.state <- Block.Error e;
                          Utils.log_with_header
                            "Broadcasting streaming error \
                             (buffer id=%d, state=%s)\n%!"
                            block.Block.buffer.BufferPool.Buffer.id
                            (Block.state_to_string block.Block.state);
                          Condition.broadcast
                            block.Block.buffer.BufferPool.Buffer.condition;
                          raise e) >>= fun () ->
                     block.Block.state <- Block.Full;
                     Utils.log_with_header
                       "Broadcasting streaming completion \
                        (buffer id=%d, state=%s)\n%!"
                       block.Block.buffer.BufferPool.Buffer.id
                       (Block.state_to_string block.Block.state);
                     Condition.broadcast
                       block.Block.buffer.BufferPool.Buffer.condition;
                     SessionM.return block
                   end
                 | Block.Full ->
                   SessionM.return block
                 | Block.Writing -> begin
                     wait_for_full_block block;
                     SessionM.return block
                   end
               end)
          end
        | Block.Full -> SessionM.return block
        | Block.Writing -> begin
            Utils.with_lock block.Block.buffer.BufferPool.Buffer.mutex
              (fun () -> wait_for_full_block block);
            SessionM.return block
          end
      end >>= fun block ->
      Utils.with_lock_m buffers.mutex
        (SessionM.return () >>= fun () ->
         block.Block.last_access <- Unix.gettimeofday ();
         Utils.with_lock block.Block.buffer.BufferPool.Buffer.mutex
           (fun () ->
              Option.may
                (fun arr -> Block.blit_to_arr arr src_offset block)
                dest_arr
           );
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
                    block.Block.buffer buffers.condition buffers.buffer_pool;
               )
               bs;
             Hashtbl.remove buffers.files remote_id;
             Utils.log_with_header
               "END: Releasing %d memory buffers (remote id=%s, \
                buffer ids=%s)\n%!"
               (List.length bs) remote_id (String.concat ";" ids)
         end
      )

  let evict_cache buffers =
    try
      while true do
        Utils.log_with_header "evict_cache loop\n%!";
        Utils.with_lock buffers.mutex
          (fun () -> release_lru_buffer_if_request_blocked buffers);
        Utils.with_lock buffers.mutex
          (fun () -> if buffers.stop_eviction_thread then raise Exit);
        Thread.delay 10.0;
      done
    with Exit -> ()

  let create_eviction_thread buffers =
    Thread.create evict_cache buffers

  let stop_eviction_thread buffers =
    Utils.with_lock buffers.mutex
      (fun () ->
         buffers.stop_eviction_thread <- true;
      )

end

