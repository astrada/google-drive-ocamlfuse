exception Invalid_block

module Block :
sig
  type t

end

module MemoryBuffers :
sig

  type t = {
    blocks : (string * int, Block.t) Hashtbl.t;
    files : (string, int list) Hashtbl.t;
    file_block_indexes : (string, int list) Hashtbl.t;
    block_size : int;
    mutex : Mutex.t;
    condition : Condition.t;
    buffer_pool : BufferPool.t;
    mutable stop_eviction_thread : bool;
  }

  val create : ?n:int -> int -> int -> t

  val read_block :
    string ->
    int64 ->
    int64 ->
    (int64 ->
     (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout)
       Bigarray.Array1.t -> unit GapiMonad.SessionM.m) ->
    ?dest_arr:(char, Bigarray.int8_unsigned_elt, Bigarray.c_layout)
      Bigarray.Array1.t ->
    t -> unit GapiMonad.SessionM.m

  val read_ahead :
    int ->
    string ->
    int64 ->
    int64 ->
    (int64 ->
     (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout)
       Bigarray.Array1.t -> unit GapiMonad.SessionM.m) ->
    t -> unit GapiMonad.SessionM.m list GapiMonad.SessionM.t

  val remove_buffers : string -> t -> unit

  val write_to_block:
    string ->
    string ->
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout)
      Bigarray.Array1.t ->
    int64 ->
    t ->
    int

  val flush_blocks: string -> t -> unit

  val create_eviction_thread : t -> Thread.t

  val stop_eviction_thread : t -> unit

end

