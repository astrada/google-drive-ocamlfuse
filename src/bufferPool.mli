type buffer = {
  id : int;
  arr : (char,
         Bigarray.int8_unsigned_elt,
         Bigarray.c_layout) Bigarray.Array1.t;
  mutex : Mutex.t;
}

type t

val create : pool_size:int -> buffer_size:int -> t

val max_buffers : t -> int

val pending_requests : t -> int

val free_buffers : t -> int

val acquire_buffer : t -> buffer

val release_buffer : buffer -> t -> unit

