module Buffer :
sig
  type t = {
    id : int;
    arr : (char,
           Bigarray.int8_unsigned_elt,
           Bigarray.c_layout) Bigarray.Array1.t;
    mutex : Mutex.t;
  }

end

type t

val create : pool_size:int -> buffer_size:int -> t

val max_buffers : t -> int

val pending_requests : t -> int

val free_buffers : t -> int

val acquire_buffer : t -> Buffer.t

val release_buffer : Buffer.t -> t -> unit

