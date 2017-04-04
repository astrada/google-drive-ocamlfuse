module Buffer =
struct
  type t = {
    id : int;
    arr : (char,
           Bigarray.int8_unsigned_elt,
           Bigarray.c_layout) Bigarray.Array1.t;
    mutex : Mutex.t;
  }

end

type t = {
  max_buffers : int;
  mutable buffer_count : int;
  mutex : Mutex.t;
  condition : Condition.t;
  buffer_size : int;
  free_buffers : Buffer.t Queue.t;
  mutable pending_requests : int;
}

let create ~pool_size ~buffer_size =
  let max_buffers =
    let n = pool_size / buffer_size in
    if pool_size mod buffer_size = 0 then n
    else n + 1 in
  { max_buffers;
    buffer_count = 0;
    mutex = Mutex.create ();
    condition = Condition.create ();
    buffer_size;
    free_buffers = Queue.create ();
    pending_requests = 0;
  }

let max_buffers buffer_pool =
  buffer_pool.max_buffers

let pending_requests buffer_pool =
  Utils.with_lock buffer_pool.mutex
    (fun () ->
       buffer_pool.pending_requests
    )

let free_buffers buffer_pool =
  Utils.with_lock buffer_pool.mutex
    (fun () ->
       (buffer_pool.max_buffers - buffer_pool.buffer_count) +
       Queue.length buffer_pool.free_buffers
    )

let acquire_buffer buffer_pool =
  let get_buffer () =
    Queue.take buffer_pool.free_buffers
  in
  Utils.with_lock buffer_pool.mutex
    (fun () ->
       try
         get_buffer ()
       with Queue.Empty ->
         if buffer_pool.buffer_count < buffer_pool.max_buffers then begin
           buffer_pool.buffer_count <- buffer_pool.buffer_count + 1;
           { Buffer.id = buffer_pool.buffer_count;
             arr = Bigarray.Array1.create
                 Bigarray.char Bigarray.c_layout buffer_pool.buffer_size;
             mutex = Mutex.create ();
           }
         end else begin
           buffer_pool.pending_requests <- buffer_pool.pending_requests + 1;
           while Queue.length buffer_pool.free_buffers = 0 do
             Condition.wait buffer_pool.condition buffer_pool.mutex;
           done;
           buffer_pool.pending_requests <- buffer_pool.pending_requests - 1;
           get_buffer ();
         end
    )

let release_buffer buffer buffer_pool =
  Utils.with_lock buffer_pool.mutex
    (fun () ->
       Queue.add buffer buffer_pool.free_buffers;
       Condition.signal buffer_pool.condition;
    )

