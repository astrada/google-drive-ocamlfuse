module type FileStore =
sig
  type data

  type t = {
    path : string;
    data : data
  }

  val save : t -> unit

  val load : string -> t

end

module type Data =
sig
  type t

  val of_table : (string, string) Hashtbl.t -> t

  val to_table : t -> (string, string) Hashtbl.t

end

module MakeFileStore(D : Data) =
struct
  type data = D.t

  type t = {
    path : string;
    data : data
  }

  let load filename =
    let sb = Scanf.Scanning.from_file filename in
    let table = Hashtbl.create 16 in
      while (not (Scanf.Scanning.end_of_input sb)) do
        let (key, value) = Scanf.bscanf sb "%s@=%s@\n" (fun k v -> (k, v)) in
          Hashtbl.add table key value
      done;
      { path = filename;
        data = D.of_table table;
      }

  let save store =
    let table = D.to_table store.data in
    let out_ch = open_out store.path in
      Hashtbl.iter
        (fun key value ->
           Printf.fprintf out_ch "%s=%s\n" key value)
        table;
      close_out out_ch

end

