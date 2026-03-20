exception File_not_found

module type FileStore = sig
  type data
  type t = { path : string; data : data }

  val path : (t, string) GapiLens.t
  val data : (t, data) GapiLens.t
  val save : t -> unit
  val load : string -> t
end

module type Data = sig
  type t

  val of_table : (string, string) Hashtbl.t -> t
  val to_table : t -> (string, string) Hashtbl.t
end

module MakeFileStore : (D : Data) -> sig
  type data = D.t
  type t = { path : string; data : data }

  val path : (t, string) GapiLens.t
  val data : (t, data) GapiLens.t
  val load : Scanf.Scanning.file_name -> t
  val save : t -> unit
end
