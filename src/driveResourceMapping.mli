module File = GapiDriveV3Model.File

val folder_mime_type : string
val shortcut_mime_type : string
val clean_filename : string -> string
val get_file_extension : string -> string
val get_filename : Config.t -> string -> bool -> (Config.t -> string) -> string
val get_file_extension_from_format : CacheData.Resource.t -> Config.t -> string
val get_file_extension_from_mime_type : string -> Config.t -> string

val build_resource_tables :
  Config.t ->
  CacheData.Resource.t list ->
  (string, int) Hashtbl.t * (string, CacheData.Resource.t) Hashtbl.t

val clean_document_extension :
  Config.t -> string -> CacheData.Resource.t -> string

val create_resource : now:(unit -> float) -> string -> CacheData.Resource.t

val get_unique_filename :
  Config.t ->
  string ->
  string ->
  string ->
  bool ->
  (Config.t -> string) ->
  (string, int) Hashtbl.t ->
  string

val get_unique_filename_from_resource :
  Config.t ->
  CacheData.Resource.t ->
  string ->
  (string, int) Hashtbl.t ->
  string

val get_unique_filename_from_file :
  Config.t -> File.t -> (string, int) Hashtbl.t -> string

val recompute_path :
  Config.t ->
  CacheData.Resource.t ->
  string ->
  (string, int) Hashtbl.t ->
  string

val update_resource_from_file :
  now:(unit -> float) ->
  recompute_path:(CacheData.Resource.t -> string -> string) ->
  ?state:CacheData.Resource.State.t ->
  ?link_target:string ->
  CacheData.Resource.t ->
  File.t ->
  CacheData.Resource.t
