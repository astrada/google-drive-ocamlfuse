exception Directory_not_empty
exception Existing_attribute
exception File_not_found
exception IO_error
exception Invalid_operation
exception No_attribute
exception Permission_denied

val folder_mime_type : string
val shortcut_mime_type : string
val file_fields : string
val file_std_params : GapiService.StandardParameters.t
val file_list_std_params : GapiService.StandardParameters.t
val file_download_std_params : GapiService.StandardParameters.t
val changes_std_params : GapiService.StandardParameters.t
val device_scope : string
val device_root_folder : string

val do_request :
  (GapiConversation.Session.t -> 'a * GapiConversation.Session.t) ->
  'a * GapiConversation.Session.t

val async_do_request :
  (GapiConversation.Session.t -> 'a * GapiConversation.Session.t) -> Thread.t

val root_directory : string
val default_root_folder_id : string
val trash_directory : string
val trash_directory_name_length : int
val trash_directory_base_path : string
val lost_and_found_directory : string
val shared_with_me_directory : string
val f_bsize : int64
val change_limit : int
val max_link_target_length : int
val max_attribute_length : int
val chars_blacklist_regexp : Str.regexp
val clean_filename : string -> string
val apostrophe_regexp : Str.regexp
val escape_apostrophe : string -> string
val json_length : string -> int
val get_remote_id_fingerprint : int -> string -> string

val disambiguate_filename :
  string -> string -> string -> (string, int) Hashtbl.t -> string

val is_in_trash_directory : string -> Config.t -> bool
val is_lost_and_found_root : string -> bool -> Config.t -> bool
val is_lost_and_found : string -> bool -> Config.t -> bool
val is_shared_with_me_root : string -> bool -> 'a -> bool
val is_shared_with_me : string -> bool -> 'a -> bool
val get_path_in_cache : string -> Config.t -> string * bool
val match_service_error : string -> exn -> bool
val handle_default_exceptions : exn -> 'a -> 'b

val build_resource_keys_header :
  (string option * string option) list -> GapiCore.Header.t list

val try_with_default : ('a -> 'b) -> 'a -> 'b
val with_retry_default : ('a -> 'b) -> 'a -> 'b
val get_file_extension : string -> string
val get_filename : string -> bool -> (Config.t -> string) -> string
val get_file_extension_from_format : CacheData.Resource.t -> Config.t -> string
val get_file_extension_from_mime_type : string -> Config.t -> string

val build_resource_tables :
  string ->
  bool ->
  (string, int) Hashtbl.t * (string, CacheData.Resource.t) Hashtbl.t

val clean_document_extension :
  string -> CacheData.Resource.t -> Config.t -> string

val create_resource : string -> CacheData.Resource.t
val create_root_resource : string -> bool -> CacheData.Resource.t
val create_well_known_resource : string -> CacheData.Resource.t

val get_unique_filename :
  string ->
  string ->
  string ->
  bool ->
  (Config.t -> string) ->
  (string, int) Hashtbl.t ->
  string

val get_unique_filename_from_resource :
  CacheData.Resource.t -> string -> (string, int) Hashtbl.t -> string

val get_unique_filename_from_file :
  GapiDriveV3Model.File.t -> (string, int) Hashtbl.t -> string

val recompute_path : CacheData.Resource.t -> string -> string

val update_resource_from_file :
  ?state:CacheData.Resource.State.t ->
  ?link_target:string ->
  CacheData.Resource.t ->
  GapiDriveV3Model.File.t ->
  CacheData.Resource.t

val insert_resource_into_cache :
  ?state:CacheData.Resource.State.t ->
  ?link_target:string ->
  CacheData.t ->
  CacheData.Resource.t ->
  GapiDriveV3Model.File.t ->
  CacheData.Resource.t

val update_cached_resource : CacheData.t -> CacheData.Resource.t -> unit

val update_cached_resource_state :
  CacheData.t -> CacheData.Resource.State.t -> int64 -> unit

val update_cached_resource_state_and_size :
  CacheData.t -> CacheData.Resource.State.t -> int64 -> int64 -> unit

val lookup_resource : string -> bool -> CacheData.Resource.t option
val update_cache_size : int64 -> CacheData.Metadata.t -> CacheData.t -> unit
val shrink_cache : ?file_size:int64 -> unit -> unit

val delete_memory_buffers :
  Buffering.MemoryBuffers.t -> CacheData.Resource.t -> unit

val delete_from_context : Context.t -> CacheData.Resource.t -> unit
val delete_cached_resource : CacheData.Resource.t -> unit

val delete_cached_resources :
  CacheData.Metadata.t -> CacheData.t -> CacheData.Resource.t list -> unit

val update_cache_size_for_documents :
  CacheData.t -> CacheData.Resource.t -> string -> (int64 -> int64) -> unit

val build_resource_keys_header_from_resource :
  CacheData.Resource.t -> GapiCore.Header.t list

val build_resource_keys_header_from_resources :
  CacheData.Resource.t list -> GapiCore.Header.t list

val get_file_from_server :
  string ->
  string ->
  bool ->
  GapiDriveV3Model.File.t option GapiMonad.SessionM.m

val get_root_folder_id_from_server : Config.t -> string GapiMonad.SessionM.m
val get_root_folder_id : Config.t -> string GapiMonad.SessionM.m
val get_root_folder_id_from_context : unit -> string
val get_well_known_resource : string -> bool -> CacheData.Resource.t
val get_metadata : unit -> CacheData.Metadata.t
val statfs : unit -> Fuse.Unix_util.statvfs

val get_resource_with_id_from_server :
  string -> CacheData.Resource.t GapiMonad.SessionM.t

val get_resource_with_id :
  string -> CacheData.t -> CacheData.Resource.t GapiMonad.SessionM.t

val get_resource_from_server :
  string ->
  string ->
  CacheData.Resource.t ->
  bool ->
  CacheData.t ->
  CacheData.Resource.t GapiMonad.SessionM.m

val check_resource_in_cache : 'a -> string -> bool -> bool
val get_folder_id : string -> bool -> string GapiMonad.SessionM.t
val get_resource : string -> bool -> CacheData.Resource.t GapiMonad.SessionM.m
val check_md5_checksum : CacheData.Resource.t -> CacheData.t -> bool

val with_retry :
  (CacheData.Resource.t ->
  GapiConversation.Session.t ->
  'a * GapiConversation.Session.t) ->
  CacheData.Resource.t ->
  'a GapiMonad.SessionM.m

val is_desktop_format : CacheData.Resource.t -> Config.t -> bool
val create_desktop_entry : CacheData.Resource.t -> string -> Config.t -> unit
val create_html_with_redirect : CacheData.Resource.t -> string -> 'a -> unit

val download_media :
  GapiMediaResource.download ->
  CacheData.Resource.t ->
  GapiConversation.Session.t ->
  GapiDriveV3Model.File.t * GapiConversation.Session.t

val flush_memory_buffers : CacheData.Resource.t -> unit
val download_resource : CacheData.Resource.t -> string GapiMonad.SessionM.m

val stream_resource :
  int64 ->
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  CacheData.Resource.t ->
  unit GapiMonad.SessionM.m

val start_buffer_eviction_thread :
  Context.t -> Buffering.MemoryBuffers.t -> unit

val stream_resource_to_memory_buffer :
  int64 ->
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  CacheData.Resource.t ->
  unit GapiMonad.SessionM.m

val stream_resource_to_read_ahead_buffers :
  int64 ->
  CacheData.Resource.t ->
  unit GapiMonad.SessionM.m list GapiMonad.SessionM.m

val is_filesystem_read_only : unit -> bool
val is_file_read_only : CacheData.Resource.t -> bool

val fetch_link_target :
  string -> bool -> CacheData.t -> string GapiMonad.SessionM.m

val get_attr : string -> Unix.LargeFile.stats
val read_dir : string -> string list
val fopen : string -> Unix.open_flag list -> 'a option
val opendir : string -> 'a -> 'b option

val default_save_resource_to_db :
  CacheData.t -> CacheData.Resource.t -> GapiDriveV3Model.File.t -> unit

val update_remote_resource :
  string ->
  ?update_file_in_cache:(string -> unit) ->
  ?save_to_db:
    (CacheData.t -> CacheData.Resource.t -> GapiDriveV3Model.File.t -> unit) ->
  ?purge_cache:(CacheData.t -> CacheData.Resource.t -> unit) ->
  (CacheData.Resource.t -> GapiDriveV3Model.File.t option GapiMonad.SessionM.m) ->
  unit GapiMonad.SessionM.m

val utime : string -> float -> float -> unit

val read :
  string ->
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  int64 ->
  'a ->
  int

val write :
  string ->
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  int64 ->
  'a ->
  int

val start_uploading_if_dirty : string -> bool
val upload : CacheData.Resource.t -> unit GapiMonad.SessionM.m

val upload_resource_with_retry :
  CacheData.Resource.t -> unit GapiMonad.SessionM.m

val upload_resource_by_id : int64 -> unit
val init_filesystem : unit -> unit
val queue_upload : CacheData.Resource.t -> unit GapiMonad.SessionM.t
val upload_with_retry : string -> unit GapiMonad.SessionM.m
val upload_if_dirty : string -> unit
val flush : string -> 'a -> unit
val fsync : string -> 'a -> 'b -> unit
val release : string -> 'a -> 'b -> unit

val create_remote_resource :
  ?link_target:string -> bool -> string -> int -> unit

val mknod : string -> int -> unit
val mkdir : string -> int -> unit
val check_if_empty : string -> bool -> bool -> unit GapiMonad.SessionM.m
val trash_resource : bool -> bool -> string -> unit GapiMonad.SessionM.m
val delete_resource : bool -> string -> unit GapiMonad.SessionM.m
val delete_remote_resource : bool -> string -> unit
val unlink : string -> unit
val rmdir : string -> unit
val rename : string -> string -> unit
val truncate : string -> int64 -> unit
val chmod : string -> int -> unit
val chown : string -> int -> int -> unit
val get_xattr : string -> string -> string
val set_xattr : string -> string -> string -> Fuse.xattr_flags -> unit
val list_xattr : string -> string list
val remove_xattr : string -> string -> unit
val read_link : string -> string
val symlink : string -> string -> unit
