type t = {
  cache_dir : string;
  db_path : string;
  busy_timeout : int;
  in_memory : bool;
  autosaving_interval : int;
}

module Resource :
sig
  module State :
  sig
    type t =
        Synchronized
      | ToDownload
      | Downloading
      | ToUpload
      | Uploading
      | NotFound

    val to_string : t -> string
    val of_string : string -> t
  end

  type t = {
    id : int64;
    remote_id : string option;
    name : string option;
    mime_type : string option;
    created_time : float option;
    modified_time : float option;
    viewed_by_me_time : float option;
    file_extension : string option;
    full_file_extension : string option;
    md5_checksum : string option;
    size : int64 option;
    can_edit : bool option;
    trashed : bool option;
    web_view_link : string option;
    version : int64 option;
    file_mode_bits : int64 option;
    uid : int64 option;
    gid : int64 option;
    link_target : string option;
    xattrs : string;
    parent_path : string;
    path : string;
    state : State.t;
    last_update : float;
  }

  val id : (t, int64) GapiLens.t
  val remote_id : (t, string option) GapiLens.t
  val name : (t, string option) GapiLens.t
  val mime_type : (t, string option) GapiLens.t
  val created_time : (t, float option) GapiLens.t
  val modified_time : (t, float option) GapiLens.t
  val viewed_by_me_time : (t, float option) GapiLens.t
  val file_extension : (t, string option) GapiLens.t
  val full_file_extension : (t, string option) GapiLens.t
  val md5_checksum : (t, string option) GapiLens.t
  val size : (t, int64 option) GapiLens.t
  val can_edit : (t, bool option) GapiLens.t
  val trashed : (t, bool option) GapiLens.t
  val web_view_link : (t, string option) GapiLens.t
  val version : (t, int64 option) GapiLens.t
  val file_mode_bits : (t, int64 option) GapiLens.t
  val uid : (t, int64 option) GapiLens.t
  val gid : (t, int64 option) GapiLens.t
  val link_target : (t, string option) GapiLens.t
  val xattrs : (t, string) GapiLens.t
  val parent_path : (t, string) GapiLens.t
  val path : (t, string) GapiLens.t
  val state : (t, State.t) GapiLens.t
  val last_update : (t, float) GapiLens.t

  val file_mode_bits_to_kind : int64 -> Unix.file_kind
  val file_mode_bits_to_perm : int64 -> int
  val render_xattrs : (string * string) list -> string
  val parse_xattrs : string -> (string * string) list
  val find_app_property : 'a -> ('a * 'b) list -> 'b option
  val app_property_to_int64 : string option -> int64 option
  val get_file_mode_bits : (string * string) list -> int64 option
  val file_mode_bits_to_app_property : int64 option -> string * string
  val mode_to_app_property : int -> string * string
  val get_uid : (string * string) list -> int64 option
  val uid_to_app_property : 'a -> string * 'a
  val get_gid : (string * string) list -> int64 option
  val gid_to_app_property : 'a -> string * 'a
  val get_link_target : (string * 'a) list -> 'a option
  val link_target_to_app_property : 'a -> string * 'a
  val get_xattrs : (string * string) list -> string
  val xattr_to_app_property : string -> 'a -> string * 'a
  val xattr_no_value_to_app_property : string -> string * string

  val is_folder : t -> bool
  val is_document_mime_type : string -> bool
  val is_document : t -> bool
  val is_symlink : t -> bool
  val is_valid : t -> float -> bool
  val is_large_file : Config.t -> t -> bool
  val to_stream : Config.t -> t -> bool * bool

  val get_format_from_mime_type : string -> Config.t -> string
  val get_format : t -> Config.t -> string
  val get_icon_from_mime_type : string -> Config.t -> string
  val get_icon : t -> Config.t -> string
  val mime_type_of_format : string -> string

end

module Metadata :
sig
  type t = {
    display_name : string;
    storage_quota_limit : int64;
    storage_quota_usage : int64;
    start_page_token : string;
    cache_size : int64;
    last_update : float;
    clean_shutdown : bool;
  }

  val display_name : (t, string) GapiLens.t
  val storage_quota_limit : (t, int64) GapiLens.t
  val storage_quota_usage : (t, int64) GapiLens.t
  val start_page_token : (t, string) GapiLens.t
  val cache_size : (t, int64) GapiLens.t
  val last_update : (t, float) GapiLens.t
  val clean_shutdown : (t, bool) GapiLens.t

  val is_valid : int -> t -> bool
end

module UploadEntry :
sig
  module State :
  sig
    type t =
      | ToUpload
      | Uploading

    val to_string : t -> string
    val of_string : string -> t
  end

  type t = {
    id : int64;
    resource_id : int64;
    state : string;
    last_update : float;
  }

  val id : (t, int64) GapiLens.t
  val resource_id : (t, int64) GapiLens.t
  val state : (t, string) GapiLens.t
  val last_update : (t, float) GapiLens.t

end

