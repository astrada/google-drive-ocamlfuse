exception File_not_found

type runtime = { cache : CacheData.t; config : Config.t }

module type PORTS = sig
  val get_content_path : CacheData.t -> CacheData.Resource.t -> string

  val select_first_resource_with_remote_id :
    CacheData.t -> string -> CacheData.Resource.t option

  val file_exists : string -> bool
  val check_md5_checksum : CacheData.Resource.t -> CacheData.t -> bool

  val update_cached_resource_state :
    CacheData.t -> CacheData.Resource.State.t -> int64 -> unit

  val update_cache_size_for_documents :
    CacheData.t -> CacheData.Resource.t -> string -> (int64 -> int64) -> unit

  val shrink_cache : ?file_size:int64 -> unit -> unit

  val with_resource_lock :
    CacheData.Resource.t ->
    unit GapiMonad.SessionM.m ->
    unit GapiMonad.SessionM.m

  val create_desktop_entry : CacheData.Resource.t -> string -> Config.t -> unit

  val create_html_with_redirect :
    CacheData.Resource.t -> string -> Config.t -> unit

  val download_export_link_to_file :
    string -> string -> unit GapiMonad.SessionM.m

  val export_document_to_file :
    string -> file_id:string -> mime_type:string -> unit GapiMonad.SessionM.m

  val download_media_to_file :
    string -> CacheData.Resource.t -> unit GapiMonad.SessionM.m

  val create_empty_file : string -> unit
  val wait_exponential_backoff : int -> unit
  val handle_download_exception : exn -> unit GapiMonad.SessionM.m
end

val is_desktop_format : Config.t -> CacheData.Resource.t -> bool

module Make (P : PORTS) : sig
  val download_resource :
    runtime -> CacheData.Resource.t -> string GapiMonad.SessionM.m
end
