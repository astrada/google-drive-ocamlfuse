type runtime = { cache : CacheData.t; config : Config.t }

module type PORTS = sig
  val get_content_path : CacheData.t -> CacheData.Resource.t -> string

  val create_file_resource :
    ?content_type:string -> string -> GapiMediaResource.t

  val media_content_type : GapiMediaResource.t -> string
  val media_content_length : GapiMediaResource.t -> int64

  val update_cached_resource_state_and_size :
    CacheData.t -> CacheData.Resource.State.t -> int64 -> int64 -> unit

  val build_resource_keys_header_from_resource :
    CacheData.Resource.t -> GapiCore.Header.t list

  val now : unit -> Netdate.t

  val remote_update :
    media_source:GapiMediaResource.t option ->
    custom_headers:GapiCore.Header.t list ->
    fileId:string ->
    GapiDriveV3Model.File.t ->
    GapiDriveV3Model.File.t GapiMonad.SessionM.m

  val update_resource_from_file :
    ?state:CacheData.Resource.State.t ->
    CacheData.Resource.t ->
    GapiDriveV3Model.File.t ->
    CacheData.Resource.t

  val select_first_resource_with_remote_id :
    CacheData.t -> string -> CacheData.Resource.t option

  val update_cached_resource : CacheData.t -> CacheData.Resource.t -> unit
  val shrink_cache : unit -> unit
end

module Make (P : PORTS) : sig
  val content_type_for_upload :
    runtime -> CacheData.Resource.t -> string -> string

  val next_state_after_upload :
    CacheData.Resource.t -> CacheData.Resource.State.t option

  val upload : runtime -> CacheData.Resource.t -> unit GapiMonad.SessionM.m
end
