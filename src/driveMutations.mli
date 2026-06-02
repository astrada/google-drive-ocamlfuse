exception Directory_not_empty
exception Existing_attribute
exception File_not_found
exception IO_error
exception Invalid_operation
exception No_attribute
exception Permission_denied

type runtime = {
  cache : CacheData.t;
  config : Config.t;
  mountpoint_path : string;
  skip_trash : bool;
}

module type PORTS = sig
  include DrivePortFragments.PATH_LOOKUP
  include DrivePortFragments.RESOURCE_LOOKUP
  include DrivePortFragments.RESOURCE_KEYS

  val max_link_target_length : int
  val json_length : string -> int
  val is_lost_and_found : string -> bool -> Config.t -> bool
  val is_lost_and_found_root : string -> bool -> Config.t -> bool
  val is_filesystem_read_only : unit -> bool
  val create_resource : string -> CacheData.Resource.t

  val clean_document_extension :
    string -> CacheData.Resource.t -> Config.t -> string

  val recompute_path : CacheData.Resource.t -> string -> string

  val update_resource_from_file :
    ?state:CacheData.Resource.State.t ->
    ?link_target:string ->
    CacheData.Resource.t ->
    GapiDriveV3Model.File.t ->
    CacheData.Resource.t

  val build_resource_keys_header_from_resources :
    CacheData.Resource.t list -> GapiCore.Header.t list

  val insert_resource_into_cache :
    ?state:CacheData.Resource.State.t ->
    ?link_target:string ->
    CacheData.t ->
    CacheData.Resource.t ->
    GapiDriveV3Model.File.t ->
    CacheData.Resource.t

  val update_cached_resource : CacheData.t -> CacheData.Resource.t -> unit
  val delete_cached_resource : CacheData.Resource.t -> unit
  val delete_all_with_parent_path : CacheData.t -> string -> bool -> unit
  val trash_all_with_parent_path : CacheData.t -> string -> unit
  val invalidate_trash_bin : CacheData.t -> unit
  val delete_not_found_resource_with_path : CacheData.t -> string -> unit

  val select_first_resource_with_remote_id :
    CacheData.t -> string -> CacheData.Resource.t option

  val remote_create :
    GapiDriveV3Model.File.t -> GapiDriveV3Model.File.t GapiMonad.SessionM.m

  val remote_update :
    custom_headers:GapiCore.Header.t list ->
    fileId:string ->
    GapiDriveV3Model.File.t ->
    GapiDriveV3Model.File.t GapiMonad.SessionM.m

  val remote_delete :
    custom_headers:GapiCore.Header.t list ->
    fileId:string ->
    unit GapiMonad.SessionM.m

  val remote_move :
    custom_headers:GapiCore.Header.t list ->
    addParents:string ->
    fileId:string ->
    removeParents:string ->
    GapiDriveV3Model.File.t ->
    GapiDriveV3Model.File.t GapiMonad.SessionM.m

  val replace_target_contents :
    source:CacheData.Resource.t ->
    target:CacheData.Resource.t ->
    unit GapiMonad.SessionM.m

  val check_if_empty_remote :
    string -> bool -> bool -> unit GapiMonad.SessionM.m
end

module Make (P : PORTS) : sig
  val update_remote_resource :
    runtime ->
    string ->
    ?save_to_db:
      (CacheData.t -> CacheData.Resource.t -> GapiDriveV3Model.File.t -> unit) ->
    ?purge_cache:(CacheData.t -> CacheData.Resource.t -> unit) ->
    (CacheData.Resource.t ->
    GapiDriveV3Model.File.t option GapiMonad.SessionM.m) ->
    unit GapiMonad.SessionM.m

  val create_remote_resource :
    runtime ->
    ?link_target:string ->
    bool ->
    string ->
    int ->
    unit GapiMonad.SessionM.m

  val trash_resource :
    runtime -> bool -> bool -> string -> unit GapiMonad.SessionM.m

  val delete_resource : runtime -> bool -> string -> unit GapiMonad.SessionM.m

  val delete_remote_resource :
    runtime -> bool -> string -> unit GapiMonad.SessionM.m

  val rename : runtime -> string -> string -> unit GapiMonad.SessionM.m
end
