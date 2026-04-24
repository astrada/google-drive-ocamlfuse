exception Existing_attribute
exception Invalid_operation
exception No_attribute
exception Permission_denied

type runtime = { cache : CacheData.t; config : Config.t }

module type PORTS = sig
  val max_attribute_length : int
  val json_length : string -> int
  val get_path_in_cache : string -> Config.t -> string * bool
  val get_resource : string -> bool -> CacheData.Resource.t GapiMonad.SessionM.m

  val build_resource_keys_header_from_resource :
    CacheData.Resource.t -> GapiCore.Header.t list

  val remote_update :
    custom_headers:GapiCore.Header.t list ->
    fileId:string ->
    GapiDriveV3Model.File.t ->
    GapiDriveV3Model.File.t GapiMonad.SessionM.m

  val update_remote_resource :
    runtime ->
    string ->
    (CacheData.Resource.t ->
    GapiDriveV3Model.File.t option GapiMonad.SessionM.m) ->
    unit GapiMonad.SessionM.m
end

module Make (P : PORTS) : sig
  val get_xattr : runtime -> string -> string -> string GapiMonad.SessionM.m

  val set_xattr :
    runtime ->
    string ->
    string ->
    string ->
    Fuse.xattr_flags ->
    unit GapiMonad.SessionM.m

  val list_xattr : runtime -> string -> string list GapiMonad.SessionM.m
  val remove_xattr : runtime -> string -> string -> unit GapiMonad.SessionM.m
end
