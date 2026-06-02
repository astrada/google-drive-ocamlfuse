open GapiMonad
open GapiMonad.SessionM.Infix

exception Permission_denied = DriveMutations.Permission_denied

type runtime = DriveRuntime.base = { cache : CacheData.t; config : Config.t }

module type PORTS = sig
  include DrivePortFragments.PATH_LOOKUP
  include DrivePortFragments.RESOURCE_LOOKUP

  val get_content_path : CacheData.t -> CacheData.Resource.t -> string
  val file_exists : string -> bool

  val update_resource_from_file :
    CacheData.Resource.t -> GapiDriveV3Model.File.t -> CacheData.Resource.t

  val update_cached_resource : CacheData.t -> CacheData.Resource.t -> unit
end

module Make (P : PORTS) = struct
  let default_save_resource_to_db cache resource file =
    let updated_resource = P.update_resource_from_file resource file in
    P.update_cached_resource cache updated_resource

  let maybe_update_file_in_cache runtime resource update_file_in_cache =
    match update_file_in_cache with
    | None -> ()
    | Some update_file ->
        if
          resource.CacheData.Resource.state
          = CacheData.Resource.State.Synchronized
        then
          let content_path = P.get_content_path runtime.cache resource in
          if P.file_exists content_path then update_file content_path

  let update_remote_resource runtime path ?update_file_in_cache
      ?(save_to_db = default_save_resource_to_db)
      ?(purge_cache = fun _cache _resource -> ()) do_remote_update =
    let path_in_cache, trashed = P.get_path_in_cache path runtime.config in
    if runtime.config.Config.read_only then raise Permission_denied
    else
      P.get_resource path_in_cache trashed >>= fun resource ->
      do_remote_update resource >>= fun file_option ->
      (match file_option with
      | None -> purge_cache runtime.cache resource
      | Some file ->
          maybe_update_file_in_cache runtime resource update_file_in_cache;
          save_to_db runtime.cache resource file);
      SessionM.return ()
end
