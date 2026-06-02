open GapiMonad
open GapiMonad.SessionM.Infix

exception Permission_denied = DriveMutations.Permission_denied

type runtime = DriveRuntime.config_only = { config : Config.t }

module type PORTS = sig
  val get_path_in_cache : string -> Config.t -> string * bool
  val get_resource : string -> bool -> CacheData.Resource.t SessionM.m
end

let is_desktop_format config resource =
  CacheData.Resource.get_format resource config = "desktop"

let is_file_read_only config resource =
  (not (Option.default true resource.CacheData.Resource.can_edit))
  || CacheData.Resource.is_document resource
     && ((not config.Config.editable_docs) || is_desktop_format config resource)
  || config.Config.large_file_read_only
     && CacheData.Resource.is_large_file config resource

module Make (P : PORTS) = struct
  let fopen runtime path flags =
    let path_in_cache, trashed = P.get_path_in_cache path runtime.config in
    let is_read_only_request = List.mem Unix.O_RDONLY flags in
    if (not is_read_only_request) && runtime.config.Config.read_only then
      raise Permission_denied
    else
      P.get_resource path_in_cache trashed >>= fun resource ->
      if (not is_read_only_request) && is_file_read_only runtime.config resource
      then Utils.raise_m Permission_denied
      else SessionM.return ()
end
