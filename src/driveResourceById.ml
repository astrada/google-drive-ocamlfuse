open GapiMonad
open GapiMonad.SessionM.Infix
module File = GapiDriveV3Model.File

type runtime = DriveRuntime.cache_only = { cache : CacheData.t }

module type PORTS = sig
  val root_directory : string
  val shared_with_me_directory : string
  val get_root_folder_id : unit -> string
  val get_well_known_resource : string -> bool -> CacheData.Resource.t

  val select_first_resource_with_remote_id :
    CacheData.t -> string -> CacheData.Resource.t option

  val clean_filename : string -> string
  val create_resource : string -> CacheData.Resource.t

  val update_resource_from_file :
    CacheData.Resource.t -> File.t -> CacheData.Resource.t

  val get_file_by_remote_id : string -> File.t SessionM.m
end

module Make (P : PORTS) = struct
  let rec get_full_path root_folder_id file path_parts shared =
    if file.File.parents = [] && shared then
      SessionM.return (P.shared_with_me_directory :: path_parts)
    else if file.File.parents = [ root_folder_id ] then
      SessionM.return ("" :: path_parts)
    else
      let parent_id = List.hd file.File.parents in
      P.get_file_by_remote_id parent_id >>= fun parent ->
      get_full_path root_folder_id parent
        (P.clean_filename parent.File.name :: path_parts)
        shared

  let get_resource_with_id_from_server remote_id =
    let root_folder_id = P.get_root_folder_id () in
    if remote_id = root_folder_id then
      SessionM.return (P.get_well_known_resource P.root_directory false)
    else (
      Utils.log_with_header
        "BEGIN: Getting resource from server (remote id=%s)\n%!" remote_id;
      P.get_file_by_remote_id remote_id >>= fun file ->
      get_full_path root_folder_id file
        [ P.clean_filename file.File.name ]
        file.File.shared
      >>= fun path_parts ->
      let path = String.concat Filename.dir_sep path_parts in
      let new_resource = P.create_resource path in
      let resource = P.update_resource_from_file new_resource file in
      Utils.log_with_header
        "END: Getting resource from server (remote id=%s path=%s)\n%!" remote_id
        path;
      SessionM.return resource)

  let get_resource_with_id runtime remote_id =
    match P.select_first_resource_with_remote_id runtime.cache remote_id with
    | Some resource -> SessionM.return resource
    | None -> get_resource_with_id_from_server remote_id
end
