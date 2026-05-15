open GapiMonad
open GapiMonad.SessionM.Infix
module File = GapiDriveV3Model.File

let root_directory = "/"
let default_root_folder_id = "root"
let trash_directory = "/.Trash"
let trash_directory_name_length = String.length trash_directory
let trash_directory_base_path = "/.Trash/"
let lost_and_found_directory = "/lost+found"
let shared_with_me_directory = "/.shared"
let device_scope = "https://www.googleapis.com/auth/drive.file"
let device_root_folder = "gdfuse"

let is_lost_and_found_root path trashed config =
  if trashed || not config.Config.lost_and_found then false
  else path = lost_and_found_directory

let is_shared_with_me_root path trashed _config =
  if trashed then false else path = shared_with_me_directory

type runtime = {
  cache : CacheData.t;
  config : Config.t;
  root_folder_id : string option;
}

module type PORTS = sig
  val folder_mime_type : string
  val create_resource : string -> CacheData.Resource.t

  val find_file_in_folder :
    parent_folder_id:string ->
    name:string ->
    trashed:bool ->
    File.t option SessionM.m

  val get_file_by_remote_id : string -> File.t SessionM.m
  val create_folder : name:string -> File.t SessionM.m
  val run_request : 'a SessionM.m -> 'a
  val set_context_root_folder_id : string -> unit

  val lookup_resource :
    CacheData.t -> string -> bool -> CacheData.Resource.t option

  val insert_resource :
    CacheData.t -> label:string -> CacheData.Resource.t -> CacheData.Resource.t
end

module Make (P : PORTS) = struct
  let create_root_resource root_folder_id trashed =
    let resource = P.create_resource root_directory in
    {
      resource with
      CacheData.Resource.remote_id = Some root_folder_id;
      mime_type = Some P.folder_mime_type;
      size = Some 0L;
      parent_path = "";
      trashed = Some trashed;
    }

  let create_well_known_resource path =
    let resource = P.create_resource path in
    {
      resource with
      CacheData.Resource.remote_id = Some "";
      mime_type = Some P.folder_mime_type;
      size = Some 0L;
      parent_path = "";
      trashed = Some false;
    }

  let get_root_folder_id_from_server config =
    Utils.log_with_header "BEGIN: Getting root resource from server\n%!";
    (if config.Config.scope = device_scope then
       P.find_file_in_folder ~parent_folder_id:default_root_folder_id
         ~name:device_root_folder ~trashed:false
       >>= function
       | None ->
           Utils.log_with_header "BEGIN: Creating root (%s) on server\n%!"
             device_root_folder;
           P.create_folder ~name:device_root_folder >>= fun created_file ->
           Utils.log_with_header "END: Creating root (id=%s) on server\n%!"
             created_file.File.id;
           SessionM.return created_file
       | Some root -> SessionM.return root
     else P.get_file_by_remote_id default_root_folder_id)
    >>= fun file ->
    Utils.log_with_header "END: Getting root resource (id=%s) from server\n%!"
      file.File.id;
    SessionM.return file.File.id

  let get_root_folder_id config =
    let rec loop path parent_folder_id =
      let name, rest =
        try ExtString.String.split path Filename.dir_sep
        with ExtString.Invalid_string -> (path, "")
      in
      match name with
      | "" -> SessionM.return parent_folder_id
      | n -> (
          P.find_file_in_folder ~parent_folder_id ~name:n ~trashed:false
          >>= function
          | None ->
              Utils.raise_m (Failure "Invalid root folder in configuration")
          | Some file -> loop rest file.File.id)
    in
    Utils.log_with_header
      "BEGIN: Getting root folder id (team drive id=%s, root folder=%s) from \
       server\n\
       %!"
      config.Config.team_drive_id config.Config.root_folder;
    let default_root_id =
      match config.Config.team_drive_id with
      | "" -> default_root_folder_id
      | id -> id
    in
    (match config.Config.root_folder with
      | "" -> SessionM.return default_root_id
      | s when not (Filename.is_relative s) ->
          loop (String.sub s 1 (String.length s - 1)) default_root_id
      | s -> SessionM.return s)
    >>= fun root_folder_id ->
    (if root_folder_id = default_root_folder_id then
       get_root_folder_id_from_server config
     else SessionM.return root_folder_id)
    >>= fun root_folder_id ->
    Utils.log_with_header "END: Getting root folder id (id=%s) from server\n%!"
      root_folder_id;
    SessionM.return root_folder_id

  let get_root_folder_id_from_context runtime =
    match runtime.root_folder_id with
    | Some root_folder_id -> root_folder_id
    | None ->
        let root_folder_id =
          P.run_request (get_root_folder_id runtime.config)
        in
        P.set_context_root_folder_id root_folder_id;
        root_folder_id

  let get_well_known_resource runtime path trashed =
    let root_folder_id = get_root_folder_id_from_context runtime in
    match P.lookup_resource runtime.cache path trashed with
    | Some resource -> resource
    | None ->
        let well_known_resource, label =
          if path = root_directory then
            (create_root_resource root_folder_id trashed, "root")
          else if is_lost_and_found_root path trashed runtime.config then
            (create_well_known_resource lost_and_found_directory, "lost+found")
          else if is_shared_with_me_root path trashed runtime.config then
            ( create_well_known_resource shared_with_me_directory,
              "shared with me" )
          else
            invalid_arg
              ("Invalid well known path: " ^ path ^ " trashed="
             ^ string_of_bool trashed)
        in
        P.insert_resource runtime.cache ~label well_known_resource
end
