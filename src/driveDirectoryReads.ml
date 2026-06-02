open GapiLens.Infix
open GapiMonad
open GapiMonad.SessionM.Infix
module File = GapiDriveV3Model.File

type runtime = DriveRuntime.base = { cache : CacheData.t; config : Config.t }

module type PORTS = sig
  val get_path_in_cache : string -> Config.t -> string * bool
  val get_resource : string -> bool -> CacheData.Resource.t SessionM.m
  val get_folder_id : string -> bool -> string SessionM.m
  val is_lost_and_found_root : string -> bool -> Config.t -> bool
  val is_shared_with_me_root : string -> bool -> Config.t -> bool
  val check_resource_in_cache : CacheData.t -> string -> bool -> bool

  val select_resources_with_parent_path :
    CacheData.t -> string -> bool -> CacheData.Resource.t list

  val list_files : string -> File.t list SessionM.m

  val build_resource_tables :
    string ->
    bool ->
    (string, int) Hashtbl.t * (string, CacheData.Resource.t) Hashtbl.t

  val update_resource_from_file :
    CacheData.Resource.t -> File.t -> CacheData.Resource.t

  val get_unique_filename_from_file :
    File.t -> (string, int) Hashtbl.t -> string

  val create_resource : string -> CacheData.Resource.t

  val insert_resources :
    CacheData.t ->
    CacheData.Resource.t list ->
    string ->
    bool ->
    CacheData.Resource.t list

  val update_cached_resource : CacheData.t -> CacheData.Resource.t -> unit
  val current_time : unit -> float
end

module Make (P : PORTS) = struct
  let root_directory = "/"
  let trash_directory = "/.Trash"
  let lost_and_found_directory = "/lost+found"
  let shared_with_me_directory = "/.shared"

  let read_dir runtime path =
    let path_in_cache, trashed = P.get_path_in_cache path runtime.config in
    let request_folder =
      Utils.log_with_header
        "BEGIN: Getting folder content (path=%s, trashed=%b)\n%!" path_in_cache
        trashed;
      P.get_resource path_in_cache trashed >>= fun resource ->
      if P.is_lost_and_found_root path trashed runtime.config then (
        Utils.log_with_header "BEGIN: Getting lost and found files\n%!";
        P.list_files "'me' in owners" >>= fun all_owned_files ->
        let lost_and_found_files =
          List.filter (fun file -> file.File.parents = []) all_owned_files
        in
        Utils.log_with_header
          "END: Getting lost and found files: Found %d files\n%!"
          (List.length lost_and_found_files);
        SessionM.return (lost_and_found_files, resource))
      else if P.is_shared_with_me_root path trashed runtime.config then (
        Utils.log_with_header "BEGIN: Getting shared with me files\n%!";
        P.list_files "sharedWithMe = true" >>= fun shared_with_me_files ->
        Utils.log_with_header
          "END: Getting shared with me files: Found %d files\n%!"
          (List.length shared_with_me_files);
        SessionM.return (shared_with_me_files, resource))
      else
        P.get_folder_id path_in_cache trashed >>= fun folder_id ->
        let q =
          Printf.sprintf "'%s' in parents and trashed = %b" folder_id trashed
        in
        P.list_files q >>= fun files ->
        Utils.log_with_header
          "END: Getting folder content (path=%s, trashed=%b)\n%!" path_in_cache
          trashed;
        if
          path = trash_directory && trashed
          && not runtime.config.Config.disable_trash
        then (
          Utils.log_with_header "BEGIN: Getting explicitly trashed files\n%!";
          let q =
            Printf.sprintf "not '%s' in parents and trashed = true" folder_id
          in
          P.list_files q >>= fun trashed_files ->
          let explicitly_trashed_files =
            List.filter (fun file -> file.File.explicitlyTrashed) trashed_files
          in
          Utils.log_with_header
            "END: Getting explicitly trashed files: Found %d files\n%!"
            (List.length explicitly_trashed_files);
          SessionM.return (files @ explicitly_trashed_files, resource))
        else SessionM.return (files, resource)
    in
    let resources_m =
      if P.check_resource_in_cache runtime.cache path_in_cache trashed then (
        Utils.log_with_header
          "BEGIN: Getting resources from db (parent path=%s, trashed=%b)\n%!"
          path_in_cache trashed;
        let resources =
          P.select_resources_with_parent_path runtime.cache path_in_cache
            trashed
        in
        Utils.log_with_header
          "END: Getting resources from db (parent path=%s, trashed=%b)\n%!"
          path_in_cache trashed;
        SessionM.return resources)
      else
        request_folder >>= fun (files, folder_resource) ->
        let filename_table, remote_id_table =
          P.build_resource_tables path_in_cache trashed
        in
        let resources_and_files =
          List.map
            (fun file ->
              let resource =
                try Some (Hashtbl.find remote_id_table file.File.id)
                with Not_found -> None
              in
              (resource, file))
            files
        in
        let resources =
          List.map
            (fun (resource, file) ->
              match resource with
              | Some r -> P.update_resource_from_file r file
              | None ->
                  let filename =
                    P.get_unique_filename_from_file file filename_table
                  in
                  let resource_path = Filename.concat path_in_cache filename in
                  let resource = P.create_resource resource_path in
                  P.update_resource_from_file resource file)
            resources_and_files
        in
        Utils.log_with_header
          "BEGIN: Inserting folder resources into db (trashed=%b)\n%!" trashed;
        let inserted_resources =
          P.insert_resources runtime.cache resources path_in_cache trashed
        in
        Utils.log_with_header
          "END: Inserting folder resources into db (trashed=%b)\n%!" trashed;
        let updated_resource =
          folder_resource
          |> CacheData.Resource.state ^= CacheData.Resource.State.Synchronized
          |> CacheData.Resource.last_update ^= P.current_time ()
        in
        P.update_cached_resource runtime.cache updated_resource;
        SessionM.return inserted_resources
    in
    resources_m >>= fun resources ->
    let filenames =
      List.map
        (fun resource -> Filename.basename resource.CacheData.Resource.path)
        resources
    in
    let filenames =
      if path = root_directory && not runtime.config.Config.disable_trash then
        Filename.basename trash_directory :: filenames
      else filenames
    in
    let filenames =
      if path = root_directory && not trashed then
        Filename.basename shared_with_me_directory :: filenames
      else filenames
    in
    if
      path = root_directory && (not trashed)
      && runtime.config.Config.lost_and_found
    then
      SessionM.return (Filename.basename lost_and_found_directory :: filenames)
    else SessionM.return filenames
end
