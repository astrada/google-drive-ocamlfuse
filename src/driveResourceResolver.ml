open GapiMonad
open GapiMonad.SessionM.Infix
module File = GapiDriveV3Model.File

exception File_not_found = DriveMutations.File_not_found

type runtime = { cache : CacheData.t; config : Config.t }

module type PORTS = sig
  val root_directory : string
  val lost_and_found_directory : string
  val shared_with_me_directory : string
  val get_metadata : unit -> CacheData.Metadata.t
  val current_metadata_last_update : unit -> float
  val get_root_folder_id : unit -> string
  val get_well_known_resource : string -> bool -> CacheData.Resource.t
  val is_lost_and_found_root : string -> bool -> Config.t -> bool
  val is_shared_with_me_root : string -> bool -> Config.t -> bool

  val lookup_resource :
    CacheData.t -> string -> bool -> CacheData.Resource.t option

  val create_resource : string -> CacheData.Resource.t

  val insert_resource :
    CacheData.t -> CacheData.Resource.t -> CacheData.Resource.t

  val insert_resource_from_file :
    CacheData.t -> CacheData.Resource.t -> File.t -> CacheData.Resource.t

  val update_resource_from_file :
    CacheData.Resource.t -> File.t -> CacheData.Resource.t

  val update_cached_resource : CacheData.t -> CacheData.Resource.t -> unit
  val delete_cached_resource : CacheData.Resource.t -> unit

  val select_first_resource_with_remote_id :
    CacheData.t -> string -> CacheData.Resource.t option

  val find_file_in_folder :
    parent_folder_id:string ->
    name:string ->
    trashed:bool ->
    File.t option SessionM.m

  val get_file_by_remote_id : string -> File.t SessionM.m
  val with_default_retry : 'a SessionM.m -> 'a SessionM.m
end

module Make (P : PORTS) = struct
  let check_resource_in_cache runtime path trashed =
    let metadata_last_update = P.current_metadata_last_update () in
    match P.lookup_resource runtime.cache path trashed with
    | None -> false
    | Some resource ->
        if CacheData.Resource.is_valid resource metadata_last_update then
          if CacheData.Resource.is_folder resource then
            resource.CacheData.Resource.state
            = CacheData.Resource.State.Synchronized
          else true
        else false

  let rec get_folder_id runtime path trashed =
    if path = P.root_directory then SessionM.return (P.get_root_folder_id ())
    else
      get_resource runtime path trashed >>= fun resource ->
      SessionM.return (Option.get resource.CacheData.Resource.remote_id)

  and get_resource runtime path trashed =
    let metadata_last_update =
      (P.get_metadata ()).CacheData.Metadata.last_update
    in

    let rec get_new_resource () =
      let parent_path = Filename.dirname path in
      if check_resource_in_cache runtime parent_path trashed then
        Utils.raise_m File_not_found
      else
        let new_resource = P.create_resource path in
        let name = Filename.basename path in
        get_folder_id runtime new_resource.CacheData.Resource.parent_path
          trashed
        >>= fun parent_folder_id ->
        P.find_file_in_folder ~parent_folder_id ~name ~trashed >>= fun file ->
        let resource =
          match file with
          | None ->
              let not_found_resource =
                {
                  new_resource with
                  CacheData.Resource.trashed = Some trashed;
                  state = CacheData.Resource.State.NotFound;
                }
              in
              P.insert_resource runtime.cache not_found_resource
          | Some file ->
              P.insert_resource_from_file runtime.cache new_resource file
        in
        SessionM.return resource
    and refresh_resource resource =
      let refreshed_file =
        match resource.CacheData.Resource.remote_id with
        | None -> SessionM.return None
        | Some remote_id ->
            P.get_file_by_remote_id remote_id >>= fun file ->
            SessionM.return (Some (remote_id, file))
      in
      refreshed_file >>= function
      | None ->
          P.delete_cached_resource resource;
          get_new_resource ()
      | Some (remote_id, file) ->
          let reloaded_resource =
            match
              P.select_first_resource_with_remote_id runtime.cache remote_id
            with
            | None -> resource
            | Some resource -> resource
          in
          Utils.log_with_header "BEGIN: Refreshing resource (id=%Ld)\n%!"
            reloaded_resource.CacheData.Resource.id;
          let updated_resource =
            P.update_resource_from_file reloaded_resource file
          in
          P.update_cached_resource runtime.cache updated_resource;
          Utils.log_with_header "END: Refreshing resource (id=%Ld)\n%!"
            updated_resource.CacheData.Resource.id;
          SessionM.return updated_resource
    in

    if path = P.root_directory then
      SessionM.return (P.get_well_known_resource P.root_directory trashed)
    else if P.is_lost_and_found_root path trashed runtime.config then
      SessionM.return
        (P.get_well_known_resource P.lost_and_found_directory trashed)
    else if P.is_shared_with_me_root path trashed runtime.config then
      SessionM.return
        (P.get_well_known_resource P.shared_with_me_directory trashed)
    else
      (match P.lookup_resource runtime.cache path trashed with
        | None -> get_new_resource ()
        | Some resource ->
            if CacheData.Resource.is_valid resource metadata_last_update then
              SessionM.return resource
            else P.with_default_retry (refresh_resource resource))
      >>= fun resource ->
      match resource.CacheData.Resource.state with
      | CacheData.Resource.State.NotFound -> Utils.raise_m File_not_found
      | _ -> SessionM.return resource
end
