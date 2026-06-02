open GapiMonad
open GapiMonad.SessionM.Infix
module Change = GapiDriveV3Model.Change
module File = GapiDriveV3Model.File

type account_metadata = {
  display_name : string;
  storage_quota_limit : int64;
  storage_quota_usage : int64;
}

type runtime = DriveRuntime.base = { cache : CacheData.t; config : Config.t }

module type PORTS = sig
  val with_metadata_lock : (unit -> 'a) -> 'a
  val get_context_metadata : unit -> CacheData.Metadata.t option
  val set_context_metadata : CacheData.Metadata.t option -> unit
  val select_metadata : CacheData.t -> CacheData.Metadata.t option
  val insert_metadata : CacheData.t -> CacheData.Metadata.t -> unit
  val compute_cache_size : CacheData.t -> int64
  val metadata_is_valid : int -> CacheData.Metadata.t -> bool
  val now : unit -> float
  val run_request : 'a SessionM.m -> 'a
  val with_default_retry : 'a SessionM.m -> 'a SessionM.m
  val request_account_metadata : unit -> account_metadata SessionM.m
  val request_new_start_page_token : unit -> string SessionM.m
  val probe_remaining_changes : start_page_token:string -> string SessionM.m

  val list_changes :
    start_page_token:string -> (Change.t list * string) SessionM.m

  val update_all_timestamps : CacheData.t -> float -> unit
  val invalidate_all_resources : CacheData.t -> unit
  val invalidate_resources : CacheData.t -> int64 list -> unit
  val invalidate_trash_bin : CacheData.t -> unit
  val invalidate_path : CacheData.t -> string -> unit

  val select_resources_with_remote_id :
    CacheData.t -> string -> CacheData.Resource.t list

  val trash_resources : CacheData.t -> CacheData.Resource.t list -> unit

  val delete_cached_resources :
    CacheData.Metadata.t -> CacheData.t -> CacheData.Resource.t list -> unit

  val build_resource_tables :
    string ->
    bool ->
    (string, int) Hashtbl.t * (string, CacheData.Resource.t) Hashtbl.t

  val get_unique_filename_from_file :
    File.t -> (string, int) Hashtbl.t -> string

  val create_resource : string -> CacheData.Resource.t

  val insert_resource_from_file :
    CacheData.t -> CacheData.Resource.t -> File.t -> CacheData.Resource.t

  val update_resource_from_file :
    CacheData.Resource.t -> File.t -> CacheData.Resource.t

  val update_cached_resource : CacheData.t -> CacheData.Resource.t -> unit
  val lost_and_found_directory : string
  val shared_with_me_directory : string
end

module Make (P : PORTS) = struct
  let get_start_page_token start_page_token_db =
    if start_page_token_db = "" then P.request_new_start_page_token ()
    else SessionM.return start_page_token_db

  let request_metadata start_page_token_db cache_size =
    P.request_account_metadata () >>= fun account_metadata ->
    get_start_page_token start_page_token_db >>= fun start_page_token ->
    SessionM.return
      {
        CacheData.Metadata.display_name = account_metadata.display_name;
        storage_quota_limit = account_metadata.storage_quota_limit;
        storage_quota_usage = account_metadata.storage_quota_usage;
        start_page_token;
        cache_size;
        last_update = P.now ();
        clean_shutdown = false;
      }

  let get_resources_and_files_to_update cache change =
    P.select_resources_with_remote_id cache change.Change.fileId
    |> List.filter (fun resource ->
        change.Change.file.File.version > 0L
        && change.Change.file.File.version
           > Option.default 0L resource.CacheData.Resource.version)
    |> List.map (fun resource -> Some (resource, change.Change.file))

  let get_resource_from_change cache change =
    P.select_resources_with_remote_id cache change.Change.fileId
    |> List.map (fun resource -> Some resource)

  let get_new_resource_from_change cache change =
    match P.select_resources_with_remote_id cache change.Change.fileId with
    | [] -> (
        let parent_resources =
          let parent_remote_ids =
            match change.Change.file.File.parents with [] -> [] | ids -> ids
          in
          List.map (P.select_resources_with_remote_id cache) parent_remote_ids
          |> List.concat
          |> List.filter (fun resource ->
              resource.CacheData.Resource.state
              = CacheData.Resource.State.Synchronized)
        in
        match parent_resources with
        | [] -> []
        | parent_resource :: _ ->
            let parent_path = parent_resource.CacheData.Resource.path in
            let filename_table, _ = P.build_resource_tables parent_path false in
            let filename =
              P.get_unique_filename_from_file change.Change.file filename_table
            in
            let resource_path = Filename.concat parent_path filename in
            let resource = P.create_resource resource_path in
            [ Some (resource, change.Change.file) ])
    | _ -> []

  let request_remaining_changes start_page_token =
    if start_page_token = "" then SessionM.return (false, true)
    else
      P.probe_remaining_changes ~start_page_token
      >>= fun new_start_page_token ->
      SessionM.return
        (new_start_page_token = start_page_token, new_start_page_token = "")

  let update_resource_cache_from_changes cache changes filter_changes map_change
      update_cache =
    let filtered_changes = List.filter filter_changes changes in
    let xs =
      List.fold_left
        (fun xs change ->
          let mapped_changes = map_change change in
          List.fold_left
            (fun xs' mapped_change ->
              match mapped_change with
              | None -> xs'
              | Some x -> if not (List.mem x xs') then x :: xs' else xs')
            xs mapped_changes)
        [] filtered_changes
    in
    update_cache cache xs

  let update_resource_cache runtime new_metadata old_metadata =
    request_remaining_changes new_metadata.CacheData.Metadata.start_page_token
    >>= fun (no_changes, over_limit) ->
    if no_changes then (
      Utils.log_with_header
        "END: Getting metadata: No need to update resource cache\n%!";
      Utils.log_with_header "BEGIN: Updating timestamps\n%!";
      P.update_all_timestamps runtime.cache
        new_metadata.CacheData.Metadata.last_update;
      Utils.log_with_header "END: Updating timestamps\n%!";
      SessionM.return new_metadata)
    else if over_limit then (
      Utils.log_with_header "END: Getting metadata: Too many changes\n";
      Utils.log_with_header "BEGIN: Getting new start page token\n%!";
      get_start_page_token "" >>= fun new_start_page_token ->
      Utils.log_with_header "END: Getting new start page token (%s)\n%!"
        new_start_page_token;
      Utils.log_with_header "BEGIN: Invalidating resources\n%!";
      P.invalidate_all_resources runtime.cache;
      Utils.log_with_header "END: Invalidating resources\n%!";
      SessionM.return
        {
          new_metadata with
          CacheData.Metadata.start_page_token = new_start_page_token;
        })
    else (
      Utils.log_with_header "BEGIN: Updating timestamps\n%!";
      P.update_all_timestamps runtime.cache
        new_metadata.CacheData.Metadata.last_update;
      Utils.log_with_header "END: Updating timestamps\n%!";
      match old_metadata with
      | None -> SessionM.return new_metadata
      | Some _ ->
          Utils.log_with_header "BEGIN: Getting changes from server\n%!";
          P.list_changes
            ~start_page_token:new_metadata.CacheData.Metadata.start_page_token
          >>= fun (changes, new_start_page_token) ->
          Utils.log_with_header "END: Getting changes from server\n%!";
          Utils.log_with_header "BEGIN: Adding new resources to cache\n%!";
          update_resource_cache_from_changes runtime.cache changes
            (fun change ->
              (not change.Change.removed) && not change.Change.file.File.trashed)
            (get_new_resource_from_change runtime.cache)
            (fun cache resources_and_files ->
              List.iter
                (fun (resource, file) ->
                  P.insert_resource_from_file cache resource file |> ignore)
                resources_and_files);
          Utils.log_with_header "END: Adding new resources to cache\n";
          Utils.log_with_header "BEGIN: Updating resource cache\n%!";
          update_resource_cache_from_changes runtime.cache changes
            (fun change ->
              (not change.Change.removed) && not change.Change.file.File.trashed)
            (get_resources_and_files_to_update runtime.cache)
            (fun cache resources_and_files ->
              List.iter
                (fun (resource, file) ->
                  Utils.log_with_header
                    "BEGIN: Refreshing resource (id=%Ld)\n%!"
                    resource.CacheData.Resource.id;
                  let updated_resource =
                    P.update_resource_from_file resource file
                  in
                  P.update_cached_resource cache updated_resource;
                  Utils.log_with_header "END: Refreshing resource (id=%Ld)\n%!"
                    updated_resource.CacheData.Resource.id)
                resources_and_files;
              let ids =
                List.map
                  (fun (resource, _) -> resource.CacheData.Resource.id)
                  resources_and_files
              in
              Utils.log_with_header "Invalidating resources: ids=%s\n%!"
                (String.concat ", " (List.map Int64.to_string ids));
              P.invalidate_resources cache ids);
          Utils.log_with_header "END: Updating resource cache\n";
          Utils.log_with_header "BEGIN: Updating trashed resources\n%!";
          update_resource_cache_from_changes runtime.cache changes
            (fun change -> change.Change.file.File.trashed)
            (get_resource_from_change runtime.cache)
            (fun cache resources ->
              Utils.log_with_header "Trashing resources: ids=%s\n%!"
                (String.concat ", "
                   (List.map
                      (fun resource ->
                        Int64.to_string resource.CacheData.Resource.id)
                      resources));
              P.trash_resources cache resources);
          Utils.log_with_header "END: Updating trashed resources\n";
          Utils.log_with_header "BEGIN: Removing deleted resources\n%!";
          update_resource_cache_from_changes runtime.cache changes
            (fun change -> change.Change.removed)
            (get_resource_from_change runtime.cache)
            (fun cache resources ->
              Utils.log_with_header "Deleting resources: ids=%s\n%!"
                (String.concat ", "
                   (List.map
                      (fun resource ->
                        Int64.to_string resource.CacheData.Resource.id)
                      resources));
              P.delete_cached_resources new_metadata cache resources);
          Utils.log_with_header "END: Removing deleted resources\n%!";
          if List.length changes > 0 then (
            if not runtime.config.Config.disable_trash then (
              Utils.log_with_header "BEGIN: Invalidating trash bin resource\n%!";
              P.invalidate_trash_bin runtime.cache;
              Utils.log_with_header "END: Invalidating trash bin resource\n%!");
            if runtime.config.Config.lost_and_found then (
              Utils.log_with_header
                "BEGIN: Invalidating lost+found resource\n%!";
              P.invalidate_path runtime.cache P.lost_and_found_directory;
              Utils.log_with_header "END: Invalidating lost+found resource\n%!");
            Utils.log_with_header "BEGIN: Invalidating .shared resource\n%!";
            P.invalidate_path runtime.cache P.shared_with_me_directory;
            Utils.log_with_header "END: Invalidating .shared resource\n%!");
          SessionM.return
            {
              new_metadata with
              CacheData.Metadata.start_page_token = new_start_page_token;
            })

  let refresh_metadata runtime old_metadata =
    let start_page_token =
      match old_metadata with
      | None -> ""
      | Some metadata -> metadata.CacheData.Metadata.start_page_token
    in
    let cache_size =
      match old_metadata with
      | None -> 0L
      | Some metadata -> metadata.CacheData.Metadata.cache_size
    in
    Utils.log_with_header "BEGIN: Refreshing metadata\n%!";
    P.with_default_retry (request_metadata start_page_token cache_size)
    >>= fun server_metadata ->
    Utils.log_with_header "END: Refreshing metadata\n";
    update_resource_cache runtime server_metadata old_metadata
    >>= fun updated_metadata ->
    Utils.log_with_header "BEGIN: Updating metadata in db\n%!";
    P.insert_metadata runtime.cache updated_metadata;
    Utils.log_with_header "END: Updating metadata in db\n";
    Utils.log_with_header "BEGIN: Updating context\n%!";
    P.set_context_metadata (Some updated_metadata);
    Utils.log_with_header "END: Updating context\n%!";
    SessionM.return updated_metadata

  let resync_cache_size cache db_metadata =
    let old_cache_size = db_metadata.CacheData.Metadata.cache_size in
    Utils.log_with_header "BEGIN: Recalculating cache size (old value=%Ld)\n%!"
      old_cache_size;
    let cache_size = P.compute_cache_size cache in
    Utils.log_with_header "END: Recalculating cache size (new value=%Ld)\n%!"
      cache_size;
    { db_metadata with CacheData.Metadata.cache_size }

  let load_metadata runtime =
    match P.get_context_metadata () with
    | None ->
        Utils.log_with_header "BEGIN: Loading metadata from db\n%!";
        let db_metadata = P.select_metadata runtime.cache in
        let db_metadata =
          Option.map (resync_cache_size runtime.cache) db_metadata
        in
        P.set_context_metadata db_metadata;
        db_metadata
    | Some metadata ->
        Utils.log_with_header "BEGIN: Getting metadata from context\n%!";
        Some metadata

  let get_metadata runtime =
    P.with_metadata_lock (fun () ->
        match load_metadata runtime with
        | None ->
            Utils.log_with_header "END: Getting metadata: Not found\n%!";
            P.run_request (refresh_metadata runtime None)
        | Some metadata ->
            if
              P.metadata_is_valid runtime.config.Config.metadata_cache_time
                metadata
            then (
              Utils.log_with_header "END: Getting metadata: Valid\n%!";
              metadata)
            else (
              Utils.log_with_header "END: Getting metadata: Not valid\n%!";
              P.run_request (refresh_metadata runtime (Some metadata))))
end
