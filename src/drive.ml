open GapiUtils.Infix
open GapiLens.Infix
open GapiMonad
open GapiMonad.SessionM.Infix
open GapiDriveV3Model
open GapiDriveV3Service

exception Directory_not_empty = DriveMutations.Directory_not_empty
exception Existing_attribute = DriveMutations.Existing_attribute
exception File_not_found = DriveMutations.File_not_found
exception IO_error = DriveMutations.IO_error
exception Invalid_operation = DriveMutations.Invalid_operation
exception No_attribute = DriveMutations.No_attribute
exception Permission_denied = DriveMutations.Permission_denied

let folder_mime_type = DriveResourceMapping.folder_mime_type
let shortcut_mime_type = DriveResourceMapping.shortcut_mime_type

let file_fields =
  "appProperties,capabilities(canEdit),createdTime,explicitlyTrashed,fileExtension,fullFileExtension,id,md5Checksum,mimeType,modifiedTime,name,parents,size,trashed,version,viewedByMeTime,webViewLink,exportLinks,shortcutDetails(targetId,targetResourceKey),shared,resourceKey"

let file_std_params =
  {
    GapiService.StandardParameters.default with
    GapiService.StandardParameters.fields = file_fields;
  }

let file_list_std_params =
  {
    GapiService.StandardParameters.default with
    GapiService.StandardParameters.fields =
      "files(" ^ file_fields ^ "),nextPageToken";
  }

let file_download_std_params =
  {
    GapiService.StandardParameters.default with
    GapiService.StandardParameters.alt = "media";
  }

let changes_std_params =
  {
    GapiService.StandardParameters.default with
    GapiService.StandardParameters.fields =
      "changes(removed,file(" ^ file_fields
      ^ "),fileId),nextPageToken,newStartPageToken";
  }

let device_scope = DriveRootResolution.device_scope
let device_root_folder = DriveRootResolution.device_root_folder
let do_request = Oauth2.do_request

let async_do_request f =
  let thread = Thread.create (fun go -> do_request go) f in
  let thread_id = Thread.id thread in
  Utils.log_with_header "Spawning new thread id=%d\n%!" thread_id;
  thread

let root_directory = DrivePathNamespace.root_directory
let default_root_folder_id = DriveRootResolution.default_root_folder_id
let trash_directory = DrivePathNamespace.trash_directory
let trash_directory_name_length = DrivePathNamespace.trash_directory_name_length
let trash_directory_base_path = DrivePathNamespace.trash_directory_base_path
let lost_and_found_directory = DrivePathNamespace.lost_and_found_directory
let shared_with_me_directory = DrivePathNamespace.shared_with_me_directory
let change_limit = 50
let max_link_target_length = 127
let max_attribute_length = 126

(* Utilities *)
let clean_filename = DriveResourceMapping.clean_filename
let apostrophe_regexp = Str.regexp (Str.quote "'")
let escape_apostrophe name = Str.global_replace apostrophe_regexp "\\'" name

let json_length s =
  let length_with_quotes =
    `String s |> Yojson.Safe.to_string |> String.length
  in
  length_with_quotes - 2

let is_in_trash_directory = DrivePathNamespace.is_in_trash_directory
let is_lost_and_found_root = DrivePathNamespace.is_lost_and_found_root
let is_lost_and_found = DrivePathNamespace.is_lost_and_found
let is_shared_with_me_root = DrivePathNamespace.is_shared_with_me_root
let is_shared_with_me = DrivePathNamespace.is_shared_with_me
let get_path_in_cache = DrivePathNamespace.get_path_in_cache

let match_service_error reason = function
  | GapiService.ServiceError (_, e) -> (
      match e.GapiError.RequestError.errors with
      | [] -> false
      | e :: _ -> e.GapiError.SingleError.reason = reason)
  | _ -> false

let handle_default_exceptions = function
  | GapiService.ServiceError (_, e) -> (
      let message =
        e |> GapiError.RequestError.to_data_model |> GapiJson.data_model_to_json
        |> Yojson.Safe.to_string
      in
      Utils.log_with_header "Service error: %s.\n%!" message;
      match e.GapiError.RequestError.errors with
      | [] -> Utils.raise_m IO_error
      | e :: _ -> (
          match e.GapiError.SingleError.reason with
          | "userRateLimitExceeded" | "rateLimitExceeded" | "backendError"
          | "downloadQuotaExceeded" ->
              Utils.raise_m Utils.Temporary_error
          | "insufficientFilePermissions" | "insufficientPermissions" ->
              Utils.raise_m Permission_denied
          | _ -> Utils.raise_m IO_error))
  | GapiRequest.PermissionDenied _ ->
      Utils.log_with_header "Server error: Permission denied.\n%!";
      Utils.raise_m Permission_denied
  | GapiRequest.RequestTimeout _ ->
      Utils.log_with_header "Server error: Request Timeout.\n%!";
      Utils.raise_m Utils.Temporary_error
  | GapiRequest.PreconditionFailed _ | GapiRequest.Conflict _ ->
      Utils.log_with_header "Server error: Conflict.\n%!";
      Utils.raise_m Utils.Temporary_error
  | GapiRequest.Forbidden _ ->
      Utils.log_with_header "Server error: Forbidden.\n%!";
      Utils.raise_m IO_error
  | GapiRequest.Gone _ ->
      Utils.log_with_header "Server error: Gone.\n%!";
      Utils.raise_m IO_error
  | GapiRequest.BadRequest _ ->
      Utils.log_with_header "Server error: bad request.\n%!";
      Utils.raise_m Utils.Temporary_error
  | Buffering.Invalid_block -> Utils.raise_m Invalid_operation
  | GapiRequest.NotFound _ ->
      Utils.log_with_header "Server error: not found.\n%!";
      Utils.raise_m File_not_found
  | e -> Utils.raise_m e

let build_resource_keys_header = DriveResourceKeys.build_resource_keys_header

(* with_try with a default exception handler *)
let try_with_default f s = Utils.try_with_m f handle_default_exceptions s

(* retry with a default exception handler *)
let with_retry_default f =
  let rec loop n =
    Utils.try_with_m f (fun e ->
        try handle_default_exceptions e with
        | Utils.Temporary_error ->
            if n >= !Utils.max_retries then Utils.raise_m IO_error
            else (
              GapiUtils.wait_exponential_backoff n;
              let n' = n + 1 in
              Utils.log_with_header "Retrying (%d/%d).\n%!" n'
                !Utils.max_retries;
              loop n')
        | _ -> Utils.raise_m e)
  in
  loop 0

let get_file_extension = DriveResourceMapping.get_file_extension

(* Resource cache *)
let get_filename name is_document get_document_format =
  let context = Context.get_ctx () in
  let config = context |. Context.config_lens in
  DriveResourceMapping.get_filename config name is_document get_document_format

let get_file_extension_from_format =
  DriveResourceMapping.get_file_extension_from_format

let get_file_extension_from_mime_type =
  DriveResourceMapping.get_file_extension_from_mime_type

let build_resource_tables parent_path trashed =
  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  let config = context |. Context.config_lens in
  let resources =
    Cache.Resource.select_resources_with_parent_path cache parent_path trashed
  in
  DriveResourceMapping.build_resource_tables config resources

let clean_document_extension file_name resource config =
  DriveResourceMapping.clean_document_extension config file_name resource

let create_resource path =
  DriveResourceMapping.create_resource ~now:Unix.gettimeofday path

let get_unique_filename name full_file_extension remote_id is_document
    get_document_format filename_table =
  let context = Context.get_ctx () in
  let config = context |. Context.config_lens in
  DriveResourceMapping.get_unique_filename config name full_file_extension
    remote_id is_document get_document_format filename_table

let get_unique_filename_from_resource resource name filename_table =
  let context = Context.get_ctx () in
  let config = context |. Context.config_lens in
  DriveResourceMapping.get_unique_filename_from_resource config resource name
    filename_table

let get_unique_filename_from_file file filename_table =
  let context = Context.get_ctx () in
  let config = context |. Context.config_lens in
  DriveResourceMapping.get_unique_filename_from_file config file filename_table

let recompute_path resource name =
  (* TODO: make an optimized version of build_resource_tables that
   * doesn't create resource table (useful for large directories). *)
  let context = Context.get_ctx () in
  let config = context |. Context.config_lens in
  let filename_table, _ =
    build_resource_tables resource.CacheData.Resource.parent_path
      (Option.default false resource.CacheData.Resource.trashed)
  in
  DriveResourceMapping.recompute_path config resource name filename_table

let update_resource_from_file ?state ?link_target resource file =
  DriveResourceMapping.update_resource_from_file ~now:Unix.gettimeofday
    ~recompute_path ?state ?link_target resource file

let insert_resource_into_cache ?state ?link_target cache resource file =
  let resource = update_resource_from_file ?state ?link_target resource file in
  Utils.log_with_header "BEGIN: Saving resource to db (remote id=%s)\n%!"
    file.File.id;
  let inserted = Cache.Resource.insert_resource cache resource in
  Utils.log_with_header
    "END: Saving resource to db (remote id=%s, id=%Ld, state=%s)\n%!"
    file.File.id inserted.CacheData.Resource.id
    (CacheData.Resource.State.to_string inserted.CacheData.Resource.state);
  inserted

let update_cached_resource cache resource =
  Utils.log_with_header "BEGIN: Updating resource in db (id=%Ld, state=%s)\n%!"
    resource.CacheData.Resource.id
    (CacheData.Resource.State.to_string resource.CacheData.Resource.state);
  Cache.Resource.update_resource cache resource;
  Utils.log_with_header "END: Updating resource in db (id=%Ld)\n%!"
    resource.CacheData.Resource.id

let update_cached_resource_state cache state id =
  Utils.log_with_header
    "BEGIN: Updating resource state in db (id=%Ld, state=%s)\n%!" id
    (CacheData.Resource.State.to_string state);
  Cache.Resource.update_resource_state cache state id;
  Utils.log_with_header "END: Updating resource state in db (id=%Ld)\n%!" id

let update_cached_resource_state_and_size cache state size id =
  Utils.log_with_header
    "BEGIN: Updating resource state and size in db (id=%Ld, state=%s, size=%Ld)\n\
     %!"
    id
    (CacheData.Resource.State.to_string state)
    size;
  Cache.Resource.update_resource_state_and_size cache state size id;
  Utils.log_with_header
    "END: Updating resource state and size in db (id=%Ld)\n%!" id

let lookup_resource path trashed =
  Utils.log_with_header "BEGIN: Loading resource %s (trashed=%b) from db\n%!"
    path trashed;
  let cache = Context.get_cache () in
  let resource = Cache.Resource.select_resource_with_path cache path trashed in
  (if Option.is_none resource then
     Utils.log_with_header
       "END: Loading resource %s (trashed=%b) from db: Not found\n%!" path
       trashed
   else
     let id = resource |. GapiLens.option_get |. CacheData.Resource.id in
     let state =
       resource |. GapiLens.option_get |. CacheData.Resource.state
       |> CacheData.Resource.State.to_string
     in
     Utils.log_with_header
       "END: Loading resource %s (trashed=%b) from db: Found (id=%Ld, state=%s)\n\
        %!"
       path trashed id state);
  resource

module DriveCacheMaintenancePorts = struct
  let with_metadata_lock f =
    let context = Context.get_ctx () in
    Utils.with_lock context.Context.metadata_lock f

  let update_cache_size_in_db = Cache.Metadata.update_cache_size

  let update_context_metadata f =
    Context.update_ctx (fun context ->
        let metadata = f (context.Context.metadata |. GapiLens.option_get) in
        context |> Context.metadata ^= Some metadata)

  let select_resources_order_by_last_update =
    Cache.Resource.select_resources_order_by_last_update

  let update_cached_resource_state = update_cached_resource_state
  let delete_files_from_cache = Cache.delete_files_from_cache
  let delete_resource = Cache.Resource.delete_resource
  let delete_resources = Cache.Resource.delete_resources

  let remove_memory_buffers remote_id =
    let context = Context.get_ctx () in
    Buffering.MemoryBuffers.remove_buffers remote_id
      context.Context.memory_buffers

  let remove_file_lock remote_id =
    Context.with_ctx_lock (fun () ->
        let context = Context.get_ctx () in
        Hashtbl.remove context.Context.file_locks remote_id)

  let file_exists = Sys.file_exists
  let stat_file = Unix.LargeFile.stat
  let log_exception = Utils.log_exception
end

module CacheMaintenanceOps =
  DriveCacheMaintenance.Make (DriveCacheMaintenancePorts)

let drive_cache_maintenance_runtime ?cache () =
  let context = Context.get_ctx () in
  let cache = Option.default context.Context.cache cache in
  {
    DriveCacheMaintenance.cache;
    config = context |. Context.config_lens;
    metadata = context.Context.metadata;
  }

let update_cache_size delta metadata cache =
  CacheMaintenanceOps.update_cache_size delta metadata cache

let shrink_cache ?file_size () =
  CacheMaintenanceOps.shrink_cache
    (drive_cache_maintenance_runtime ())
    ?file_size ()

let delete_cached_resource resource =
  CacheMaintenanceOps.delete_cached_resource
    (drive_cache_maintenance_runtime ())
    resource

let delete_cached_resources metadata cache resources =
  CacheMaintenanceOps.delete_cached_resources
    (drive_cache_maintenance_runtime ~cache ())
    metadata resources

let update_cache_size_for_documents cache resource content_path op =
  CacheMaintenanceOps.update_cache_size_for_documents
    (drive_cache_maintenance_runtime ~cache ())
    resource content_path op

let build_resource_keys_header_from_resource =
  DriveResourceKeys.build_resource_keys_header_from_resource

let build_resource_keys_header_from_resources =
  DriveResourceKeys.build_resource_keys_header_from_resources

(* END Resource cache *)

(* Metadata *)
let get_file_from_server parent_folder_id name trashed =
  let config = Context.get_ctx () |. Context.config_lens in
  Utils.log_with_header "BEGIN: Getting resource %s (%s) from server\n%!" name
    (if parent_folder_id = "" then "shared with me"
     else "in folder" ^ parent_folder_id);
  let q =
    if parent_folder_id <> "" then
      Printf.sprintf "name='%s' and '%s' in parents and trashed=%b"
        (escape_apostrophe name) parent_folder_id trashed
    else
      Printf.sprintf "name='%s' and sharedWithMe = true"
        (escape_apostrophe name)
  in
  with_retry_default
    (FilesResource.list ~supportsAllDrives:true
       ~driveId:config.Config.team_drive_id
       ~includeItemsFromAllDrives:(config.Config.team_drive_id <> "")
       ~corpora:(if config.Config.team_drive_id <> "" then "drive" else "user")
       ~std_params:file_list_std_params ~q ~pageSize:1)
  >>= fun file_list ->
  Utils.log_with_header
    "END: Getting resource %s (in folder %s) from server\n%!" name
    parent_folder_id;
  let files = file_list.FileList.files in
  if List.length files = 0 then SessionM.return None
  else
    let file = files |. GapiLens.head in
    SessionM.return (Some file)

module DriveRootResolutionPorts = struct
  let folder_mime_type = folder_mime_type
  let create_resource = create_resource

  let find_file_in_folder ~parent_folder_id ~name ~trashed =
    get_file_from_server parent_folder_id name trashed

  let get_file_by_remote_id remote_id =
    with_retry_default
      (FilesResource.get ~supportsAllDrives:true ~std_params:file_std_params
         ~fileId:remote_id)

  let create_folder ~name =
    let file = { File.empty with File.name; mimeType = folder_mime_type } in
    with_retry_default
      (FilesResource.create ~enforceSingleParent:true ~supportsAllDrives:true
         ~std_params:file_std_params file)

  let run_request request = do_request request |> fst

  let set_context_root_folder_id root_folder_id =
    Context.update_ctx (Context.root_folder_id ^= Some root_folder_id)

  let lookup_resource cache path trashed =
    ignore cache;
    lookup_resource path trashed

  let insert_resource cache ~label resource =
    Utils.log_with_header "BEGIN: Saving %s resource to db\n%!" label;
    let inserted = Cache.Resource.insert_resource cache resource in
    Utils.log_with_header "END: Saving %s resource to db (id=%Ld)\n%!" label
      inserted.CacheData.Resource.id;
    inserted
end

module RootResolutionOps = DriveRootResolution.Make (DriveRootResolutionPorts)

let drive_root_resolution_runtime () =
  let context = Context.get_ctx () in
  {
    DriveRootResolution.cache = context.Context.cache;
    config = context |. Context.config_lens;
    root_folder_id = context.Context.root_folder_id;
  }

let create_root_resource root_folder_id trashed =
  RootResolutionOps.create_root_resource root_folder_id trashed

let create_well_known_resource path =
  RootResolutionOps.create_well_known_resource path

let get_root_folder_id_from_server config =
  RootResolutionOps.get_root_folder_id_from_server config

let get_root_folder_id config = RootResolutionOps.get_root_folder_id config

let get_root_folder_id_from_context () =
  RootResolutionOps.get_root_folder_id_from_context
    (drive_root_resolution_runtime ())

let get_well_known_resource path trashed =
  RootResolutionOps.get_well_known_resource
    (drive_root_resolution_runtime ())
    path trashed

module DriveMetadataRefreshPorts = struct
  let with_metadata_lock f =
    let context = Context.get_ctx () in
    Utils.with_lock context.Context.metadata_lock f

  let get_context_metadata () = (Context.get_ctx ()).Context.metadata

  let set_context_metadata metadata =
    Context.update_ctx (Context.metadata ^= metadata)

  let select_metadata = Cache.Metadata.select_metadata
  let insert_metadata = Cache.Metadata.insert_metadata
  let compute_cache_size = Cache.compute_cache_size
  let metadata_is_valid = CacheData.Metadata.is_valid
  let now = Unix.gettimeofday
  let run_request request = do_request request |> fst
  let with_default_retry = with_retry_default

  let request_account_metadata () =
    let std_params =
      {
        GapiService.StandardParameters.default with
        GapiService.StandardParameters.fields =
          "user(displayName),storageQuota(limit,usage)";
      }
    in
    with_retry_default (AboutResource.get ~std_params) >>= fun about ->
    SessionM.return
      {
        DriveMetadataRefresh.display_name = about.About.user.User.displayName;
        storage_quota_limit = about.About.storageQuota.About.StorageQuota.limit;
        storage_quota_usage = about.About.storageQuota.About.StorageQuota.usage;
      }

  let request_new_start_page_token () =
    let config = Context.get_ctx () |. Context.config_lens in
    let std_params =
      {
        GapiService.StandardParameters.default with
        GapiService.StandardParameters.fields = "startPageToken";
      }
    in
    with_retry_default
      (ChangesResource.getStartPageToken ~supportsAllDrives:true
         ~driveId:config.Config.team_drive_id ~std_params)
    >>= fun startPageToken ->
    SessionM.return startPageToken.StartPageToken.startPageToken

  let probe_remaining_changes ~start_page_token =
    if start_page_token = "" then SessionM.return ""
    else
      let config = Context.get_ctx () |. Context.config_lens in
      let std_params =
        {
          GapiService.StandardParameters.default with
          GapiService.StandardParameters.fields = "newStartPageToken";
        }
      in
      with_retry_default
        (ChangesResource.list ~supportsAllDrives:true
           ~driveId:config.Config.team_drive_id
           ~includeItemsFromAllDrives:(config.Config.team_drive_id <> "")
           ~std_params ~includeRemoved:true ~pageSize:change_limit
           ~pageToken:start_page_token)
      >>= fun change_list ->
      SessionM.return change_list.ChangeList.newStartPageToken

  let list_changes ~start_page_token =
    let config = Context.get_ctx () |. Context.config_lens in
    let rec loop pageToken accu =
      with_retry_default
        (ChangesResource.list ~supportsAllDrives:true
           ~driveId:config.Config.team_drive_id
           ~includeItemsFromAllDrives:(config.Config.team_drive_id <> "")
           ~std_params:changes_std_params ~includeRemoved:true ~pageToken)
      >>= fun change_list ->
      let changes = change_list.ChangeList.changes @ accu in
      if change_list.ChangeList.nextPageToken = "" then
        SessionM.return (changes, change_list.ChangeList.newStartPageToken)
      else loop change_list.ChangeList.nextPageToken changes
    in
    loop start_page_token []

  let update_all_timestamps = Cache.Resource.update_all_timestamps
  let invalidate_all_resources = Cache.Resource.invalidate_all
  let invalidate_resources = Cache.Resource.invalidate_resources
  let invalidate_trash_bin = Cache.Resource.invalidate_trash_bin
  let invalidate_path = Cache.Resource.invalidate_path

  let select_resources_with_remote_id =
    Cache.Resource.select_resources_with_remote_id

  let trash_resources = Cache.Resource.trash_resources
  let delete_cached_resources = delete_cached_resources
  let build_resource_tables = build_resource_tables
  let get_unique_filename_from_file = get_unique_filename_from_file
  let create_resource = create_resource

  let insert_resource_from_file cache resource file =
    insert_resource_into_cache cache resource file

  let update_resource_from_file resource file =
    update_resource_from_file resource file

  let update_cached_resource = update_cached_resource
  let lost_and_found_directory = lost_and_found_directory
  let shared_with_me_directory = shared_with_me_directory
end

module MetadataRefreshOps = DriveMetadataRefresh.Make (DriveMetadataRefreshPorts)

let drive_metadata_refresh_runtime () =
  let context = Context.get_ctx () in
  {
    DriveMetadataRefresh.cache = context.Context.cache;
    config = context |. Context.config_lens;
  }

let get_metadata () =
  MetadataRefreshOps.get_metadata (drive_metadata_refresh_runtime ())

let drive_filesystem_stats_runtime metadata =
  let context = Context.get_ctx () in
  { DriveFilesystemStats.metadata; config = context |. Context.config_lens }

let statfs () =
  let metadata = get_metadata () in
  DriveFilesystemStats.statfs (drive_filesystem_stats_runtime metadata)

(* END Metadata *)

(* Resources *)
module DriveResourceByIdPorts = struct
  let root_directory = root_directory
  let shared_with_me_directory = shared_with_me_directory
  let get_root_folder_id = get_root_folder_id_from_context
  let get_well_known_resource = get_well_known_resource

  let select_first_resource_with_remote_id cache remote_id =
    Cache.Resource.select_first_resource_with_remote_id cache remote_id

  let clean_filename = clean_filename
  let create_resource = create_resource

  let update_resource_from_file resource file =
    update_resource_from_file resource file

  let get_file_by_remote_id rid =
    Utils.log_with_header "BEGIN: Getting file from server (remote id=%s)\n%!"
      rid;
    with_retry_default
      (FilesResource.get ~supportsAllDrives:true ~std_params:file_std_params
         ~fileId:rid)
    >>= fun file ->
    Utils.log_with_header "END: Getting file from server (remote id=%s)\n%!" rid;
    SessionM.return file
end

module ResourceByIdOps = DriveResourceById.Make (DriveResourceByIdPorts)

let get_resource_with_id_from_server remote_id =
  ResourceByIdOps.get_resource_with_id_from_server remote_id

let get_resource_with_id remote_id cache =
  ResourceByIdOps.get_resource_with_id { DriveResourceById.cache } remote_id

module DriveResourceResolverPorts = struct
  let root_directory = root_directory
  let lost_and_found_directory = lost_and_found_directory
  let shared_with_me_directory = shared_with_me_directory
  let get_metadata = get_metadata

  let current_metadata_last_update () =
    Context.get_ctx () |. Context.metadata_last_update_lens

  let get_root_folder_id = get_root_folder_id_from_context
  let get_well_known_resource = get_well_known_resource
  let is_lost_and_found_root = is_lost_and_found_root
  let is_shared_with_me_root = is_shared_with_me_root
  let lookup_resource _cache path trashed = lookup_resource path trashed
  let create_resource = create_resource

  let insert_resource cache resource =
    Utils.log_with_header "BEGIN: Saving not found resource to db (name=%s)\n%!"
      (Filename.basename resource.CacheData.Resource.path);
    let inserted = Cache.Resource.insert_resource cache resource in
    Utils.log_with_header "END: Saving not found resource to db (name=%s)\n%!"
      (Filename.basename resource.CacheData.Resource.path);
    inserted

  let insert_resource_from_file cache resource file =
    insert_resource_into_cache cache resource file

  let update_resource_from_file resource file =
    update_resource_from_file resource file

  let update_cached_resource = update_cached_resource
  let delete_cached_resource = delete_cached_resource

  let select_first_resource_with_remote_id =
    Cache.Resource.select_first_resource_with_remote_id

  let find_file_in_folder ~parent_folder_id ~name ~trashed =
    get_file_from_server parent_folder_id name trashed

  let get_file_by_remote_id remote_id =
    Utils.log_with_header "BEGIN: Getting file from server (remote id=%s)\n%!"
      remote_id;
    with_retry_default
      (FilesResource.get ~supportsAllDrives:true ~std_params:file_std_params
         ~fileId:remote_id)
    >>= fun file ->
    Utils.log_with_header "END: Getting file from server (remote id=%s)\n%!"
      remote_id;
    SessionM.return file

  let with_default_retry = with_retry_default
end

module ResourceResolverOps =
  DriveResourceResolver.Make (DriveResourceResolverPorts)

let drive_resource_resolver_runtime ?cache () =
  let context = Context.get_ctx () in
  {
    DriveResourceResolver.cache = Option.default context.Context.cache cache;
    config = context |. Context.config_lens;
  }

let check_resource_in_cache cache path trashed =
  ResourceResolverOps.check_resource_in_cache
    (drive_resource_resolver_runtime ~cache ())
    path trashed

let get_folder_id path trashed =
  ResourceResolverOps.get_folder_id
    (drive_resource_resolver_runtime ())
    path trashed

let get_resource path trashed =
  ResourceResolverOps.get_resource
    (drive_resource_resolver_runtime ())
    path trashed

let check_md5_checksum resource cache =
  let path = resource.CacheData.Resource.path in
  let content_path = Cache.get_content_path cache resource in
  let md5_checksum =
    Option.default "" resource.CacheData.Resource.md5_checksum
  in
  if md5_checksum <> "" then (
    Utils.log_with_header
      "BEGIN: Checking MD5 checksum (path=%s, cache path=%s, hash=%s)\n%!" path
      content_path md5_checksum;
    if Sys.file_exists content_path then (
      let md5 = Cryptokit.Hash.md5 () in
      Utils.with_in_channel content_path (fun ch ->
          try
            while true do
              let byte = input_byte ch in
              md5#add_byte byte
            done
          with End_of_file -> ());
      let md5_result = md5#result in
      let hexa = Cryptokit.Hexa.encode () in
      hexa#put_string md5_result;
      hexa#finish;
      let checksum = hexa#get_string in
      Utils.log_with_header
        "END: Checking MD5 checksum (path=%s, cache path=%s, hash=%s): \
         Computed MD5 checksum: %s\n\
         %!"
        path content_path md5_checksum checksum;
      checksum = md5_checksum)
    else (
      Utils.log_with_header
        "END: Checking MD5 checksum (path=%s, cache path=%s, hash=%s): File \
         does not exists\n\
         %!"
        path content_path md5_checksum;
      false))
  else false

let with_retry f resource =
  let rec loop res n =
    Utils.try_with_m (f res) (function
      | Utils.Temporary_error ->
          if n >= !Utils.max_retries then Utils.raise_m IO_error
          else (
            GapiUtils.wait_exponential_backoff n;
            let fileId = res.CacheData.Resource.remote_id |> Option.get in
            with_retry_default
              (FilesResource.get ~supportsAllDrives:true
                 ~std_params:file_std_params ~fileId)
            >>= fun file ->
            let state, verb =
              if
                resource.CacheData.Resource.state
                = CacheData.Resource.State.ToUpload
              then (CacheData.Resource.State.ToUpload, "uploading")
              else (CacheData.Resource.State.ToDownload, "downloading")
            in
            let refreshed_resource =
              update_resource_from_file ~state res file
            in
            let context = Context.get_ctx () in
            let cache = context.Context.cache in
            update_cached_resource cache refreshed_resource;
            let n' = n + 1 in
            Utils.log_with_header "Retry (%d/%d) %s resource (id=%Ld).\n%!" n'
              !Utils.max_retries verb resource.CacheData.Resource.id;
            loop refreshed_resource n')
      | e -> Utils.raise_m e)
  in
  loop resource 0

let is_desktop_format resource config =
  DriveDownloads.is_desktop_format config resource

let create_desktop_entry resource content_path config =
  Utils.with_out_channel ~mode:[ Open_creat; Open_trunc; Open_wronly ]
    content_path (fun out_ch ->
      let icon_entry =
        let icon = CacheData.Resource.get_icon resource config in
        if icon = "" then "" else "Icon=" ^ icon ^ "\n"
      in
      let url = Option.default "" resource.CacheData.Resource.web_view_link in
      let exec = config.Config.desktop_entry_exec in
      let entry_type = if exec <> "" then "Type=Application" else "Type=Link" in
      let exec_or_url_entry =
        if exec <> "" then Printf.sprintf "Exec=%s \"%s\"" exec url
        else "URL=" ^ url
      in
      Printf.fprintf out_ch "[Desktop Entry]\n%s\nName=%s\n%s\n%s" entry_type
        (Option.default "" resource.CacheData.Resource.name)
        exec_or_url_entry icon_entry)

let create_html_with_redirect resource content_path config =
  Utils.with_out_channel ~mode:[ Open_creat; Open_trunc; Open_wronly ]
    content_path (fun out_ch ->
      let url = Option.default "" resource.CacheData.Resource.web_view_link in
      let name = Option.default "" resource.CacheData.Resource.name in
      Printf.fprintf out_ch
        "<!DOCTYPE html>\n\
         <html>\n\
         <head>\n\
         <title>%s</title>\n\
         <meta http-equiv=\"refresh\" content=\"0;URL='%s'\" />\n\
         </head>\n\
         <body>\n\
         <p>This page has moved to a <a href=\"%s\">%s</a>.</p>\n\
         </body>\n\
         </html>"
        name url url name)

let download_media media_download resource =
  let fileId = resource.CacheData.Resource.remote_id |> Option.get in
  let custom_headers = build_resource_keys_header_from_resource resource in
  Utils.try_with_m
    (FilesResource.get ~supportsAllDrives:true
       ~std_params:file_download_std_params ~media_download ~custom_headers
       ~fileId) (fun e ->
      let config = Context.get_ctx () |. Context.config_lens in
      if
        match_service_error "cannotDownloadAbusiveFile" e
        && config.Config.acknowledge_abuse
      then (
        Utils.log_with_header
          "Warning: abusive file detected, but downloading anyway (fileId=%s)\n\
           %!"
          fileId;
        with_retry_default
          (FilesResource.get ~supportsAllDrives:true ~acknowledgeAbuse:true
             ~std_params:file_download_std_params ~media_download
             ~custom_headers ~fileId)
        >>= fun file -> SessionM.return file)
      else handle_default_exceptions e)

let flush_memory_buffers resource =
  let context = Context.get_ctx () in
  let config = context |. Context.config_lens in
  if config.Config.write_buffers then
    let memory_buffers = context.Context.memory_buffers in
    Buffering.MemoryBuffers.flush_blocks
      (resource.CacheData.Resource.remote_id |> Option.get)
      memory_buffers

module DriveDownloadPorts = struct
  let get_content_path = Cache.get_content_path

  let select_first_resource_with_remote_id cache remote_id =
    Cache.Resource.select_first_resource_with_remote_id cache remote_id

  let file_exists = Sys.file_exists
  let check_md5_checksum = check_md5_checksum
  let update_cached_resource_state = update_cached_resource_state
  let update_cache_size_for_documents = update_cache_size_for_documents
  let shrink_cache ?file_size () = shrink_cache ?file_size ()

  let with_resource_lock resource request =
    let context = Context.get_ctx () in
    let mutex =
      Context.with_ctx_lock (fun () ->
          let remote_id = resource.CacheData.Resource.remote_id |> Option.get in
          match Utils.safe_find context.Context.file_locks remote_id with
          | None ->
              let mutex = Mutex.create () in
              Hashtbl.add context.Context.file_locks remote_id mutex;
              mutex
          | Some mutex -> mutex)
    in
    Utils.with_lock_m mutex request

  let create_desktop_entry = create_desktop_entry
  let create_html_with_redirect = create_html_with_redirect

  let download_export_link_to_file content_path export_link =
    let destination = GapiMediaResource.TargetFile content_path in
    let media_download = { GapiMediaResource.destination; range_spec = "" } in
    GapiService.get ~media_download export_link GapiRequest.parse_empty_response

  let export_document_to_file content_path ~file_id ~mime_type =
    let destination = GapiMediaResource.TargetFile content_path in
    let media_download = { GapiMediaResource.destination; range_spec = "" } in
    with_retry_default
      (FilesResource.export ~media_download ~fileId:file_id ~mimeType:mime_type)
    >>= fun () -> SessionM.return ()

  let download_media_to_file content_path resource =
    let destination = GapiMediaResource.TargetFile content_path in
    let media_download = { GapiMediaResource.destination; range_spec = "" } in
    download_media media_download resource >>= fun _ -> SessionM.return ()

  let create_empty_file content_path = close_out (open_out content_path)
  let wait_exponential_backoff = GapiUtils.wait_exponential_backoff
  let handle_download_exception e = handle_default_exceptions e
end

module DownloadOps = DriveDownloads.Make (DriveDownloadPorts)

let drive_download_runtime () =
  let context = Context.get_ctx () in
  {
    DriveDownloads.cache = context.Context.cache;
    config = context |. Context.config_lens;
  }

let download_resource resource =
  DownloadOps.download_resource (drive_download_runtime ()) resource

let stream_resource offset buffer resource =
  let length = Bigarray.Array1.dim buffer in
  let finish = Int64.add offset (Int64.of_int (length - 1)) in
  Utils.log_with_header
    "BEGIN: Stream resource (id=%Ld, offset=%Ld, finish=%Ld, length=%d)\n%!"
    resource.CacheData.Resource.id offset finish length;
  let destination = GapiMediaResource.ArrayBuffer buffer in
  let range_spec =
    GapiMediaResource.generate_range_spec [ (Some offset, Some finish) ]
  in
  let media_download = { GapiMediaResource.destination; range_spec } in
  download_media media_download resource >>= fun _ ->
  Utils.log_with_header
    "END: Stream resource (id=%Ld, offset=%Ld, finish=%Ld, length=%d)\n%!"
    resource.CacheData.Resource.id offset finish length;
  SessionM.return ()

let start_buffer_eviction_thread context memory_buffers =
  let config = context |. Context.config_lens in
  if config.Config.stream_large_files then
    if Option.is_none context.Context.buffer_eviction_thread then (
      let thread =
        Buffering.MemoryBuffers.create_eviction_thread memory_buffers
      in
      Utils.log_with_header "Starting buffer eviction thread (TID=%d)\n%!"
        (Thread.id thread);
      Context.update_ctx (Context.buffer_eviction_thread ^= Some thread))

let stream_resource_to_memory_buffer offset buffer resource =
  let context = Context.get_ctx () in
  let memory_buffers = context.Context.memory_buffers in
  start_buffer_eviction_thread context memory_buffers;
  let remote_id = resource.CacheData.Resource.remote_id |> Option.get in
  Buffering.MemoryBuffers.read_block remote_id offset
    (resource.CacheData.Resource.size |> Option.get)
    (fun start_pos block_buffer ->
      stream_resource start_pos block_buffer resource)
    ~dest_arr:buffer memory_buffers
  >>= fun () -> SessionM.return ()

let stream_resource_to_read_ahead_buffers offset resource =
  let context = Context.get_ctx () in
  let memory_buffers = context.Context.memory_buffers in
  start_buffer_eviction_thread context memory_buffers;
  let remote_id = resource.CacheData.Resource.remote_id |> Option.get in
  let config = context |. Context.config_lens in
  Buffering.MemoryBuffers.read_ahead config.Config.read_ahead_buffers remote_id
    offset
    (resource.CacheData.Resource.size |> Option.get)
    (fun start_pos block_buffer ->
      stream_resource start_pos block_buffer resource)
    memory_buffers
  >>= fun ms ->
  List.map (fun m -> with_retry (fun _ -> m) resource) ms |> SessionM.return

let is_filesystem_read_only () =
  Context.get_ctx () |. Context.config_lens |. Config.read_only

let is_file_read_only resource =
  let config = Context.get_ctx () |. Context.config_lens in
  DriveOpens.is_file_read_only config resource

module DriveViewPorts = struct
  let get_path_in_cache = get_path_in_cache
  let get_resource = get_resource
  let get_resource_with_id = get_resource_with_id
  let update_cached_resource = update_cached_resource

  let materialize_for_stat resource =
    flush_memory_buffers resource;
    with_retry download_resource resource

  let file_exists = Sys.file_exists
  let stat_file = Unix.LargeFile.stat
  let is_file_read_only = is_file_read_only
  let is_lost_and_found_root = is_lost_and_found_root
  let is_shared_with_me_root = is_shared_with_me_root
end

module ViewOps = DriveViews.Make (DriveViewPorts)

let drive_view_runtime () =
  let context = Context.get_ctx () in
  {
    DriveViews.cache = context.Context.cache;
    config = context |. Context.config_lens;
    mountpoint_path = context.Context.mountpoint_path;
    mountpoint_stats = context.Context.mountpoint_stats;
  }

module DriveDirectoryReadPorts = struct
  let get_path_in_cache = get_path_in_cache
  let get_resource = get_resource
  let get_folder_id = get_folder_id

  let is_lost_and_found_root path trashed config =
    is_lost_and_found_root path trashed config

  let is_shared_with_me_root path trashed config =
    is_shared_with_me_root path trashed config

  let check_resource_in_cache cache path trashed =
    check_resource_in_cache cache path trashed

  let select_resources_with_parent_path =
    Cache.Resource.select_resources_with_parent_path

  let list_files q =
    let config = Context.get_ctx () |. Context.config_lens in
    let rec loop ?pageToken accu =
      with_retry_default
        (FilesResource.list ~supportsAllDrives:true
           ~driveId:config.Config.team_drive_id
           ~includeItemsFromAllDrives:(config.Config.team_drive_id <> "")
           ~corpora:
             (if config.Config.team_drive_id <> "" then "drive" else "user")
           ~std_params:file_list_std_params ~q ?pageToken)
      >>= fun file_list ->
      let files = file_list.FileList.files @ accu in
      if file_list.FileList.nextPageToken = "" then SessionM.return files
      else loop ~pageToken:file_list.FileList.nextPageToken files
    in
    loop []

  let build_resource_tables = build_resource_tables

  let update_resource_from_file resource file =
    update_resource_from_file resource file

  let get_unique_filename_from_file = get_unique_filename_from_file
  let create_resource = create_resource
  let insert_resources = Cache.Resource.insert_resources
  let update_cached_resource = update_cached_resource
  let current_time = Unix.gettimeofday
end

module DirectoryReadOps = DriveDirectoryReads.Make (DriveDirectoryReadPorts)

let drive_directory_read_runtime () =
  let context = Context.get_ctx () in
  {
    DriveDirectoryReads.cache = context.Context.cache;
    config = context |. Context.config_lens;
  }

(* stat *)
let get_attr path =
  do_request (ViewOps.get_attr (drive_view_runtime ()) path) |> fst

(* END stat *)

(* readdir *)
let read_dir path =
  do_request (DirectoryReadOps.read_dir (drive_directory_read_runtime ()) path)
  |> fst

(* END readdir *)

(* fopen *)
module DriveOpenPorts = struct
  let get_path_in_cache = get_path_in_cache
  let get_resource = get_resource
end

module OpenOps = DriveOpens.Make (DriveOpenPorts)

let drive_open_runtime () =
  let context = Context.get_ctx () in
  { DriveOpens.config = context |. Context.config_lens }

let fopen path flags =
  do_request (OpenOps.fopen (drive_open_runtime ()) path flags) |> ignore;
  None

(* END fopen *)

(* opendir *)
let opendir path flags =
  do_request (ViewOps.opendir (drive_view_runtime ()) path) |> ignore;
  None

(* END opendir *)

(* Update operations *)
module DriveRemoteUpdatePorts = struct
  let get_path_in_cache = get_path_in_cache
  let get_resource = get_resource
  let get_content_path = Cache.get_content_path
  let file_exists = Sys.file_exists

  let update_resource_from_file resource file =
    update_resource_from_file resource file

  let update_cached_resource = update_cached_resource
end

module RemoteUpdateOps = DriveRemoteUpdates.Make (DriveRemoteUpdatePorts)

let drive_remote_update_runtime () =
  let context = Context.get_ctx () in
  {
    DriveRemoteUpdates.cache = context.Context.cache;
    config = context |. Context.config_lens;
  }

let update_remote_resource path ?update_file_in_cache ?save_to_db ?purge_cache
    do_remote_update =
  RemoteUpdateOps.update_remote_resource
    (drive_remote_update_runtime ())
    path ?update_file_in_cache ?save_to_db ?purge_cache do_remote_update

(* END Update operations *)

module DriveMetadataMutationPorts = struct
  let build_resource_keys_header_from_resource =
    build_resource_keys_header_from_resource

  let remote_update ~custom_headers ~fileId file_patch =
    with_retry_default
      (FilesResource.update ~enforceSingleParent:true ~supportsAllDrives:true
         ~std_params:file_std_params ~custom_headers ~fileId file_patch)

  let update_remote_resource _runtime path ?update_file_in_cache
      do_remote_update =
    update_remote_resource path ?update_file_in_cache do_remote_update

  let update_file_times = Unix.utimes
end

module MetadataMutationOps =
  DriveMetadataMutations.Make (DriveMetadataMutationPorts)

let drive_metadata_mutation_runtime () =
  let context = Context.get_ctx () in
  {
    DriveMetadataMutations.cache = context.Context.cache;
    config = context |. Context.config_lens;
  }

(* utime *)
let utime path atime mtime =
  do_request
    (MetadataMutationOps.utime
       (drive_metadata_mutation_runtime ())
       path atime mtime)
  |> ignore

(* END utime *)

module DriveReadPorts = struct
  let get_path_in_cache = get_path_in_cache
  let get_resource = get_resource

  let stream_resource offset buf resource =
    with_retry (stream_resource offset buf) resource

  let stream_resource_to_memory_buffer offset buf resource =
    with_retry (stream_resource_to_memory_buffer offset buf) resource

  let stream_resource_to_read_ahead_buffers offset resource =
    stream_resource_to_read_ahead_buffers offset resource

  let flush_memory_buffers = flush_memory_buffers
  let ensure_local_content resource = with_retry download_resource resource

  let read_local_file content_path buf offset =
    Utils.with_in_channel content_path (fun ch ->
        let file_descr = Unix.descr_of_in_channel ch in
        Unix.LargeFile.lseek file_descr offset Unix.SEEK_SET |> ignore;
        Fuse.Unix_util.read file_descr buf)

  let enqueue_async_request request = async_do_request request |> ignore
end

module ReadOps = DriveReads.Make (DriveReadPorts)

let drive_read_runtime () =
  let context = Context.get_ctx () in
  {
    DriveReads.cache = context.Context.cache;
    config = context |. Context.config_lens;
  }

(* read *)
let read path buf offset file_descr =
  do_request (ReadOps.read (drive_read_runtime ()) path buf offset) |> fst

(* END read *)

module DriveFileMutationPorts = struct
  let get_path_in_cache = get_path_in_cache
  let get_resource = get_resource
  let ensure_local_content resource = with_retry download_resource resource
  let flush_memory_buffers = flush_memory_buffers

  let write_to_memory_buffers resource content_path buf offset =
    let memory_buffers = (Context.get_ctx ()).Context.memory_buffers in
    Buffering.MemoryBuffers.write_to_block
      (resource.CacheData.Resource.remote_id |> Option.get)
      content_path buf offset memory_buffers

  let write_to_file content_path buf offset =
    Utils.with_out_channel content_path (fun ch ->
        let file_descr = Unix.descr_of_out_channel ch in
        Unix.LargeFile.lseek file_descr offset Unix.SEEK_SET |> ignore;
        Fuse.Unix_util.write file_descr buf)

  let truncate_local_file = Unix.LargeFile.truncate
  let file_exists = Sys.file_exists
  let update_cached_resource = update_cached_resource
  let update_cached_resource_state = update_cached_resource_state
  let shrink_cache ?file_size () = shrink_cache ?file_size ()
end

module FileMutationOps = DriveFileMutations.Make (DriveFileMutationPorts)

let drive_file_mutation_runtime () =
  let context = Context.get_ctx () in
  {
    DriveFileMutations.cache = context.Context.cache;
    config = context |. Context.config_lens;
  }

(* write *)
let write path buf offset file_descr =
  do_request
    (FileMutationOps.write (drive_file_mutation_runtime ()) path buf offset)
  |> fst

(* END write *)

module DriveUploadPorts = struct
  let get_content_path = Cache.get_content_path
  let create_file_resource = GapiMediaResource.create_file_resource
  let media_content_type media = media |. GapiMediaResource.content_type
  let media_content_length media = media.GapiMediaResource.content_length

  let update_cached_resource_state_and_size =
    update_cached_resource_state_and_size

  let build_resource_keys_header_from_resource =
    build_resource_keys_header_from_resource

  let now = GapiDate.now

  let remote_update ~media_source ~custom_headers ~fileId file_patch =
    with_retry_default
      (FilesResource.update ~enforceSingleParent:true ~supportsAllDrives:true
         ~std_params:file_std_params ?media_source ~custom_headers ~fileId
         file_patch)

  let update_resource_from_file ?state resource file =
    update_resource_from_file ?state resource file

  let select_first_resource_with_remote_id =
    Cache.Resource.select_first_resource_with_remote_id

  let update_cached_resource = update_cached_resource
  let shrink_cache () = shrink_cache ()
end

module UploadOps = DriveUploads.Make (DriveUploadPorts)

let drive_upload_runtime () =
  let context = Context.get_ctx () in
  {
    DriveUploads.cache = context.Context.cache;
    config = context |. Context.config_lens;
  }

let upload resource = UploadOps.upload (drive_upload_runtime ()) resource

let upload_resource_with_retry resource =
  flush_memory_buffers resource;
  with_retry (fun r -> try_with_default (upload r)) resource

let upload_resource_by_id resource_id =
  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  let resource = Cache.Resource.select_resource_with_id cache resource_id in
  match resource with
  | Some r -> do_request (upload_resource_with_retry r) |> ignore
  | None ->
      Utils.log_with_header
        "Cannot find queued resource to upload with resource_id=%Ld.\n%!"
        resource_id

let init_filesystem () =
  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  MemoryCache.start_flush_db_thread cache;
  let config = context |. Context.config_lens in
  if config.Config.async_upload_queue then
    UploadQueue.start_async_upload_thread cache
      config.Config.async_upload_threads upload_resource_by_id;
  if config.Config.background_folder_fetching then
    BackgroundFolderFetching.start_folder_fetching_thread cache (fun path ->
        read_dir path |> ignore)

module DriveUploadDispatchPorts = struct
  let get_path_in_cache = get_path_in_cache
  let lookup_resource _cache path trashed = lookup_resource path trashed
  let update_cached_resource_state = update_cached_resource_state
  let get_resource = get_resource
  let flush_memory_buffers = flush_memory_buffers
  let enqueue_async_upload = UploadQueue.queue_resource
  let upload_now_with_retry = upload_resource_with_retry
end

module UploadDispatchOps = DriveUploadDispatch.Make (DriveUploadDispatchPorts)

let drive_upload_dispatch_runtime () =
  let context = Context.get_ctx () in
  {
    DriveUploadDispatch.cache = context.Context.cache;
    config = context |. Context.config_lens;
  }

let upload_if_dirty path =
  match
    UploadDispatchOps.upload_if_dirty (drive_upload_dispatch_runtime ()) path
  with
  | None -> ()
  | Some upload_request -> do_request upload_request |> ignore

(* flush *)
let flush path file_descr = upload_if_dirty path

(* fsync *)
let fsync path ds file_descr = upload_if_dirty path

(* release *)
let release path flags hnd = upload_if_dirty path

module DriveMutationPorts = struct
  let max_link_target_length = max_link_target_length
  let json_length = json_length
  let is_lost_and_found = is_lost_and_found
  let is_lost_and_found_root = is_lost_and_found_root
  let get_path_in_cache = get_path_in_cache
  let is_filesystem_read_only = is_filesystem_read_only
  let create_resource = create_resource
  let clean_document_extension = clean_document_extension
  let recompute_path = recompute_path
  let update_resource_from_file = update_resource_from_file
  let get_resource = get_resource

  let build_resource_keys_header_from_resource =
    build_resource_keys_header_from_resource

  let build_resource_keys_header_from_resources =
    build_resource_keys_header_from_resources

  let insert_resource_into_cache = insert_resource_into_cache
  let update_cached_resource = update_cached_resource
  let delete_cached_resource = delete_cached_resource
  let delete_all_with_parent_path = Cache.Resource.delete_all_with_parent_path
  let trash_all_with_parent_path = Cache.Resource.trash_all_with_parent_path
  let invalidate_trash_bin = Cache.Resource.invalidate_trash_bin

  let delete_not_found_resource_with_path =
    Cache.Resource.delete_not_found_resource_with_path

  let select_first_resource_with_remote_id =
    Cache.Resource.select_first_resource_with_remote_id

  let remote_create file =
    with_retry_default
      (FilesResource.create ~enforceSingleParent:true ~supportsAllDrives:true
         ~std_params:file_std_params file)

  let remote_update ~custom_headers ~fileId file_patch =
    with_retry_default
      (FilesResource.update ~enforceSingleParent:true ~supportsAllDrives:true
         ~std_params:file_std_params ~custom_headers ~fileId file_patch)

  let remote_delete ~custom_headers ~fileId =
    with_retry_default
      (FilesResource.delete ~supportsAllDrives:true ~std_params:file_std_params
         ~custom_headers ~fileId)

  let remote_move ~custom_headers ~addParents ~fileId ~removeParents file =
    with_retry_default
      (FilesResource.update ~enforceSingleParent:true ~supportsAllDrives:true
         ~std_params:file_std_params ~custom_headers ~addParents ~fileId
         ~removeParents file)

  let replace_target_contents ~source ~target =
    let context = Context.get_ctx () in
    flush_memory_buffers source;
    with_retry download_resource source >>= fun content_path ->
    let cache = context.Context.cache in
    let target_content_path = Cache.get_content_path cache target in
    Utils.log_with_header
      "Replacing cache content (source content path=%s, target content path = \
       %s)\n\
       %!"
      content_path target_content_path;
    Utils.file_copy content_path target_content_path;
    let stats = Unix.LargeFile.stat target_content_path in
    let file_size = stats.Unix.LargeFile.st_size in
    let metadata = context |. Context.metadata_lens in
    Utils.with_lock context.Context.metadata_lock (fun () ->
        update_cache_size file_size metadata cache);
    update_cached_resource_state cache CacheData.Resource.State.ToUpload
      target.CacheData.Resource.id;
    UploadDispatchOps.queue_upload (drive_upload_dispatch_runtime ()) target

  let check_if_empty_remote remote_id is_folder trashed =
    let config = Context.get_ctx () |. Context.config_lens in
    if is_folder then
      let q =
        Printf.sprintf "'%s' in parents and trashed = %b" remote_id trashed
      in
      let std_params =
        {
          GapiService.StandardParameters.default with
          GapiService.StandardParameters.fields = "files(id)";
        }
      in
      with_retry_default
        (FilesResource.list ~supportsAllDrives:true
           ~driveId:config.Config.team_drive_id
           ~includeItemsFromAllDrives:(config.Config.team_drive_id <> "")
           ~corpora:
             (if config.Config.team_drive_id <> "" then "drive" else "user")
           ~std_params ~pageSize:1 ~q)
      >>= fun children ->
      if children.FileList.files = [] then (
        Utils.log_with_header "Folder (remote id=%s) is empty\n%!" remote_id;
        SessionM.return ())
      else (
        Utils.log_with_header "Folder (remote id=%s) is not empty\n%!" remote_id;
        raise Directory_not_empty)
    else SessionM.return ()
end

module MutationOps = DriveMutations.Make (DriveMutationPorts)

let drive_mutation_runtime () =
  let context = Context.get_ctx () in
  {
    DriveMutations.cache = context.Context.cache;
    config = context |. Context.config_lens;
    mountpoint_path = context.Context.mountpoint_path;
    skip_trash = context.Context.skip_trash;
  }

module DriveXattrPorts = struct
  let max_attribute_length = max_attribute_length
  let json_length = json_length
  let get_path_in_cache = get_path_in_cache
  let get_resource = get_resource

  let build_resource_keys_header_from_resource =
    build_resource_keys_header_from_resource

  let remote_update ~custom_headers ~fileId file_patch =
    with_retry_default
      (FilesResource.update ~enforceSingleParent:true ~supportsAllDrives:true
         ~std_params:file_std_params ~custom_headers ~fileId file_patch)

  let update_remote_resource runtime path do_remote_update =
    let mutation_runtime =
      {
        DriveMutations.cache = runtime.DriveXattrs.cache;
        config = runtime.DriveXattrs.config;
        mountpoint_path = "";
        skip_trash = false;
      }
    in
    MutationOps.update_remote_resource mutation_runtime path do_remote_update
end

module XattrOps = DriveXattrs.Make (DriveXattrPorts)

let drive_xattr_runtime () =
  let context = Context.get_ctx () in
  {
    DriveXattrs.cache = context.Context.cache;
    config = context |. Context.config_lens;
  }

(* Create resources *)
let create_remote_resource ?link_target is_folder path mode =
  do_request
    (MutationOps.create_remote_resource
       (drive_mutation_runtime ())
       ?link_target is_folder path mode)
  |> ignore

(* END Create resources *)

(* mknod *)
let mknod path mode = create_remote_resource false path mode

(* END mknod *)

(* mkdir *)
let mkdir path mode = create_remote_resource true path mode

(* END mkdir *)

(* Delete (trash) resources *)

let delete_remote_resource is_folder path =
  do_request
    (MutationOps.delete_remote_resource
       (drive_mutation_runtime ())
       is_folder path)
  |> ignore

(* END Delete (trash) resources *)

(* unlink *)
let unlink path = delete_remote_resource false path

(* END unlink *)

(* rmdir *)
let rmdir path = delete_remote_resource true path

(* END rmdir *)

(* rename *)
let rename path new_path =
  do_request (MutationOps.rename (drive_mutation_runtime ()) path new_path)
  |> ignore

(* END rename *)

(* truncate *)
let truncate path size =
  do_request
    (FileMutationOps.truncate (drive_file_mutation_runtime ()) path size)
  |> ignore

(* END truncate *)

(* chmod *)
let chmod path mode =
  do_request
    (MetadataMutationOps.chmod (drive_metadata_mutation_runtime ()) path mode)
  |> ignore

(* END chmod *)

(* chown *)
let chown path uid gid =
  do_request
    (MetadataMutationOps.chown
       (drive_metadata_mutation_runtime ())
       path uid gid)
  |> ignore

(* END chown *)

(* getxattr *)
let get_xattr path name =
  do_request (XattrOps.get_xattr (drive_xattr_runtime ()) path name) |> fst

(* END getxattr *)

(* setxattr *)
let set_xattr path name value xflags =
  do_request
    (XattrOps.set_xattr (drive_xattr_runtime ()) path name value xflags)
  |> ignore

(* END setxattr *)

(* listxattr *)
let list_xattr path =
  do_request (XattrOps.list_xattr (drive_xattr_runtime ()) path) |> fst

(* END listxattr *)

(* removexattr *)
let remove_xattr path name =
  do_request (XattrOps.remove_xattr (drive_xattr_runtime ()) path name)
  |> ignore

(* END removexattr *)

(* readlink *)
let read_link path =
  do_request (ViewOps.read_link (drive_view_runtime ()) path) |> fst

(* END readlink *)

(* symlink *)
let symlink target linkpath =
  create_remote_resource ~link_target:target false linkpath 0o777

(* END symlink *)
