open GapiUtils.Infix
open GapiLens.Infix
open GapiMonad
open GapiMonad.SessionM.Infix
open GapiDriveV3Model
open GapiDriveV3Service

exception File_not_found
exception Permission_denied
exception IO_error
exception Directory_not_empty
exception No_attribute
exception Existing_attribute
exception Invalid_operation

let folder_mime_type = "application/vnd.google-apps.folder"
let file_fields =
  "appProperties,capabilities(canEdit),createdTime,explicitlyTrashed,\
   fileExtension,id,md5Checksum,mimeType,modifiedTime,name,parents,\
   size,trashed,version,viewedByMeTime,webViewLink"
let file_std_params =
  { GapiService.StandardParameters.default with
        GapiService.StandardParameters.fields = file_fields
  }
let file_list_std_params =
  { GapiService.StandardParameters.default with
        GapiService.StandardParameters.fields =
          "files(" ^ file_fields ^ "),nextPageToken"
  }
let file_download_std_params =
  { GapiService.StandardParameters.default with
        GapiService.StandardParameters.alt = "media"
  }

let do_request = Oauth2.do_request
let async_do_request f =
  let thread = Thread.create (fun go -> do_request go) f in
  let thread_id = Thread.id thread in
  Utils.log_with_header "Spawning new thread id=%d\n%!" thread_id

let root_directory = "/"
let root_folder_id = "root"
let trash_directory = "/.Trash"
let trash_directory_name_length = String.length trash_directory
let trash_directory_base_path = "/.Trash/"
let f_bsize = 4096L
let change_limit = 50
let mb = 1048576L
let max_link_target_length = 127
let max_attribute_length = 126

(* Utilities *)
let chars_blacklist_regexp = Str.regexp "[/\000]"
let clean_filename name = Str.global_replace chars_blacklist_regexp "_" name

let apostrophe_regexp = Str.regexp (Str.quote "'")
let escape_apostrophe name = Str.global_replace apostrophe_regexp "\\'" name

let common_double_extension_suffixes = [".gz"; ".z"; ".bz2"]
let split_filename filename =
  let split full =
    try
      let last_dot = String.rindex full '.' in
      (Str.string_before full last_dot,
       Str.string_after full last_dot)
    with Not_found -> (full, "")
  in

  let (rest, first_extension) =
    split filename in
  let (base_name, second_extension) =
    if List.mem
         (String.lowercase first_extension)
         common_double_extension_suffixes then
      split rest
    else
      (rest, "")
  in
  let extension =
    match (first_extension, second_extension) with
        (_, "") -> first_extension
      | (f, s) -> s ^ f
  in
  (base_name, extension)

let get_path_hash path =
  if path == "" then None
  else begin
    let md5 = Cryptokit.Hash.md5 () in
    md5#add_string path;
    let md5_result = md5#result in
    let base64 = Cryptokit.Base64.encode_compact () in
    base64#put_string md5_result;
    base64#finish;
    Some base64#get_string
  end

let disambiguate_filename filename without_extension filename_table  =
  let rec find_first_unique_filename filename name_counter =
    let new_candidate =
      if without_extension then
        Printf.sprintf "%s (%d)" filename name_counter
      else
        let (base_name, extension) = split_filename filename in
        Printf.sprintf "%s (%d)%s" base_name name_counter extension
    in
    if not (Hashtbl.mem filename_table new_candidate) then begin
      Utils.log_with_header "Checking: %s: OK\n%!" new_candidate;
      (new_candidate, name_counter)
    end else begin
      Utils.log_with_header "Checking: %s: KO\n%!" new_candidate;
      find_first_unique_filename filename (succ name_counter)
    end
  in
  if Hashtbl.mem filename_table filename then begin
    Utils.log_with_header "Filename collision detected: %s\n%!" filename;
    let old_name_counter = Hashtbl.find filename_table filename in
    let (unique_filename, name_counter) =
      find_first_unique_filename filename (succ old_name_counter) in
    Hashtbl.replace filename_table filename name_counter;
    (unique_filename, name_counter)
  end else begin
    Utils.log_with_header "Filename (unused): %s\n%!" filename;
    Hashtbl.add filename_table filename 0;
    (filename, 0)
  end

let is_in_trash_directory path =
  if path = trash_directory then false
  else ExtString.String.starts_with path trash_directory_base_path

let get_path_in_cache path =
  if path = root_directory then
    (root_directory, false)
  else if path = trash_directory then
    (root_directory, true)
  else if is_in_trash_directory path then
    let path_in_cache = Str.string_after path trash_directory_name_length in
    (path_in_cache, true)
  else
    (path, false)

(* Used to do a try/with on a monadic f: state parameter s is eta-expanded,
 * otherwise the try/with will be ignored because f is only partially applied
 *)
let with_try f handle_exception s =
  try
    f s
  with e ->
    handle_exception e s

(* Raise with an extra parameter used in monadic functions *)
let throw e _ =
  raise e

let handle_default_exceptions =
  function
      GapiRequest.PermissionDenied session ->
        Utils.log_with_header "Server error: Permission denied.\n%!";
        throw Permission_denied
    | GapiRequest.RequestTimeout _ ->
        Utils.log_with_header "Server error: Request Timeout.\n%!";
        throw IO_error
    | GapiRequest.PreconditionFailed _
    | GapiRequest.Conflict _ ->
        Utils.log_with_header "Server error: Conflict.\n%!";
        throw IO_error
    | GapiRequest.Forbidden _ ->
        Utils.log_with_header "Server error: Forbidden.\n%!";
        throw IO_error
    | e -> throw e

(* with_try with a default exception handler *)
let try_with_default f s =
  with_try f handle_default_exceptions s

(* Resource cache *)
let get_filename name is_document get_document_format =
  let context = Context.get_ctx () in
  let config = context |. Context.config_lens in
  let clean_name = clean_filename name in
  let document_format =
    if is_document then get_document_format config
    else "" in
  if is_document &&
      config.Config.docs_file_extension &&
      document_format <> "" then
    (clean_name ^ "." ^ document_format, false)
  else
    (clean_name, is_document)

let build_resource_tables parent_path trashed =
  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  let resources =
    Cache.Resource.select_resources_with_parent_path
      cache parent_path trashed in
  let filename_table = Hashtbl.create 64 in
  let remote_id_table = Hashtbl.create (List.length resources) in
  List.iter
    (fun resource ->
       let name = Option.get resource.Cache.Resource.name in
       let (clean_name, _) =
         get_filename
           name
           (Cache.Resource.is_document resource)
           (fun config -> Cache.Resource.get_format resource config)
       in
       let filename = Filename.basename resource.Cache.Resource.path in
       if clean_name <> filename then begin
         let name_counter =
           try
             Hashtbl.find filename_table clean_name
           with Not_found -> 0
         in
         Hashtbl.replace filename_table clean_name name_counter;
       end;
       Hashtbl.add filename_table filename 0;
       Hashtbl.add remote_id_table
         (Option.get resource.Cache.Resource.remote_id) resource)
    resources;
  (filename_table, remote_id_table)

let create_resource ?local_name path =
  let parent_path = Filename.dirname path in
  { Cache.Resource.id = 0L;
    remote_id = None;
    name = None;
    mime_type = None;
    created_time = None;
    modified_time = None;
    viewed_by_me_time = None;
    file_extension = None;
    md5_checksum = None;
    size = None;
    can_edit = None;
    trashed = None;
    web_view_link = None;
    version = None;
    file_mode_bits = None;
    parent_path_hash = get_path_hash parent_path;
    local_name;
    uid = None;
    gid = None;
    link_target = None;
    xattrs = "";
    parent_path;
    path;
    state = Cache.Resource.State.ToDownload;
    last_update = Unix.gettimeofday ();
  }

let create_root_resource trashed =
  let resource = create_resource root_directory in
    { resource with
          Cache.Resource.remote_id = Some root_folder_id;
          mime_type = Some folder_mime_type;
          size = Some 0L;
          parent_path = "";
          trashed = Some trashed;
    }

let get_unique_filename name is_document get_document_format filename_table =
  let (complete_name, without_extension) =
    get_filename name is_document get_document_format
  in
  disambiguate_filename complete_name without_extension filename_table

let get_unique_filename_from_resource resource name filename_table =
  if Option.is_some resource.Cache.Resource.local_name &&
     get_path_hash resource.Cache.Resource.parent_path =
       resource.Cache.Resource.parent_path_hash then
    let local_name = resource.Cache.Resource.local_name |> Option.get in
    (local_name, resource.Cache.Resource.local_name)
  else
    let (filename, name_counter) =
      get_unique_filename
        name
        (Cache.Resource.is_document resource)
        (fun config -> Cache.Resource.get_format resource config)
        filename_table in
    let local_name = if name_counter > 0 then Some filename else None in
    (filename, local_name)

let get_unique_filename_from_file file filename_table =
  get_unique_filename
    file.File.name
    (Cache.Resource.is_document_mime_type file.File.mimeType)
    (fun config ->
      Cache.Resource.get_format_from_mime_type file.File.mimeType config)
    filename_table

let recompute_path resource name =
  (* TODO: make an optimized version of build_resource_tables that
   * doesn't create resource table (useful for large directories). *)
  let (filename_table, _) =
    build_resource_tables
      resource.Cache.Resource.parent_path
      (Option.default false resource.Cache.Resource.trashed) in
  let (filename, local_name) =
    get_unique_filename_from_resource resource name filename_table
  in
  let path = Filename.concat resource.Cache.Resource.parent_path filename in
  (path, local_name)

let update_resource_from_file ?state resource file =
  let (path, local_name) =
    match resource.Cache.Resource.name with
        Some cached_name ->
          if cached_name <> file.File.name then
            recompute_path resource file.File.name
          else (resource.Cache.Resource.path, None)
      | None -> (resource.Cache.Resource.path, None)
  in
  let parent_path = Filename.dirname path in
  { resource with
        Cache.Resource.remote_id = Some file.File.id;
        name = Some file.File.name;
        mime_type = Some file.File.mimeType;
        created_time = Some (Netdate.since_epoch file.File.createdTime);
        modified_time = Some (Netdate.since_epoch file.File.modifiedTime);
        viewed_by_me_time =
          Some (Netdate.since_epoch file.File.viewedByMeTime);
        file_extension = Some file.File.fileExtension;
        md5_checksum = Some file.File.md5Checksum;
        size = Some file.File.size;
        can_edit = Some file.File.capabilities.File.Capabilities.canEdit;
        trashed = Some file.File.trashed;
        web_view_link = Some file.File.webViewLink;
        version = Some file.File.version;
        file_mode_bits = Cache.Resource.get_file_mode_bits
            file.File.appProperties;
        parent_path_hash = get_path_hash parent_path;
        local_name;
        uid = Cache.Resource.get_uid file.File.appProperties;
        gid = Cache.Resource.get_gid file.File.appProperties;
        link_target = Cache.Resource.get_link_target file.File.appProperties;
        xattrs = Cache.Resource.get_xattrs file.File.appProperties;
        last_update = Unix.gettimeofday ();
        path;
        parent_path;
        state = Option.default resource.Cache.Resource.state state;
  }

let insert_resource_into_cache ?state cache resource file =
  let resource = update_resource_from_file ?state resource file in
  Utils.log_with_header "BEGIN: Saving resource to db (remote id=%s)\n%!"
    file.File.id;
  let inserted = Cache.Resource.insert_resource cache resource in
  Utils.log_with_header "END: Saving resource to db (remote id=%s, id=%Ld, state=%s)\n%!"
    file.File.id
    inserted.Cache.Resource.id
    (Cache.Resource.State.to_string inserted.Cache.Resource.state);
  inserted

let update_cached_resource cache resource =
  Utils.log_with_header
    "BEGIN: Updating resource in db (id=%Ld, state=%s)\n%!"
    resource.Cache.Resource.id
    (Cache.Resource.State.to_string resource.Cache.Resource.state);
  Cache.Resource.update_resource cache resource;
  Utils.log_with_header "END: Updating resource in db (id=%Ld)\n%!"
    resource.Cache.Resource.id

let update_cached_resource_state cache state id =
  Utils.log_with_header
    "BEGIN: Updating resource state in db (id=%Ld, state=%s)\n%!"
    id (Cache.Resource.State.to_string state);
  Cache.Resource.update_resource_state cache state id;
  Utils.log_with_header "END: Updating resource state in db (id=%Ld)\n%!" id

let lookup_resource path trashed =
  Utils.log_with_header "BEGIN: Loading resource %s (trashed=%b) from db\n%!"
    path trashed;
  let cache = Context.get_cache () in
  let resource = Cache.Resource.select_resource_with_path cache path trashed in
  begin if Option.is_none resource then begin
    Utils.log_with_header
      "END: Loading resource %s (trashed=%b) from db: Not found\n%!"
      path trashed;
  end else begin
    let id = resource |. GapiLens.option_get |. Cache.Resource.id in
    let state = resource
      |. GapiLens.option_get
      |. Cache.Resource.state
      |> Cache.Resource.State.to_string in
    Utils.log_with_header
      "END: Loading resource %s (trashed=%b) from db: Found (id=%Ld, state=%s)\n%!"
      path trashed id state;
  end end;
  resource

let get_root_resource trashed =
  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  match lookup_resource root_directory trashed with
      None ->
        let root_resource = create_root_resource trashed in
          Utils.log_with_header
            "BEGIN: Saving root resource to db (id=%Ld)\n%!"
            root_resource.Cache.Resource.id;
          let inserted =
            Cache.Resource.insert_resource cache root_resource in
          Utils.log_with_header
            "END: Saving root resource to db (id=%Ld)\n%!"
            root_resource.Cache.Resource.id;
          inserted
    | Some resource -> resource

let cache_size_mutex = Mutex.create ()

let update_cache_size new_size metadata cache =
  let metadata = metadata
    |> Cache.Metadata.cache_size ^= new_size in
  Utils.log_with_header "BEGIN: Updating cache size (%Ld) in db\n%!" new_size;
  Cache.Metadata.insert_metadata cache metadata;
  Utils.log_with_header "END: Updating cache size (%Ld) in db\n%!" new_size;
  Context.update_ctx (Context.metadata ^= Some metadata)

let shrink_cache file_size max_cache_size_mb metadata cache =
  Utils.try_finally
    (fun () ->
      Mutex.lock cache_size_mutex;
      if file_size <> 0L then begin
        let max_cache_size = Int64.mul (Int64.of_int max_cache_size_mb) mb in
        let target_size =
          Int64.add metadata.Cache.Metadata.cache_size file_size in
        if target_size <= max_cache_size then begin
          update_cache_size target_size metadata cache;
        end else begin
          let resources =
            Cache.Resource.select_resources_order_by_last_update cache in
          let (new_cache_size, resources_to_free) =
            List.fold_left
              (fun (new_cache_size, rs) resource ->
                if new_cache_size <= max_cache_size then
                  (new_cache_size, rs)
                else begin
                  let new_size = Int64.sub
                    new_cache_size
                    (Option.get resource.Cache.Resource.size) in
                  (new_size, resource :: rs)
                end)
              (target_size, [])
              resources in
          update_cache_size new_cache_size metadata cache;
          List.iter
            (fun resource ->
               update_cached_resource_state cache
                 Cache.Resource.State.ToDownload resource.Cache.Resource.id)
            resources_to_free;
          Cache.delete_files_from_cache cache resources_to_free |> ignore
        end
      end)
    (fun () -> Mutex.unlock cache_size_mutex)

let delete_resources metadata cache resources =
  Utils.try_finally
    (fun () ->
      Mutex.lock cache_size_mutex;
      Cache.Resource.delete_resources cache resources;
      let total_size =
        Cache.delete_files_from_cache cache resources in
      let new_cache_size =
        Int64.sub metadata.Cache.Metadata.cache_size total_size in
      update_cache_size new_cache_size metadata cache)
    (fun () -> Mutex.unlock cache_size_mutex)

let update_cache_size_for_documents cache resource content_path op =
  Utils.try_finally
    (fun () ->
      Mutex.lock cache_size_mutex;
      if resource.Cache.Resource.size = Some 0L &&
          Sys.file_exists content_path then begin
        try
          let stats = Unix.LargeFile.stat content_path in
          let size = stats.Unix.LargeFile.st_size in
          let context = Context.get_ctx () in
          let metadata = context.Context.metadata |> Option.get in
          let new_cache_size =
            op metadata.Cache.Metadata.cache_size size in
          update_cache_size new_cache_size metadata cache
        with e -> Utils.log_exception e
      end)
    (fun () -> Mutex.unlock cache_size_mutex)
(* END Resource cache *)

(* Metadata *)
let get_metadata () =
  let request_new_start_page_token =
    let std_params =
      { GapiService.StandardParameters.default with
            GapiService.StandardParameters.fields =
              "startPageToken"
      } in
    ChangesResource.getStartPageToken
      ~std_params >>= fun startPageToken ->
    SessionM.return startPageToken.StartPageToken.startPageToken
  in

  let get_start_page_token start_page_token_db =
    if start_page_token_db = "" then
      request_new_start_page_token
    else
      SessionM.return start_page_token_db
  in

  let request_metadata start_page_token_db cache_size =
    let std_params =
      { GapiService.StandardParameters.default with
            GapiService.StandardParameters.fields =
              "user(displayName),storageQuota(limit,usage)"
      } in
    AboutResource.get
      ~std_params >>= fun about ->
    SessionM.get >>= fun session ->
    get_start_page_token start_page_token_db >>= fun start_page_token ->
    let metadata = {
      Cache.Metadata.display_name = about.About.user.User.displayName;
      storage_quota_limit = about.About.storageQuota.About.StorageQuota.limit;
      storage_quota_usage = about.About.storageQuota.About.StorageQuota.usage;
      start_page_token;
      cache_size;
      last_update = Unix.gettimeofday ();
    } in
    SessionM.return metadata
  in

  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  let metadata =
    if Option.is_none context.Context.metadata then begin
      Utils.log_with_header "BEGIN: Loading metadata from db\n%!";
      let db_metadata = Cache.Metadata.select_metadata context.Context.cache in
      Context.update_ctx (Context.metadata ^= db_metadata);
      db_metadata
    end else begin
      Utils.log_with_header "BEGIN: Getting metadata from context\n%!";
      context.Context.metadata
    end in

  let update_resource_cache start_page_token new_metadata =
    let get_all_changes =
      let rec loop pageToken accu =
        let std_params =
          { GapiService.StandardParameters.default with
                GapiService.StandardParameters.fields =
                  "changes(removed,file(" ^ file_fields
                  ^ "),fileId),nextPageToken,newStartPageToken"
          } in
        ChangesResource.list
          ~std_params
          ~includeRemoved:true
          ~pageToken >>= fun change_list ->
        let changes = change_list.ChangeList.changes @ accu in
        if change_list.ChangeList.nextPageToken = "" then
          SessionM.return (changes, change_list.ChangeList.newStartPageToken)
        else
          loop change_list.ChangeList.nextPageToken changes
      in
      loop start_page_token []
    in

    let request_changes =
      Utils.log_with_header "BEGIN: Getting changes from server\n%!";
      get_all_changes >>= fun (changes, new_start_page_token) ->
      Utils.log_with_header "END: Getting changes from server\n%!";
      SessionM.return (changes, new_start_page_token)
    in

    let get_ids_to_update change =
      let get_id = Option.map (fun r -> r.Cache.Resource.id) in
      let file = change.Change.file in
      let selected_resource =
        Cache.Resource.select_resource_with_remote_id cache file.File.id in
      let resources =
        match selected_resource with
            None ->
              let remote_ids =
                match file.File.parents with
                    [] -> [root_folder_id]
                  | ids -> ids
              in
              List.map
                (Cache.Resource.select_resource_with_remote_id cache)
                remote_ids
          | Some r ->
              let parent_resource =
                Cache.Resource.select_resource_with_path cache
                  r.Cache.Resource.parent_path
                  false
              in
              [parent_resource; selected_resource]
      in
      List.map get_id resources
    in

    let get_file_id_from_change change =
      [Cache.Resource.select_resource_with_remote_id cache
         change.Change.fileId]
    in

    let request_remaining_changes start_page_token_db =
      if start_page_token_db = "" then
        SessionM.return change_limit
      else
        let std_params =
          { GapiService.StandardParameters.default with
            GapiService.StandardParameters.fields =
              "changes(kind),newStartPageToken"
          } in
        ChangesResource.list
          ~std_params
          ~includeRemoved:true
          ~pageSize:change_limit
          ~pageToken:start_page_token_db >>= fun change_list ->
        if change_list.ChangeList.newStartPageToken = "" then
          SessionM.return change_limit
        else
          SessionM.return (List.length change_list.ChangeList.changes)
    in

    request_remaining_changes start_page_token >>= fun remaining_changes ->
    if remaining_changes = 0 then begin
      Utils.log_with_header
        "END: Getting metadata: No need to update resource cache\n%!";
      SessionM.return new_metadata
    end else if remaining_changes = change_limit then begin
      Utils.log_with_header
        "END: Getting metadata: Too many changes\n%!";
      SessionM.return new_metadata
    end else begin match metadata with
        None ->
          SessionM.return new_metadata
      | Some _ ->
          request_changes >>= fun (changes, new_start_page_token) ->
          let update_resource_cache_from_changes
              filter_changes map_change update_cache =
            let filtered_changes =
              List.filter filter_changes changes in
            let xs =
              List.fold_left
                (fun xs change ->
                   let mapped_changes = map_change change in
                     List.fold_left
                       (fun xs' c ->
                          match c with
                              None -> xs'
                            | Some x ->
                                if not (List.mem x xs') then
                                  x :: xs'
                                else xs')
                       xs
                       mapped_changes)
                []
                filtered_changes in
            update_cache cache xs;
          in

          Utils.log_with_header "BEGIN: Updating resource cache\n%!";
          update_resource_cache_from_changes
            (fun change ->
               not change.Change.removed &&
               not change.Change.file.File.trashed)
            get_ids_to_update
            (fun cache ids ->
               Cache.Resource.invalidate_resources cache ids);
          Utils.log_with_header "END: Updating resource cache\n";
          Utils.log_with_header "BEGIN: Updating trashed resources\n%!";
          update_resource_cache_from_changes
            (fun change -> change.Change.file.File.trashed)
            get_file_id_from_change
            (fun cache resources ->
               Cache.Resource.trash_resources cache resources);
          Utils.log_with_header "END: Updating trashed resources\n";
          Utils.log_with_header "BEGIN: Removing deleted resources\n%!";
          update_resource_cache_from_changes
            (fun change -> change.Change.removed)
            get_file_id_from_change
            (delete_resources new_metadata);
          Utils.log_with_header "END: Removing deleted resources\n%!";
          if List.length changes > 0 then begin
            Utils.log_with_header "BEGIN: Invalidating trash bin resource\n%!";
            Cache.Resource.invalidate_trash_bin cache;
            Utils.log_with_header "END: Invalidating trash bin resource\n%!";
          end;
          SessionM.return {
            new_metadata with
                Cache.Metadata.start_page_token = new_start_page_token;
          }
    end
  in

  let refresh_metadata =
    let start_page_token =
      Option.map_default
        Cache.Metadata.start_page_token.GapiLens.get "" metadata in
    let cache_size =
      Option.map_default
        Cache.Metadata.cache_size.GapiLens.get 0L metadata in
    Utils.log_with_header "BEGIN: Refreshing metadata\n%!";
    let get_server_metadata =
      with_try
        (request_metadata start_page_token cache_size)
        (function
            GapiRequest.NotModified session ->
              Utils.log_with_header
                "Refreshing metadata: Not modified\n%!";
              SessionM.put session >>= fun () ->
              let m = Option.get metadata in
              let m' =
                m |> Cache.Metadata.last_update ^= Unix.gettimeofday () in
              SessionM.return m'
          | e -> throw e) in
    get_server_metadata >>= fun server_metadata ->
    Utils.log_with_header "END: Refreshing metadata\n";
    update_resource_cache
      start_page_token server_metadata >>= fun updated_metadata ->
    Utils.log_with_header "BEGIN: Updating metadata in db\n%!";
    Cache.Metadata.insert_metadata context.Context.cache updated_metadata;
    Utils.log_with_header "END: Updating metadata in db\n";
    Utils.log_with_header "BEGIN: Updating context\n%!";
    Context.update_ctx (Context.metadata ^= Some updated_metadata);
    Utils.log_with_header "END: Updating context\n%!";
    SessionM.return updated_metadata
  in

  match metadata with
      None ->
        Utils.log_with_header "END: Getting metadata: Not found\n%!";
        do_request refresh_metadata |> fst
    | Some m ->
        let metadata_cache_time =
          context |. Context.config_lens |. Config.metadata_cache_time
        in
        if Cache.Metadata.is_valid metadata_cache_time m then begin
          Utils.log_with_header "END: Getting metadata: Valid\n%!";
          m
        end else begin
          Utils.log_with_header "END: Getting metadata: Not valid\n%!";
          do_request refresh_metadata |> fst
        end

let statfs () =
  let metadata = get_metadata () in
  let f_blocks =
    Int64.div metadata.Cache.Metadata.storage_quota_limit f_bsize in
  let free_bytes = Int64.sub
                     metadata.Cache.Metadata.storage_quota_limit
                     metadata.Cache.Metadata.storage_quota_usage in
  let f_bfree = Int64.div free_bytes f_bsize in
    { Unix_util.f_bsize;
      f_blocks;
      f_bfree;
      f_bavail = f_bfree;
      f_files = f_blocks;
      f_ffree = f_bfree;
      f_namemax = 256L;
      (* ignored *)
      f_frsize = 0L;
      f_favail = 0L;
      f_fsid = 0L;
      f_flag = 0L;
    }
(* END Metadata *)

(* Resources *)
let refresh_remote_resource resource current_file =
  let remote_id = resource.Cache.Resource.remote_id |> Option.get in
  Utils.log_with_header
    "BEGIN: Refreshing remote resource (remote id=%s)\n%!"
    remote_id;
  with_try
    (FilesResource.get
       ~std_params:file_std_params
       ~fileId:remote_id)
    (function
        GapiRequest.NotModified session ->
        Utils.log_with_header
          "END: Refreshing remote resource (remote id=%s): Not modified\n%!"
          remote_id;
        SessionM.put session >>= fun () ->
        SessionM.return current_file
      | e -> throw e) >>= fun file ->
  Utils.log_with_header
    "END: Refreshing remote resource (remote id=%s)\n%!"
    remote_id;
  SessionM.return file

let get_file_from_server parent_folder_id name trashed =
  Utils.log_with_header
    "BEGIN: Getting resource %s (in folder %s) from server\n%!"
    name parent_folder_id;
  let q =
    Printf.sprintf "name='%s' and '%s' in parents and trashed=%b"
      (escape_apostrophe name) parent_folder_id trashed in
  FilesResource.list
    ~std_params:file_list_std_params
    ~q
    ~pageSize:1 >>= fun file_list ->
  Utils.log_with_header
    "END: Getting resource %s (in folder %s) from server\n%!"
    name parent_folder_id;
  let files = file_list.FileList.files in
  if List.length files = 0 then
    SessionM.return None
  else
    let file = files |. GapiLens.head in
    SessionM.return (Some file)

let get_resource_from_server parent_folder_id name new_resource trashed cache =
  get_file_from_server parent_folder_id name trashed >>= fun file ->
  match file with
      None ->
        Utils.log_with_header
          "BEGIN: Saving not found resource to db (name=%s)\n%!"
          name;
        let resource = new_resource
          |> Cache.Resource.trashed ^= Some trashed
          |> Cache.Resource.state ^= Cache.Resource.State.NotFound in
        let inserted = Cache.Resource.insert_resource cache resource in
        Utils.log_with_header
          "END: Saving not found resource to db (name=%s)\n%!"
          name;
        SessionM.return inserted
    | Some entry ->
        let inserted = insert_resource_into_cache cache new_resource entry in
        SessionM.return inserted

let check_resource_in_cache cache path trashed =
  let metadata_last_update =
    Context.get_ctx () |. Context.metadata_last_update_lens in
  match lookup_resource path trashed with
      None -> false
    | Some resource ->
        if Cache.Resource.is_valid resource metadata_last_update then
          if Cache.Resource.is_folder resource then
            resource.Cache.Resource.state = Cache.Resource.State.Synchronized
          else true
        else false

let rec get_folder_id path trashed =
  if path = root_directory then
    SessionM.return root_folder_id
  else
    get_resource path trashed >>= fun resource ->
    let remote_id =
      resource |. Cache.Resource.remote_id |. GapiLens.option_get in
    SessionM.return remote_id
and get_resource path trashed =
  let metadata_last_update =
    get_metadata () |. Cache.Metadata.last_update in

  let get_new_resource cache =
    let parent_path = Filename.dirname path in
      if check_resource_in_cache cache parent_path trashed then begin
        throw File_not_found
      end else begin
        let new_resource = create_resource path in
        let name = Filename.basename path in
        get_folder_id
          new_resource.Cache.Resource.parent_path
          trashed >>= fun parent_folder_id ->
        get_resource_from_server
          parent_folder_id name new_resource trashed cache >>= fun resource ->
        SessionM.return resource
      end
  in

  let refresh_resource resource cache =
    begin if Option.is_some resource.Cache.Resource.remote_id then begin
      let remote_id = resource.Cache.Resource.remote_id |> Option.get in
      Utils.log_with_header
        "BEGIN: Getting file from server (remote id=%s)\n%!"
        remote_id;
      FilesResource.get
        ~std_params:file_std_params
        ~fileId:remote_id >>= fun file ->
      Utils.log_with_header
        "END: Getting file from server (remote id=%s)\n%!"
        remote_id;
      SessionM.return (Some file)
    end else
      SessionM.return None
    end >>= fun refreshed_file ->
    match refreshed_file with
        None ->
          Cache.Resource.delete_resource cache resource;
          get_new_resource cache
      | Some file ->
          SessionM.get >>= fun session ->
          let updated_resource = update_resource_from_file
              resource file in
          update_cached_resource cache updated_resource;
          Utils.log_with_header
            "END: Refreshing resource (id=%Ld)\n%!"
            updated_resource.Cache.Resource.id;
          SessionM.return updated_resource
  in

  if path = root_directory then
    let root_resource = get_root_resource trashed in
    SessionM.return root_resource
  else
    let cache = Context.get_cache () in
    begin match lookup_resource path trashed with
        None ->
          get_new_resource cache
      | Some resource ->
          if Cache.Resource.is_valid resource metadata_last_update then
            SessionM.return resource
          else
            with_try
              (refresh_resource resource cache)
              (function
                   GapiRequest.NotModified session ->
                     Utils.log_with_header
                       "END: Refreshing resource (id=%Ld): Remote id=%s not modified\n%!"
                       resource.Cache.Resource.id
                       (resource.Cache.Resource.remote_id |> Option.get);
                     SessionM.put session >>= fun () ->
                     let updated_resource = resource
                       |> Cache.Resource.last_update ^= Unix.gettimeofday () in
                     update_cached_resource cache updated_resource;
                     SessionM.return updated_resource
                 | e -> throw e)
    end >>= fun resource ->
    begin match resource.Cache.Resource.state with
        Cache.Resource.State.NotFound ->
          throw File_not_found
      | _ ->
          SessionM.return resource
    end

let check_md5_checksum resource cache =
  let path = resource.Cache.Resource.path in
  let content_path = Cache.get_content_path cache resource in
  let md5_checksum = Option.default "" resource.Cache.Resource.md5_checksum in
  if md5_checksum <> "" then begin
    Utils.log_with_header
      "BEGIN: Checking MD5 checksum (path=%s, cache path=%s, hash=%s)\n%!"
      path content_path md5_checksum;
    if Sys.file_exists content_path then begin
      let md5 = Cryptokit.Hash.md5 () in
      Utils.with_in_channel content_path
        (fun ch ->
           try
             while true do
               let byte = input_byte ch in
                 md5#add_byte byte;
             done
           with End_of_file -> ());
      let md5_result = md5#result in
      let hexa = Cryptokit.Hexa.encode () in
      hexa#put_string md5_result;
      hexa#finish;
      let checksum = hexa#get_string in
      Utils.log_with_header
        "END: Checking MD5 checksum (path=%s, cache path=%s, hash=%s): Computed MD5 checksum: %s\n%!"
        path content_path md5_checksum checksum;
      checksum = md5_checksum
    end else begin
      Utils.log_with_header
        "END: Checking MD5 checksum (path=%s, cache path=%s, hash=%s): File does not exists\n%!"
        path content_path md5_checksum;
      false
    end;
  end else false

let with_retry f resource =
  let rec loop res n =
    with_try
      (f res)
      (function
           IO_error as e ->
             if n >= !Utils.max_retries then begin
               let context = Context.get_ctx () in
               let conflict_resolution = context
                 |. Context.config_lens
                 |. Config.conflict_resolution in
               if resource.Cache.Resource.state =
                     Cache.Resource.State.ToUpload
                   && conflict_resolution =
                     Config.ConflictResolutionStrategy.Server then begin
                 let cache = context.Context.cache in
                 update_cached_resource_state cache
                   Cache.Resource.State.ToDownload resource.Cache.Resource.id;
               end;
               throw e
             end else begin
               GapiUtils.wait_exponential_backoff n;
               let fileId = res.Cache.Resource.remote_id |> Option.get in
               FilesResource.get
                 ~std_params:file_std_params
                 ~fileId >>= fun file ->
               let (state, verb) =
                 if resource.Cache.Resource.state = Cache.Resource.State.ToUpload then
                   (Cache.Resource.State.ToUpload, "uploading")
                 else
                   (Cache.Resource.State.ToDownload, "downloading") in
               SessionM.get >>= fun session ->
               let refreshed_resource =
                 update_resource_from_file
                   ~state res file in
               let context = Context.get_ctx () in
               let cache = context.Context.cache in
               update_cached_resource cache refreshed_resource;
               let n' = n + 1 in
               Utils.log_with_header
                 "Retry (%d/%d) %s resource (id=%Ld).\n%!"
                 n' !Utils.max_retries verb resource.Cache.Resource.id;
               loop refreshed_resource n'
             end
         | e -> throw e)
  in
    loop resource 0

let is_desktop_format resource config =
  Cache.Resource.get_format resource config = "desktop"

let create_desktop_entry resource content_path config =
  Utils.with_out_channel
    ~mode:[Open_creat; Open_trunc; Open_wronly] content_path
    (fun out_ch ->
      let icon_entry =
        let icon = Cache.Resource.get_icon resource config in
        if icon = "" then ""
        else "Icon=" ^ icon ^ "\n"
      in
      Printf.fprintf out_ch
        "[Desktop Entry]\n\
         Type=Link\n\
         Name=%s\n\
         URL=%s\n%s"
        (Option.default "" resource.Cache.Resource.name)
        (Option.default "" resource.Cache.Resource.web_view_link)
        icon_entry;
      SessionM.return ())

let download_resource resource =
  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  let config = context |. Context.config_lens in
  let content_path = Cache.get_content_path cache resource in
  let shrink_cache () =
    Option.may
      (fun file_size ->
         let metadata = context.Context.metadata |> Option.get in
         shrink_cache
           file_size
           config.Config.max_cache_size_mb
           metadata
           cache)
      resource.Cache.Resource.size;
    SessionM.return () in
  let do_api_download () =
    let destination = GapiMediaResource.TargetFile content_path in
    let media_download = {
      GapiMediaResource.destination;
      range_spec = "";
    } in
    let fileId = resource |. Cache.Resource.remote_id |> Option.get in
    if Cache.Resource.is_document resource then begin
      let fmt = Cache.Resource.get_format resource config in
      let mimeType = Cache.Resource.mime_type_of_format fmt in
      FilesResource.export
        ~media_download
        ~fileId
        ~mimeType >>= fun () ->
      SessionM.return ()
    end else if Option.default 0L resource.Cache.Resource.size > 0L then begin
      FilesResource.get
        ~std_params:file_download_std_params
        ~media_download
        ~fileId >>= fun _ ->
      SessionM.return ()
    end else begin
      Utils.log_with_header
        "BEGIN: Creating resource without content (path=%s)\n%!"
        content_path;
      close_out (open_out content_path);
      SessionM.return ()
    end
  in
  let do_download () =
    Utils.log_with_header
      "BEGIN: Downloading resource (id=%Ld)\n%!"
      resource.Cache.Resource.id;
    if is_desktop_format resource config then
      create_desktop_entry resource content_path config
    else begin
      shrink_cache () >>= fun () ->
      update_cached_resource_state cache
        Cache.Resource.State.Downloading resource.Cache.Resource.id;
      update_cache_size_for_documents cache resource content_path Int64.sub;
      with_try
        (do_api_download ())
        (fun e ->
           update_cached_resource_state cache
             Cache.Resource.State.ToDownload resource.Cache.Resource.id;
           handle_default_exceptions e)
    end >>= fun () ->
    update_cache_size_for_documents cache resource content_path Int64.add;
    Utils.log_with_header
      "END: Downloading resource (id=%Ld)\n%!"
      resource.Cache.Resource.id;
    update_cached_resource_state cache
      Cache.Resource.State.Synchronized resource.Cache.Resource.id;
    SessionM.return ()
  in
  let rec check_state n =
    let reloaded_resource = Option.map_default
      (Cache.Resource.select_resource_with_remote_id cache)
      (Some resource)
      resource.Cache.Resource.remote_id
    in
    let reloaded_state = match reloaded_resource with
        None -> Cache.Resource.State.NotFound
      | Some r -> r.Cache.Resource.state
    in
    let download_if_not_updated () =
      if check_md5_checksum resource cache then begin
        update_cached_resource_state cache
          Cache.Resource.State.Synchronized resource.Cache.Resource.id;
        SessionM.return ()
      end else
        do_download ()
    in
    begin match reloaded_state with
        Cache.Resource.State.Synchronized
      | Cache.Resource.State.ToUpload
      | Cache.Resource.State.Uploading ->
          if Sys.file_exists content_path then
            SessionM.return ()
          else
            do_download ()
      | Cache.Resource.State.ToDownload ->
          download_if_not_updated ()
      | Cache.Resource.State.Downloading ->
          if n > 300 then begin
            Utils.log_with_header
              "Still downloading resource (id=%Ld) after about 5 hours: start downloading again\n%!"
              resource.Cache.Resource.id;
            download_if_not_updated ()
          end else begin
            Utils.log_with_header
              "Already downloading resource (id=%Ld): check number %d\n%!"
              resource.Cache.Resource.id
              n;
            let n' = if n > 6 then 6 else n in
            GapiUtils.wait_exponential_backoff n';
            check_state (n + 1)
          end
      | Cache.Resource.State.NotFound ->
          throw File_not_found
    end
  in
  check_state 0 >>= fun () ->
  SessionM.return content_path

let stream_resource offset buffer resource =
  let length = Bigarray.Array1.dim buffer in
  let finish = Int64.add offset (Int64.of_int (length - 1)) in
  Utils.log_with_header
    "BEGIN: Stream resource (id=%Ld, offset=%Ld, finish=%Ld, length=%d)\n%!"
    resource.Cache.Resource.id offset finish length;
  let destination = GapiMediaResource.ArrayBuffer buffer in
  let range_spec =
    GapiMediaResource.generate_range_spec [(Some offset, Some finish)] in
  let media_download = {
    GapiMediaResource.destination;
    range_spec;
  } in
  let fileId = resource |. Cache.Resource.remote_id |> Option.get in
  try_with_default
    (FilesResource.get
       ~std_params:file_download_std_params
       ~media_download
       ~fileId
    ) >>= fun _ ->
  Utils.log_with_header
    "END: Stream resource (id=%Ld, offset=%Ld, finish=%Ld, length=%d)\n%!"
    resource.Cache.Resource.id offset finish length;
  SessionM.return ()

let is_filesystem_read_only () =
  Context.get_ctx () |. Context.config_lens |. Config.read_only

let is_file_read_only resource =
  not (Option.default true resource.Cache.Resource.can_edit) ||
  Cache.Resource.is_document resource
(* END Resources *)

(* stat *)
let get_attr path =
  let context = Context.get_ctx () in
  let config = context |. Context.config_lens in
  let (path_in_cache, trashed) = get_path_in_cache path in

  let request_resource =
    get_resource path_in_cache trashed >>= fun resource ->
    begin if Cache.Resource.is_document resource &&
             config.Config.download_docs then
      with_try
        (with_retry download_resource resource)
        (function
             File_not_found -> SessionM.return ""
           | e -> throw e)
    else
      SessionM.return ""
    end >>= fun content_path ->
    SessionM.return (resource, content_path)
  in

  if path = root_directory then
    context.Context.mountpoint_stats
  else if path = trash_directory then
    let stats = context.Context.mountpoint_stats in
    { stats with
          Unix.LargeFile.st_perm = stats.Unix.LargeFile.st_perm land 0o555
    }
  else begin
    let (resource, content_path) = do_request request_resource |> fst in
    let stat =
      if content_path <> "" then Some (Unix.LargeFile.stat content_path)
      else None in
    let st_kind =
      if Cache.Resource.is_folder resource then Unix.S_DIR
      else
        Option.map_default
          Cache.Resource.file_mode_bits_to_kind
          Unix.S_REG
          resource.Cache.Resource.file_mode_bits
    in
    let st_perm =
      let default_perm =
        if Cache.Resource.is_folder resource then 0o777
        else 0o666 in
      let perm =
        Option.map_default
          Cache.Resource.file_mode_bits_to_perm
          default_perm
          resource.Cache.Resource.file_mode_bits
      in
      let mask =
        lnot config.Config.umask land (
          if config.Config.read_only ||
             not (Option.default true resource.Cache.Resource.can_edit) ||
             Cache.Resource.is_document resource
          then 0o555
          else 0o777)
      in
      perm land mask in
    let st_nlink =
      if Cache.Resource.is_folder resource then 2
      else 1 in
    let st_uid =
      Option.map_default 
        Int64.to_int
        context.Context.mountpoint_stats.Unix.LargeFile.st_uid
        resource.Cache.Resource.uid in
    let st_gid =
      Option.map_default 
        Int64.to_int
        context.Context.mountpoint_stats.Unix.LargeFile.st_gid
        resource.Cache.Resource.gid in
    let st_size =
      match stat with
          None ->
            if Cache.Resource.is_folder resource then f_bsize
            else Option.default 0L resource.Cache.Resource.size
        | Some st ->
            st.Unix.LargeFile.st_size in
    let st_atime =
      match stat with
          None ->
            resource.Cache.Resource.viewed_by_me_time |> Option.get
        | Some st ->
            st.Unix.LargeFile.st_atime in
    let is_to_upload =
      resource.Cache.Resource.state = Cache.Resource.State.ToUpload in
    let st_mtime =
      match stat with
          Some st when is_to_upload ->
            st.Unix.LargeFile.st_mtime
        | _ ->
            resource.Cache.Resource.modified_time |> Option.get in
    let st_ctime =
      match stat with
          Some st when is_to_upload ->
            st.Unix.LargeFile.st_ctime
        | _ ->
            st_mtime
    in
    { context.Context.mountpoint_stats with
          Unix.LargeFile.st_kind;
          st_perm;
          st_nlink;
          st_uid;
          st_gid;
          st_size;
          st_atime;
          st_mtime;
          st_ctime;
    }
  end
(* END stat *)

(* readdir *)
let read_dir path =
  let get_all_files q =
    let rec loop ?pageToken accu =
      FilesResource.list
        ~std_params:file_list_std_params
        ~q
        ?pageToken >>= fun file_list ->
      let files = file_list.FileList.files @ accu in
      if file_list.FileList.nextPageToken = "" then
        SessionM.return files
      else
        loop ~pageToken:file_list.FileList.nextPageToken files
    in
      loop []
  in

  let (path_in_cache, trashed) = get_path_in_cache path in

  let request_folder =
    Utils.log_with_header
      "BEGIN: Getting folder content (path=%s, trashed=%b)\n%!"
      path_in_cache trashed;
    get_resource path_in_cache trashed >>= fun resource ->
    get_folder_id path_in_cache trashed >>= fun folder_id ->
    let q =
      Printf.sprintf "'%s' in parents and trashed = %b" folder_id trashed in
    get_all_files q >>= fun files ->
    Utils.log_with_header
      "END: Getting folder content (path=%s, trashed=%b)\n%!"
      path_in_cache trashed;
    begin if path = trash_directory && trashed then begin
      Utils.log_with_header "BEGiN: Getting explicitly trashed files\n%!";
      let q =
        Printf.sprintf "not '%s' in parents and trashed = true" folder_id in
      get_all_files q >>= fun trashed_files ->
      let explicitly_trashed_files =
        List.filter (fun file -> file.File.explicitlyTrashed) trashed_files in
      Utils.log_with_header
        "END: Getting explicitly trashed files: Found %d files\n%!"
        (List.length explicitly_trashed_files);
      SessionM.return (files @ explicitly_trashed_files, resource);
    end else
      SessionM.return (files, resource);
    end
  in

  let cache = Context.get_cache () in
  let resources =
    if check_resource_in_cache cache path_in_cache trashed then begin
      Utils.log_with_header
        "BEGIN: Getting resources from db (parent path=%s, trashed=%b)\n%!"
        path_in_cache trashed;
      let resources =
        Cache.Resource.select_resources_with_parent_path
          cache path_in_cache trashed in
      Utils.log_with_header
        "END: Getting resources from db (parent path=%s, trashed=%b)\n%!"
        path_in_cache trashed;
      resources
    end else begin
      let (files, folder_resource) = do_request request_folder |> fst in
      let (filename_table, remote_id_table) =
        build_resource_tables path_in_cache trashed in
      let resources_and_files =
        List.map
          (fun file ->
             try
               let cached_resource =
                 Hashtbl.find remote_id_table file.File.id in
               let updated_resource =
                 update_resource_from_file cached_resource file in
               (Some updated_resource, file)
             with Not_found ->
               (None, file))
          files
      in
      let resources =
        List.map
          (fun (resource, file) ->
             match resource with
                 Some r -> r
               | None ->
                   let (filename, name_counter) =
                     get_unique_filename_from_file file filename_table in
                   let local_name =
                     if name_counter > 0 then Some filename else None in
                   let resource_path =
                     Filename.concat path_in_cache filename in
                   let resource = create_resource ?local_name resource_path in
                   update_resource_from_file resource file)
          resources_and_files
      in
      Utils.log_with_header
        "BEGIN: Inserting folder resources into db (trashed=%b)\n%!"
        trashed;
      let inserted_resources =
        Cache.Resource.insert_resources
          cache resources path_in_cache trashed in
      Utils.log_with_header
        "END: Inserting folder resources into db (trashed=%b)\n%!"
        trashed;
      let updated_resource = folder_resource
        |> Cache.Resource.state ^= Cache.Resource.State.Synchronized
        |> Cache.Resource.last_update ^= Unix.gettimeofday ()
      in
      update_cached_resource cache updated_resource;
      inserted_resources
    end
  in
  let filenames =
    List.map
      (fun resource ->
         Filename.basename resource.Cache.Resource.path)
      resources in
  if path = root_directory then
    (Filename.basename trash_directory) :: filenames
  else filenames
(* END readdir *)

(* fopen *)
let fopen path flags =
  let (path_in_cache, trashed) = get_path_in_cache path in
  let is_read_only_request = List.mem Unix.O_RDONLY flags in

  let check_editable =
    get_resource path_in_cache trashed >>= fun resource ->
    if not is_read_only_request && is_file_read_only resource then
      throw Permission_denied
    else
      SessionM.return ()
  in

  if not is_read_only_request && is_filesystem_read_only () then
    raise Permission_denied
  else begin
    do_request check_editable |> ignore
  end;
  None
(* END fopen *)

(* opendir *)
let opendir path flags =
  let (path_in_cache, trashed) = get_path_in_cache path in
  do_request (get_resource path_in_cache trashed) |> ignore;
  None
(* END opendir *)

(* Update operations *)
let default_save_resource_to_db cache resource file =
  let updated_resource = update_resource_from_file resource file in
  update_cached_resource cache updated_resource

let update_remote_resource path
      ?update_file_in_cache
      ?(save_to_db = default_save_resource_to_db)
      ?(purge_cache = fun cache resource -> ())
      do_remote_update
      retry_update =
  let context = Context.get_ctx () in
  let config = context |. Context.config_lens in
  let cache = context.Context.cache in
  let (path_in_cache, trashed) = get_path_in_cache path in
  let update_file =
    get_resource path_in_cache trashed >>= fun resource ->
    with_try
      (do_remote_update resource)
      (function
           GapiRequest.PreconditionFailed session
         | GapiRequest.Conflict session ->
             Utils.log_with_header "BEGIN: Conflict detected\n%!";
             GapiMonad.SessionM.put session >>
             begin match config.Config.conflict_resolution with
                 Config.ConflictResolutionStrategy.Client ->
                   Utils.log_with_header "Retrying after conflict\n%!";
                   retry_update resource >>= fun file ->
                   Utils.log_with_header "END: Conflict detected: Local changes successfully submitted\n%!";
                   SessionM.return file
               | Config.ConflictResolutionStrategy.Server ->
                   Utils.log_with_header
                     "END: Conflict detected: Keeping server changes\n";
                   Utils.log_with_header
                     "Deleting resource (id=%Ld) from cache\n%!"
                     resource.Cache.Resource.id;
                   Cache.Resource.delete_resource cache resource;
                   throw IO_error
             end
         | e -> throw e) >>= fun file_option ->
    begin match file_option with
        None ->
          purge_cache cache resource
      | Some file ->
          begin match update_file_in_cache with
              None -> ()
            | Some go ->
                if resource.Cache.Resource.state =
                   Cache.Resource.State.Synchronized
                then begin
                  let content_path = Cache.get_content_path cache resource in
                  if Sys.file_exists content_path then
                    go content_path
                end;
          end;
          let refresh_file = refresh_remote_resource resource file in
          let refreshed_file =
            do_request refresh_file |> fst
          in
          save_to_db cache resource refreshed_file
    end;
    SessionM.return ()
  in
  if is_filesystem_read_only () then
    raise Permission_denied
  else
    update_file
(* Update operations *)

(* utime *)
let utime path atime mtime =
  let update =
    let touch resource =
      let remote_id = resource |. Cache.Resource.remote_id |> Option.get in
      Utils.log_with_header "BEGIN: Updating file mtime (remote id=%s, mtime=%f)\n%!"
        remote_id mtime;
      let file_patch = File.empty
            |> File.modifiedTime ^= Netdate.create mtime in
      FilesResource.update
        ~std_params:file_std_params
        ~fileId:remote_id
        file_patch >>= fun patched_file ->
      Utils.log_with_header "END: Updating file mtime (remote id=%s, mtime=%f)\n%!"
        remote_id mtime;
      SessionM.return (Some patched_file)
    in
    update_remote_resource
      ~update_file_in_cache:(
        fun content_path ->
          Unix.utimes content_path atime mtime)
      path
      touch
      touch
  in
  do_request update |> ignore
(* END utime *)

(* read *)
let read path buf offset file_descr =
  let (path_in_cache, trashed) = get_path_in_cache path in
  let config = Context.get_ctx () |. Context.config_lens in
  let large_file_threshold_mb = config |. Config.large_file_threshold_mb in
  let large_file_threshold =
    Int64.mul (Int64.of_int large_file_threshold_mb) mb in

  let request_resource =
    get_resource path_in_cache trashed >>= fun resource ->
    let to_stream = config |. Config.stream_large_files &&
      not (Cache.Resource.is_document resource) &&
      resource.Cache.Resource.state = Cache.Resource.State.ToDownload &&
      (Option.default 0L resource.Cache.Resource.size) >
        large_file_threshold in
    if to_stream then
      with_retry (stream_resource offset buf) resource >>= fun () ->
      SessionM.return ""
    else
      with_retry download_resource resource
  in

  let content_path = do_request request_resource |> fst in
  if content_path <> "" then
    Utils.with_in_channel content_path
      (fun ch ->
         let file_descr = Unix.descr_of_in_channel ch in
         Unix.LargeFile.lseek file_descr offset Unix.SEEK_SET |> ignore;
         Unix_util.read file_descr buf)
  else
    Bigarray.Array1.dim buf
(* END read *)

(* write *)
let write path buf offset file_descr =
  let (path_in_cache, trashed) = get_path_in_cache path in

  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  let write_to_resource =
    get_resource path_in_cache trashed >>= fun resource ->
    with_retry download_resource resource >>= fun content_path ->
    Utils.log_with_header
      "BEGIN: Writing local file (path=%s, trashed=%b)\n%!"
      path_in_cache trashed;
    let bytes =
      Utils.with_out_channel content_path
        (fun ch ->
           let file_descr = Unix.descr_of_out_channel ch in
           Unix.LargeFile.lseek file_descr offset Unix.SEEK_SET |> ignore;
           Unix_util.write file_descr buf) in
    Utils.log_with_header
      "END: Writing local file (path=%s, trashed=%b)\n%!"
      path_in_cache trashed;
    update_cached_resource_state cache
      Cache.Resource.State.ToUpload resource.Cache.Resource.id;
    SessionM.return bytes
  in
  do_request write_to_resource |> fst
(* END write *)

let start_uploading_if_dirty path =
  let (path_in_cache, trashed) = get_path_in_cache path in
  let resource = lookup_resource path_in_cache trashed in
  match resource with
      None ->
        false
    | Some r ->
        if r.Cache.Resource.state == Cache.Resource.State.ToUpload then begin
          let cache = Context.get_cache () in
          update_cached_resource_state cache
            Cache.Resource.State.Uploading r.Cache.Resource.id;
          true
        end else false

let upload resource =
  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  update_cached_resource_state cache
    Cache.Resource.State.Uploading resource.Cache.Resource.id;
  let content_path = Cache.get_content_path cache resource in
  let remote_id = resource |. Cache.Resource.remote_id |> Option.get in
  let file_source =
    GapiMediaResource.create_file_resource content_path in
  let resource_mime_type =
    resource |. Cache.Resource.mime_type |> Option.get in
  let content_type = file_source |. GapiMediaResource.content_type in
  (* Workaround to set the correct MIME type *)
  let mime_type =
    if resource_mime_type <> "" then resource_mime_type
    else content_type in
  let file_source = file_source
    |> GapiMediaResource.content_type ^= mime_type in
  let media_source =
    if file_source.GapiMediaResource.content_length = 0L then None
    else Some file_source in
  Utils.log_with_header
    "BEGIN: Uploading file (id=%Ld, path=%s, cache path=%s, content type=%s).\n%!"
    resource.Cache.Resource.id resource.Cache.Resource.path content_path mime_type;
  let file_patch = File.empty |> File.modifiedTime ^= GapiDate.now () in
  FilesResource.update
    ~std_params:file_std_params
    ?media_source
    ~fileId:remote_id
    file_patch >>= fun updated_file ->
  let updated_resource =
    update_resource_from_file resource updated_file in
  refresh_remote_resource updated_resource updated_file >>= fun file ->
  Utils.log_with_header
    "END: Uploading file (id=%Ld, path=%s, cache path=%s, content type=%s).\n%!"
    resource.Cache.Resource.id resource.Cache.Resource.path content_path mime_type;
  let reloaded_resource =
    Cache.Resource.select_resource_with_remote_id cache file.File.id in
  let resource = Option.default resource reloaded_resource in
  let state =
    match resource.Cache.Resource.state with
        Cache.Resource.State.Uploading ->
          Some Cache.Resource.State.Synchronized
      | _ -> None in
  let updated_resource =
    update_resource_from_file ?state resource file in
  update_cached_resource cache updated_resource;
  let metadata = context.Context.metadata |> Option.get in
  let config = context |. Context.config_lens in
  Option.may
    (fun file_size ->
       shrink_cache
         file_size
         config.Config.max_cache_size_mb
         metadata
         cache)
    updated_resource.Cache.Resource.size;
  SessionM.return ()

let upload_with_retry path =
  let try_upload resource =
    try_with_default (upload resource)
  in
  let (path_in_cache, trashed) = get_path_in_cache path in
  get_resource path_in_cache trashed >>= fun resource ->
  with_retry try_upload resource

let upload_if_dirty path =
  if start_uploading_if_dirty path then
    let config = Context.get_ctx () |. Context.config_lens in
    if config.Config.async_upload then begin
      async_do_request (upload_with_retry path)
    end else begin
      do_request (upload_with_retry path) |> ignore
    end

(* flush *)
let flush path file_descr =
  upload_if_dirty path

(* fsync *)
let fsync path ds file_descr =
  upload_if_dirty path

(* release *)
let release path flags hnd =
  upload_if_dirty path

(* Create resources *)
let create_remote_resource ?link_target is_folder path mode =
  let (path_in_cache, trashed) = get_path_in_cache path in
  if trashed then raise Permission_denied;

  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  let parent_path = Filename.dirname path_in_cache in
  let create_file =
    get_resource parent_path trashed >>= fun parent_resource ->
    let parent_id =
      parent_resource |. Cache.Resource.remote_id |> Option.get
    in
    let name = Filename.basename path_in_cache in
    let mimeType =
      if is_folder
      then folder_mime_type
      else Mime.map_filename_to_mime_type name in
    let appProperties = [Cache.Resource.mode_to_app_property mode] in
    let appProperties = match link_target with
        None -> appProperties
      | Some link ->
          if String.length link > max_link_target_length then
            raise Invalid_operation
          else
            Cache.Resource.link_target_to_app_property link :: appProperties in
    let file = {
      File.empty with
          File.name;
          parents = [parent_id];
          mimeType;
          appProperties;
    } in
    Utils.log_with_header
      "BEGIN: Creating %s (path=%s, trashed=%b) on server\n%!"
      (if is_folder then "folder" else "file") path_in_cache trashed;
    FilesResource.create
      ~std_params:file_std_params
      file >>= fun created_file ->
    SessionM.get >>= fun session ->
    Utils.log_with_header
      "END: Creating file/folder (path=%s, trashed=%b) on server\n%!"
      path_in_cache trashed;
    let new_resource = create_resource path_in_cache in
    Utils.log_with_header
      "BEGIN: Deleting 'NotFound' resources (path=%s) from cache\n%!"
      path_in_cache;
    Cache.Resource.delete_not_found_resource_with_path cache path_in_cache;
    Utils.log_with_header
      "END: Deleting 'NotFound' resources (path=%s) from cache\n%!"
      path_in_cache;
    let inserted =
      insert_resource_into_cache
        ~state:Cache.Resource.State.Synchronized
        cache new_resource created_file in
    SessionM.return inserted
  in
  if is_filesystem_read_only () then
    raise Permission_denied
  else
    do_request create_file |> ignore
(* END Create resources *)

(* mknod *)
let mknod path mode =
  create_remote_resource false path mode
(* END mknod *)

(* mkdir *)
let mkdir path mode =
  create_remote_resource true path mode
(* END mkdir *)

(* Check if a folder is empty or not *)
let check_if_empty remote_id is_folder trashed =
  if is_folder then begin
    let q = Printf.sprintf "'%s' in parents and trashed = %b"
        remote_id trashed in
    let std_params =
      { GapiService.StandardParameters.default with
            GapiService.StandardParameters.fields = "files(id)"
      }
    in
    FilesResource.list
      ~std_params
      ~pageSize:1
      ~q >>= fun children ->
    if children.FileList.files = [] then begin
      Utils.log_with_header "Folder (remote id=%s) is empty\n%!" remote_id;
      SessionM.return ()
    end else begin
      Utils.log_with_header "Folder (remote id=%s) is not empty\n%!" remote_id;
      raise Directory_not_empty
    end
  end else
    SessionM.return ()

(* Delete (trash) resources *)
let trash_resource is_folder trashed path =
  if trashed then raise Permission_denied;

  let trash resource =
    let remote_id = resource |. Cache.Resource.remote_id |> Option.get in
    check_if_empty remote_id is_folder trashed >>= fun () ->
    Utils.log_with_header "BEGIN: Trashing file (remote id=%s)\n%!" remote_id;
    let file_patch =
      { File.empty with
            File.trashed = true;
      }
    in
    FilesResource.update
      ~std_params:file_std_params
      ~fileId:remote_id
      file_patch >>= fun trashed_file ->
    Utils.log_with_header "END: Trashing file (remote id=%s)\n%!" remote_id;
    SessionM.return (Some trashed_file)
  in
  update_remote_resource
    ~save_to_db:(
      fun cache resource file ->
        let updated_resource = resource
          |> Cache.Resource.trashed ^= Some true in
        update_cached_resource cache updated_resource;
        Cache.Resource.invalidate_trash_bin cache;
        if is_folder then begin
          let (path_in_cache, _) = get_path_in_cache path in
          Utils.log_with_header
            "BEGiN: Trashing folder old content (path=%s)\n%!"
            path_in_cache;
          Cache.Resource.trash_all_with_parent_path cache path_in_cache;
          Utils.log_with_header
            "END: Trashing folder old content (path=%s)\n%!"
            path_in_cache;
        end)
    path
    trash
    trash

(* Permanently delete resources *)
let delete_resource is_folder path =
  let (path_in_cache, trashed) = get_path_in_cache path in

  let delete resource =
    let remote_id = resource |. Cache.Resource.remote_id |> Option.get in
    check_if_empty remote_id is_folder trashed >>= fun () ->
    Utils.log_with_header
      "BEGiN: Permanently deleting file (remote id=%s)\n%!"
      remote_id;
    FilesResource.delete
      ~std_params:file_std_params
      ~fileId:remote_id >>= fun () ->
    Utils.log_with_header
      "END: Permanently deleting file (remote id=%s)\n%!"
      remote_id;
    SessionM.return None
  in
  update_remote_resource
    ~purge_cache:(
      fun cache resource ->
        Cache.Resource.delete_resource cache resource;
        if is_folder then begin
          Utils.log_with_header
            "BEGiN: Deleting folder old content (path=%s, trashed=%b) from cache\n%!"
            path_in_cache trashed;
          Cache.Resource.delete_all_with_parent_path
            cache path_in_cache trashed;
          Utils.log_with_header
            "END: Deleting folder old content (path=%s, trashed=%b) from cache\n%!"
            path_in_cache trashed;
        end)
    path
    delete
    delete

let delete_remote_resource is_folder path =
  let (_, trashed) = get_path_in_cache path in

  let context = Context.get_ctx () in
  let config = context |. Context.config_lens in
  let trash_or_delete_file =
    if context.Context.skip_trash ||
        trashed && config.Config.delete_forever_in_trash_folder then
      delete_resource is_folder path
    else
      trash_resource is_folder trashed path
  in
  do_request trash_or_delete_file |> ignore
(* END Delete (trash) resources *)

(* unlink *)
let unlink path =
  delete_remote_resource false path
(* END unlink *)

(* rmdir *)
let rmdir path =
  delete_remote_resource true path
(* END rmdir *)

(* rename *)
let rename path new_path =
  let (path_in_cache, trashed) = get_path_in_cache path in
  let (new_path_in_cache, target_trashed) = get_path_in_cache new_path in
  if trashed <> target_trashed then raise Permission_denied;

  let old_parent_path = Filename.dirname path_in_cache in
  let new_parent_path = Filename.dirname new_path_in_cache in
  let old_name = Filename.basename path_in_cache in
  let new_name = Filename.basename new_path_in_cache in
  let delete_target_path =
    let trash_target_path () =
      get_resource new_path_in_cache target_trashed >>= fun new_resource ->
      trash_resource
        (Cache.Resource.is_folder new_resource) target_trashed new_path
    in
    begin if not target_trashed &&
       not (Context.get_ctx ()
        |. Context.config_lens
        |. Config.keep_duplicates) then
      with_try
        (trash_target_path ())
        (function
             File_not_found -> SessionM.return ()
           | e -> throw e)
    else
      SessionM.return ()
    end
  in
  let update =
    let rename_file resource =
      let remote_id = resource |. Cache.Resource.remote_id |> Option.get in
      if old_name <> new_name then begin
        delete_target_path >>= fun () ->
        Utils.log_with_header "BEGIN: Renaming file (remote id=%s) from %s to %s\n%!"
          remote_id old_name new_name;
        let file_patch =
          { File.empty with
                File.name = new_name;
          } in
        FilesResource.update
          ~std_params:file_std_params
          ~fileId:remote_id
          file_patch >>= fun patched_file ->
        Utils.log_with_header "END: Renaming file (remote id=%s) from %s to %s\n%!"
          remote_id old_name new_name;
        SessionM.return (Some patched_file)
      end else begin
        SessionM.return None
      end
    in
    let move resource =
      let remote_id = resource |. Cache.Resource.remote_id |> Option.get in
      begin if old_parent_path <> new_parent_path then begin
        delete_target_path >>= fun () ->
        Utils.log_with_header "BEGIN: Moving file (remote id=%s) from %s to %s\n%!"
          remote_id old_parent_path new_parent_path;
        get_resource
          new_parent_path target_trashed >>= fun new_parent_resource ->
        let new_parent_id =
          new_parent_resource |. Cache.Resource.remote_id |> Option.get
        in
        let file_patch = File.empty
          |> File.parents ^= [new_parent_id] in
        FilesResource.update
          ~std_params:file_std_params
          ~fileId:remote_id
          file_patch >>= fun patched_file ->
        Utils.log_with_header "END: Moving file (remote id=%s) from %s to %s\n%!"
          remote_id old_parent_path new_parent_path;
        SessionM.return (Some patched_file)
      end else
        SessionM.return None
      end >>= fun moved_file ->
      rename_file resource >>= fun renamed_file ->
      if Option.is_some renamed_file
      then SessionM.return renamed_file
      else SessionM.return moved_file
    in
    update_remote_resource
      path
      move
      rename_file
      ~save_to_db:(
        fun cache resource file ->
          let updated_resource =
            update_resource_from_file resource file in
          let resource_with_new_path =
            updated_resource
              |> Cache.Resource.path ^= new_path_in_cache
              |> Cache.Resource.parent_path ^= new_parent_path
              |> Cache.Resource.trashed ^= Some target_trashed
              |> Cache.Resource.state ^=
                (if Cache.Resource.is_folder resource ||
                    Cache.Resource.is_document resource
                 then Cache.Resource.State.ToDownload
                 else Cache.Resource.State.Synchronized) in
          let resource_to_save =
            if new_parent_path <> old_parent_path &&
               new_name = old_name then
              let (path, local_name) =
                recompute_path resource_with_new_path new_name in
              let parent_path = Filename.dirname path in
              resource_with_new_path
                |> Cache.Resource.path ^= path
                |> Cache.Resource.local_name ^= local_name
                |> Cache.Resource.parent_path ^= parent_path
                |> Cache.Resource.parent_path_hash ^= get_path_hash parent_path
            else resource_with_new_path
          in
          update_cached_resource cache resource_to_save;
          Utils.log_with_header
            "BEGIN: Deleting 'NotFound' resources (path=%s) from cache\n%!"
            new_path_in_cache;
          Cache.Resource.delete_not_found_resource_with_path
            cache new_path_in_cache;
          Utils.log_with_header
            "END: Deleting 'NotFound' resources (path=%s) from cache\n%!"
            new_path_in_cache;
          if Cache.Resource.is_folder resource then begin
            Utils.log_with_header
              "BEGIN: Deleting folder old content (path=%s, trashed=%b) from cache\n%!"
              path_in_cache trashed;
            Cache.Resource.delete_all_with_parent_path
              cache path_in_cache trashed;
            Utils.log_with_header
              "END: Deleting folder old content (path=%s, trashed=%b) from cache\n%!"
              path_in_cache trashed;
          end)
  in
  do_request update |> ignore
(* END rename *)

(* truncate *)
let truncate path size =
  let (path_in_cache, trashed) = get_path_in_cache path in
  let truncate_resource =
    get_resource path_in_cache trashed >>= fun resource ->
    with_retry download_resource resource >>= fun content_path ->
    let remote_id = resource |. Cache.Resource.remote_id |> Option.get in
    Utils.log_with_header "BEGIN: Truncating file (remote id=%s)\n%!" remote_id;
    let context = Context.get_ctx () in
    let cache = context.Context.cache in
    let metadata = context.Context.metadata |> Option.get in
    let config = context |. Context.config_lens in
    let updated_resource = resource
      |> Cache.Resource.size ^= Some size
      |> Cache.Resource.state ^= Cache.Resource.State.ToUpload in
    update_cached_resource cache updated_resource;
    shrink_cache
      (Int64.sub size (Option.default 0L resource.Cache.Resource.size))
      config.Config.max_cache_size_mb
      metadata cache;
    Unix.LargeFile.truncate content_path size;
    Utils.log_with_header "END: Truncating file (remote id=%s)\n%!" remote_id;
    SessionM.return ()
  in
  do_request truncate_resource |> ignore
(* END truncate *)

(* chmod *)
let chmod path mode =
  let update =
    let chmod resource =
      let remote_id = resource |. Cache.Resource.remote_id |> Option.get in
      Utils.log_with_header "BEGIN: Updating mode (remote id=%s, mode=%o)\n%!"
        remote_id mode;
      let file_patch = File.empty
        |> File.appProperties ^= [Cache.Resource.mode_to_app_property mode] in
      FilesResource.update
        ~std_params:file_std_params
        ~fileId:remote_id
        file_patch >>= fun patched_file ->
      Utils.log_with_header "END: Updating mode (remote id=%s, mode=%o)\n%!"
        remote_id mode;
      SessionM.return (Some patched_file)
    in
    update_remote_resource
      path
      chmod
      chmod
  in
  do_request update |> ignore
(* END chmod *)

(* chown *)
let chown path uid gid =
  let update =
    let chown resource =
      let remote_id = resource |. Cache.Resource.remote_id |> Option.get in
      Utils.log_with_header "BEGIN: Updating owner (remote id=%s, uid=%d gid=%d)\n%!"
        remote_id uid gid;
      let file_patch = File.empty
        |> File.appProperties ^= [
             Cache.Resource.uid_to_app_property uid;
             Cache.Resource.gid_to_app_property gid;
           ] in
      FilesResource.update
        ~std_params:file_std_params
        ~fileId:remote_id
        file_patch >>= fun patched_file ->
      Utils.log_with_header "End: Updating owner (remote id=%s, uid=%d gid=%d)\n%!"
        remote_id uid gid;
      SessionM.return (Some patched_file)
    in
    update_remote_resource
      path
      chown
      chown
  in
  do_request update |> ignore
(* END chown *)

(* getxattr *)
let get_xattr path name =
  let (path_in_cache, trashed) = get_path_in_cache path in
  let fetch_xattr =
    get_resource path_in_cache trashed >>= fun resource ->
    let xattrs = Cache.Resource.parse_xattrs resource.Cache.Resource.xattrs in
    let value =
      try
        List.assoc name xattrs
      with Not_found -> raise No_attribute
    in
    SessionM.return value
  in
  do_request fetch_xattr |> fst
(* END getxattr *)

(* setxattr *)
let set_xattr path name value xflags =
  let update =
    let setxattr resource =
      let remote_id = resource |. Cache.Resource.remote_id |> Option.get in
      Utils.log_with_header "BEGIN: Setting xattr (remote id=%s, name=%s value=%s xflags=%s)\n%!"
        remote_id name value (Utils.xattr_flags_to_string xflags);
      let xattrs = Cache.Resource.parse_xattrs resource.Cache.Resource.xattrs in
      let existing = List.mem_assoc name xattrs in
      begin match xflags with
          Fuse.CREATE -> if existing then raise Existing_attribute
        | Fuse.REPLACE -> if not existing then raise No_attribute
        | Fuse.AUTO -> ()
      end;
      let attribute_length = (String.length name) + (String.length value) in
      if attribute_length > max_attribute_length then raise Invalid_operation;
      let file_patch = File.empty
        |> File.appProperties ^= [
             Cache.Resource.xattr_to_app_property name value;
           ] in
      FilesResource.update
        ~std_params:file_std_params
        ~fileId:remote_id
        file_patch >>= fun patched_file ->
      Utils.log_with_header "END: Setting xattr (remote id=%s, name=%s value=%s xflags=%s)\n%!"
        remote_id name value (Utils.xattr_flags_to_string xflags);
      SessionM.return (Some patched_file)
    in
    update_remote_resource
      path
      setxattr
      setxattr
  in
  do_request update |> ignore
(* END setxattr *)

(* listxattr *)
let list_xattr path =
  let (path_in_cache, trashed) = get_path_in_cache path in
  let fetch_xattrs =
    get_resource path_in_cache trashed >>= fun resource ->
    let xattrs = Cache.Resource.parse_xattrs resource.Cache.Resource.xattrs in
    let keys = List.map (fun (n, _) -> n) xattrs in
    SessionM.return keys
  in
  do_request fetch_xattrs |> fst
(* END listxattr *)

(* removexattr *)
let remove_xattr path name =
  let update =
    let removexattr resource =
      let remote_id = resource |. Cache.Resource.remote_id |> Option.get in
      Utils.log_with_header "BEGIN: Removing xattr (remote id=%s, name=%s)\n%!"
        remote_id name;
      let xattrs = Cache.Resource.parse_xattrs resource.Cache.Resource.xattrs in
      let existing = List.mem_assoc name xattrs in
      if not existing then raise No_attribute;
      let file_patch = File.empty
        |> File.appProperties ^= [
             Cache.Resource.xattr_no_value_to_app_property name;
           ] in
      FilesResource.update
        ~std_params:file_std_params
        ~fileId:remote_id
        file_patch >>= fun patched_file ->
      Utils.log_with_header "END: Removing xattr (remote id=%s, name=%s)\n%!"
        remote_id name;
      SessionM.return (Some patched_file)
    in
    update_remote_resource
      path
      removexattr
      removexattr
  in
  do_request update |> ignore
(* END removexattr *)

(* readlink *)
let read_link path =
  let (path_in_cache, trashed) = get_path_in_cache path in
  let fetch_link_target =
    get_resource path_in_cache trashed >>= fun resource ->
    let link_target =
      match resource.Cache.Resource.link_target with
          None -> raise Invalid_operation
        | Some link -> link
    in
    SessionM.return link_target
  in
  do_request fetch_link_target |> fst
(* END readlink *)

(* symlink *)
let symlink target linkpath =
  create_remote_resource ~link_target:target false linkpath 0o120777
(* END symlink *)

