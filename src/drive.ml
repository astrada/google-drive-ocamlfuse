open GapiUtils.Infix
open GapiLens.Infix
open GapiMonad
open GapiMonad.SessionM.Infix
open GapiDriveV2Model
open GapiDriveV2Service

exception File_not_found
exception Permission_denied
exception Resource_busy

let folder_mime_type = "application/vnd.google-apps.folder"
let file_fields =
  "createdDate,downloadUrl,editable,etag,explicitlyTrashed,exportLinks,\
   fileExtension,fileSize,id,labels,lastViewedByMeDate,md5Checksum,mimeType,\
   modifiedDate,parents,title"
let file_std_params =
  { GapiService.StandardParameters.default with
        GapiService.StandardParameters.fields = file_fields
  }
let file_list_std_params =
  { GapiService.StandardParameters.default with
        GapiService.StandardParameters.fields =
          "items(" ^ file_fields ^ "),nextPageToken"
  }

let do_request = Oauth2.do_request

let root_directory = "/"
let trash_directory = "/.Trash"
let trash_directory_name_length = String.length trash_directory
let trash_directory_base_path = "/.Trash/"
let f_bsize = 4096L
let change_id_limit = 50L

(* Utilities *)
let chars_blacklist_regexp = Str.regexp ("[/\000]")
let clean_filename title = Str.global_replace chars_blacklist_regexp "_" title

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

let disambiguate_title title is_document filename_table  =
  let rec find_first_unique_filename filename copy_number =
    let new_candidate =
      if is_document then
        Printf.sprintf "%s (%d)" filename copy_number
      else
        let (base_name, extension) = split_filename filename in
        Printf.sprintf "%s (%d)%s" base_name copy_number extension
    in
    Utils.log_message "Checking: %s...%!" new_candidate;
    if not (Hashtbl.mem filename_table new_candidate) then begin
      Utils.log_message "OK\n%!";
      (new_candidate, copy_number)
    end else begin
      Utils.log_message "KO\n%!";
      find_first_unique_filename filename (copy_number + 1)
    end
  in
  let filename = clean_filename title in
  if Hashtbl.mem filename_table filename then begin
    Utils.log_message "Filename collision detected: %s\n%!" filename;
    let last_copy_number = Hashtbl.find filename_table filename in
    let (unique_filename, copy_number) =
      find_first_unique_filename filename (last_copy_number + 1) in
    Hashtbl.replace filename_table filename copy_number;
    unique_filename
  end else begin
    Utils.log_message "Filename (unused): %s\n%!" filename;
    Hashtbl.add filename_table filename 0;
    filename
  end

let is_in_trash_directory path =
  if path = trash_directory then false
  else ExtString.String.starts_with path trash_directory_base_path

let normalize_path path =
  if path = root_directory then
    (root_directory, false)
  else if path = trash_directory then
    (root_directory, true)
  else if is_in_trash_directory path then
    let normalized_path = Str.string_after path trash_directory_name_length in
    (normalized_path, true)
  else
    (path, false)

(* Used to do a try/with on a monadic f: state parameter s is eta-expanded,
 * otherwise the try/with will be ignored because f is only partially applied
 *)
let with_try f exception_handler s =
  try
    f s
  with e ->
    exception_handler e s

(* Resource cache *)
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
       let title = Option.get resource.Cache.Resource.title in
       let clean_title = clean_filename title in
       let filename = Filename.basename resource.Cache.Resource.path in
       if clean_title <> filename then begin
         let copy_number =
           try
             Hashtbl.find filename_table clean_title
           with Not_found -> 0
         in
         Hashtbl.replace filename_table clean_title copy_number;
       end;
       Hashtbl.add filename_table filename 0;
       Hashtbl.add remote_id_table
         (Option.get resource.Cache.Resource.remote_id) resource)
    resources;
  (filename_table, remote_id_table)

let create_resource path change_id =
  { Cache.Resource.id = 0L;
    etag = None;
    remote_id = None;
    title = None;
    mime_type = None;
    created_date = None;
    modified_date = None;
    last_viewed_by_me_date = None;
    parent_remote_ids = None;
    download_url = None;
    export_links = None;
    file_extension = None;
    md5_checksum = None;
    file_size = None;
    editable = None;
    trashed = None;
    parent_path = Filename.dirname path;
    path;
    state = Cache.Resource.State.ToDownload;
    change_id;
    last_update = Unix.gettimeofday ();
  }

let create_root_resource root_folder_id change_id trashed =
  let resource = create_resource root_directory change_id in
    { resource with
          Cache.Resource.remote_id = Some root_folder_id;
          mime_type = Some "application/vnd.google-apps.folder";
          file_size = Some 0L;
          parent_path = "";
          trashed = Some trashed;
    }

let recompute_path resource title =
  (* TODO: make an optimized version of build_resource_tables that
   * doesn't create resource table (useful for large directories). *)
  let (filename_table, _) =
    build_resource_tables
      resource.Cache.Resource.parent_path
      (Option.default false resource.Cache.Resource.trashed) in
  let clean_title = clean_filename title in
  let is_document = Cache.Resource.is_document resource in
  let filename = disambiguate_title clean_title is_document filename_table in
  Filename.concat resource.Cache.Resource.parent_path filename

let update_resource_from_file ?state resource file =
  let largest_change_id =
    Context.get_ctx () |. Context.largest_change_id_lens in
  let path =
    match resource.Cache.Resource.title with
        Some cached_title ->
          if cached_title <> file.File.title then
            recompute_path resource file.File.title
          else resource.Cache.Resource.path
      | None -> resource.Cache.Resource.path
  in
    { resource with
          Cache.Resource.etag = Some file.File.etag;
          remote_id = Some file.File.id;
          title = Some file.File.title;
          mime_type = Some file.File.mimeType;
          created_date = Some (Netdate.since_epoch file.File.createdDate);
          modified_date = Some (Netdate.since_epoch file.File.modifiedDate);
          last_viewed_by_me_date =
            Some (Netdate.since_epoch file.File.lastViewedByMeDate);
          parent_remote_ids =
            Some (Cache.Resource.render_parent_remote_ids file.File.parents);
          download_url = Some file.File.downloadUrl;
          export_links =
            Some (Cache.Resource.render_export_links file.File.exportLinks);
          file_extension = Some file.File.fileExtension;
          md5_checksum = Some file.File.md5Checksum;
          file_size = Some file.File.fileSize;
          editable = Some file.File.editable;
          trashed = Some file.File.labels.File.Labels.trashed;
          change_id = largest_change_id;
          last_update = Unix.gettimeofday ();
          path;
          state =
            if file.File.labels.File.Labels.restricted then
              Cache.Resource.State.Restricted
            else Option.default resource.Cache.Resource.state state;
    }

let get_parent_resource_ids file =
  List.map
    (fun parent -> parent.ParentReference.id)
    file.File.parents

let insert_resource_into_cache ?state cache resource file =
  let resource = update_resource_from_file ?state resource file in
  Utils.log_message "Saving resource to db...%!";
  let inserted = Cache.Resource.insert_resource cache resource in
  Utils.log_message "done\n%!";
  inserted

let update_cached_resource cache resource =
  Utils.log_message "Updating resource in db (id=%Ld)...%!"
    resource.Cache.Resource.id;
  Cache.Resource.update_resource cache resource;
  Utils.log_message "done\n%!"

let lookup_resource path trashed =
  Utils.log_message "Loading resource %s (trashed=%b) from db...%!"
    path trashed;
  let cache = Context.get_cache () in
  let resource =
    Cache.Resource.select_resource_with_path cache path trashed
  in
    if Option.is_none resource then
      Utils.log_message "not found\n%!"
    else
      Utils.log_message "found\n%!";
    resource

let get_root_resource trashed =
  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  let metadata = context.Context.metadata |> Option.get in
    match lookup_resource root_directory trashed with
        None ->
          let root_resource =
            create_root_resource
              metadata.Cache.Metadata.root_folder_id
              metadata.Cache.Metadata.largest_change_id
              trashed in
            Utils.log_message "Saving root resource to db...%!";
            let inserted =
              Cache.Resource.insert_resource cache root_resource in
            Utils.log_message "done\n%!";
            inserted
      | Some resource -> resource
(* END Resource cache *)

(* Metadata *)
let get_metadata () =
  let request_metadata last_change_id etag =
    let std_params =
      { GapiService.StandardParameters.default with
            GapiService.StandardParameters.fields =
              "etag,largestChangeId,name,permissionId,quotaBytesTotal,\
               quotaBytesUsed,remainingChangeIds,rootFolderId"
      } in
    let startChangeId = Int64.succ last_change_id in
    AboutResource.get
      ~std_params
      ?etag
      ~startChangeId
      ~maxChangeIdCount:500L >>= fun about ->
    let metadata = {
      Cache.Metadata.etag = about.About.etag;
      username = about.About.name;
      quota_bytes_total = about.About.quotaBytesTotal;
      quota_bytes_used =
        Int64.add about.About.quotaBytesUsed about.About.quotaBytesUsedInTrash;
      largest_change_id = about.About.largestChangeId;
      remaining_change_ids = about.About.remainingChangeIds;
      root_folder_id = about.About.rootFolderId;
      permission_id = about.About.permissionId;
      last_update = Unix.gettimeofday ();
    } in
    SessionM.return metadata
  in

  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  let metadata =
    if Option.is_none context.Context.metadata then begin
      Utils.log_message "Loading metadata from db...%!";
      let db_metadata = Cache.Metadata.select_metadata context.Context.cache in
        Context.update_ctx (Context.metadata ^= db_metadata);
        db_metadata
    end else begin
      Utils.log_message "Getting metadata from context...%!";
      context.Context.metadata
    end in

  let update_resource_cache last_change_id new_metadata =
    let get_all_changes startChangeId =
      let rec loop ?pageToken accu =
        let std_params =
          { GapiService.StandardParameters.default with
                GapiService.StandardParameters.fields =
                  "items(deleted,file,fileId),nextPageToken"
          } in
        ChangesResource.list
          ~std_params
          ~includeDeleted:true
          ~startChangeId
          ?pageToken >>= fun change_list ->
        let changes = change_list.ChangeList.items @ accu in
        if change_list.ChangeList.nextPageToken = "" then
          SessionM.return changes
        else
          loop ~pageToken:change_list.ChangeList.nextPageToken changes
      in
        loop []
    in

    let request_changes =
      Utils.log_message "Getting changes from server...%!";
      let startChangeId = Int64.succ last_change_id in
      get_all_changes startChangeId >>= fun changes ->
      Utils.log_message "done\n%!";
      SessionM.return changes
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
                match get_parent_resource_ids file with
                    [] -> [context |. Context.root_folder_id_lens]
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

    let remaining_change_ids =
      new_metadata.Cache.Metadata.remaining_change_ids in
    if remaining_change_ids = 0L then
      Utils.log_message "no need to update resource cache\n%!"
    else if remaining_change_ids > change_id_limit then
      Utils.log_message
        "too many changes (remaining_change_ids=%Ld)\n%!"
        remaining_change_ids
    else match metadata with
        None -> ()
      | Some old_metadata ->
          let change_id =
            new_metadata.Cache.Metadata.largest_change_id in
          let changes = do_request request_changes |> fst in
          let update_resource_cache filter_changes map_change update_cache =
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

          Utils.log_message "Updating resource cache...%!";
          update_resource_cache
            (fun change ->
               not change.Change.deleted &&
               not change.Change.file.File.labels.File.Labels.trashed)
            get_ids_to_update
            (fun cache ids ->
               Cache.Resource.invalidate_resources cache ids change_id);
          Utils.log_message "done\nUpdating trashed resources...%!";
          update_resource_cache
            (fun change -> change.Change.file.File.labels.File.Labels.trashed)
            get_file_id_from_change
            (fun cache resources ->
               Cache.Resource.trash_resources cache resources change_id);
          Utils.log_message "done\nRemoving deleted resources...%!";
          update_resource_cache
            (fun change -> change.Change.deleted)
            get_file_id_from_change
            Cache.delete_resources;
          if List.length changes > 0 then begin
            Utils.log_message "done\nInvalidating trash bin resource...%!";
            Cache.Resource.invalidate_trash_bin cache change_id;
          end;
          Utils.log_message "done\n%!";
  in

  let refresh_metadata () =
    let last_change_id =
      Option.map_default
        Cache.Metadata.largest_change_id.GapiLens.get 0L metadata in
    let etag = Option.map Cache.Metadata.etag.GapiLens.get metadata in
    Utils.log_message "Refreshing metadata...%!";
    let updated_metadata =
      try
        do_request (request_metadata last_change_id etag) |> fst
      with GapiRequest.NotModified _ ->
        Utils.log_message "not modified...%!";
        let m = Option.get metadata in
          m |> Cache.Metadata.last_update ^= Unix.gettimeofday ()
    in
    Utils.log_message "done\nUpdating metadata in db...%!";
    Cache.Metadata.insert_metadata context.Context.cache updated_metadata;
    Utils.log_message "done\nUpdating context...%!";
    Context.update_ctx (Context.metadata ^= Some updated_metadata);
    Utils.log_message "done\n%!";
    update_resource_cache last_change_id updated_metadata;
    updated_metadata
  in

  match metadata with
      None ->
        Utils.log_message "not found\n%!";
        refresh_metadata ()
    | Some m ->
        let metadata_cache_time =
          context |. Context.config_lens |. Config.metadata_cache_time
        in
        if Cache.Metadata.is_valid metadata_cache_time m then begin
          Utils.log_message "valid\n%!"; m
        end else begin
          Utils.log_message "not valid\n%!";
          refresh_metadata ()
        end

let statfs () =
  let metadata = get_metadata () in
  let f_blocks = Int64.div metadata.Cache.Metadata.quota_bytes_total f_bsize in
  let free_bytes = Int64.sub
                     metadata.Cache.Metadata.quota_bytes_total
                     metadata.Cache.Metadata.quota_bytes_used in
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
let get_file_from_server parent_folder_id title trashed =
  Utils.log_message "Getting resource %s (in folder %s) from server...%!"
    title parent_folder_id;
  let q =
    Printf.sprintf "title = '%s' and '%s' in parents and trashed = %b"
      title parent_folder_id trashed in
  FilesResource.list
    ~std_params:file_list_std_params
    ~q
    ~maxResults:1 >>= fun file_list ->
  Utils.log_message "done\n%!";
  let files = file_list.FileList.items in
  if List.length files = 0 then
    SessionM.return None
  else
    let file = files |. GapiLens.head in
    SessionM.return (Some file)

let get_resource_from_server parent_folder_id title new_resource trashed cache =
  get_file_from_server parent_folder_id title trashed >>= fun file ->
  match file with
      None ->
        Utils.log_message "Saving not found resource to db...%!";
        let resource = new_resource
          |> Cache.Resource.trashed ^= Some trashed
          |> Cache.Resource.state ^= Cache.Resource.State.NotFound in
        let inserted = Cache.Resource.insert_resource cache resource in
        Utils.log_message "done\n%!";
        SessionM.return inserted
    | Some entry ->
        let inserted =
          insert_resource_into_cache cache new_resource entry in
        SessionM.return inserted

let check_resource_in_cache cache path trashed =
  let change_id = Context.get_ctx () |. Context.largest_change_id_lens in
    match lookup_resource path trashed with
        None -> false
      | Some resource ->
          if Cache.Resource.is_valid resource change_id then
            if Cache.Resource.is_folder resource then
              resource.Cache.Resource.state = Cache.Resource.State.InSync
            else true
          else false

let rec get_folder_id path trashed =
  if path = root_directory then
    let root_folder_id =
      get_metadata () |. Cache.Metadata.root_folder_id in
    SessionM.return root_folder_id
  else
    get_resource path trashed >>= fun resource ->
    let remote_id = resource |. Cache.Resource.remote_id |> Option.get in
    SessionM.return remote_id
and get_resource path trashed =
  let change_id =
    get_metadata () |. Cache.Metadata.largest_change_id in

  let get_new_resource cache =
    let parent_path = Filename.dirname path in
      if check_resource_in_cache cache parent_path trashed then begin
        raise File_not_found
      end else begin
        let new_resource = create_resource path change_id in
        let title = Filename.basename path in
        get_folder_id
          new_resource.Cache.Resource.parent_path
          trashed >>= fun parent_folder_id ->
        get_resource_from_server
          parent_folder_id title new_resource trashed cache >>= fun resource ->
        SessionM.return resource
      end
  in

  let refresh_resource resource cache =
    begin if Option.is_some resource.Cache.Resource.remote_id then begin
      let remote_id = resource.Cache.Resource.remote_id |> Option.get in
      let etag = resource.Cache.Resource.etag in
      Utils.log_message "Getting file from server (id=%s)...%!" remote_id;
      FilesResource.get
        ~std_params:file_std_params
        ?etag
        ~fileId:remote_id >>= fun file ->
      SessionM.return (Some file)
    end else
      SessionM.return None
    end >>= fun refreshed_file ->
    Utils.log_message "done\n%!";
    match refreshed_file with
        None ->
          Cache.Resource.delete_resource cache resource;
          get_new_resource cache
      | Some file ->
          let updated_resource = update_resource_from_file resource file in
          update_cached_resource cache updated_resource;
          Utils.log_message "done\n%!";
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
          if Cache.Resource.is_valid resource change_id then
            SessionM.return resource
          else
            with_try
              (refresh_resource resource cache)
              (function
                   GapiRequest.NotModified session ->
                     Utils.log_message
                       "Resource (remote_id=%s) not modified\n%!"
                       (resource.Cache.Resource.remote_id |> Option.get);
                     SessionM.put session >>= fun () ->
                     let updated_resource = resource
                       |> Cache.Resource.last_update ^= Unix.gettimeofday () in
                     update_cached_resource cache updated_resource;
                     SessionM.return updated_resource
                 | e -> raise e)
    end >>= fun resource ->
    begin match resource.Cache.Resource.state with
        Cache.Resource.State.NotFound ->
          raise File_not_found 
      | _ ->
          SessionM.return resource
    end

let check_md5_checksum resource =
  let path = resource.Cache.Resource.path in
  let md5_checksum = Option.default "" resource.Cache.Resource.md5_checksum in
  if md5_checksum <> "" then begin
    Utils.log_message "Checking MD5 checksum (path=%s, hash=%s)...\n%!"
      path md5_checksum;
    if Sys.file_exists path then begin
      let md5 = Cryptokit.Hash.md5 () in
      Utils.with_in_channel path
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
      Utils.log_message "Computed MD5 checksum: %s\n%!" checksum;
      checksum = md5_checksum
    end else begin
      Utils.log_message "File does not exists.\n%!";
      false
    end;
  end else false

let download_resource resource =
  let get_export_link fmt =
    let mime_type = Cache.Resource.mime_type_of_format fmt in
    let export_links =
      Option.map_default
        Cache.Resource.parse_export_links
        []
        resource.Cache.Resource.export_links
    in
      List.fold_left
        (fun accu (m, l) ->
           if m = mime_type then l else accu)
        ""
        export_links
  in

  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  let content_path = Cache.get_content_path cache resource in
  let create_empty_file () =
    Utils.log_message "Creating resource without content (path=%s)...\n%!"
      content_path;
    close_out (open_out content_path);
    SessionM.return () in
  let do_download () =
    Utils.log_message "Downloading resource (id=%Ld)...%!"
      resource.Cache.Resource.id;
    let download_link =
      if Cache.Resource.is_document resource then
        let config = context |. Context.config_lens in
        let fmt = Cache.Resource.get_format resource config in
          get_export_link fmt
      else
        Option.default "" resource.Cache.Resource.download_url in
    begin if download_link <> "" then
      with_try
        (let media_destination = GapiMediaResource.TargetFile content_path in
         GapiService.download_resource download_link media_destination)
        (function
             GapiRequest.PermissionDenied session ->
               Utils.log_message "Server error: Permission denied.\n%!";
               raise Permission_denied
           | GapiRequest.RequestTimeout _ ->
               Utils.log_message "Server error: Request Timeout.\n%!";
               raise Resource_busy
           | GapiRequest.Conflict _ ->
               Utils.log_message "Server error: Conflict.\n%!";
               raise Resource_busy
           | e -> raise e)
    else
      create_empty_file ()
    end >>= fun () ->
    let updated_resource = resource
      |> Cache.Resource.state ^= Cache.Resource.State.InSync in
    Utils.log_message "done\n%!";
    Cache.Resource.update_resource cache updated_resource;
    SessionM.return content_path
  in
  match resource.Cache.Resource.state with
      Cache.Resource.State.InSync
    | Cache.Resource.State.ToUpload ->
        if Sys.file_exists content_path then
          SessionM.return content_path
        else
          do_download ()
    | Cache.Resource.State.ToDownload ->
        if check_md5_checksum resource then
          SessionM.return content_path
        else
          do_download ()
    | Cache.Resource.State.NotFound ->
        raise File_not_found
    | Cache.Resource.State.Restricted ->
        begin if not (Sys.file_exists content_path) then
          create_empty_file ()
        else
          SessionM.return ()
        end >>
        SessionM.return content_path

let download_resource_with_retry resource =
  let rec loop n =
    with_try
      (download_resource resource)
      (fun e ->
         match e with
             Resource_busy ->
               if n > 4 then raise e
               else begin
                 GapiUtils.wait_exponential_backoff n;
                 let n' = n + 1 in
                 Utils.log_message
                   "Retry (%d) downloading resource (id=%Ld)...%!"
                   n' resource.Cache.Resource.id;
                 loop n'
               end
           | e -> raise e)
  in
    loop 0

let is_filesystem_read_only () =
  Context.get_ctx () |. Context.config_lens |. Config.read_only

let is_file_read_only resource =
  not (Option.default true resource.Cache.Resource.editable) ||
  Cache.Resource.is_document resource
(* END Resources *)

(* stat *)
let get_attr path =
  let context = Context.get_ctx () in
  let config = context |. Context.config_lens in
  let (normalized_path, trashed) = normalize_path path in

  let request_resource =
    get_resource normalized_path trashed >>= fun resource ->
    begin if Cache.Resource.is_document resource &&
             config.Config.download_docs then
      with_try
        (download_resource_with_retry resource)
        (function
             File_not_found ->
               SessionM.return ""
           | e -> raise e)
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
    let st_nlink =
      if Cache.Resource.is_folder resource then 2
      else 1 in
    let st_kind =
      if Cache.Resource.is_folder resource then Unix.S_DIR
      else Unix.S_REG in
    let st_perm =
      let perm =
        if Cache.Resource.is_folder resource then 0o777
        else if Cache.Resource.is_document resource then 0o444
        else 0o666 in
      let mask =
        lnot config.Config.umask land (
          if config.Config.read_only ||
             not (Option.default true resource.Cache.Resource.editable)
          then 0o555
          else 0o777)
      in
        perm land mask in
    let st_size =
      match stat with
          None ->
            if Cache.Resource.is_folder resource then f_bsize
            else resource |. Cache.Resource.file_size |. GapiLens.option_get
        | Some st ->
            st.Unix.LargeFile.st_size in
    let st_atime =
      match stat with
          None ->
            resource
              |. Cache.Resource.last_viewed_by_me_date
              |. GapiLens.option_get
        | Some st ->
            st.Unix.LargeFile.st_atime in
    let st_mtime =
      match stat with
          None ->
            resource |. Cache.Resource.modified_date |. GapiLens.option_get
        | Some st ->
            st.Unix.LargeFile.st_mtime in
    let st_ctime =
      match stat with
          None ->
            st_mtime
        | Some st ->
            st.Unix.LargeFile.st_ctime
    in
      { context.Context.mountpoint_stats with 
            Unix.LargeFile.st_nlink;
            st_kind;
            st_perm;
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
      let files = file_list.FileList.items @ accu in
      if file_list.FileList.nextPageToken = "" then
        SessionM.return files
      else
        loop ~pageToken:file_list.FileList.nextPageToken files
    in
      loop []
  in

  let (normalized_path, trashed) = normalize_path path in

  let request_folder =
    Utils.log_message "Getting folder content (path=%s,trashed=%b)\n%!"
      normalized_path trashed;
    get_resource normalized_path trashed >>= fun resource ->
    get_folder_id normalized_path trashed >>= fun folder_id ->
    let q =
      Printf.sprintf "'%s' in parents and trashed = %b" folder_id trashed in
    get_all_files q >>= fun files ->
    Utils.log_message "Done getting folder content (path=%s,trashed=%b)\n%!"
      normalized_path trashed;
    begin if path = trash_directory && trashed then begin
      Utils.log_message "Getting explicitly trashed files...%!";
      let q =
        Printf.sprintf "not '%s' in parents and trashed = true" folder_id in
      get_all_files q >>= fun trashed_files ->
      let explicitly_trashed_files =
        List.filter (fun file -> file.File.explicitlyTrashed) trashed_files in
      Utils.log_message "done (found %d files)\n%!"
        (List.length explicitly_trashed_files);
      SessionM.return (files @ explicitly_trashed_files, resource);
    end else
      SessionM.return (files, resource);
    end
  in

  let cache = Context.get_cache () in
  let resources =
    if check_resource_in_cache cache normalized_path trashed then begin
      Utils.log_message
        "Getting resources from db (parent path=%s,trashed=%b)...%!"
        normalized_path trashed;
      let resources =
        Cache.Resource.select_resources_with_parent_path
          cache normalized_path trashed in
      Utils.log_message "done\n%!";
      resources
    end else begin
      let (files, folder_resource) = do_request request_folder |> fst in
      let change_id = folder_resource.Cache.Resource.change_id in
      let (filename_table, remote_id_table) =
        build_resource_tables normalized_path trashed in
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
                   let title = file.File.title in
                   let is_document =
                     Cache.Resource.is_document_mime_type file.File.mimeType in
                   let filename =
                     disambiguate_title title is_document filename_table in
                   let resource_path =
                     Filename.concat normalized_path filename in
                   let resource = create_resource resource_path change_id in
                   update_resource_from_file resource file)
          resources_and_files
      in
        Utils.log_message
          "Inserting folder resources into db (trashed=%b)...%!" trashed;
        let inserted_resources =
          Cache.Resource.insert_resources
            cache resources normalized_path change_id trashed in
        Utils.log_message "done\n%!";
        let change_id =
          Context.get_ctx () |. Context.largest_change_id_lens in
        let updated_resource = folder_resource
          |> Cache.Resource.change_id ^= change_id
          |> Cache.Resource.state ^= Cache.Resource.State.InSync
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
  let (normalized_path, trashed) = normalize_path path in
  let is_read_only_request = List.mem Unix.O_RDONLY flags in

  let check_editable =
    get_resource normalized_path trashed >>= fun resource ->
    if not is_read_only_request && is_file_read_only resource then
      raise Permission_denied
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
  let (normalized_path, trashed) = normalize_path path in
  do_request (get_resource normalized_path trashed) |> ignore;
  None
(* END opendir *)

(* Update operations *)
let default_save_resource_to_db cache resource file =
  let updated_resource = update_resource_from_file resource file in
  update_cached_resource cache updated_resource

let update_remote_resource path
      ?update_file_in_cache
      ?(save_to_db = default_save_resource_to_db)
      do_remote_update
      retry_update =
  let context = Context.get_ctx () in
  let config = context |. Context.config_lens in
  let cache = context.Context.cache in
  let (normalized_path, trashed) = normalize_path path in
  let update_file =
    get_resource normalized_path trashed >>= fun resource ->
    with_try
      (do_remote_update resource)
      (function
           GapiRequest.Conflict session ->
             Utils.log_message "Conflict detected: %!";
             GapiMonad.SessionM.put session >>
             begin match config.Config.conflict_resolution with
                 Config.ConflictResolutionStrategy.Client ->
                   Utils.log_message "Retrying...%!";
                   retry_update resource >>= fun file ->
                   Utils.log_message "done\n%!";
                   SessionM.return file
               | Config.ConflictResolutionStrategy.Server ->
                   Utils.log_message "Keeping server changes\n%!";
                   SessionM.return None
             end
         | e -> raise e) >>= fun file_option ->
    begin match file_option with
        None -> ()
      | Some file ->
          begin match update_file_in_cache with
              None -> ()
            | Some go ->
                if resource.Cache.Resource.state = Cache.Resource.State.InSync
                then begin
                  let content_path = Cache.get_content_path cache resource in
                  if Sys.file_exists content_path then
                    go content_path
                end;
          end;
          save_to_db cache resource file
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
      Utils.log_message "Touching file (id=%s)...%!" remote_id;
      FilesResource.touch
        ~std_params:file_std_params
        ~fileId:remote_id >>= fun touched_file ->
      Utils.log_message "done\n%!";
      SessionM.return (Some touched_file)
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
  let (normalized_path, trashed) = normalize_path path in
    
  let request_resource =
    get_resource normalized_path trashed >>= fun resource ->
    download_resource_with_retry resource
  in

  let content_path = do_request request_resource |> fst in
    Utils.with_in_channel content_path
      (fun ch ->
         let file_descr = Unix.descr_of_in_channel ch in
         Unix.LargeFile.lseek file_descr offset Unix.SEEK_SET |> ignore;
         Unix_util.read file_descr buf)
(* END read *)

(* write *)
let write path buf offset file_descr =
  let (normalized_path, trashed) = normalize_path path in

  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  let write_to_resource =
    get_resource normalized_path trashed >>= fun resource ->
    download_resource_with_retry resource >>= fun content_path ->
    Utils.log_message "Writing local file (path=%s,trashed=%b)...%!"
      normalized_path trashed;
    let bytes =
      Utils.with_out_channel content_path
        (fun ch ->
           let file_descr = Unix.descr_of_out_channel ch in
           Unix.LargeFile.lseek file_descr offset Unix.SEEK_SET |> ignore;
           Unix_util.write file_descr buf) in
    Utils.log_message "done\n%!";
    let updated_resource =
      resource |> Cache.Resource.state ^= Cache.Resource.State.ToUpload in
    update_cached_resource cache updated_resource;
    SessionM.return bytes
  in
  do_request write_to_resource |> fst
(* END write *)

let upload_if_dirty path =
  let (normalized_path, trashed) = normalize_path path in

  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  get_resource normalized_path trashed >>= fun resource ->
  match resource.Cache.Resource.state with
      Cache.Resource.State.ToUpload ->
        let content_path = Cache.get_content_path cache resource in
        let remote_id = resource |. Cache.Resource.remote_id |> Option.get in
        let media_source =
          GapiMediaResource.create_file_resource content_path in
        Utils.log_message "Uploading file (cache path=%s)...%!" content_path;
        FilesResource.get
          ~std_params:file_std_params
          ~fileId:remote_id >>= fun refreshed_file ->
        FilesResource.update
          ~std_params:file_std_params
          ~media_source
          ~fileId:remote_id
          refreshed_file >>= fun file ->
        Utils.log_message "done\n%!";
        let updated_resource =
          update_resource_from_file
            ~state:Cache.Resource.State.InSync resource file in
        update_cached_resource cache updated_resource;
        SessionM.return ()
    | _ ->
        SessionM.return ()

(* flush *)
let flush path file_descr =
  do_request (upload_if_dirty path) |> ignore

(* release *)
let release path flags hnd =
  do_request (upload_if_dirty path) |> ignore

(* Create resources *)
let create_remote_resource is_folder path mode =
  let (normalized_path, trashed) = normalize_path path in
  if trashed then raise Permission_denied;

  let context = Context.get_ctx () in
  let largest_change_id = context |. Context.largest_change_id_lens in
  let cache = context.Context.cache in
  let parent_path = Filename.dirname normalized_path in
  let create_file =
    get_resource parent_path trashed >>= fun parent_resource ->
    let parent_id =
      parent_resource |. Cache.Resource.remote_id |> Option.get
    in
    let parent_reference = ParentReference.empty
      |> ParentReference.id ^= parent_id in
    let mimeType =
      if is_folder
      then folder_mime_type
      else "" in
    let file = {
      File.empty with
          File.title = Filename.basename normalized_path;
          parents = [parent_reference];
          mimeType;
    } in
    Utils.log_message "Creating %s (path=%s,trashed=%b) on server...%!"
      (if is_folder then "folder" else "file") normalized_path trashed;
    FilesResource.insert
      ~std_params:file_std_params
      file >>= fun created_file ->
    Utils.log_message "done\n%!";
    let new_resource = create_resource normalized_path largest_change_id in
    Utils.log_message "Deleting 'NotFound' resources (path=%s) from cache...%!"
      normalized_path;
    Cache.Resource.delete_not_found_resource_with_path cache normalized_path;
    Utils.log_message "done\n%!";
    let inserted =
      insert_resource_into_cache
        ~state:Cache.Resource.State.InSync cache new_resource created_file in
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

(* Delete (trash) resources *)
let trash_resource is_folder path =
  let (_, trashed) = normalize_path path in
  if trashed then raise Permission_denied;

  let trash resource =
    let remote_id = resource |. Cache.Resource.remote_id |> Option.get in
    Utils.log_message "Trashing file (id=%s)...%!" remote_id;
    FilesResource.trash
      ~std_params:file_std_params
      ~fileId:remote_id >>= fun trashed_file ->
    Utils.log_message "done\n%!";
    SessionM.return (Some trashed_file)
  in
  update_remote_resource
    ~save_to_db:(
      fun cache resource file ->
        let updated_resource = resource
          |> Cache.Resource.trashed ^= Some true in
        update_cached_resource cache updated_resource;
        Cache.Resource.invalidate_trash_bin
          cache resource.Cache.Resource.change_id;
        if is_folder then begin
          let (normalized_path, _) = normalize_path path in
          Utils.log_message "Trashing folder old content (path=%s)...%!"
            normalized_path;
          Cache.Resource.trash_all_with_parent_path cache normalized_path;
          Utils.log_message "done\n%!";
        end)
    path
    trash
    trash

let delete_remote_resource is_folder path =
  let trash_file = trash_resource is_folder path in
  do_request trash_file |> ignore
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
  let (normalized_path, trashed) = normalize_path path in
  let (normalized_new_path, target_trashed) = normalize_path new_path in
  if trashed <> target_trashed then raise Permission_denied;

  let old_parent_path = Filename.dirname normalized_path in
  let new_parent_path = Filename.dirname normalized_new_path in
  let old_name = Filename.basename normalized_path in
  let new_name = Filename.basename normalized_new_path in
  let delete_target_path =
    if not target_trashed &&
       not (Context.get_ctx ()
        |. Context.config_lens
        |. Config.keep_duplicates) then begin
      with_try
        (get_resource normalized_new_path target_trashed >>= fun new_resource ->
         trash_resource
           (Cache.Resource.is_folder new_resource) new_path)
        (function
             File_not_found -> SessionM.return ()
           | e -> raise e)
    end else
      SessionM.return ()
  in
  let update =
    let rename_file use_etag resource =
      let remote_id = resource |. Cache.Resource.remote_id |> Option.get in
      if old_name <> new_name then begin
        delete_target_path >>= fun () ->
        Utils.log_message "Renaming file (id=%s) from %s to %s...%!"
          remote_id old_name new_name;
        let etag = Option.default "" resource.Cache.Resource.etag in
        let file_patch =
          { File.empty with
                File.etag = if use_etag then etag else "";
                title = new_name;
          }
        in
        FilesResource.patch
          ~std_params:file_std_params
          ~fileId:remote_id
          file_patch >>= fun patched_file ->
        Utils.log_message "done\n%!";
        SessionM.return (Some patched_file)
      end else begin
        SessionM.return None
      end
    in
    let move resource =
      let remote_id = resource |. Cache.Resource.remote_id |> Option.get in
      begin if old_parent_path <> new_parent_path then begin
        delete_target_path >>= fun () ->
        Utils.log_message "Moving file (id=%s) from %s to %s...%!"
          remote_id old_parent_path new_parent_path;
        get_resource
          new_parent_path target_trashed >>= fun new_parent_resource ->
        let new_parent_id =
          new_parent_resource |. Cache.Resource.remote_id |> Option.get
        in
        let parent_reference = ParentReference.empty
          |> ParentReference.id ^= new_parent_id in
        let file_patch = File.empty
          |> File.parents ^= [parent_reference] in
        FilesResource.patch
          ~std_params:file_std_params
          ~fileId:remote_id
          file_patch >>= fun patched_file ->
        Utils.log_message "done\n%!";
        SessionM.return (Some patched_file)
      end else
        SessionM.return None
      end >>= fun moved_file ->
      rename_file true resource >>= fun renamed_file ->
      if Option.is_some renamed_file
      then SessionM.return renamed_file
      else SessionM.return moved_file
    in
    update_remote_resource
      path
      move
      (rename_file false)
      ~save_to_db:(
        fun cache resource file ->
          let updated_resource =
            update_resource_from_file resource file in
          let resource_with_new_path =
            updated_resource
              |> Cache.Resource.path ^= normalized_new_path
              |> Cache.Resource.parent_path ^= new_parent_path
              |> Cache.Resource.trashed ^= Some target_trashed
              |> Cache.Resource.state ^=
                (if Cache.Resource.is_folder resource ||
                    Cache.Resource.is_document resource
                 then Cache.Resource.State.ToDownload
                 else Cache.Resource.State.InSync) in
          let resource_to_save =
            if new_parent_path <> old_parent_path &&
               new_name = old_name then
              resource_with_new_path
                |> Cache.Resource.path ^=
                  recompute_path resource_with_new_path new_name
            else resource_with_new_path
          in
          update_cached_resource cache resource_to_save;
          Utils.log_message
            "Deleting 'NotFound' resources (path=%s) from cache...%!"
            normalized_new_path;
          Cache.Resource.delete_not_found_resource_with_path
            cache normalized_new_path;
          Utils.log_message "done\n%!";
          if Cache.Resource.is_folder resource then begin
            Utils.log_message
              "Deleting folder old content (path=%s,trashed=%b) from cache...%!"
              normalized_path trashed;
            Cache.Resource.delete_all_with_parent_path
              cache normalized_path trashed;
            Utils.log_message "done\n%!";
          end)
  in
    do_request update |> ignore
(* END rename *)

(* truncate *)
let truncate path size =
  let update =
    let truncate_file resource =
      let remote_id = resource |. Cache.Resource.remote_id |> Option.get in
      let etag = Option.default "" resource.Cache.Resource.etag in
      Utils.log_message "Truncating file (id=%s)...%!" remote_id;
      FilesResource.patch
        ~std_params:file_std_params
        ~fileId:remote_id
        { File.empty with
              File.etag;
              fileSize = size; } >>= fun file ->
      Utils.log_message "done\n%!";
      SessionM.return (Some file)
    in
    update_remote_resource
      ~update_file_in_cache:(
        fun content_path ->
          Unix.LargeFile.truncate content_path size)
      path
      truncate_file
      truncate_file
  in
    do_request update |> ignore
(* END truncate *)

