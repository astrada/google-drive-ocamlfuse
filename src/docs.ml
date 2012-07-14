open GapiUtils.Infix
open GapiLens.Infix
open GapiMonad
open GapiMonad.SessionM.Infix
open GapiDriveV2Model
open GapiDriveV2Service

exception File_not_found
exception Permission_denied
exception Resource_busy

let do_request = Oauth2.do_request

let root_directory = "/"
let f_bsize = 4096L
let change_id_limit = 50L

let chars_blacklist_regexp = Str.regexp ("[/\000]")
let clean_filename title = Str.global_replace chars_blacklist_regexp "_" title

(* Resource cache *)
let create_resource path change_id =
  { Cache.Resource.id = 0L;
    etag = None;
    remote_id = None;
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
    parent_path = Filename.dirname path;
    path;
    state = Cache.Resource.State.ToDownload;
    change_id;
    last_update = Unix.gettimeofday ();
  }

let create_root_resource root_folder_id change_id =
  let resource = create_resource root_directory change_id in
    { resource with
          Cache.Resource.remote_id = Some root_folder_id;
          mime_type = Some "application/vnd.google-apps.folder";
          file_size = Some 0L;
          parent_path = "";
    }

let update_resource_from_file resource file =
  let largest_change_id =
    Context.get_ctx () |. Context.largest_change_id_lens
  in
    { resource with
          Cache.Resource.etag = Some file.File.etag;
          remote_id = Some file.File.id;
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
          change_id = largest_change_id;
          last_update = Unix.gettimeofday ();
    }

let get_parent_resource_ids file =
  List.map
    (fun parent -> parent.ParentReference.id)
    file.File.parents

let insert_resource_into_cache cache resource file =
  let resource = update_resource_from_file resource file in
  Utils.log_message "Saving resource to db...%!";
  let inserted = Cache.Resource.insert_resource cache resource in
  Utils.log_message "done\n%!";
  inserted

let update_cached_resource cache resource =
  Utils.log_message "Updating resource in db (id=%Ld)...%!"
    resource.Cache.Resource.id;
  Cache.Resource.update_resource cache resource;
  Utils.log_message "done\n%!"

let lookup_resource path =
  Utils.log_message "Loading resource %s from db...%!" path;
  let cache = Context.get_cache () in
  let resource =
    Cache.Resource.select_resource_with_path cache path
  in
    if Option.is_none resource then
      Utils.log_message "not found\n%!"
    else
      Utils.log_message "found\n%!";
    resource

let get_root_resource () =
  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  let metadata = context.Context.metadata |> Option.get in
    match lookup_resource root_directory with
        None ->
          let root_resource =
            create_root_resource
              metadata.Cache.Metadata.root_folder_id
              metadata.Cache.Metadata.largest_change_id in
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
    let startChangeId =
      Int64.to_string (Int64.add last_change_id 1L) in
    AboutResource.get
      ?etag
      ~startChangeId
      ~maxChangeIdCount:"500" >>= fun about ->
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
        context |> Context.metadata ^= db_metadata |> Context.set_ctx;
        db_metadata
    end else begin
      Utils.log_message "Getting metadata from context...%!";
      context.Context.metadata
    end in

  let update_resource_cache last_change_id new_metadata =
    let request_changes =
      Utils.log_message "Getting changes from server...%!";
      let startChangeId = Int64.to_string (Int64.add last_change_id 1L) in
      ChangesResource.list
        ~includeDeleted:true
        ~startChangeId >>= fun changes ->
      Utils.log_message "done\n%!";
      SessionM.return changes.ChangeList.items
    in

    let get_ids_to_update change =
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
          | _ -> [selected_resource]
      in
        List.map
          (fun r -> Option.map (fun r' -> r'.Cache.Resource.id) r)
          resources
    in

    let get_file_id_from_change change =
      [Cache.Resource.select_resource_with_remote_id cache
         change.Change.file.File.id]
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
               not change.Change.file.File.labels.File.Labels.trashed &&
               not change.Change.deleted)
            get_ids_to_update
            (fun cache ids ->
               Cache.Resource.invalidate_resources cache ids change_id);
          Utils.log_message "done\nRemoving deleted resources...%!";
          update_resource_cache
            (fun change -> change.Change.deleted)
            get_file_id_from_change
            Cache.delete_resources;
          Utils.log_message "done\nRemoving trashed resources...%!";
          update_resource_cache
            (fun change ->
               change.Change.file.File.labels.File.Labels.trashed)
            get_file_id_from_change
            Cache.delete_resources;
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
    context |> Context.metadata ^= Some updated_metadata |> Context.set_ctx;
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
let get_file_from_server parent_folder_id title =
  Utils.log_message "Getting resource %s (in folder %s) from server...%!"
    title parent_folder_id;
  let q = Printf.sprintf "title = '%s' and '%s' in parents"
            title parent_folder_id in
  try
    FilesResource.list ~q ~maxResults:1 >>= fun file_list ->
    Utils.log_message "done\n%!";
    let files = file_list.FileList.items in
    if List.length files = 0 then
      SessionM.return None
    else
      let file = files |. GapiLens.head in
      SessionM.return (Some file)
  with GapiRequest.NotFound session ->
    SessionM.put session >>
    SessionM.return None

let get_resource_from_server parent_folder_id title new_resource cache =
  get_file_from_server parent_folder_id title >>= fun file ->
  match file with
      None ->
        Utils.log_message "Saving not found resource to db...%!";
        let resource = new_resource
          |> Cache.Resource.state ^= Cache.Resource.State.NotFound in
        let inserted = Cache.Resource.insert_resource cache resource in
        Utils.log_message "done\n%!";
        SessionM.return inserted
    | Some entry ->
        let inserted = insert_resource_into_cache cache new_resource entry in
        SessionM.return inserted

let check_resource_in_cache cache path =
  let change_id = Context.get_ctx () |. Context.largest_change_id_lens in
    match lookup_resource path with
        None -> false
      | Some resource ->
          if Cache.Resource.is_valid resource change_id then
            if Cache.Resource.is_folder resource then
              resource.Cache.Resource.state = Cache.Resource.State.InSync
            else true
          else false

let rec get_folder_id path =
  if path = root_directory then
    let root_folder_id =
      get_metadata () |. Cache.Metadata.root_folder_id in
    SessionM.return root_folder_id
  else
    get_resource path >>= fun resource ->
    let remote_id = resource |. Cache.Resource.remote_id |> Option.get in
    SessionM.return remote_id
and get_resource path =
  let change_id =
    get_metadata () |. Cache.Metadata.largest_change_id in

  let get_new_resource cache =
    let parent_path = Filename.dirname path in
      if check_resource_in_cache cache parent_path then begin
        raise File_not_found
      end else begin
        let new_resource = create_resource path change_id in
        let title = Filename.basename path in
        get_folder_id
          new_resource.Cache.Resource.parent_path >>= fun parent_folder_id ->
        get_resource_from_server
          parent_folder_id title new_resource cache >>= fun resource ->
        SessionM.return resource
      end
  in

  let refresh_resource resource cache =
    begin if Option.is_some resource.Cache.Resource.remote_id then begin
      let remote_id = resource.Cache.Resource.remote_id |> Option.get in
      let etag = resource.Cache.Resource.etag in
      Utils.log_message "Getting file from server (id=%s)...%!" remote_id;
      FilesResource.get ?etag ~fileId:remote_id >>= fun file ->
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
    let root_resource = get_root_resource () in
    SessionM.return root_resource
  else
    let cache = Context.get_cache () in
    begin match lookup_resource path with
        None ->
          get_new_resource cache
      | Some resource ->
          if Cache.Resource.is_valid resource change_id then
            SessionM.return resource
          else
            try
              refresh_resource resource cache
            with GapiRequest.NotModified session ->
              SessionM.put session >>
              SessionM.return resource
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
  Utils.log_message "Checking MD5 checksum (path=%s, hash=%s)...\n%!"
    path md5_checksum;
  if md5_checksum <> "" && Sys.file_exists path then begin
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
  end

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
      try
        let media_destination = GapiMediaResource.TargetFile content_path in
          GapiService.download_resource
            download_link
            media_destination
      with
          GapiRequest.PermissionDenied session ->
            Utils.log_message "Server error: Permission denied.\n%!";
            GapiMonad.SessionM.put session >>
            create_empty_file ()
        | GapiRequest.RequestTimeout _ ->
            Utils.log_message "Server error: Request Timeout.\n%!";
            raise Resource_busy
        | GapiRequest.Conflict _ ->
            Utils.log_message "Server error: Conflict.\n%!";
            raise Resource_busy
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
        Cache.Resource.State.InSync ->
          if Sys.file_exists content_path then
            SessionM.return content_path
          else
            do_download ()
      | Cache.Resource.State.ToDownload ->
          if check_md5_checksum resource then
            SessionM.return content_path
          else
            do_download ()
      | _ -> raise File_not_found
(* END Resources *)

(* stat *)
let get_attr path =
  let context = Context.get_ctx () in
  let config = context |. Context.config_lens in

  let request_resource =
    get_resource path >>= fun resource ->
    begin if Cache.Resource.is_document resource &&
             config.Config.download_docs then
      try
        download_resource resource
      with File_not_found ->
        SessionM.return ""
    else
      SessionM.return ""
    end >>= fun content_path ->
    SessionM.return (resource, content_path)
  in

  if path = root_directory then
    context.Context.mountpoint_stats
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
             not (Option.default
                    true
                    resource.Cache.Resource.editable) then 0o555
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
            resource |. Cache.Resource.last_viewed_by_me_date |. GapiLens.option_get
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
  let request_folder =
    Utils.log_message "Getting folder content (path=%s)\n%!" path;
    get_resource path >>= fun resource ->
    get_folder_id path >>= fun folder_id ->
    let q = Printf.sprintf "'%s' in parents" folder_id in
    FilesResource.list ~q >>= fun file_list ->
    Utils.log_message "Done getting folder content (path=%s)\n%!" path;
    SessionM.return (file_list.FileList.items, resource)
  in

  let cache = Context.get_cache () in
  let resources =
    if check_resource_in_cache cache path then begin
      Utils.log_message "Getting resources from db (parent path=%s)...%!"
        path;
      let resources =
        Cache.Resource.select_resources_with_parent_path cache path in
      Utils.log_message "done\n%!";
      resources
    end else begin
      let (files, folder_resource) = do_request request_folder |> fst in
      let change_id = folder_resource.Cache.Resource.change_id in
      let resources =
        List.map
          (fun file ->
             let title = file.File.title in
             let filename = clean_filename title in
             let resource_path = Filename.concat path filename in
             let resource = create_resource resource_path change_id in
               update_resource_from_file resource file)
          files
      in
        Utils.log_message "Inserting folder resources into db...%!";
        let inserted_resources =
          Cache.Resource.insert_resources cache resources path change_id in
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
    List.map
      (fun resource ->
         Filename.basename resource.Cache.Resource.path)
      resources
(* END readdir *)

(* fopen *)
let fopen path flags =
  let is_read_only_request = List.mem Unix.O_RDONLY flags in
  let check_editable =
    get_resource path >>= fun resource ->
    if not (Option.default true resource.Cache.Resource.editable) &&
       not is_read_only_request then
      raise Permission_denied
    else
      SessionM.return ()
  in

  let read_only =
    Context.get_ctx () |. Context.config_lens |. Config.read_only
  in
    if read_only && not is_read_only_request then
      raise Permission_denied
    else begin
      do_request check_editable |> ignore
    end;
    None
(* END fopen *)

(* read *)
let read path buf offset file_descr =
  let request_resource =
    get_resource path >>= fun resource ->
    download_resource resource
  in

  let content_path = do_request request_resource |> fst in
    Utils.with_in_channel content_path
      (fun ch ->
         let file_descr = Unix.descr_of_in_channel ch in
           Unix.LargeFile.lseek file_descr offset Unix.SEEK_SET |> ignore;
           Unix_util.read file_descr buf)
(* END read *)

