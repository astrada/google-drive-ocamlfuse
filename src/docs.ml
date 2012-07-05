open GapiUtils.Infix
open GapiLens.Infix
open GapiMonad
open GapiMonad.SessionM.Infix
open GdataDocumentsV3Model
open GdataDocumentsV3Service

exception File_not_found
exception Permission_denied
exception Resource_busy

let do_request = Oauth2.do_request

let root_directory = "/"
let f_bsize = 4096L
let changestamp_limit = 50L

let chars_blacklist_regexp = Str.regexp ("[/\000]")
let clean_filename title = Str.global_replace chars_blacklist_regexp "_" title

(* Resource cache *)
let create_resource path changestamp =
  { Cache.Resource.id = 0L;
    resource_id = None;
    remote_id = None;
    kind = None;
    md5_checksum = None;
    size = None;
    last_viewed = None;
    last_modified = None;
    parent_path = Filename.dirname path;
    path;
    state = Cache.Resource.State.ToDownload;
    changestamp;
    last_update = Unix.gettimeofday ();
    read_only = false;
  }

let create_root_resource changestamp =
  let resource = create_resource root_directory changestamp in
    { resource with
          Cache.Resource.resource_id = Some root_folder_id;
          kind = Some "folder";
          size = Some 0L;
          parent_path = "";
    }

let update_resource_from_entry resource entry =
  let changestamp = Context.get_ctx () |. Context.changestamp_lens in
  let kind =
    try
      let category =
        List.find
          (fun category ->
             category.GdataAtom.Category.scheme = Document.kind_scheme)
          (entry |. Document.Entry.categories)
      in
        Some (category |. GdataAtom.Category.label)
    with Not_found -> None in
  let resource_id = entry |. Document.Entry.resourceId in
    { resource with
          Cache.Resource.resource_id =
            Some resource_id;
          remote_id =
            Some (Cache.Resource.get_remote_id resource_id);
          kind;
          md5_checksum = Some (entry |. Document.Entry.md5Checksum);
          size = Some (entry |. Document.Entry.size);
          last_viewed =
            Some (entry |. Document.Entry.lastViewed |> Netdate.since_epoch);
          last_modified =
            Some (entry |. Document.Entry.edited |> Netdate.since_epoch);
          changestamp;
          last_update = Unix.gettimeofday ();
          (* TODO: check ACL to verify if the file is read-only *)
          read_only = false;
    }

let folder_regexp = Str.regexp (Str.quote "folder:")

let get_parent_resource_id entry =
  try
    let href =
      entry |. Document.Entry.links
      |> find_url `Parent
      |> Netencoding.Url.decode in
    let pos = Str.search_forward folder_regexp href 0 in
    let resource_id = Str.string_after href pos in
      Some resource_id
  with Not_found -> None

let insert_resource_into_cache cache resource entry =
  let resource = update_resource_from_entry resource entry in
  Utils.log_message "Saving resource to db...%!";
  let inserted = Cache.Resource.insert_resource cache resource in
  Utils.log_message "done\nSaving resource entry...%!";
  Cache.save_xml_entry cache inserted entry;
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
  let changestamp = context |. Context.changestamp_lens in
    match lookup_resource root_directory with
        None ->
          let root_resource = create_root_resource changestamp in
            Utils.log_message "Saving root resource to db...%!";
            let inserted = Cache.Resource.insert_resource cache root_resource in
            Utils.log_message "done\n%!";
            inserted
      | Some resource -> resource
(* END Resource cache *)

(* Metadata *)
let get_metadata () =
  let request_metadata last_changestamp =
    let parameters = QueryParameters.default
      |> QueryParameters.remaining_changestamps_first ^=
        (Int64.to_int last_changestamp) + 1
      |> QueryParameters.remaining_changestamps_limit ^= 500 in
    query_metadata ~parameters >>= fun entry ->
    let metadata = {
      Cache.Metadata.largest_changestamp =
        entry |. Metadata.Entry.largestChangestamp |> Int64.of_int;
      remaining_changestamps =
        entry |. Metadata.Entry.remainingChangestamps |> Int64.of_int;
      quota_bytes_total = entry |. Metadata.Entry.quotaBytesTotal;
      quota_bytes_used = Int64.add
                           (entry |. Metadata.Entry.quotaBytesUsed)
                           (entry |. Metadata.Entry.quotaBytesUsedInTrash);
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

  let update_resource_cache last_changestamp new_metadata =
    let request_changes =
      Utils.log_message "Getting changes from server...%!";
      let parameters = QueryParameters.default
        |> QueryParameters.start_index ^= (Int64.to_int last_changestamp) + 1 in
      query_changes ~parameters >>= fun feed ->
      Utils.log_message "done\n%!";
      let entries = feed |. Document.Feed.entries in
      SessionM.return entries
    in

    let get_id_to_update entry =
      let selected_resource =
        Cache.Resource.select_resource_with_resource_id cache
          entry.Document.Entry.resourceId in
      let resource =
        match selected_resource with
            None ->
              let parent_resource_id = get_parent_resource_id entry in
              let rid = Option.default root_folder_id parent_resource_id in
                Cache.Resource.select_resource_with_resource_id cache rid
          | _ -> selected_resource
      in
        Option.map (fun r -> r.Cache.Resource.id) resource
    in

    let get_resource_to_remove entry =
      let resource_id = entry.Document.Entry.resourceId in
      let remote_id = Cache.Resource.get_remote_id resource_id in
        Cache.Resource.select_resource_with_remote_id cache remote_id
    in

    let get_resource_to_delete entry =
      Cache.Resource.select_resource_with_resource_id cache
        entry.Document.Entry.resourceId
    in

    let remaining_changestamps =
      new_metadata.Cache.Metadata.remaining_changestamps in
    if remaining_changestamps = 0L then
      Utils.log_message "no need to update resource cache\n%!"
    else if remaining_changestamps > changestamp_limit then
      Utils.log_message
        "too many changes (remaining_changestamps=%Ld)\n%!"
        remaining_changestamps
    else match metadata with
        None -> ()
      | Some old_metadata ->
          let changestamp =
            new_metadata.Cache.Metadata.largest_changestamp in
          let changed_document_entries = do_request request_changes |> fst in
          let update_resource_cache filter_entries map_entry update_cache =
            let filtered_entries =
              List.filter filter_entries changed_document_entries in
            let xs =
              List.fold_left
                (fun xs entry ->
                   match map_entry entry with
                       None -> xs
                     | Some x ->
                         if not (List.mem x xs) then
                           x :: xs
                         else xs)
                []
                filtered_entries in
            update_cache cache xs;
          in

          Utils.log_message "Updating resource cache...%!";
          update_resource_cache
            (fun entry -> not entry.Document.Entry.removed &&
                          not entry.Document.Entry.deleted)
            get_id_to_update
            (fun cache ids ->
               Cache.Resource.invalidate_resources cache ids changestamp);
          Utils.log_message "done\nRemoving permanently deleted resources...%!";
          update_resource_cache
            (fun entry -> entry.Document.Entry.removed)
            get_resource_to_remove
            Cache.delete_resources;
          Utils.log_message "done\nRemoving trashed resources...%!";
          update_resource_cache
            (fun entry -> entry.Document.Entry.deleted)
            get_resource_to_delete
            Cache.delete_resources;
          Utils.log_message "done\n%!";
  in

  let refresh_metadata () =
    let last_changestamp =
      Option.map_default
        Cache.Metadata.largest_changestamp.GapiLens.get 0L metadata in
    Utils.log_message "Refreshing metadata...%!";
    let updated_metadata =
      do_request (request_metadata last_changestamp) |> fst in
    Utils.log_message "done\nUpdating metadata in db...%!";
    Cache.Metadata.insert_metadata context.Context.cache updated_metadata;
    Utils.log_message "done\nUpdating context...%!";
    context |> Context.metadata ^= Some updated_metadata |> Context.set_ctx;
    Utils.log_message "done\n%!";
    update_resource_cache last_changestamp updated_metadata;
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
let get_entry_from_server parent_folder_id title =
  Utils.log_message "Getting resource %s (in folder %s) from server...%!"
    title parent_folder_id;
  let parameters = QueryParameters.default
    |> QueryParameters.showfolders ^= true
    |> QueryParameters.showroot ^= true
    |> QueryParameters.title ^= title
    |> QueryParameters.title_exact ^= true in
  query_folder_contents ~parameters parent_folder_id >>= fun feed ->
  Utils.log_message "done\n%!";
  let entries = feed |. Document.Feed.entries in
  if List.length entries = 0 then
    SessionM.return None
  else
    let entry = entries |. GapiLens.head in
    SessionM.return (Some entry)

let get_resource_from_server parent_folder_id title new_resource cache =
  get_entry_from_server parent_folder_id title >>= fun entry ->
  match entry with
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
  let changestamp = Context.get_ctx () |. Context.changestamp_lens in
    match lookup_resource path with
        None -> false
      | Some resource ->
          if Cache.Resource.is_valid resource changestamp then
            if Cache.Resource.is_folder resource then
              resource.Cache.Resource.state = Cache.Resource.State.InSync
            else true
          else false

let rec get_folder_id path =
  if path = root_directory then
    SessionM.return root_folder_id
  else
    get_resource path >>= fun resource ->
    let resource_id = resource |. Cache.Resource.resource_id |> Option.get in
    SessionM.return resource_id
and get_resource path =
  let changestamp = get_metadata () |. Cache.Metadata.largest_changestamp in

  let get_new_resource cache =
    let parent_path = Filename.dirname path in
      if check_resource_in_cache cache parent_path then begin
        raise File_not_found
      end else begin
        let new_resource = create_resource path changestamp in
        let title = Filename.basename path in
        get_folder_id
          new_resource.Cache.Resource.parent_path >>= fun parent_folder_id ->
        get_resource_from_server
          parent_folder_id title new_resource cache >>= fun resource ->
        SessionM.return resource
      end
  in

  let refresh_resource resource cache =
    begin if Cache.xml_entry_exists cache resource then begin
      Utils.log_message "Loading xml entry id=%Ld...%!"
        resource.Cache.Resource.id;
      let entry = Cache.load_xml_entry cache resource in
      Utils.log_message "done\nRefreshing entry from server...%!";
      refresh_document entry >>= fun entry ->
      SessionM.return (Some entry)
    end else if Option.is_some resource.Cache.Resource.resource_id then begin
      let resource_id = resource.Cache.Resource.resource_id |> Option.get in
      Utils.log_message "Getting entry from server (id=%s)...%!" resource_id;
      get_document resource_id >>= fun entry ->
      SessionM.return (Some entry)
    end else
      SessionM.return None
    end >>= fun refreshed_entry ->
    Utils.log_message "done\n%!";
    match refreshed_entry with
        None ->
          Cache.Resource.delete_resource cache resource;
          get_new_resource cache
      | Some entry ->
          let updated_resource = update_resource_from_entry resource entry in
          update_cached_resource cache updated_resource;
          Cache.save_xml_entry cache updated_resource entry;
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
          if Cache.Resource.is_valid resource changestamp then
            SessionM.return resource
          else
            refresh_resource resource cache
    end >>= fun resource ->
    begin match resource.Cache.Resource.state with
        Cache.Resource.State.NotFound ->
          raise File_not_found 
      | _ ->
          SessionM.return resource
    end

let download_resource resource =
  (* TODO: use md5_checksum to verify content when state=ToDownload *)
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
    let entry = Cache.load_xml_entry cache resource in
    let download_link = entry
      |. Document.Entry.content
      |. GdataAtom.Content.src in
    begin if download_link <> "" then
      try
        let media_destination = GapiMediaResource.TargetFile content_path in
        begin if Cache.Resource.is_document resource then
          let config = context |. Context.config_lens in
          let format = Cache.Resource.get_format resource config in
          if format <> "" then
            download_document
              ~format
              entry
              media_destination
          else
            create_empty_file ()
        else
          partial_download
            download_link
            media_destination
        end
      with
          GapiRequest.PermissionDenied session ->
            Utils.log_message "Server error: Permission denied.\n%!";
            GapiMonad.SessionM.put session >>= fun () ->
            create_empty_file ()
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
             resource.Cache.Resource.read_only then 0o555
          else 0o777)
      in
        perm land mask in
    let st_size =
      match stat with
          None ->
            if Cache.Resource.is_folder resource then f_bsize
            else resource |. Cache.Resource.size |. GapiLens.option_get
        | Some st ->
            st.Unix.LargeFile.st_size in
    let st_atime =
      match stat with
          None ->
            resource |. Cache.Resource.last_viewed |. GapiLens.option_get
        | Some st ->
            st.Unix.LargeFile.st_atime in
    let st_mtime =
      match stat with
          None ->
            resource |. Cache.Resource.last_modified |. GapiLens.option_get
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
    let parameters = QueryParameters.default
      |> QueryParameters.showfolders ^= true in
    get_folder_id path >>= fun folder_id ->
    query_folder_contents ~parameters folder_id >>= fun feed ->
    Utils.log_message "Done getting folder content (path=%s)\n%!" path;
    SessionM.return (feed, resource)
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
      let (feed, folder_resource) = do_request request_folder |> fst in
      let changestamp = folder_resource.Cache.Resource.changestamp in
      let resources =
        List.map
          (fun entry ->
             let title =
               entry |. Document.Entry.title |. GdataAtom.Title.value in
             let filename = clean_filename title in
             let resource_path = Filename.concat path filename in
             let resource = create_resource resource_path changestamp in
               update_resource_from_entry resource entry)
          feed.Document.Feed.entries
      in
        Utils.log_message "Inserting folder resources into db...%!";
        let inserted_resources =
          Cache.Resource.insert_resources cache resources path changestamp in
        Utils.log_message "done\nSaving resource entries...%!";
        List.iter
          (fun resource ->
             let entry =
               List.find
                 (fun e ->
                    e |. Document.Entry.resourceId =
                      Option.get resource.Cache.Resource.resource_id)
                 feed.Document.Feed.entries
             in
               Cache.save_xml_entry cache resource entry)
          inserted_resources;
        Utils.log_message "done\n%!";
        let changestamp = Context.get_ctx () |. Context.changestamp_lens in
        let updated_resource = folder_resource
          |> Cache.Resource.changestamp ^= changestamp
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
  let read_only =
    Context.get_ctx () |. Context.config_lens |. Config.read_only
  in
    if read_only && not (List.mem Unix.O_RDONLY flags) then
      raise Permission_denied
    else begin
      do_request (get_resource path) |> ignore
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

