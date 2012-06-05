open GapiUtils.Infix
open GapiLens.Infix
open GapiMonad
open GapiMonad.SessionM.Infix
open GdataDocumentsV3Model
open GdataDocumentsV3Service

exception File_not_found
exception Permission_denied

(* Gapi request wrapper *)
let do_request interact =
  let rec try_request () =
    try
      let context = Context.get_ctx () in
      let state = context |. Context.state_lens in
      let config = context.Context.gapi_config in
      let curl_state = context.Context.curl_state in
      let auth_context =
        GapiConversation.Session.OAuth2
          { GapiConversation.Session.oauth2_token =
              state.State.last_access_token;
            refresh_token = state.State.refresh_token }
      in
        GapiConversation.with_session
          ~auth_context
          config
          curl_state
          interact
    with
        Failure message as e ->
          if ExtString.String.exists message "CURLE_OPERATION_TIMEOUTED" then
            (* Retry on timeout *)
            try_request ()
          else raise e
      | GapiRequest.RefreshTokenFailed _ ->
          GaeProxy.refresh_access_token ();
          (* Retry with refreshed token *)
          try_request ()
      | GapiService.ServiceError e ->
          Utils.log_message "ServiceError\n%!";
          let message =
            e |> GapiError.RequestError.to_data_model
              |> GapiJson.data_model_to_json
              |> Json_io.string_of_json
          in
            failwith message
  in
    try_request ()
(* END Gapi request wrapper *)

let root_directory = "/"
let f_bsize = 4096L

(* Metadata *)
let get_metadata () =
  let context = Context.get_ctx () in
  let go =
    query_metadata >>= fun entry ->
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

  let refresh_metadata () =
    Utils.log_message "Refreshing metadata...%!";
    let metadata = do_request go |> fst in
    Utils.log_message "done\nUpdating db...%!";
    Cache.Metadata.insert_metadata context.Context.cache metadata;
    Utils.log_message "done\nUpdating context...%!";
    context |> Context.metadata ^= Some metadata |> Context.set_ctx;
    Utils.log_message "done\n%!";
    metadata
  in

  let metadata =
    if Option.is_none context.Context.metadata then begin
      Utils.log_message "Loading metadata from db...%!";
      let db_metadata = Cache.Metadata.select_metadata context.Context.cache in
        context |> Context.metadata ^= db_metadata |> Context.set_ctx;
        db_metadata
    end else begin
      Utils.log_message "Getting metadata from context...%!";
      context.Context.metadata
    end
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
let create_resource path changestamp =
  { Cache.Resource.id = 0L;
    resource_id = None;
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
  let kind =
    try
      let category =
        List.find
          (fun category ->
             category.GdataAtom.Category.scheme = Document.kind_scheme)
          (entry |. Document.Entry.categories)
      in
        Some (category |. GdataAtom.Category.label)
    with Not_found -> None
  in
    { resource with
          Cache.Resource.resource_id =
            Some (entry |. Document.Entry.resourceId);
          kind;
          md5_checksum = Some (entry |. Document.Entry.md5Checksum);
          size = Some (entry |. Document.Entry.size);
          last_viewed =
            Some (entry |. Document.Entry.lastViewed |> Netdate.since_epoch);
          last_modified =
            Some (entry |. Document.Entry.modifiedByMeDate
                  |> Netdate.since_epoch);
    }

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

let update_resource resource =
  let cache = Context.get_cache () in
    Utils.log_message "Updating resource in db (id=%Ld)...%!"
      resource.Cache.Resource.id;
    Cache.Resource.update_resource cache resource;
    Utils.log_message "done\n%!"

let get_resource_from_server parent_folder_id title new_resource cache =
  Utils.log_message "Getting resource %s (in folder %s) from server...%!"
    title parent_folder_id;
  let parameters = QueryParameters.default
    |> QueryParameters.showfolders ^= true
    |> QueryParameters.title ^= title
    |> QueryParameters.title_exact ^= true
  in
    query_folder_contents ~parameters parent_folder_id >>= fun feed ->
    Utils.log_message "done\n%!";
    let entries = feed |. Document.Feed.entries in
    let result =
    if List.length entries = 0 then begin
      Utils.log_message "Saving not found resource to db...%!";
      let resource = new_resource
        |> Cache.Resource.state ^= Cache.Resource.State.NotFound in
      let inserted =
        Cache.Resource.insert_resource cache resource in
      Utils.log_message "done\n%!";
      inserted
    end else begin
      let entry = entries |. GapiLens.head in
      let resource = update_resource_from_entry new_resource entry in
      Utils.log_message "Saving resource to db...%!";
      let inserted = Cache.Resource.insert_resource cache resource in
      Utils.log_message "done\nSaving resource entry...%!";
      Cache.save_xml_entry cache inserted entry;
      Utils.log_message "done\n%!";
      inserted
    end in
    SessionM.return result

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

  let refresh_resource resource cache =
    Utils.log_message "Loading xml entry id=%Ld...%!"
      resource.Cache.Resource.id;
    let entry = Cache.load_xml_entry cache resource in
    Utils.log_message "done\nRefreshing entry...%!";
    refresh_document entry >>= fun refreshed_entry ->
    Utils.log_message "done\n%!";
    let updated_resource =
      update_resource_from_entry resource refreshed_entry in
    update_resource updated_resource;
    Cache.save_xml_entry cache updated_resource refreshed_entry;
    Utils.log_message "done\n%!";
    SessionM.return (updated_resource)
  in

  if path = root_directory then
    let root_resource = get_root_resource () in
    SessionM.return root_resource
  else
    let cache = Context.get_cache () in
    begin match lookup_resource path with
        None ->
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
(* END Resources *)

(* stat *)
let get_attr path =
  let context = Context.get_ctx () in
  if path = root_directory then
    context.Context.mountpoint_stats
  else begin
    let resource = do_request (get_resource path) |> fst in
    let st_nlink =
      if Cache.Resource.is_folder resource then 2
      else 1 in
    let st_kind =
        (* TODO?
        | Some "document"
        | Some "drawing"
        | Some "form"
        | Some "presentation"
        | Some "spreadsheet" -> Unix.S_LNK *)
      if Cache.Resource.is_folder resource then Unix.S_DIR
      else Unix.S_REG in
    let config = context |. Context.config_lens in
    let perm =
      if Cache.Resource.is_folder resource then 0o777
      else 0o666 in
    let mask =
      lnot config.Config.umask land
      (if config.Config.read_only then 0o555 else 0o777) in
    let st_perm = perm land mask in
    let st_size =
      if Cache.Resource.is_folder resource then f_bsize
      else resource |. Cache.Resource.size |. GapiLens.option_get
    in
      (* TODO: atime, ctime, mtime *)
      { context.Context.mountpoint_stats with 
            Unix.LargeFile.st_nlink;
            st_kind;
            st_perm;
            st_size;
      }
  end
(* END stat *)

(* readdir *)
let read_dir path =
  let go =
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
      let (feed, folder_resource) = do_request go |> fst in
      let changestamp = folder_resource.Cache.Resource.changestamp in
      let resources =
        List.map
          (fun entry ->
             let filename =
               entry |. Document.Entry.title |. GdataAtom.Title.value in
             let resource_path =
               Filename.concat path filename in
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
          update_resource updated_resource;
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
  let cache = Context.get_cache () in
  let go =
    get_resource path >>= fun resource ->
    let content_path = Cache.get_content_path cache resource in
    begin match resource.Cache.Resource.state with
        Cache.Resource.State.InSync ->
          SessionM.return content_path
      | Cache.Resource.State.ToDownload ->
          let entry = Cache.load_xml_entry cache resource in
          let download_link = entry
            |. Document.Entry.content
            |. GdataAtom.Content.src in
          let media_destination = GapiMediaResource.TargetFile content_path in
          partial_download
            download_link
            media_destination >>
          let updated_resource = resource
            |> Cache.Resource.state ^= Cache.Resource.State.InSync in
          Cache.Resource.update_resource cache updated_resource;
            (* TODO:
             * handle documents/spreadsheets/... *)
          SessionM.return content_path
      | _ -> raise File_not_found
    end
  in

  let content_path = do_request go |> fst in
    Utils.with_in_channel content_path
      (fun ch ->
         let file_descr = Unix.descr_of_in_channel ch in
           Unix.LargeFile.lseek file_descr offset Unix.SEEK_SET |> ignore;
           Unix_util.read file_descr buf)
(* END read *)

