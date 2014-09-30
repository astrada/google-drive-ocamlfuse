open GapiUtils.Infix
open GapiLens.Infix
open GapiMonad
open GapiMonad.SessionM.Infix
open GapiDriveV2Model
open GapiDriveV2Service

exception File_not_found
exception Permission_denied
exception Resource_busy
exception Directory_not_empty

let folder_mime_type = "application/vnd.google-apps.folder"
let file_fields =
  "alternateLink,createdDate,downloadUrl,editable,etag,explicitlyTrashed,\
   exportLinks,fileExtension,fileSize,id,labels,lastViewedByMeDate,\
   md5Checksum,mimeType,modifiedDate,parents,title"
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
let mb = 1048576L

(* Utilities *)
let chars_blacklist_regexp = Str.regexp "[/\000]"
let clean_filename title = Str.global_replace chars_blacklist_regexp "_" title

let apostrophe_regexp = Str.regexp (Str.quote "'")
let escape_apostrophe title = Str.global_replace apostrophe_regexp "\\'" title

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

let disambiguate_filename filename without_extension filename_table  =
  let rec find_first_unique_filename filename copy_number =
    let new_candidate =
      if without_extension then
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

(* Resource cache *)
let get_filename title is_document get_document_format =
  let context = Context.get_ctx () in
  let config = context |. Context.config_lens in
  let clean_title = clean_filename title in
  let document_format =
    if is_document
    then get_document_format config
    else "" in
  if is_document && config.Config.docs_file_extension
  then (clean_title ^ "." ^ document_format, false)
  else (clean_title, is_document)

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
       let (clean_title, _) =
         get_filename
           title
           (Cache.Resource.is_document resource)
           (fun config -> Cache.Resource.get_format resource config)
       in
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
    alternate_link = None;
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

let get_unique_filename title is_document get_document_format filename_table =
  let (complete_title, without_extension) =
    get_filename title is_document get_document_format
  in
  disambiguate_filename complete_title without_extension filename_table

let get_unique_filename_from_resource resource title filename_table =
  get_unique_filename
    title
    (Cache.Resource.is_document resource)
    (fun config -> Cache.Resource.get_format resource config)
    filename_table

let get_unique_filename_from_file file filename_table =
  get_unique_filename
    file.File.title
    (Cache.Resource.is_document_mime_type file.File.mimeType)
    (fun config ->
      Cache.Resource.get_format_from_mime_type file.File.mimeType config)
    filename_table

let recompute_path resource title =
  (* TODO: make an optimized version of build_resource_tables that
   * doesn't create resource table (useful for large directories). *)
  let (filename_table, _) =
    build_resource_tables
      resource.Cache.Resource.parent_path
      (Option.default false resource.Cache.Resource.trashed) in
  let filename =
    get_unique_filename_from_resource resource title filename_table
  in
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
        alternate_link = Some file.File.alternateLink;
        change_id = largest_change_id;
        last_update = Unix.gettimeofday ();
        path;
        state = Option.default resource.Cache.Resource.state state;
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

let update_cache_size new_size metadata cache =
  let updated_metadata = metadata
    |> Cache.Metadata.cache_size ^= new_size in
  Utils.log_message "Updating cache size (%Ld) in db...%!" new_size;
  Cache.Metadata.insert_metadata cache updated_metadata;
  Utils.log_message "done\nUpdating context...%!";
  Context.update_ctx (Context.metadata ^= Some updated_metadata);
  Utils.log_message "done\n%!"

let shrink_cache file_size max_cache_size_mb metadata cache =
  let check_cache_size cache_size =
    let max_cache_size = Int64.mul (Int64.of_int max_cache_size_mb) mb in
    cache_size <= max_cache_size
  in

  if file_size <> 0L then begin
    let target_size = Int64.add metadata.Cache.Metadata.cache_size file_size in
    if check_cache_size target_size then begin
      update_cache_size target_size metadata cache;
    end else begin
      let resources =
        Cache.Resource.select_resources_order_by_last_update cache in
      let (new_cache_size, resources_to_free) =
        List.fold_left
          (fun (new_cache_size, rs) resource ->
            if check_cache_size new_cache_size then
              (new_cache_size, rs)
            else begin
              let new_size = Int64.sub
                new_cache_size
                (Option.get resource.Cache.Resource.file_size) in
              (new_size, resource :: rs)
            end)
          (target_size, [])
          resources in
      update_cache_size new_cache_size metadata cache;
      List.iter
        (fun resource ->
           let updated_resource = resource
             |> Cache.Resource.state ^= Cache.Resource.State.ToDownload in
           update_cached_resource cache updated_resource)
        resources_to_free;
      Cache.delete_files_from_cache cache resources_to_free |> ignore
    end
  end

let delete_resources metadata cache resources =
  Cache.Resource.delete_resources cache resources;
  let total_size =
    Cache.delete_files_from_cache cache resources in
  let new_cache_size =
    Int64.sub metadata.Cache.Metadata.cache_size total_size in
  update_cache_size new_cache_size metadata cache
(* END Resource cache *)

(* Metadata *)
let get_metadata () =
  let request_metadata last_change_id etag cache_size =
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
      cache_size;
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
                  "items(deleted,file(" ^ file_fields
                  ^ "),fileId),nextPageToken"
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
            (delete_resources new_metadata);
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
    let cache_size =
      Option.map_default
        Cache.Metadata.cache_size.GapiLens.get 0L metadata in
    Utils.log_message "Refreshing metadata...%!";
    let update_metadata =
      with_try
        (request_metadata last_change_id etag cache_size)
        (function
            GapiRequest.NotModified session ->
              Utils.log_message "not modified...%!";
              SessionM.put session >>= fun () ->
              let m = Option.get metadata in
              let m' =
                m |> Cache.Metadata.last_update ^= Unix.gettimeofday () in
              SessionM.return m'
          | e -> throw e) in
    let updated_metadata = do_request update_metadata |> fst in
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
let refresh_remote_resource resource current_file =
  let remote_id = resource.Cache.Resource.remote_id |> Option.get in
  let etag = resource.Cache.Resource.etag in
  Utils.log_message
    "Refreshing remote resource (id=%s,etag=%s)...%!"
    remote_id (Option.default "" etag);
  with_try
    (FilesResource.get
       ~std_params:file_std_params
       ?etag
       ~fileId:remote_id)
    (function
        GapiRequest.NotModified session ->
        Utils.log_message "Resource (remote_id=%s) not modified\n%!"
          remote_id;
        SessionM.put session >>= fun () ->
        SessionM.return current_file
      | e -> throw e) >>= fun file ->
  Utils.log_message "done\n%!";
  SessionM.return file

let get_file_from_server parent_folder_id title trashed =
  Utils.log_message "Getting resource %s (in folder %s) from server...%!"
    title parent_folder_id;
  let q =
    Printf.sprintf "title = '%s' and '%s' in parents and trashed = %b"
      (escape_apostrophe title) parent_folder_id trashed in
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
        throw File_not_found
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
                 | e -> throw e)
    end >>= fun resource ->
    begin match resource.Cache.Resource.state with
        Cache.Resource.State.NotFound ->
          throw File_not_found
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

let with_retry f resource =
  let rec loop res n =
    with_try
      (f res)
      (function
           Resource_busy as e ->
             if n > 4 then throw e
             else begin
               GapiUtils.wait_exponential_backoff n;
               let fileId = res.Cache.Resource.remote_id |> Option.get in
               FilesResource.get
                 ~std_params:file_std_params
                 ~fileId >>= fun file ->
               let refreshed_resource =
                 update_resource_from_file
                   ~state:Cache.Resource.State.ToDownload res file in
               let context = Context.get_ctx () in
               let cache = context.Context.cache in
               update_cached_resource cache refreshed_resource;
               let n' = n + 1 in
               Utils.log_message
                 "Retry (%d) downloading resource (id=%Ld)...\n%!"
                 n' resource.Cache.Resource.id;
               loop refreshed_resource n'
             end
         | e -> throw e)
  in
    loop resource 0

let is_desktop_format resource config =
  Cache.Resource.get_format resource config = "desktop"

let get_export_link fmt resource =
  if fmt = "desktop" then ""
  else begin
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
  end

let create_desktop_entry resource content_path =
  Utils.with_out_channel
    ~mode:[Open_creat; Open_trunc; Open_wronly] content_path
    (fun out_ch ->
      Printf.fprintf out_ch
        "[Desktop Entry]\n\
         Type=Link\n\
         Name=%s\n\
         URL=%s\n"
        (Option.default "" resource.Cache.Resource.title)
        (Option.default "" resource.Cache.Resource.alternate_link);
      SessionM.return ())

let get_resource_size resource =
  let get_content_length headers =
    List.fold_left
      (fun u h ->
         match h with
             GapiCore.Header.ContentLength value -> Some (Int64.of_string value)
           | _ -> u)
      None
      headers
  in

  let context = Context.get_ctx () in
  let cache = Context.get_cache () in
  let config = context |. Context.config_lens in
  Utils.log_message "Getting resource size (id=%Ld)...%!"
    resource.Cache.Resource.id;
  let download_link =
    if Cache.Resource.is_document resource then
      let config = context |. Context.config_lens in
      let fmt = Cache.Resource.get_format resource config in
      get_export_link fmt resource
    else
      Option.default "" resource.Cache.Resource.download_url in
  begin if download_link <> "" then
    with_try
      (GapiService.head download_link get_content_length)
      (function
           GapiRequest.PermissionDenied session ->
             Utils.log_message "Server error: Permission denied.\n%!";
             throw Permission_denied
         | GapiRequest.RequestTimeout _ ->
             Utils.log_message "Server error: Request Timeout.\n%!";
             throw Resource_busy
         | GapiRequest.PreconditionFailed _
         | GapiRequest.Conflict _ ->
             Utils.log_message "Server error: Conflict.\n%!";
             throw Resource_busy
         | e -> throw e)
  else if is_desktop_format resource config then begin
    let content_path = Cache.get_content_path cache resource in
    create_desktop_entry resource content_path >>= fun () ->
    let stats = Unix.LargeFile.stat content_path in
    SessionM.return (Some stats.Unix.LargeFile.st_size)
  end else
    SessionM.return None
  end >>= fun content_length ->
  Utils.log_message "done: Content-Length=%Ld\n%!"
    (Option.default 0L content_length);
  SessionM.return content_length

let download_resource resource =
  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  let config = context |. Context.config_lens in
  let content_path = Cache.get_content_path cache resource in
  let create_empty_file () =
    Utils.log_message "Creating resource without content (path=%s)...\n%!"
      content_path;
    close_out (open_out content_path);
    SessionM.return () in
  let shrink_cache () =
    Option.may
      (fun file_size ->
         let metadata = context.Context.metadata |> Option.get in
         shrink_cache
           file_size
           config.Config.max_cache_size_mb
           metadata
           cache)
      resource.Cache.Resource.file_size;
    SessionM.return () in
  let do_download () =
    Utils.log_message "Downloading resource (id=%Ld)...%!"
      resource.Cache.Resource.id;
    let download_link =
      if Cache.Resource.is_document resource then
        let fmt = Cache.Resource.get_format resource config in
        get_export_link fmt resource
      else
        Option.default "" resource.Cache.Resource.download_url in
    begin if download_link <> "" then begin
      shrink_cache () >>= fun () ->
      with_try
        (let media_destination = GapiMediaResource.TargetFile content_path in
         GapiService.download_resource download_link media_destination)
        (function
             GapiRequest.PermissionDenied session ->
               Utils.log_message "Server error: Permission denied.\n%!";
               throw Permission_denied
           | GapiRequest.RequestTimeout _ ->
               Utils.log_message "Server error: Request Timeout.\n%!";
               throw Resource_busy
           | GapiRequest.PreconditionFailed _
           | GapiRequest.Conflict _ ->
               Utils.log_message "Server error: Conflict.\n%!";
               throw Resource_busy
           | GapiRequest.Forbidden _ ->
               Utils.log_message "Server error: Forbidden.\n%!";
               throw Resource_busy
           | e -> throw e)
    end else if is_desktop_format resource config then
      create_desktop_entry resource content_path
    else
      create_empty_file ()
    end >>= fun () ->
    let updated_resource = resource
      |> Cache.Resource.state ^= Cache.Resource.State.InSync in
    Utils.log_message "done\n%!";
    Cache.Resource.update_resource cache updated_resource;
    SessionM.return content_path
  in
  begin match resource.Cache.Resource.state with
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
        throw File_not_found
  end >>
  SessionM.return content_path

let stream_resource offset buffer resource =
  let download_link = Option.get resource.Cache.Resource.download_url in
  let length = Bigarray.Array1.dim buffer in
  let finish = Int64.add offset (Int64.of_int (length - 1)) in
  Utils.log_message
    "Stream resource (id=%Ld,offset=%Ld,finish=%Ld,length=%d)...%!"
    resource.Cache.Resource.id offset finish length;
  with_try
    (let media_destination = GapiMediaResource.ArrayBuffer buffer in
     GapiService.download_resource
       ~ranges:[(Some offset, Some finish)]
       download_link
       media_destination)
    (function
         GapiRequest.PermissionDenied session ->
           Utils.log_message "Server error: Permission denied.\n%!";
           throw Permission_denied
       | GapiRequest.RequestTimeout _ ->
           Utils.log_message "Server error: Request Timeout.\n%!";
           throw Resource_busy
       | GapiRequest.PreconditionFailed _
       | GapiRequest.Conflict _ ->
           Utils.log_message "Server error: Conflict.\n%!";
           throw Resource_busy
       | GapiRequest.Forbidden _ ->
           Utils.log_message "Server error: Forbidden.\n%!";
           throw Resource_busy
       | e -> throw e)
    >>= fun () ->
  Utils.log_message "done\n%!";
  SessionM.return ()

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
  let cache = context.Context.cache in
  let (path_in_cache, trashed) = get_path_in_cache path in

  let request_resource =
    get_resource path_in_cache trashed >>= fun resource ->
    let content_path = Cache.get_content_path cache resource in
    if Sys.file_exists content_path then begin
      SessionM.return (resource, content_path)
    end else begin if Cache.Resource.is_document resource &&
        config.Config.download_docs &&
        resource.Cache.Resource.file_size = Some 0L then begin
      with_try
        (with_retry get_resource_size resource)
        (function
             File_not_found -> SessionM.return None
           | e -> throw e) >>= fun file_size ->
      let updated_resource = resource
        |> Cache.Resource.file_size ^= file_size in
      Cache.Resource.update_resource cache updated_resource;
      SessionM.return (updated_resource, "")
    end else
      SessionM.return (resource, "")
    end
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
    let is_to_upload =
      resource.Cache.Resource.state = Cache.Resource.State.ToUpload in
    let st_mtime =
      match stat with
          Some st when is_to_upload ->
            st.Unix.LargeFile.st_mtime
        | _ ->
            resource |. Cache.Resource.modified_date |. GapiLens.option_get in
    let st_ctime =
      match stat with
          Some st when is_to_upload ->
            st.Unix.LargeFile.st_ctime
        | _ ->
            st_mtime
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

  let (path_in_cache, trashed) = get_path_in_cache path in

  let request_folder =
    Utils.log_message "Getting folder content (path=%s,trashed=%b)\n%!"
      path_in_cache trashed;
    get_resource path_in_cache trashed >>= fun resource ->
    get_folder_id path_in_cache trashed >>= fun folder_id ->
    let q =
      Printf.sprintf "'%s' in parents and trashed = %b" folder_id trashed in
    get_all_files q >>= fun files ->
    Utils.log_message "Done getting folder content (path=%s,trashed=%b)\n%!"
      path_in_cache trashed;
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
    if check_resource_in_cache cache path_in_cache trashed then begin
      Utils.log_message
        "Getting resources from db (parent path=%s,trashed=%b)...%!"
        path_in_cache trashed;
      let resources =
        Cache.Resource.select_resources_with_parent_path
          cache path_in_cache trashed in
      Utils.log_message "done\n%!";
      resources
    end else begin
      let (files, folder_resource) = do_request request_folder |> fst in
      let change_id = folder_resource.Cache.Resource.change_id in
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
                   let filename =
                     get_unique_filename_from_file file filename_table in
                   let resource_path =
                     Filename.concat path_in_cache filename in
                   let resource = create_resource resource_path change_id in
                   update_resource_from_file resource file)
          resources_and_files
      in
        Utils.log_message
          "Inserting folder resources into db (trashed=%b)...%!" trashed;
        let inserted_resources =
          Cache.Resource.insert_resources
            cache resources path_in_cache change_id trashed in
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
             Utils.log_message "Conflict detected: %!";
             GapiMonad.SessionM.put session >>
             begin match config.Config.conflict_resolution with
                 Config.ConflictResolutionStrategy.Client ->
                   Utils.log_message "Retrying...%!";
                   let resource_without_etag =
                     resource |> Cache.Resource.etag ^= None in
                   retry_update resource_without_etag >>= fun file ->
                   Utils.log_message "done\n%!";
                   SessionM.return file
               | Config.ConflictResolutionStrategy.Server ->
                   Utils.log_message "Keeping server changes.\n%!";
                   Utils.log_message
                     "Deleting resource (id=%Ld) from cache...%!"
                     resource.Cache.Resource.id;
                   Cache.Resource.delete_resource cache resource;
                   throw Resource_busy
             end
         | e -> throw e) >>= fun file_option ->
    begin match file_option with
        None ->
          purge_cache cache resource
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
      Utils.log_message "Touching file (id=%s)...%!" remote_id;
      FilesResource.touch
        ~std_params:file_std_params
        ~fileId:remote_id >>= fun touched_file ->
      Utils.log_message "done\n%!";
      Utils.log_message "Updating file mtime (id=%s,mtime=%f)...%!"
        remote_id mtime;
      let file_patch = File.empty
            |> File.modifiedDate ^= Netdate.create mtime in
      FilesResource.patch
        ~std_params:file_std_params
        ~setModifiedDate:true
        ~fileId:remote_id
        file_patch >>= fun patched_file ->
      Utils.log_message "done\n%!";
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
      (Option.default 0L resource.Cache.Resource.file_size) >
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
    Utils.log_message "Writing local file (path=%s,trashed=%b)...%!"
      path_in_cache trashed;
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
  let (path_in_cache, trashed) = get_path_in_cache path in

  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  let config = context |. Context.config_lens in
  get_resource path_in_cache trashed >>= fun resource ->
  match resource.Cache.Resource.state with
      Cache.Resource.State.ToUpload ->
        let content_path = Cache.get_content_path cache resource in
        let remote_id = resource |. Cache.Resource.remote_id |> Option.get in
        let media_source =
          GapiMediaResource.create_file_resource content_path in
        let resource_mime_type =
          resource |. Cache.Resource.mime_type |> Option.get in
        let content_type = media_source |. GapiMediaResource.content_type in
        (* Workaround to set the correct MIME type *)
        let mime_type =
          if resource_mime_type <> "" then resource_mime_type
          else content_type in
        let media_source = media_source
          |> GapiMediaResource.content_type ^= mime_type in
        Utils.log_message
          "Uploading file (cache path=%s, content type=%s)...%!"
          content_path mime_type;
        FilesResource.get
          ~std_params:file_std_params
          ~fileId:remote_id >>= fun refreshed_file ->
        let newRevision = config |. Config.new_revision in
        FilesResource.update
          ~std_params:file_std_params
          ~newRevision
          ~media_source
          ~fileId:remote_id
          refreshed_file >>= fun updated_file ->
        refresh_remote_resource resource updated_file >>= fun file ->
        Utils.log_message "done\n%!";
        let updated_resource =
          update_resource_from_file
            ~state:Cache.Resource.State.InSync resource file in
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
          updated_resource.Cache.Resource.file_size;
        SessionM.return ()
    | _ ->
        SessionM.return ()

(* flush *)
let flush path file_descr =
  do_request (upload_if_dirty path) |> ignore

(* fsync *)
let fsync path ds file_descr =
  do_request (upload_if_dirty path) |> ignore

(* release *)
let release path flags hnd =
  do_request (upload_if_dirty path) |> ignore

(* Create resources *)
let create_remote_resource is_folder path mode =
  let (path_in_cache, trashed) = get_path_in_cache path in
  if trashed then raise Permission_denied;

  let context = Context.get_ctx () in
  let largest_change_id = context |. Context.largest_change_id_lens in
  let cache = context.Context.cache in
  let parent_path = Filename.dirname path_in_cache in
  let create_file =
    get_resource parent_path trashed >>= fun parent_resource ->
    let parent_id =
      parent_resource |. Cache.Resource.remote_id |> Option.get
    in
    let parent_reference = ParentReference.empty
      |> ParentReference.id ^= parent_id in
    let title = Filename.basename path_in_cache in
    let mimeType =
      if is_folder
      then folder_mime_type
      else Mime.map_filename_to_mime_type title in
    let file = {
      File.empty with
          File.title;
          parents = [parent_reference];
          mimeType;
    } in
    Utils.log_message "Creating %s (path=%s,trashed=%b) on server...%!"
      (if is_folder then "folder" else "file") path_in_cache trashed;
    FilesResource.insert
      ~std_params:file_std_params
      file >>= fun created_file ->
    Utils.log_message "done\n%!";
    let new_resource = create_resource path_in_cache largest_change_id in
    Utils.log_message "Deleting 'NotFound' resources (path=%s) from cache...%!"
      path_in_cache;
    Cache.Resource.delete_not_found_resource_with_path cache path_in_cache;
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

(* Check if a folder is empty or not *)
let check_if_empty remote_id is_folder trashed =
  if is_folder then begin
    let q = Printf.sprintf "trashed = %b" trashed in
    ChildrenResource.list
      ~maxResults:1
      ~q
      ~folderId:remote_id >>= fun children ->
    if children.ChildList.items = [] then begin
      Utils.log_message "Folder (id=%s) is empty\n%!" remote_id;
      SessionM.return ()
    end else begin
      Utils.log_message "Folder (id=%s) is not empty\n%!" remote_id;
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
          let (path_in_cache, _) = get_path_in_cache path in
          Utils.log_message "Trashing folder old content (path=%s)...%!"
            path_in_cache;
          Cache.Resource.trash_all_with_parent_path cache path_in_cache;
          Utils.log_message "done\n%!";
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
    Utils.log_message "Permanently deleting file (id=%s)...%!" remote_id;
    FilesResource.delete
      ~std_params:file_std_params
      ~fileId:remote_id >>= fun () ->
    Utils.log_message "done\n%!";
    SessionM.return None
  in
  update_remote_resource
    ~purge_cache:(
      fun cache resource ->
        Cache.Resource.delete_resource cache resource;
        if is_folder then begin
          Utils.log_message
            "Deleting folder old content (path=%s,trashed=%b) from cache...%!"
            path_in_cache trashed;
          Cache.Resource.delete_all_with_parent_path
            cache path_in_cache trashed;
          Utils.log_message "done\n%!";
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
      trash_resource is_folder trashed path in
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
              |> Cache.Resource.path ^= new_path_in_cache
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
            new_path_in_cache;
          Cache.Resource.delete_not_found_resource_with_path
            cache new_path_in_cache;
          Utils.log_message "done\n%!";
          if Cache.Resource.is_folder resource then begin
            Utils.log_message
              "Deleting folder old content (path=%s,trashed=%b) from cache...%!"
              path_in_cache trashed;
            Cache.Resource.delete_all_with_parent_path
              cache path_in_cache trashed;
            Utils.log_message "done\n%!";
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
    Utils.log_message "Truncating file (id=%s)...%!" remote_id;
    let context = Context.get_ctx () in
    let cache = context.Context.cache in
    let metadata = context.Context.metadata |> Option.get in
    let config = context |. Context.config_lens in
    let updated_resource = resource
      |> Cache.Resource.file_size ^= Some size
      |> Cache.Resource.state ^= Cache.Resource.State.ToUpload in
    update_cached_resource cache updated_resource;
    shrink_cache
      (Int64.sub size (Option.default 0L resource.Cache.Resource.file_size))
      config.Config.max_cache_size_mb
      metadata cache;
    Unix.LargeFile.truncate content_path size;
    Utils.log_message "done\n%!";
    SessionM.return ()
  in
  do_request truncate_resource |> ignore
(* END truncate *)

