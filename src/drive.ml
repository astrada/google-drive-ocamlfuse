open GapiUtils.Infix
open GapiLens.Infix
open GapiMonad
open GapiMonad.SessionM.Infix
open GapiDriveV3Model
open GapiDriveV3Service

exception Directory_not_empty
exception Existing_attribute
exception File_not_found
exception IO_error
exception Invalid_operation
exception No_attribute
exception Permission_denied

let folder_mime_type = "application/vnd.google-apps.folder"
let file_fields =
  "appProperties,capabilities(canEdit),createdTime,explicitlyTrashed,\
   fileExtension,fullFileExtension,id,md5Checksum,mimeType,modifiedTime,\
   name,parents,size,trashed,version,viewedByMeTime,webViewLink,exportLinks"
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
let changes_std_params =
  { GapiService.StandardParameters.default with
        GapiService.StandardParameters.fields =
          "changes(removed,file(" ^ file_fields ^ "),fileId),\
           nextPageToken,newStartPageToken"
  }
let device_scope = "https://www.googleapis.com/auth/drive.file"
let device_root_folder = "gdfuse"

let do_request = Oauth2.do_request
let async_do_request f =
  let thread = Thread.create (fun go -> do_request go) f in
  let thread_id = Thread.id thread in
  Utils.log_with_header "Spawning new thread id=%d\n%!" thread_id;
  thread

let root_directory = "/"
let default_root_folder_id = "root"
let trash_directory = "/.Trash"
let trash_directory_name_length = String.length trash_directory
let trash_directory_base_path = "/.Trash/"
let lost_and_found_directory = "/lost+found"
let shared_with_me_directory = "/.shared"
let f_bsize = 4096L
let change_limit = 50
let max_link_target_length = 127
let max_attribute_length = 126

(* Utilities *)
let chars_blacklist_regexp = Str.regexp "[/\000]"
let clean_filename name = Str.global_replace chars_blacklist_regexp "_" name

let apostrophe_regexp = Str.regexp (Str.quote "'")
let escape_apostrophe name = Str.global_replace apostrophe_regexp "\\'" name

let json_length s =
  let length_with_quotes =
    `String s |> Yojson.Safe.to_string |> String.length in
  length_with_quotes - 2

let get_remote_id_fingerprint word_length remote_id =
  if word_length > 4 then
    invalid_arg "Too many filename conflicts";
  let md5 = Cryptokit.Hash.md5 () in
  md5#add_string remote_id;
  let md5_result = md5#result in
  let hexa = Cryptokit.Hexa.encode () in
  hexa#put_string md5_result;
  hexa#finish;
  let h = hexa#get_string in
  let length = word_length * 8 in
  let offset = 32 - length in
  String.sub h offset length

let disambiguate_filename
    filename
    full_file_extension
    remote_id
    filename_table =
  let rec find_first_unique_filename filename counter =
    let new_candidate =
      let fingerprint = get_remote_id_fingerprint counter remote_id in
      match full_file_extension with
      | "" ->
        Printf.sprintf "%s (%s)" filename fingerprint
      | extension ->
        let base_name =
          String.sub filename 0 (String.length filename -
                                 String.length extension - 1) in
        Printf.sprintf "%s (%s).%s" base_name fingerprint extension
    in
    if not (Hashtbl.mem filename_table new_candidate) then begin
      Utils.log_with_header "Checking: %s: OK\n%!" new_candidate;
      new_candidate
    end else begin
      Utils.log_with_header "Checking: %s: KO\n%!" new_candidate;
      find_first_unique_filename filename (counter + 1)
    end
  in
  if Hashtbl.mem filename_table filename then begin
    Utils.log_with_header "Filename collision detected: %s\n%!" filename;
    let unique_filename =
      find_first_unique_filename filename 1 in
    let name_counter = Hashtbl.find filename_table filename in
    Hashtbl.replace filename_table filename (name_counter + 1);
    unique_filename
  end else begin
    Utils.log_with_header "Filename (unused): %s\n%!" filename;
    Hashtbl.add filename_table filename 0;
    filename
  end

let is_in_trash_directory path config =
  if path = trash_directory || config.Config.disable_trash then false
  else ExtString.String.starts_with path trash_directory_base_path

let is_lost_and_found_root path trashed config =
  if trashed || not config.Config.lost_and_found then false
  else path = lost_and_found_directory

let is_lost_and_found path trashed config =
  if trashed || not config.Config.lost_and_found then false
  else ExtString.String.starts_with path lost_and_found_directory

let is_shared_with_me_root path trashed config =
  if trashed || not config.Config.shared_with_me then false
  else path = shared_with_me_directory

let is_shared_with_me path trashed config =
  if trashed || not config.Config.shared_with_me then false
  else ExtString.String.starts_with path shared_with_me_directory

let get_path_in_cache path config =
  if path = root_directory then
    (root_directory, false)
  else if path = trash_directory && not config.Config.disable_trash then
    (root_directory, true)
  else if is_in_trash_directory path config then
    let path_in_cache = Str.string_after path trash_directory_name_length in
    (path_in_cache, true)
  else
    (path, false)

let match_service_error reason =
  function
  | GapiService.ServiceError (_, e) ->
    begin match e.GapiError.RequestError.errors with
      | [] -> false
      | e :: _ ->
        e.GapiError.SingleError.reason = reason
    end
  | _ -> false

let handle_default_exceptions =
  function
  | GapiService.ServiceError (_, e) ->
    let message =
      e
      |> GapiError.RequestError.to_data_model
      |> GapiJson.data_model_to_json
      |> Yojson.Safe.to_string in
    Utils.log_with_header "Service error: %s.\n%!" message;
    begin match e.GapiError.RequestError.errors with
      | [] -> Utils.raise_m IO_error
      | e :: _ ->
        begin match e.GapiError.SingleError.reason with
          | "userRateLimitExceeded"
          | "rateLimitExceeded"
          | "backendError"
          | "downloadQuotaExceeded" -> Utils.raise_m Utils.Temporary_error
          | "insufficientFilePermissions"
          | "insufficientPermissions" -> Utils.raise_m Permission_denied
          | _ -> Utils.raise_m IO_error
        end
    end
  | GapiRequest.PermissionDenied _ ->
    Utils.log_with_header "Server error: Permission denied.\n%!";
    Utils.raise_m Permission_denied
  | GapiRequest.RequestTimeout _ ->
    Utils.log_with_header "Server error: Request Timeout.\n%!";
    Utils.raise_m Utils.Temporary_error
  | GapiRequest.PreconditionFailed _
  | GapiRequest.Conflict _ ->
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
  | Buffering.Invalid_block ->
    Utils.raise_m Invalid_operation
  | e -> Utils.raise_m e

(* with_try with a default exception handler *)
let try_with_default f s =
  Utils.try_with_m f handle_default_exceptions s

(* Resource cache *)
let get_filename name is_document get_document_format =
  let context = Context.get_ctx () in
  let config = context |. Context.config_lens in
  let clean_name = clean_filename name in
  let document_format =
    if is_document then get_document_format config
    else "" in
  Utils.log_with_header "document format: %s\n%!" document_format;
  if is_document &&
      config.Config.docs_file_extension &&
      document_format <> "" then
    let current_extension =
      try
        let dot_index = String.index clean_name '.' in
        String.sub clean_name (dot_index + 1)
          (String.length clean_name - dot_index - 1)
      with Not_found -> ""
    in
    if current_extension <> document_format then
      clean_name ^ "." ^ document_format
    else clean_name
  else clean_name

let get_file_extension_from_format resource config =
  let fmt = CacheData.Resource.get_format resource config in
  match fmt with
  | "desktop" when config.Config.desktop_entry_as_html -> "html"
  | _ -> fmt

let get_file_extension_from_mime_type mime_type config =
  let fmt =
    CacheData.Resource.get_format_from_mime_type mime_type config in
  match fmt with
  | "desktop" when config.Config.desktop_entry_as_html -> "html"
  | _ -> fmt

let build_resource_tables parent_path trashed =
  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  let resources =
    Cache.Resource.select_resources_with_parent_path
      cache parent_path trashed in
  let filename_table = Hashtbl.create Utils.hashtable_initial_size in
  let remote_id_table = Hashtbl.create (List.length resources) in
  List.iter
    (fun resource ->
       let name = Option.get resource.CacheData.Resource.name in
       let clean_name =
         get_filename
           name
           (CacheData.Resource.is_document resource)
           (fun config -> get_file_extension_from_format resource config)
       in
       let filename = Filename.basename resource.CacheData.Resource.path in
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
         (Option.get resource.CacheData.Resource.remote_id) resource)
    resources;
  (filename_table, remote_id_table)

let create_resource path =
  let parent_path = Filename.dirname path in
  { CacheData.Resource.id = 0L;
    remote_id = None;
    name = None;
    mime_type = None;
    created_time = None;
    modified_time = None;
    viewed_by_me_time = None;
    file_extension = None;
    full_file_extension = None;
    md5_checksum = None;
    size = None;
    can_edit = None;
    trashed = None;
    web_view_link = None;
    export_links = None;
    version = None;
    file_mode_bits = None;
    uid = None;
    gid = None;
    link_target = None;
    xattrs = "";
    parent_path;
    path;
    state = CacheData.Resource.State.ToDownload;
    last_update = Unix.gettimeofday ();
  }

let create_root_resource root_folder_id trashed =
  let resource = create_resource root_directory in
  { resource with
        CacheData.Resource.remote_id = Some root_folder_id;
        mime_type = Some folder_mime_type;
        size = Some 0L;
        parent_path = "";
        trashed = Some trashed;
  }

let create_well_known_resource path =
  let resource = create_resource path in
  { resource with
        CacheData.Resource.remote_id = Some "";
        mime_type = Some folder_mime_type;
        size = Some 0L;
        parent_path = "";
        trashed = Some false;
  }

let get_unique_filename
    name
    full_file_extension
    remote_id
    is_document
    get_document_format
    filename_table =
  let complete_name = get_filename name is_document get_document_format in
  disambiguate_filename
    complete_name
    full_file_extension
    remote_id
    filename_table

let get_unique_filename_from_resource resource name filename_table =
  get_unique_filename
    name
    (Option.default "" resource.CacheData.Resource.full_file_extension)
    (Option.default "" resource.CacheData.Resource.remote_id)
    (CacheData.Resource.is_document resource)
    (fun config -> get_file_extension_from_format resource config)
    filename_table

let get_unique_filename_from_file file filename_table =
  get_unique_filename
    file.File.name
    file.File.fullFileExtension
    file.File.id
    (CacheData.Resource.is_document_mime_type file.File.mimeType)
    (fun config ->
      get_file_extension_from_mime_type file.File.mimeType config)
    filename_table

let recompute_path resource name =
  (* TODO: make an optimized version of build_resource_tables that
   * doesn't create resource table (useful for large directories). *)
  let (filename_table, _) =
    build_resource_tables
      resource.CacheData.Resource.parent_path
      (Option.default false resource.CacheData.Resource.trashed) in
  let filename =
    get_unique_filename_from_resource resource name filename_table in
  Filename.concat resource.CacheData.Resource.parent_path filename

let update_resource_from_file ?state resource file =
  let path =
    match resource.CacheData.Resource.name with
        Some cached_name ->
          if cached_name <> file.File.name then
            recompute_path resource file.File.name
          else resource.CacheData.Resource.path
      | None -> resource.CacheData.Resource.path in
  let parent_path = Filename.dirname path in
  let new_state = Option.default resource.CacheData.Resource.state state in
  let new_size =
    match new_state with
    | CacheData.Resource.State.Uploading
    | CacheData.Resource.State.ToUpload ->
      resource.CacheData.Resource.size
    | _ ->
      Some file.File.size
  in
  { resource with
        CacheData.Resource.remote_id = Some file.File.id;
        name = Some file.File.name;
        mime_type = Some file.File.mimeType;
        created_time = Some (Netdate.since_epoch file.File.createdTime);
        modified_time = Some (Netdate.since_epoch file.File.modifiedTime);
        viewed_by_me_time =
          Some (Netdate.since_epoch file.File.viewedByMeTime);
        file_extension = Some file.File.fileExtension;
        full_file_extension = Some file.File.fullFileExtension;
        md5_checksum = Some file.File.md5Checksum;
        size = new_size;
        can_edit = Some file.File.capabilities.File.Capabilities.canEdit;
        trashed = Some file.File.trashed;
        web_view_link = Some file.File.webViewLink;
        export_links =
          Some (CacheData.Resource.serialize_export_links
                  file.File.exportLinks);
        version = Some file.File.version;
        file_mode_bits = CacheData.Resource.get_file_mode_bits
            file.File.appProperties;
        uid = CacheData.Resource.get_uid file.File.appProperties;
        gid = CacheData.Resource.get_gid file.File.appProperties;
        link_target = CacheData.Resource.get_link_target file.File.appProperties;
        xattrs = CacheData.Resource.get_xattrs file.File.appProperties;
        last_update = Unix.gettimeofday ();
        path;
        parent_path;
        state = new_state
  }

let insert_resource_into_cache ?state cache resource file =
  let resource = update_resource_from_file ?state resource file in
  Utils.log_with_header "BEGIN: Saving resource to db (remote id=%s)\n%!"
    file.File.id;
  let inserted = Cache.Resource.insert_resource cache resource in
  Utils.log_with_header "END: Saving resource to db (remote id=%s, id=%Ld, state=%s)\n%!"
    file.File.id
    inserted.CacheData.Resource.id
    (CacheData.Resource.State.to_string inserted.CacheData.Resource.state);
  inserted

let update_cached_resource cache resource =
  Utils.log_with_header
    "BEGIN: Updating resource in db (id=%Ld, state=%s)\n%!"
    resource.CacheData.Resource.id
    (CacheData.Resource.State.to_string resource.CacheData.Resource.state);
  Cache.Resource.update_resource cache resource;
  Utils.log_with_header "END: Updating resource in db (id=%Ld)\n%!"
    resource.CacheData.Resource.id

let update_cached_resource_state cache state id =
  Utils.log_with_header
    "BEGIN: Updating resource state in db (id=%Ld, state=%s)\n%!"
    id (CacheData.Resource.State.to_string state);
  Cache.Resource.update_resource_state cache state id;
  Utils.log_with_header "END: Updating resource state in db (id=%Ld)\n%!" id

let update_cached_resource_state_and_size cache state size id =
  Utils.log_with_header
    "BEGIN: Updating resource state and size in db (id=%Ld, state=%s, \
     size=%Ld)\n%!"
    id (CacheData.Resource.State.to_string state) size;
  Cache.Resource.update_resource_state_and_size cache state size id;
  Utils.log_with_header
    "END: Updating resource state and size in db (id=%Ld)\n%!" id

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
    let id = resource |. GapiLens.option_get |. CacheData.Resource.id in
    let state = resource
      |. GapiLens.option_get
      |. CacheData.Resource.state
      |> CacheData.Resource.State.to_string in
    Utils.log_with_header
      "END: Loading resource %s (trashed=%b) from db: Found (id=%Ld, state=%s)\n%!"
      path trashed id state;
  end end;
  resource

let update_cache_size delta metadata cache =
  Utils.log_with_header "BEGIN: Updating cache size (delta=%Ld) in db\n%!"
    delta;
  if delta = 0L then begin
    Utils.log_with_header "END: No need to update cache size\n%!";
  end else begin
    Cache.Metadata.update_cache_size cache delta;
    let update_metadata context =
      let metadata = context.Context.metadata
                     |. GapiLens.option_get
                     |> CacheData.Metadata.cache_size ^=
                        Int64.add metadata.CacheData.Metadata.cache_size delta in
      Utils.log_with_header "END: Updating cache size (new size=%Ld) in db\n%!"
        metadata.CacheData.Metadata.cache_size;
      context |> Context.metadata ^= Some metadata
    in
    Context.update_ctx update_metadata
  end

let shrink_cache ?(file_size = 0L) () =
  let context = Context.get_ctx () in
  let metadata = context |. Context.metadata_lens in
  let config = context |. Context.config_lens in
  let max_cache_size_mb = config.Config.max_cache_size_mb in
  let cache = context.Context.cache in
  Utils.with_lock context.Context.metadata_lock
    (fun () ->
       let max_cache_size =
         Int64.mul (Int64.of_int max_cache_size_mb) Utils.mb in
       let target_size =
         Int64.add metadata.CacheData.Metadata.cache_size file_size in
       if target_size > max_cache_size then begin
         let resources =
           Cache.Resource.select_resources_order_by_last_update cache in
         let (new_cache_size, total_delta, resources_to_free) =
           List.fold_left
             (fun (new_cache_size, delta, rs) resource ->
                if new_cache_size <= max_cache_size then
                  (new_cache_size, delta, rs)
                else begin
                  let size_to_free =
                    Option.default 0L resource.CacheData.Resource.size in
                  let new_size = Int64.sub new_cache_size size_to_free in
                  let new_delta = Int64.add delta (Int64.neg size_to_free) in
                  (new_size, new_delta, resource :: rs)
                end)
             (target_size, file_size, [])
             resources in
         update_cache_size total_delta metadata cache;
         List.iter
           (fun resource ->
              update_cached_resource_state cache
                CacheData.Resource.State.ToDownload resource.CacheData.Resource.id)
           resources_to_free;
         Cache.delete_files_from_cache cache resources_to_free |> ignore
       end else begin
          update_cache_size file_size metadata cache;
       end)

let delete_memory_buffers memory_buffers resource =
  Option.may
    (fun remote_id ->
       Buffering.MemoryBuffers.remove_buffers remote_id memory_buffers
    )
    resource.CacheData.Resource.remote_id

let delete_from_context context resource =
  let memory_buffers = context.Context.memory_buffers in
  delete_memory_buffers memory_buffers resource;
  Option.may
    (fun remote_id ->
       Context.with_ctx_lock
         (fun () -> Hashtbl.remove context.Context.file_locks remote_id)
    )
    resource.CacheData.Resource.remote_id

let delete_cached_resource resource =
  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  Cache.Resource.delete_resource cache resource;
  let total_size =
    Cache.delete_files_from_cache cache [resource] in
  Option.may
    (fun metadata ->
       update_cache_size (Int64.neg total_size) metadata cache
    )
    context.Context.metadata;
  delete_from_context context resource

let delete_cached_resources metadata cache resources =
  Cache.Resource.delete_resources cache resources;
  let total_size =
    Cache.delete_files_from_cache cache resources in
  update_cache_size (Int64.neg total_size) metadata cache;
  let context = Context.get_ctx () in
  List.iter
    (delete_from_context context)
    resources

let update_cache_size_for_documents cache resource content_path op =
  let context = Context.get_ctx () in
  Utils.with_lock context.Context.metadata_lock
    (fun () ->
      if resource.CacheData.Resource.size = Some 0L &&
          Sys.file_exists content_path then begin
        try
          let stats = Unix.LargeFile.stat content_path in
          let size = stats.Unix.LargeFile.st_size in
          let metadata = context |. Context.metadata_lens in
          let delta = op size in
          update_cache_size delta metadata cache
        with e -> Utils.log_exception e
      end)
(* END Resource cache *)

(* Metadata *)
let get_file_from_server parent_folder_id name trashed =
  let config = Context.get_ctx () |. Context.config_lens in
  Utils.log_with_header
    "BEGIN: Getting resource %s (in folder %s) from server\n%!"
    name parent_folder_id;
  let q =
    Printf.sprintf "name='%s' and '%s' in parents and trashed=%b"
      (escape_apostrophe name) parent_folder_id trashed in
  FilesResource.list
    ~supportsAllDrives:true
    ~driveId:config.Config.team_drive_id
    ~includeItemsFromAllDrives:(config.Config.team_drive_id <> "")
    ~corpora:(if config.Config.team_drive_id <> "" then "drive" else "user")
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

let get_root_folder_id_from_server config =
  Utils.log_with_header "BEGIN: Getting root resource from server\n%!";
  begin if config.Config.scope = device_scope then
      get_file_from_server
        default_root_folder_id device_root_folder false >>= fun root_option ->
      match root_option with
      | None ->
        let file = {
          File.empty with
          File.name = device_root_folder;
          mimeType = "application/vnd.google-apps.folder";
        } in
        Utils.log_with_header "BEGIN: Creating root (%s) on server\n%!"
          device_root_folder;
        FilesResource.create
          ~supportsAllDrives:true
          ~std_params:file_std_params
          file >>= fun created_file ->
        Utils.log_with_header "END: Creating root (id=%s) on server\n%!"
          created_file.File.id;
        SessionM.return created_file
      | Some root ->
        SessionM.return root
    else
      FilesResource.get
        ~supportsAllDrives:true
        ~std_params:file_std_params
        ~fileId:default_root_folder_id >>= fun file ->
      SessionM.return file
  end >>= fun file ->
  Utils.log_with_header "END: Getting root resource (id=%s) from server\n%!"
    file.File.id;
  SessionM.return file.File.id

let get_root_folder_id config =
  let rec loop path parent_folder_id =
    let (name, rest) =
      try
        ExtString.String.split path "/"
      with ExtString.Invalid_string -> (path, "")
    in
    match name with
    | "" -> SessionM.return parent_folder_id
    | n ->
      get_file_from_server parent_folder_id n false >>= fun file ->
      match file with
      | None -> Utils.raise_m (Failure "Invalid root folder in configuration")
      | Some f -> loop rest f.File.id
  in
  Utils.log_with_header
    "BEGIN: Getting root folder id (team drive id=%s, root folder=%s) \
     from server\n%!"
    config.Config.team_drive_id
    config.Config.root_folder;
  let default_root_id =
    match config.Config.team_drive_id with
    | "" -> default_root_folder_id
    | id -> id
  in
  begin match config.Config.root_folder with
    | "" -> SessionM.return default_root_id
    | s when ExtString.String.starts_with s "/" ->
      loop (String.sub s 1 (String.length s - 1)) default_root_id
    | s -> SessionM.return s
  end >>= fun root_folder_id ->
  begin if root_folder_id = default_root_folder_id then
      get_root_folder_id_from_server config
    else
      SessionM.return root_folder_id
  end >>= fun root_folder_id ->
  Utils.log_with_header
    "END: Getting root folder id (id=%s) from server\n%!"
    root_folder_id;
  SessionM.return root_folder_id

let get_root_folder_id_from_context () =
  let context = Context.get_ctx () in
  let config = context |. Context.config_lens in
  let root_folder_id_option = context.Context.root_folder_id in
  match root_folder_id_option with
  | None ->
    let root_folder_id = do_request (get_root_folder_id config) |> fst in
    Context.update_ctx (Context.root_folder_id ^= Some root_folder_id);
    root_folder_id
  | Some r -> r

let get_well_known_resource path trashed =
  let root_folder_id = get_root_folder_id_from_context () in
  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  let config = context |. Context.config_lens in
  match lookup_resource path trashed with
  | None ->
    let (well_known_resource, label) =
      if path = root_directory then
        (create_root_resource root_folder_id trashed, "root")
      else if is_lost_and_found_root path trashed config then
        (create_well_known_resource lost_and_found_directory, "lost+found")
      else if is_shared_with_me_root path trashed config then
        (create_well_known_resource shared_with_me_directory, "shared with me")
      else invalid_arg ("Invalid well known path: " ^ path ^ " trashed=" ^
                        (string_of_bool trashed))
    in
    Utils.log_with_header
      "BEGIN: Saving %s resource to db\n%!"
      label;
    let inserted =
      Cache.Resource.insert_resource cache well_known_resource in
    Utils.log_with_header
      "END: Saving %s resource to db (id=%Ld)\n%!"
      label inserted.CacheData.Resource.id;
    inserted
  | Some resource -> resource

let get_metadata () =
  let config = Context.get_ctx () |. Context.config_lens in
  let request_new_start_page_token =
    let std_params =
      { GapiService.StandardParameters.default with
            GapiService.StandardParameters.fields =
              "startPageToken"
      } in
    ChangesResource.getStartPageToken
      ~supportsAllDrives:true
      ~driveId:config.Config.team_drive_id
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
    get_start_page_token start_page_token_db >>= fun start_page_token ->
    let metadata = {
      CacheData.Metadata.display_name = about.About.user.User.displayName;
      storage_quota_limit = about.About.storageQuota.About.StorageQuota.limit;
      storage_quota_usage = about.About.storageQuota.About.StorageQuota.usage;
      start_page_token;
      cache_size;
      last_update = Unix.gettimeofday ();
      clean_shutdown = false;
    } in
    SessionM.return metadata
  in

  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  let config = context |. Context.config_lens in

  let update_resource_cache new_metadata old_metadata =
    let get_all_changes =
      let rec loop pageToken accu =
        ChangesResource.list
          ~supportsAllDrives:true
          ~driveId:config.Config.team_drive_id
          ~includeItemsFromAllDrives:(config.Config.team_drive_id <> "")
          ~std_params:changes_std_params
          ~includeRemoved:true
          ~pageToken >>= fun change_list ->
        let changes = change_list.ChangeList.changes @ accu in
        if change_list.ChangeList.nextPageToken = "" then
          SessionM.return (changes, change_list.ChangeList.newStartPageToken)
        else
          loop change_list.ChangeList.nextPageToken changes
      in
      loop new_metadata.CacheData.Metadata.start_page_token []
    in

    let request_changes =
      Utils.log_with_header "BEGIN: Getting changes from server\n%!";
      get_all_changes >>= fun (changes, new_start_page_token) ->
      Utils.log_with_header "END: Getting changes from server\n%!";
      SessionM.return (changes, new_start_page_token)
    in

    let get_resources_and_files_to_update change =
      let selected_resources =
        Cache.Resource.select_resources_with_remote_id
          cache change.Change.fileId
      in
      List.filter
        (fun r -> change.Change.file.File.version > 0L &&
                  change.Change.file.File.version >
                  Option.default 0L r.CacheData.Resource.version)
        selected_resources |>
      List.map (fun r -> Some (r, change.Change.file))
    in

    let get_resource_from_change change =
      Cache.Resource.select_resources_with_remote_id cache
        change.Change.fileId |>
      List.map
        (fun r -> Some r)
    in

    let get_new_resource_from_change change =
      match Cache.Resource.select_resources_with_remote_id cache
              change.Change.fileId with
      | [] ->
        let parent_resources =
          let parent_remote_ids =
            match change.Change.file.File.parents with
            | [] -> []
            | ids -> ids
          in
          List.map
            (Cache.Resource.select_resources_with_remote_id cache)
            parent_remote_ids |>
          List.concat |>
          List.filter
            (fun r -> r.CacheData.Resource.state =
                      CacheData.Resource.State.Synchronized
            )
        in
        begin match parent_resources with
        | [] -> []
        | prs ->
          let parent_path =
            List.hd prs |. CacheData.Resource.path in 
          let (filename_table, _) =
            build_resource_tables parent_path false in
          let filename =
            get_unique_filename_from_file change.Change.file filename_table in
          let resource_path =
            Filename.concat parent_path filename in
          let resource = create_resource resource_path in
          [Some (resource, change.Change.file)]
        end
      | _ -> []
    in

    let request_remaining_changes start_page_token_db =
      if start_page_token_db = "" then
        SessionM.return (false, true)
      else
        let std_params =
          { GapiService.StandardParameters.default with
                GapiService.StandardParameters.fields = "newStartPageToken"
          } in
        ChangesResource.list
          ~supportsAllDrives:true
          ~driveId:config.Config.team_drive_id
          ~includeItemsFromAllDrives:(config.Config.team_drive_id <> "")
          ~std_params
          ~includeRemoved:true
          ~pageSize:change_limit
          ~pageToken:start_page_token_db >>= fun change_list ->
        let (no_changes, over_limit) =
          (change_list.ChangeList.newStartPageToken = start_page_token_db,
           change_list.ChangeList.newStartPageToken = "") in
        SessionM.return (no_changes, over_limit)
    in

    request_remaining_changes
      new_metadata.CacheData.Metadata.start_page_token >>= fun (no_changes,
                                                            over_limit) ->
    if no_changes then begin
      Utils.log_with_header
        "END: Getting metadata: No need to update resource cache\n%!";
      Utils.log_with_header "BEGIN: Updating timestamps\n%!";
      Cache.Resource.update_all_timestamps cache
        new_metadata.CacheData.Metadata.last_update;
      Utils.log_with_header "END: Updating timestamps\n%!";
      SessionM.return new_metadata
    end else if over_limit then begin
      Utils.log_with_header "END: Getting metadata: Too many changes\n";
      Utils.log_with_header "BEGIN: Getting new start page token\n%!";
      get_start_page_token "" >>= fun new_start_page_token ->
      Utils.log_with_header "END: Getting new start page token (%s)\n%!"
        new_start_page_token;
      Utils.log_with_header "BEGIN: Invalidating resources\n%!";
      Cache.Resource.invalidate_all cache;
      Utils.log_with_header "END: Invalidating resources\n%!";
      SessionM.return {
        new_metadata with
            CacheData.Metadata.start_page_token = new_start_page_token;
      }
    end else begin
      Utils.log_with_header "BEGIN: Updating timestamps\n%!";
      Cache.Resource.update_all_timestamps cache
        new_metadata.CacheData.Metadata.last_update;
      Utils.log_with_header "END: Updating timestamps\n%!";
      match old_metadata with
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
                        | None -> xs'
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

          Utils.log_with_header "BEGIN: Adding new resources to cache\n%!";
          update_resource_cache_from_changes
            (fun change ->
               not change.Change.removed &&
               not change.Change.file.File.trashed)
            get_new_resource_from_change
            (fun cache resources_and_files ->
               List.iter
                 (fun (resource, file) ->
                    insert_resource_into_cache cache resource file |>
                    ignore
                 )
                 resources_and_files
               );
          Utils.log_with_header "END: Adding new resources to cache\n";
          Utils.log_with_header "BEGIN: Updating resource cache\n%!";
          update_resource_cache_from_changes
            (fun change ->
               not change.Change.removed &&
               not change.Change.file.File.trashed)
            get_resources_and_files_to_update
            (fun cache resources_and_files ->
               List.iter
                 (fun (r, f) ->
                    Utils.log_with_header
                      "BEGIN: Refreshing resource (id=%Ld)\n%!"
                      r.CacheData.Resource.id;
                    let updated_resource =
                      update_resource_from_file r f in
                    update_cached_resource cache updated_resource;
                    Utils.log_with_header
                      "END: Refreshing resource (id=%Ld)\n%!"
                      updated_resource.CacheData.Resource.id;
                 )
                 resources_and_files;
               let ids =
                 List.map
                   (fun (r, _) -> r.CacheData.Resource.id)
                   resources_and_files in
               Utils.log_with_header "Invalidating resources: ids=%s\n%!"
                 (String.concat ", " (List.map Int64.to_string ids));
               Cache.Resource.invalidate_resources cache ids);
          Utils.log_with_header "END: Updating resource cache\n";
          Utils.log_with_header "BEGIN: Updating trashed resources\n%!";
          update_resource_cache_from_changes
            (fun change -> change.Change.file.File.trashed)
            get_resource_from_change
            (fun cache resources ->
               Utils.log_with_header "Trashing resources: ids=%s\n%!"
                 (String.concat ", "
                    (List.map
                       (fun r -> Int64.to_string r.CacheData.Resource.id)
                       resources));
               Cache.Resource.trash_resources cache resources);
          Utils.log_with_header "END: Updating trashed resources\n";
          Utils.log_with_header "BEGIN: Removing deleted resources\n%!";
          update_resource_cache_from_changes
            (fun change -> change.Change.removed)
            get_resource_from_change
            (fun cache resources ->
               Utils.log_with_header "Deleting resources: ids=%s\n%!"
                 (String.concat ", "
                    (List.map
                       (fun r -> Int64.to_string r.CacheData.Resource.id)
                       resources));
               delete_cached_resources new_metadata cache resources);
          Utils.log_with_header "END: Removing deleted resources\n%!";
          if List.length changes > 0 then begin
            if not config.Config.disable_trash then begin
              Utils.log_with_header
                "BEGIN: Invalidating trash bin resource\n%!";
              Cache.Resource.invalidate_trash_bin cache;
              Utils.log_with_header "END: Invalidating trash bin resource\n%!";
            end;
            if config.Config.lost_and_found then begin
              Utils.log_with_header
                "BEGIN: Invalidating lost+found resource\n%!";
              Cache.Resource.invalidate_path cache lost_and_found_directory;
              Utils.log_with_header
                "END: Invalidating lost+found resource\n%!";
            end;
            if config.Config.shared_with_me then begin
              Utils.log_with_header
                "BEGIN: Invalidating .shared resource\n%!";
              Cache.Resource.invalidate_path cache shared_with_me_directory;
              Utils.log_with_header
                "END: Invalidating .shared resource\n%!";
            end
          end;
          SessionM.return {
            new_metadata with
                CacheData.Metadata.start_page_token = new_start_page_token;
          }
    end
  in

  let refresh_metadata old_metadata =
    let start_page_token =
      Option.map_default
        CacheData.Metadata.start_page_token.GapiLens.get "" old_metadata in
    let cache_size =
      Option.map_default
        CacheData.Metadata.cache_size.GapiLens.get 0L old_metadata in
    Utils.log_with_header "BEGIN: Refreshing metadata\n%!";
    try_with_default
      (request_metadata start_page_token cache_size) >>= fun server_metadata ->
    Utils.log_with_header "END: Refreshing metadata\n";
    update_resource_cache
      server_metadata old_metadata >>= fun updated_metadata ->
    Utils.log_with_header "BEGIN: Updating metadata in db\n%!";
    Cache.Metadata.insert_metadata context.Context.cache updated_metadata;
    Utils.log_with_header "END: Updating metadata in db\n";
    Utils.log_with_header "BEGIN: Updating context\n%!";
    Context.update_ctx (Context.metadata ^= Some updated_metadata);
    Utils.log_with_header "END: Updating context\n%!";
    SessionM.return updated_metadata
  in

  let resync_cache_size db_metadata =
    let old_cache_size = db_metadata.CacheData.Metadata.cache_size in
    Utils.log_with_header
      "BEGIN: Recalculating cache size (old value=%Ld)\n%!"
      old_cache_size;
    let cache_size = Cache.compute_cache_size context.Context.cache in
    Utils.log_with_header
      "END: Recalculating cache size (new value=%Ld)\n%!"
      cache_size;
    db_metadata |> CacheData.Metadata.cache_size ^= cache_size
  in

  Utils.with_lock context.Context.metadata_lock
    (fun () ->
       let metadata =
         let context = Context.get_ctx () in
         if Option.is_none context.Context.metadata then begin
           Utils.log_with_header "BEGIN: Loading metadata from db\n%!";
           let db_metadata =
             Cache.Metadata.select_metadata context.Context.cache in
           let db_metadata =
             Option.map resync_cache_size db_metadata in
           Context.update_ctx (Context.metadata ^= db_metadata);
           db_metadata
         end else begin
           Utils.log_with_header "BEGIN: Getting metadata from context\n%!";
           context.Context.metadata
         end in

       match metadata with
         None ->
           Utils.log_with_header "END: Getting metadata: Not found\n%!";
           do_request (refresh_metadata metadata) |> fst
       | Some m ->
           let metadata_cache_time =
             context |. Context.config_lens |. Config.metadata_cache_time
           in
           if CacheData.Metadata.is_valid metadata_cache_time m then begin
             Utils.log_with_header "END: Getting metadata: Valid\n%!";
             m
           end else begin
             Utils.log_with_header "END: Getting metadata: Not valid\n%!";
             do_request (refresh_metadata metadata) |> fst
           end
    )

let statfs () =
  let metadata = get_metadata () in
  let config = Context.get_ctx () |. Context.config_lens in
  let limit =
    if metadata.CacheData.Metadata.storage_quota_limit = 0L ||
       config.Config.team_drive_id <> "" then
      Int64.max_int
    else metadata.CacheData.Metadata.storage_quota_limit in
  let f_blocks = Int64.div limit f_bsize in
  let free_bytes =
    Int64.sub limit metadata.CacheData.Metadata.storage_quota_usage in
  let f_bfree = Int64.div free_bytes f_bsize in
  { Fuse.Unix_util.f_bsize;
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
let get_resource_from_server parent_folder_id name new_resource trashed cache =
  get_file_from_server parent_folder_id name trashed >>= fun file ->
  match file with
      None ->
        Utils.log_with_header
          "BEGIN: Saving not found resource to db (name=%s)\n%!"
          name;
        let resource = new_resource
          |> CacheData.Resource.trashed ^= Some trashed
          |> CacheData.Resource.state ^= CacheData.Resource.State.NotFound in
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
        if CacheData.Resource.is_valid resource metadata_last_update then
          if CacheData.Resource.is_folder resource then
            resource.CacheData.Resource.state = CacheData.Resource.State.Synchronized
          else true
        else false

let rec get_folder_id path trashed =
  if path = root_directory then
    let root_folder_id = get_root_folder_id_from_context () in
    SessionM.return root_folder_id
  else
    get_resource path trashed >>= fun resource ->
    let remote_id =
      resource |. CacheData.Resource.remote_id |. GapiLens.option_get in
    SessionM.return remote_id
and get_resource path trashed =
  let config = Context.get_ctx () |. Context.config_lens in
  let metadata_last_update =
    get_metadata () |. CacheData.Metadata.last_update in

  let get_new_resource cache =
    let parent_path = Filename.dirname path in
      if check_resource_in_cache cache parent_path trashed then begin
        (* If parent_path is up to date, all resources are already cached,
         * so a new resource must be a "not found" one. *)
        Utils.raise_m File_not_found
      end else begin
        let new_resource = create_resource path in
        let name = Filename.basename path in
        get_folder_id
          new_resource.CacheData.Resource.parent_path
          trashed >>= fun parent_folder_id ->
        get_resource_from_server
          parent_folder_id name new_resource trashed cache >>= fun resource ->
        SessionM.return resource
      end
  in

  let refresh_resource resource cache =
    begin if Option.is_some resource.CacheData.Resource.remote_id then begin
      let remote_id = resource.CacheData.Resource.remote_id |> Option.get in
      Utils.log_with_header
        "BEGIN: Getting file from server (remote id=%s)\n%!"
        remote_id;
      FilesResource.get
        ~supportsAllDrives:true
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
          delete_cached_resource resource;
          get_new_resource cache
      | Some file ->
          let reloaded_resource = Option.map_default
              (Cache.Resource.select_first_resource_with_remote_id cache)
              (Some resource)
              resource.CacheData.Resource.remote_id |> Option.default resource in
          let updated_resource = update_resource_from_file
              reloaded_resource file in
          update_cached_resource cache updated_resource;
          Utils.log_with_header
            "END: Refreshing resource (id=%Ld)\n%!"
            updated_resource.CacheData.Resource.id;
          SessionM.return updated_resource
  in

  if path = root_directory then
    let root_resource =
      get_well_known_resource root_directory trashed in
    SessionM.return root_resource
  else if is_lost_and_found_root path trashed config then
    let lost_and_found_resource =
      get_well_known_resource lost_and_found_directory trashed in
    SessionM.return lost_and_found_resource
  else if is_shared_with_me_root path trashed config then
    let shared_with_me_resource =
      get_well_known_resource shared_with_me_directory trashed in
    SessionM.return shared_with_me_resource
  else
    let cache = Context.get_cache () in
    begin match lookup_resource path trashed with
        None ->
          get_new_resource cache
      | Some resource ->
          if CacheData.Resource.is_valid resource metadata_last_update then
            SessionM.return resource
          else
            try_with_default (refresh_resource resource cache)
    end >>= fun resource ->
    begin match resource.CacheData.Resource.state with
        CacheData.Resource.State.NotFound ->
          Utils.raise_m File_not_found
      | _ ->
          SessionM.return resource
    end

let check_md5_checksum resource cache =
  let path = resource.CacheData.Resource.path in
  let content_path = Cache.get_content_path cache resource in
  let md5_checksum = Option.default "" resource.CacheData.Resource.md5_checksum in
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
    Utils.try_with_m
      (f res)
      (function
           Utils.Temporary_error ->
             if n >= !Utils.max_retries then begin
               Utils.raise_m IO_error
             end else begin
               GapiUtils.wait_exponential_backoff n;
               let fileId = res.CacheData.Resource.remote_id |> Option.get in
               FilesResource.get
                 ~supportsAllDrives:true
                 ~std_params:file_std_params
                 ~fileId >>= fun file ->
               let (state, verb) =
                 if resource.CacheData.Resource.state =
                    CacheData.Resource.State.ToUpload then
                   (CacheData.Resource.State.ToUpload, "uploading")
                 else
                   (CacheData.Resource.State.ToDownload, "downloading") in
               let refreshed_resource =
                 update_resource_from_file
                   ~state res file in
               let context = Context.get_ctx () in
               let cache = context.Context.cache in
               update_cached_resource cache refreshed_resource;
               let n' = n + 1 in
               Utils.log_with_header
                 "Retry (%d/%d) %s resource (id=%Ld).\n%!"
                 n' !Utils.max_retries verb resource.CacheData.Resource.id;
               loop refreshed_resource n'
             end
         | e -> Utils.raise_m e)
  in
    loop resource 0

let is_desktop_format resource config =
  CacheData.Resource.get_format resource config = "desktop"

let create_desktop_entry resource content_path config =
  Utils.with_out_channel
    ~mode:[Open_creat; Open_trunc; Open_wronly] content_path
    (fun out_ch ->
      let icon_entry =
        let icon = CacheData.Resource.get_icon resource config in
        if icon = "" then ""
        else "Icon=" ^ icon ^ "\n"
      in
      let url = Option.default "" resource.CacheData.Resource.web_view_link in
      let exec = config.Config.desktop_entry_exec in
      let entry_type =
        if exec <> "" then "Type=Application"
        else "Type=Link" in
      let exec_or_url_entry =
        if exec <> "" then Printf.sprintf "Exec=%s \"%s\"" exec url
        else "URL=" ^ url
      in
      Printf.fprintf out_ch
        "[Desktop Entry]\n\
         %s\n\
         Name=%s\n\
         %s\n\
         %s"
        entry_type
        (Option.default "" resource.CacheData.Resource.name)
        exec_or_url_entry
        icon_entry)

let create_html_with_redirect resource content_path config =
  Utils.with_out_channel
    ~mode:[Open_creat; Open_trunc; Open_wronly] content_path
    (fun out_ch ->
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

let download_media media_download fileId =
  Utils.try_with_m
    (FilesResource.get
       ~supportsAllDrives:true
       ~std_params:file_download_std_params
       ~media_download
       ~fileId)
    (fun e ->
       let config = Context.get_ctx () |. Context.config_lens in
       if match_service_error "cannotDownloadAbusiveFile" e &&
          config.Config.acknowledge_abuse then begin
         Utils.log_with_header
           "Warning: abusive file detected, but downloading anyway (fileId=%s)\n%!"
           fileId;
         FilesResource.get
           ~supportsAllDrives:true
           ~acknowledgeAbuse:true
           ~std_params:file_download_std_params
           ~media_download
           ~fileId >>= fun file ->
         SessionM.return file
       end else
         handle_default_exceptions e)

let flush_memory_buffers resource =
  let context = Context.get_ctx () in
  let config = context |. Context.config_lens in
  if config.Config.write_buffers then begin
    let memory_buffers = context.Context.memory_buffers in
    Buffering.MemoryBuffers.flush_blocks
      (resource.CacheData.Resource.remote_id |> Option.get)
      memory_buffers
  end

let download_resource resource =
  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  let config = context |. Context.config_lens in
  let content_path = Cache.get_content_path cache resource in
  let shrink_cache_before_downloading () =
    let file_size = Option.default 0L resource.CacheData.Resource.size in
    shrink_cache ~file_size ();
    SessionM.return () in
  let do_api_download () =
    let destination = GapiMediaResource.TargetFile content_path in
    let media_download = {
      GapiMediaResource.destination;
      range_spec = "";
    } in
    let fileId = resource.CacheData.Resource.remote_id |> Option.get in
    if CacheData.Resource.is_document resource then begin
      let fmt = CacheData.Resource.get_format resource config in
      let mimeType = CacheData.Resource.mime_type_of_format fmt in
      let export_links =
        CacheData.Resource.parse_export_links
          (Option.default "" resource.CacheData.Resource.export_links) in
      begin try
        let export_link = List.assoc mimeType export_links in
        GapiService.get ~media_download export_link
          GapiRequest.parse_empty_response
      with Not_found ->
        FilesResource.export
          ~media_download
          ~fileId
          ~mimeType >>= fun () ->
        SessionM.return ()
      end
    end else if Option.default 0L
        resource.CacheData.Resource.size > 0L then begin
      download_media media_download fileId >>= fun _ ->
      SessionM.return ()
    end else begin
      Utils.log_with_header
        "BEGIN: Creating resource without content (path=%s)\n%!"
        content_path;
      close_out (open_out content_path);
      SessionM.return ()
    end
  in
  let do_download =
    SessionM.return () >>= fun () ->
    Utils.log_with_header
      "BEGIN: Downloading resource (id=%Ld) to %s\n%!"
      resource.CacheData.Resource.id content_path;
    begin if is_desktop_format resource config then begin
      shrink_cache_before_downloading () >>= fun () ->
      update_cache_size_for_documents cache resource content_path Int64.neg;
      begin if config.Config.desktop_entry_as_html then begin
        create_html_with_redirect resource content_path config;
      end else begin
        create_desktop_entry resource content_path config;
      end end;
      SessionM.return ()
    end else begin
      shrink_cache_before_downloading () >>= fun () ->
      update_cached_resource_state cache
        CacheData.Resource.State.Downloading resource.CacheData.Resource.id;
      update_cache_size_for_documents cache resource content_path Int64.neg;
      Utils.try_with_m
        (do_api_download ())
        (fun e ->
           update_cached_resource_state cache
             CacheData.Resource.State.ToDownload resource.CacheData.Resource.id;
           handle_default_exceptions e)
    end end >>= fun () ->
    update_cache_size_for_documents cache resource content_path Std.identity;
    Utils.log_with_header
      "END: Downloading resource (id=%Ld) to %s\n%!"
      resource.CacheData.Resource.id content_path;
    update_cached_resource_state cache
      CacheData.Resource.State.Synchronized resource.CacheData.Resource.id;
    SessionM.return ()
  in
  let get_lock () =
    let context = Context.get_ctx () in
    Context.with_ctx_lock
      (fun () ->
         let remote_id = resource.CacheData.Resource.remote_id |> Option.get in
         match Utils.safe_find context.Context.file_locks remote_id with
         | None ->
           let mutex = Mutex.create () in
           Hashtbl.add context.Context.file_locks remote_id mutex;
           mutex
         | Some mutex -> mutex
      )
  in
  let do_download_with_lock () =
    let mutex = get_lock () in
    Utils.with_lock_m mutex do_download
  in
  let rec check_state n =
    let reloaded_resource = Option.map_default
      (Cache.Resource.select_first_resource_with_remote_id cache)
      (Some resource)
      resource.CacheData.Resource.remote_id
    in
    let reloaded_state = match reloaded_resource with
        None -> CacheData.Resource.State.NotFound
      | Some r -> r.CacheData.Resource.state
    in
    let download_if_not_updated () =
      let r = reloaded_resource |> Option.get in
      if check_md5_checksum r cache then begin
        update_cached_resource_state cache
          CacheData.Resource.State.Synchronized resource.CacheData.Resource.id;
        SessionM.return ()
      end else
        do_download_with_lock ()
    in
    begin match reloaded_state with
      | CacheData.Resource.State.Synchronized
      | CacheData.Resource.State.ToUpload
      | CacheData.Resource.State.Uploading ->
          if Sys.file_exists content_path then begin
            SessionM.return ()
          end else
            do_download_with_lock ()
      | CacheData.Resource.State.ToDownload ->
          download_if_not_updated ()
      | CacheData.Resource.State.Downloading ->
          if n > 300 then begin
            Utils.log_with_header
              "Still downloading resource (id=%Ld) after about 5 hours: start downloading again\n%!"
              resource.CacheData.Resource.id;
            download_if_not_updated ()
          end else begin
            Utils.log_with_header
              "Already downloading resource (id=%Ld): check number %d\n%!"
              resource.CacheData.Resource.id
              n;
            let n' = min n 6 in
            GapiUtils.wait_exponential_backoff n';
            check_state (n + 1)
          end
      | CacheData.Resource.State.NotFound ->
          Utils.raise_m File_not_found
    end
  in
  check_state 0 >>= fun () ->
  SessionM.return content_path

let stream_resource offset buffer resource =
  let length = Bigarray.Array1.dim buffer in
  let finish = Int64.add offset (Int64.of_int (length - 1)) in
  Utils.log_with_header
    "BEGIN: Stream resource (id=%Ld, offset=%Ld, finish=%Ld, length=%d)\n%!"
    resource.CacheData.Resource.id offset finish length;
  let destination = GapiMediaResource.ArrayBuffer buffer in
  let range_spec =
    GapiMediaResource.generate_range_spec [(Some offset, Some finish)] in
  let media_download = {
    GapiMediaResource.destination;
    range_spec;
  } in
  let fileId = resource |. CacheData.Resource.remote_id |> Option.get in
  download_media media_download fileId >>= fun _ ->
  Utils.log_with_header
    "END: Stream resource (id=%Ld, offset=%Ld, finish=%Ld, length=%d)\n%!"
    resource.CacheData.Resource.id offset finish length;
  SessionM.return ()

let start_buffer_eviction_thread context memory_buffers =
  let config = context |. Context.config_lens in
  if config.Config.stream_large_files then begin
    if Option.is_none context.Context.buffer_eviction_thread then begin
      let thread =
        Buffering.MemoryBuffers.create_eviction_thread memory_buffers in
      Utils.log_with_header
        "Starting buffer eviction thread (TID=%d)\n%!"
        (Thread.id thread);
      Context.update_ctx (Context.buffer_eviction_thread ^= Some thread)
    end
  end

let stream_resource_to_memory_buffer offset buffer resource =
  let context = Context.get_ctx () in
  let memory_buffers = context.Context.memory_buffers in
  start_buffer_eviction_thread context memory_buffers;
  let remote_id = resource.CacheData.Resource.remote_id |> Option.get in
  Buffering.MemoryBuffers.read_block
    remote_id offset (resource.CacheData.Resource.size |> Option.get)
    (fun start_pos block_buffer ->
       stream_resource start_pos block_buffer resource)
    ~dest_arr:buffer memory_buffers >>= fun () ->
  SessionM.return ()

let stream_resource_to_read_ahead_buffers offset resource =
  let context = Context.get_ctx () in
  let memory_buffers = context.Context.memory_buffers in
  start_buffer_eviction_thread context memory_buffers;
  let remote_id = resource.CacheData.Resource.remote_id |> Option.get in
  let config = context |. Context.config_lens in
  Buffering.MemoryBuffers.read_ahead config.Config.read_ahead_buffers
    remote_id offset (resource.CacheData.Resource.size |> Option.get)
    (fun start_pos block_buffer ->
       stream_resource start_pos block_buffer resource)
    memory_buffers >>= fun ms ->
  List.map
    (fun m -> with_retry (fun _ -> m) resource)
    ms |> SessionM.return

let is_filesystem_read_only () =
  Context.get_ctx () |. Context.config_lens |. Config.read_only

let is_file_read_only resource =
  let config = Context.get_ctx () |. Context.config_lens in
  not (Option.default true resource.CacheData.Resource.can_edit) ||
  CacheData.Resource.is_document resource ||
  config.Config.large_file_read_only &&
    CacheData.Resource.is_large_file config resource

(* stat *)
let get_attr path =
  let context = Context.get_ctx () in
  let config = context |. Context.config_lens in
  let (path_in_cache, trashed) = get_path_in_cache path config in

  let request_resource =
    get_resource path_in_cache trashed >>= fun resource ->
    begin if CacheData.Resource.is_document resource &&
             config.Config.download_docs then
      Utils.try_with_m
        (flush_memory_buffers resource;
         with_retry download_resource resource)
        (function
             File_not_found -> SessionM.return ""
           | e -> Utils.raise_m e)
    else
      SessionM.return ""
    end >>= fun content_path ->
      SessionM.return (resource, content_path)
  in

  if path = root_directory then
    context.Context.mountpoint_stats
  else if (path = trash_directory && not config.Config.disable_trash) ||
          is_shared_with_me_root path trashed config then
    let stats = context.Context.mountpoint_stats in
    { stats with
      Unix.LargeFile.st_perm = stats.Unix.LargeFile.st_perm land 0o555
    }
  else if is_lost_and_found_root path trashed config then
    context.Context.mountpoint_stats
  else begin
    let (resource, content_path) = do_request request_resource |> fst in
    let stat =
      if content_path <> "" then Some (Unix.LargeFile.stat content_path)
      else None in
    let st_kind =
      if CacheData.Resource.is_folder resource then Unix.S_DIR
      else
        Option.map_default
          CacheData.Resource.file_mode_bits_to_kind
          Unix.S_REG
          resource.CacheData.Resource.file_mode_bits
    in
    let st_perm =
      let default_perm =
        if CacheData.Resource.is_folder resource then 0o777
        else 0o666 in
      let perm =
        Option.map_default
          CacheData.Resource.file_mode_bits_to_perm
          default_perm
          resource.CacheData.Resource.file_mode_bits
      in
      let mask =
        if CacheData.Resource.is_symlink resource then 0o777
        else
          lnot config.Config.umask land (
            if is_file_read_only resource
            then 0o555
            else 0o777)
      in
      perm land mask in
    (* To avoid potential performance issues, counting the number of subdirs
     * (as st_nlink is usually equals to 2 + subdir number), let set the value
     * to 1, as it can be used to mean "I don't know the subdirectory count"
     * (https://github.com/cryptomator/fuse-nio-adapter/issues/34). See also:
     * https://bugzilla.kernel.org/show_bug.cgi?id=196405#c5
     *)
    let st_nlink = 1 in
    let st_uid =
      Option.map_default 
        Int64.to_int
        context.Context.mountpoint_stats.Unix.LargeFile.st_uid
        resource.CacheData.Resource.uid in
    let st_gid =
      Option.map_default 
        Int64.to_int
        context.Context.mountpoint_stats.Unix.LargeFile.st_gid
        resource.CacheData.Resource.gid in
    let st_size =
      if CacheData.Resource.is_symlink resource then
        resource.CacheData.Resource.link_target
          |> Option.get
          |> String.length
          |> Int64.of_int
      else match stat with
          None ->
            if CacheData.Resource.is_folder resource then f_bsize
            else Option.default 0L resource.CacheData.Resource.size
        | Some st ->
            st.Unix.LargeFile.st_size in
    let st_atime =
      match stat with
          None ->
            resource.CacheData.Resource.viewed_by_me_time |> Option.get
        | Some st ->
            st.Unix.LargeFile.st_atime in
    let is_to_upload =
      resource.CacheData.Resource.state = CacheData.Resource.State.ToUpload in
    let st_mtime =
      match stat with
          Some st when is_to_upload ->
            st.Unix.LargeFile.st_mtime
        | _ ->
            resource.CacheData.Resource.modified_time |> Option.get in
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
  let config = Context.get_ctx () |. Context.config_lens in
  let get_all_files q =
    let rec loop ?pageToken accu =
      FilesResource.list
        ~supportsAllDrives:true
        ~driveId:config.Config.team_drive_id
        ~includeItemsFromAllDrives:(config.Config.team_drive_id <> "")
        ~corpora:(if config.Config.team_drive_id <> "" then "drive"
                  else "user")
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

  let context = Context.get_ctx () in
  let config = context |. Context.config_lens in
  let (path_in_cache, trashed) = get_path_in_cache path config in
  let cache = context.Context.cache in

  let request_folder =
    Utils.log_with_header
      "BEGIN: Getting folder content (path=%s, trashed=%b)\n%!"
      path_in_cache trashed;
    get_resource path_in_cache trashed >>= fun resource ->
    if is_lost_and_found_root path trashed config then begin
      Utils.log_with_header "BEGIN: Getting lost and found files\n%!";
      let q = "'me' in owners" in
      get_all_files q >>= fun all_owned_files ->
      let lost_and_found_files =
        List.filter
          (fun file -> file.File.parents = [])
          all_owned_files in
      Utils.log_with_header
        "END: Getting lost and found files: Found %d files\n%!"
        (List.length lost_and_found_files);
      SessionM.return (lost_and_found_files, resource);
    end else if is_shared_with_me_root path trashed config then begin
      Utils.log_with_header "BEGIN: Getting shared with me files\n%!";
      let q = "sharedWithMe = true" in
      get_all_files q >>= fun shared_with_me_files ->
      Utils.log_with_header
        "END: Getting shared with me files: Found %d files\n%!"
        (List.length shared_with_me_files);
      SessionM.return (shared_with_me_files, resource);
    end else begin
      get_folder_id path_in_cache trashed >>= fun folder_id ->
      let q =
        Printf.sprintf "'%s' in parents and trashed = %b" folder_id trashed in
      get_all_files q >>= fun files ->
      Utils.log_with_header
        "END: Getting folder content (path=%s, trashed=%b)\n%!"
        path_in_cache trashed;
      begin if path = trash_directory && trashed && not
                 config.Config.disable_trash then begin
        Utils.log_with_header "BEGIN: Getting explicitly trashed files\n%!";
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
    end
  in

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
                   let filename =
                     get_unique_filename_from_file file filename_table in
                   let resource_path =
                     Filename.concat path_in_cache filename in
                   let resource = create_resource resource_path in
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
        |> CacheData.Resource.state ^= CacheData.Resource.State.Synchronized
        |> CacheData.Resource.last_update ^= Unix.gettimeofday ()
      in
      update_cached_resource cache updated_resource;
      inserted_resources
    end
  in
  let filenames =
    List.map
      (fun resource ->
         Filename.basename resource.CacheData.Resource.path)
      resources in
  let filenames =
    if path = root_directory && not config.Config.disable_trash then
      (Filename.basename trash_directory) :: filenames
    else filenames in
  let filenames =
    if path = root_directory && not trashed &&
       config.Config.shared_with_me then
      (Filename.basename shared_with_me_directory) :: filenames
    else filenames in
  if path = root_directory && not trashed &&
     config.Config.lost_and_found then
    (Filename.basename lost_and_found_directory) :: filenames
  else filenames
(* END readdir *)

(* fopen *)
let fopen path flags =
  let config = Context.get_ctx () |. Context.config_lens in
  let (path_in_cache, trashed) = get_path_in_cache path config in
  let is_read_only_request = List.mem Unix.O_RDONLY flags in

  let check_editable =
    get_resource path_in_cache trashed >>= fun resource ->
    if not is_read_only_request && is_file_read_only resource then
      Utils.raise_m Permission_denied
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
  let config = Context.get_ctx () |. Context.config_lens in
  let (path_in_cache, trashed) = get_path_in_cache path config in
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
  let cache = context.Context.cache in
  let config = context |. Context.config_lens in
  let (path_in_cache, trashed) = get_path_in_cache path config in
  let update_file =
    get_resource path_in_cache trashed >>= fun resource ->
    try_with_default (do_remote_update resource) >>= fun file_option ->
    begin match file_option with
        None ->
          purge_cache cache resource
      | Some file ->
          begin match update_file_in_cache with
              None -> ()
            | Some go ->
                if resource.CacheData.Resource.state =
                   CacheData.Resource.State.Synchronized
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
      let remote_id = resource |. CacheData.Resource.remote_id |> Option.get in
      Utils.log_with_header "BEGIN: Updating file mtime (remote id=%s, mtime=%f)\n%!"
        remote_id mtime;
      let file_patch = File.empty
            |> File.modifiedTime ^= Netdate.create mtime in
      FilesResource.update
        ~supportsAllDrives:true
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
  let config = Context.get_ctx () |. Context.config_lens in
  let (path_in_cache, trashed) = get_path_in_cache path config in

  let request_resource =
    get_resource path_in_cache trashed >>= fun resource ->
    let (to_stream, to_memory_buffer) =
      CacheData.Resource.to_stream config resource in
    if to_stream then
      if to_memory_buffer then
        with_retry
          (stream_resource_to_memory_buffer offset buf) resource >>= fun () ->
        SessionM.return ""
      else
        with_retry (stream_resource offset buf) resource >>= fun () ->
        SessionM.return ""
    else begin
      flush_memory_buffers resource;
      with_retry download_resource resource
    end
  in

  let build_read_ahead_requests =
    if config.Config.read_ahead_buffers > 0 then
      get_resource path_in_cache trashed >>= fun resource ->
      let (to_stream, to_memory_buffer) =
        CacheData.Resource.to_stream config resource in
      if to_stream && to_memory_buffer then
        stream_resource_to_read_ahead_buffers offset resource
      else
        SessionM.return []
    else
      SessionM.return []
  in

  let content_path = do_request request_resource |> fst in
  let read_ahead_requests = do_request build_read_ahead_requests |> fst in
  List.iter
    (fun m -> async_do_request m |> ignore)
    read_ahead_requests;
  if content_path <> "" then
    Utils.with_in_channel content_path
      (fun ch ->
         let file_descr = Unix.descr_of_in_channel ch in
         Unix.LargeFile.lseek file_descr offset Unix.SEEK_SET |> ignore;
         Fuse.Unix_util.read file_descr buf)
  else
    Bigarray.Array1.dim buf
(* END read *)

(* write *)
let write path buf offset file_descr =
  let context = Context.get_ctx () in
  let config = context |. Context.config_lens in
  let (path_in_cache, trashed) = get_path_in_cache path config in

  let write_to_resource =
    get_resource path_in_cache trashed >>= fun resource ->
    with_retry download_resource resource >>= fun content_path ->
    Utils.log_with_header
      "BEGIN: Writing local file (path=%s, trashed=%b)\n%!"
      path_in_cache trashed;
    let write_to_memory_buffers () =
      let memory_buffers = context.Context.memory_buffers in
      Buffering.MemoryBuffers.write_to_block
        (resource.CacheData.Resource.remote_id |> Option.get)
        content_path
        buf
        offset
        memory_buffers
    in
    let write_to_file () =
      Utils.with_out_channel content_path
        (fun ch ->
           let file_descr = Unix.descr_of_out_channel ch in
           Unix.LargeFile.lseek file_descr offset Unix.SEEK_SET |> ignore;
           Fuse.Unix_util.write file_descr buf)
    in
    let bytes =
      if config.Config.write_buffers then
        write_to_memory_buffers ()
      else
        write_to_file () in
    Utils.log_with_header
      "END: Writing local file (path=%s, trashed=%b, bytes=%d)\n%!"
      path_in_cache trashed bytes;
    let top_offset = Int64.add offset (Int64.of_int bytes) in
    let file_size = Option.default 0L resource.CacheData.Resource.size in
    let cache = context.Context.cache in
    if top_offset > file_size then begin
      let updated_resource = resource
        |> CacheData.Resource.size ^= Some top_offset
        |> CacheData.Resource.state ^= CacheData.Resource.State.ToUpload in
      update_cached_resource cache updated_resource;
      let file_size = Int64.sub top_offset file_size in
      shrink_cache ~file_size ()
    end else begin
      update_cached_resource_state cache
        CacheData.Resource.State.ToUpload resource.CacheData.Resource.id;
    end;
    SessionM.return bytes
  in
  do_request write_to_resource |> fst
(* END write *)

let start_uploading_if_dirty path =
  let config = Context.get_ctx () |. Context.config_lens in
  let (path_in_cache, trashed) = get_path_in_cache path config in
  let resource = lookup_resource path_in_cache trashed in
  match resource with
      None ->
        false
    | Some r ->
        if r.CacheData.Resource.state == CacheData.Resource.State.ToUpload then begin
          let cache = Context.get_cache () in
          update_cached_resource_state cache
            CacheData.Resource.State.Uploading r.CacheData.Resource.id;
          true
        end else false

let upload resource =
  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  let content_path = Cache.get_content_path cache resource in
  let config = context |. Context.config_lens in
  let content_type =
    if config.Config.autodetect_mime then ""
    else
      let file_source =
        GapiMediaResource.create_file_resource content_path in
      let resource_mime_type =
        resource |. CacheData.Resource.mime_type |> Option.get in
      let content_type = file_source |. GapiMediaResource.content_type in
      (* Workaround to set the correct MIME type *)
      if resource_mime_type <> "" then resource_mime_type
      else content_type
  in
  let file_source =
    GapiMediaResource.create_file_resource
      ~content_type
      content_path in
  let size = file_source.GapiMediaResource.content_length in
  update_cached_resource_state_and_size cache
    CacheData.Resource.State.Uploading size resource.CacheData.Resource.id;
  let remote_id = resource |. CacheData.Resource.remote_id |> Option.get in
  let media_source =
    if file_source.GapiMediaResource.content_length = 0L then None
    else Some file_source in
  Utils.log_with_header
    "BEGIN: Uploading file (id=%Ld, path=%s, cache path=%s, \
     content type=%s, content_length=%Ld).\n%!"
    resource.CacheData.Resource.id resource.CacheData.Resource.path
    content_path
    (if content_type = "" then "autodetect" else content_type)
    size;
  let file_patch = File.empty |> File.modifiedTime ^= GapiDate.now () in
  FilesResource.update
    ~supportsAllDrives:true
    ~std_params:file_std_params
    ?media_source
    ~fileId:remote_id
    file_patch >>= fun file ->
  let resource = update_resource_from_file resource file in
  Utils.log_with_header
    "END: Uploading file (id=%Ld, path=%s, cache path=%s, content type=%s).\n%!"
    resource.CacheData.Resource.id
    resource.CacheData.Resource.path
    content_path
    file.File.mimeType;
  let reloaded_resource =
    Cache.Resource.select_first_resource_with_remote_id cache file.File.id in
  let resource = Option.default resource reloaded_resource in
  let state =
    match resource.CacheData.Resource.state with
        CacheData.Resource.State.Uploading ->
          Some CacheData.Resource.State.Synchronized
      | _ -> None in
  let updated_resource =
    update_resource_from_file ?state resource file in
  update_cached_resource cache updated_resource;
  shrink_cache ();
  SessionM.return ()

let upload_resource_with_retry resource =
  flush_memory_buffers resource;
  with_retry (fun r -> try_with_default (upload r)) resource

let upload_resource_by_id resource_id =
  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  let resource = Cache.Resource.select_resource_with_id cache resource_id in
  match resource with
  | Some r ->
    do_request (upload_resource_with_retry r) |> ignore;
  | None ->
    Utils.log_with_header
      "Cannot find queued resource to upload with resource_id=%Ld.\n%!"
      resource_id

let init_filesystem () =
  let context = Context.get_ctx () in
  let cache = context.Context.cache in
  MemoryCache.start_flush_db_thread cache;
  let config = context |. Context.config_lens in
  if config.Config.async_upload_queue then begin
    UploadQueue.start_async_upload_thread
      cache config.Config.async_upload_threads upload_resource_by_id;
  end

let queue_upload resource =
  let context = Context.get_ctx () in
  let config = context |. Context.config_lens in
  if config.Config.async_upload_queue then begin
    let cache = context.Context.cache in
    flush_memory_buffers resource;
    UploadQueue.queue_resource cache resource;
    SessionM.return ()
  end else
    upload_resource_with_retry resource

let upload_with_retry path =
  let config = Context.get_ctx () |. Context.config_lens in
  let (path_in_cache, trashed) = get_path_in_cache path config in
  get_resource path_in_cache trashed >>= fun resource ->
  queue_upload resource

let upload_if_dirty path =
  if start_uploading_if_dirty path then begin
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
  let config = Context.get_ctx () |. Context.config_lens in
  let (path_in_cache, trashed) = get_path_in_cache path config in
  if trashed then raise Permission_denied;

  let context = Context.get_ctx () in
  let config = context |. Context.config_lens in
  if is_lost_and_found path trashed config ||
     is_shared_with_me path trashed config then
    raise Permission_denied;

  let cache = context.Context.cache in
  let parent_path = Filename.dirname path_in_cache in
  let create_file =
    get_resource parent_path trashed >>= fun parent_resource ->
    let parent_id =
      parent_resource |. CacheData.Resource.remote_id |> Option.get
    in
    let name = Filename.basename path_in_cache in
    let mimeType =
      if is_folder
      then folder_mime_type
      else if config.Config.autodetect_mime then ""
      else Mime.map_filename_to_mime_type name
    in
    let appProperties = [CacheData.Resource.mode_to_app_property mode] in
    let appProperties = match link_target with
        None -> appProperties
      | Some link ->
          if json_length link > max_link_target_length then
            raise Invalid_operation
          else
            CacheData.Resource.link_target_to_app_property link :: appProperties in
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
      ~supportsAllDrives:true
      ~std_params:file_std_params
      file >>= fun created_file ->
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
        ~state:CacheData.Resource.State.Synchronized
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
  let config = Context.get_ctx () |. Context.config_lens in
  if is_folder then begin
    let q = Printf.sprintf "'%s' in parents and trashed = %b"
        remote_id trashed in
    let std_params =
      { GapiService.StandardParameters.default with
            GapiService.StandardParameters.fields = "files(id)"
      }
    in
    FilesResource.list
      ~supportsAllDrives:true
      ~driveId:config.Config.team_drive_id
      ~includeItemsFromAllDrives:(config.Config.team_drive_id <> "")
      ~corpora:(if config.Config.team_drive_id <> "" then "drive"
                else "user")
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

  let context = Context.get_ctx () in
  let config = context |. Context.config_lens in
  if is_lost_and_found path trashed config ||
     is_shared_with_me path trashed config then
    raise Permission_denied;

  let trash resource =
    let remote_id = resource |. CacheData.Resource.remote_id |> Option.get in
    check_if_empty remote_id is_folder trashed >>= fun () ->
    Utils.log_with_header "BEGIN: Trashing file (remote id=%s)\n%!" remote_id;
    let file_patch =
      { File.empty with
            File.trashed = true;
      }
    in
    FilesResource.update
      ~supportsAllDrives:true
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
          |> CacheData.Resource.trashed ^= Some true in
        update_cached_resource cache updated_resource;
        Cache.Resource.invalidate_trash_bin cache;
        if is_folder then begin
          let (path_in_cache, _) = get_path_in_cache path config in
          Utils.log_with_header
            "BEGIN: Trashing folder old content (path=%s)\n%!"
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
  let config = Context.get_ctx () |. Context.config_lens in
  let (path_in_cache, trashed) = get_path_in_cache path config in

  let delete resource =
    let remote_id = resource |. CacheData.Resource.remote_id |> Option.get in
    check_if_empty remote_id is_folder trashed >>= fun () ->
    Utils.log_with_header
      "BEGIN: Permanently deleting file (remote id=%s)\n%!"
      remote_id;
    FilesResource.delete
      ~supportsAllDrives:true
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
        delete_cached_resource resource;
        if is_folder then begin
          Utils.log_with_header
            "BEGIN: Deleting folder old content (path=%s, trashed=%b) from cache\n%!"
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
  let context = Context.get_ctx () in
  let config = context |. Context.config_lens in
  let (_, trashed) = get_path_in_cache path config in

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
  let context = Context.get_ctx () in
  let config = context |. Context.config_lens in
  let (path_in_cache, trashed) = get_path_in_cache path config in
  let (new_path_in_cache, target_trashed) =
    get_path_in_cache new_path config in
  if trashed <> target_trashed then raise Permission_denied;

  if is_lost_and_found_root path trashed config ||
     is_lost_and_found new_path target_trashed config then
    raise Permission_denied;
  if is_shared_with_me path trashed config ||
     is_shared_with_me new_path target_trashed config then
    raise Permission_denied;

  let old_parent_path = Filename.dirname path_in_cache in
  let new_parent_path = Filename.dirname new_path_in_cache in
  let old_name = Filename.basename path_in_cache in
  let new_name = Filename.basename new_path_in_cache in
  let delete_path path path_in_cache is_trashed =
    let trash () =
      get_resource path_in_cache is_trashed >>= fun resource ->
      trash_resource (CacheData.Resource.is_folder resource) is_trashed path
    in
    begin if not is_trashed &&
             not config.Config.keep_duplicates then
      Utils.try_with_m
        (trash ())
        (function
          | File_not_found -> SessionM.return ()
          | e -> Utils.raise_m e)
    else
      SessionM.return ()
    end
  in
  let delete_target_path =
    delete_path new_path new_path_in_cache target_trashed in
  let delete_source_path =
    delete_path path path_in_cache trashed in
  let update =
    let trash_target_and_rename_file resource remote_id =
      delete_target_path >>= fun () ->
      Utils.log_with_header
        "BEGIN: Renaming file (remote id=%s) from %s to %s\n%!"
        remote_id old_name new_name;
      let file_patch =
        { File.empty with
              File.name = new_name;
        } in
      FilesResource.update
        ~supportsAllDrives:true
        ~std_params:file_std_params
        ~fileId:remote_id
        file_patch >>= fun patched_file ->
      Utils.log_with_header
        "END: Renaming file (remote id=%s) from %s to %s\n%!"
        remote_id old_name new_name;
      SessionM.return patched_file
    in
    let replace_target resource remote_id not_found_callback =
      let replace_target_content () =
        get_resource
          new_path_in_cache target_trashed >>= fun target_resource ->
        let target_remote_id =
          target_resource |. CacheData.Resource.remote_id |> Option.get in
        Utils.log_with_header
          "BEGIN: Replacing content of file %s (remote id=%s) with content \
           of file %s (remote id=%s)\n%!"
          new_name target_remote_id old_name remote_id;
        flush_memory_buffers resource;
        with_retry download_resource resource >>= fun content_path ->
        let file_patch =
          { File.empty with
                File.mimeType =
                  Option.default "" resource.CacheData.Resource.mime_type;
          } in
        FilesResource.update
          ~supportsAllDrives:true
          ~std_params:file_std_params
          ~fileId:target_remote_id
          file_patch >>= fun patched_file ->
        let cache = context.Context.cache in
        let target_content_path =
          Cache.get_content_path cache target_resource in
        Utils.log_with_header
          "Replacing cache content (source content path=%s, target content \
           path = %s)\n%!"
          content_path target_content_path;
        Utils.file_copy content_path target_content_path;
        let stats = Unix.LargeFile.stat target_content_path in
        let file_size = stats.Unix.LargeFile.st_size in
        let metadata = context |. Context.metadata_lens in
        Utils.with_lock context.Context.metadata_lock
          (fun () ->
             update_cache_size file_size metadata cache;
          );
        update_cached_resource_state cache
          CacheData.Resource.State.ToUpload
          target_resource.CacheData.Resource.id;
        queue_upload target_resource >>= fun () ->
        delete_source_path >>= fun () ->
        Utils.log_with_header
          "END: Replacing content of file %s (remote id=%s) with content \
           of file %s (remote id=%s)\n%!"
          new_name target_remote_id old_name remote_id;
        SessionM.return patched_file
      in
      Utils.try_with_m
        (replace_target_content ())
        (function
          | File_not_found -> not_found_callback resource remote_id
          | e -> Utils.raise_m e)
    in
    let rename_file resource =
      if old_name <> new_name then begin
        let remote_id =
          resource |. CacheData.Resource.remote_id |> Option.get in
        begin if config.Config.mv_keep_target then
          replace_target resource remote_id trash_target_and_rename_file
        else
          trash_target_and_rename_file resource remote_id
        end >>= fun renamed_file ->
        SessionM.return (Some renamed_file)
      end else begin
        SessionM.return None
      end
    in
    let trash_target_and_move resource remote_id =
      let remote_id =
        resource |. CacheData.Resource.remote_id |> Option.get in
      delete_target_path >>= fun () ->
      Utils.log_with_header
        "BEGIN: Moving file (remote id=%s) from %s to %s\n%!"
        remote_id old_parent_path new_parent_path;
      get_resource
        new_parent_path target_trashed >>= fun new_parent_resource ->
      let new_parent_id =
        new_parent_resource.CacheData.Resource.remote_id |> Option.get in
      begin if is_lost_and_found_root old_parent_path trashed config then
        SessionM.return ""
      else
        get_resource
          old_parent_path trashed >>= fun old_parent_resource ->
        let id =
          old_parent_resource.CacheData.Resource.remote_id |> Option.get in
        SessionM.return id
      end >>= fun old_parent_id ->
      FilesResource.update
        ~supportsAllDrives:true
        ~std_params:file_std_params
        ~addParents:new_parent_id
        ~fileId:remote_id
        ~removeParents:old_parent_id
        File.empty >>= fun patched_file ->
      Utils.log_with_header "END: Moving file (remote id=%s) from %s to %s\n%!"
        remote_id old_parent_path new_parent_path;
      SessionM.return patched_file
    in
    let move resource =
      begin if old_parent_path <> new_parent_path then begin
        let remote_id =
          resource |. CacheData.Resource.remote_id |> Option.get in
        begin if config.Config.mv_keep_target then
          replace_target resource remote_id trash_target_and_move
        else trash_target_and_move resource remote_id
        end >>= fun moved_file ->
        SessionM.return (Some moved_file)
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
          let is_file_replaced =
            resource.CacheData.Resource.remote_id <> Some file.File.id in
          let updated_resource =
            (* When mv_keep_target is true, the resource to update is
             * the target file. *)
            if is_file_replaced then
              let reloaded_resource =
                Option.default
                  resource
                  (Cache.Resource.select_first_resource_with_remote_id cache
                     file.File.id)
              in
              update_resource_from_file reloaded_resource file
            else
              update_resource_from_file resource file
          in
          let resource_with_new_path =
            updated_resource
              |> CacheData.Resource.path ^= new_path_in_cache
              |> CacheData.Resource.parent_path ^= new_parent_path
              |> CacheData.Resource.trashed ^= Some target_trashed
              |> CacheData.Resource.state ^=
                (if CacheData.Resource.is_folder resource ||
                    CacheData.Resource.is_document resource
                 then CacheData.Resource.State.ToDownload
                 else CacheData.Resource.State.Synchronized) in
          let resource_to_save =
            if new_parent_path <> old_parent_path &&
               new_name = old_name &&
               not is_file_replaced then begin
              let path =
                recompute_path resource_with_new_path new_name in
              let parent_path = Filename.dirname path in
              resource_with_new_path
                |> CacheData.Resource.path ^= path
                |> CacheData.Resource.parent_path ^= parent_path
            end else resource_with_new_path
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
          if CacheData.Resource.is_folder resource then begin
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
  let context = Context.get_ctx () in
  let config = context |. Context.config_lens in
  let (path_in_cache, trashed) = get_path_in_cache path config in
  let truncate_resource =
    get_resource path_in_cache trashed >>= fun resource ->
    flush_memory_buffers resource;
    with_retry download_resource resource >>= fun content_path ->
    let remote_id = resource |. CacheData.Resource.remote_id |> Option.get in
    Utils.log_with_header "BEGIN: Truncating file (remote id=%s)\n%!" remote_id;
    let cache = context.Context.cache in
    let updated_resource = resource
      |> CacheData.Resource.size ^= Some size
      |> CacheData.Resource.state ^= CacheData.Resource.State.ToUpload in
    update_cached_resource cache updated_resource;
    let file_size =
      Int64.sub size (Option.default 0L resource.CacheData.Resource.size) in
    shrink_cache ~file_size ();
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
      let remote_id = resource |. CacheData.Resource.remote_id |> Option.get in
      Utils.log_with_header "BEGIN: Updating mode (remote id=%s, mode=%o)\n%!"
        remote_id mode;
      let file_patch = File.empty
        |> File.appProperties ^= [CacheData.Resource.mode_to_app_property mode] in
      FilesResource.update
        ~supportsAllDrives:true
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
      let remote_id = resource |. CacheData.Resource.remote_id |> Option.get in
      let id_to_string id =
        let id64 = Int64.of_int id in
        let minus_one_32_unsigned = Int64.pred (Int64.shift_left 1L 32) in
        if id64 = Int64.minus_one || id64 = minus_one_32_unsigned then ""
        else string_of_int id in
      let uid_string = id_to_string uid in
      let gid_string = id_to_string gid in
      Utils.log_with_header "BEGIN: Updating owner (remote id=%s, uid=%s gid=%s)\n%!"
        remote_id uid_string gid_string;
      let app_properties =
        if gid_string = "" then []
        else [CacheData.Resource.gid_to_app_property gid_string] in
      let app_properties =
        if uid_string = "" then app_properties
        else CacheData.Resource.uid_to_app_property uid_string :: app_properties
      in
      let file_patch = File.empty
         |> File.appProperties ^= app_properties in
      FilesResource.update
        ~supportsAllDrives:true
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
  let config = Context.get_ctx () |. Context.config_lens in
  let (path_in_cache, trashed) = get_path_in_cache path config in
  let fetch_xattr =
    get_resource path_in_cache trashed >>= fun resource ->
    let xattrs = CacheData.Resource.parse_xattrs resource.CacheData.Resource.xattrs in
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
      let remote_id = resource |. CacheData.Resource.remote_id |> Option.get in
      Utils.log_with_header "BEGIN: Setting xattr (remote id=%s, name=%s value=%s xflags=%s)\n%!"
        remote_id name value (Utils.xattr_flags_to_string xflags);
      let xattrs = CacheData.Resource.parse_xattrs resource.CacheData.Resource.xattrs in
      let existing = List.mem_assoc name xattrs in
      begin match xflags with
          Fuse.CREATE -> if existing then raise Existing_attribute
        | Fuse.REPLACE -> if not existing then raise No_attribute
        | Fuse.AUTO -> ()
      end;
      let attribute_length = json_length name + json_length value in
      if attribute_length > max_attribute_length then raise Invalid_operation;
      let file_patch = File.empty
        |> File.appProperties ^= [
             CacheData.Resource.xattr_to_app_property name value;
           ] in
      FilesResource.update
        ~supportsAllDrives:true
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
  let config = Context.get_ctx () |. Context.config_lens in
  let (path_in_cache, trashed) = get_path_in_cache path config in
  let fetch_xattrs =
    get_resource path_in_cache trashed >>= fun resource ->
    let xattrs = CacheData.Resource.parse_xattrs resource.CacheData.Resource.xattrs in
    let keys = List.map (fun (n, _) -> n) xattrs in
    SessionM.return keys
  in
  do_request fetch_xattrs |> fst
(* END listxattr *)

(* removexattr *)
let remove_xattr path name =
  let update =
    let removexattr resource =
      let remote_id = resource |. CacheData.Resource.remote_id |> Option.get in
      Utils.log_with_header "BEGIN: Removing xattr (remote id=%s, name=%s)\n%!"
        remote_id name;
      let xattrs = CacheData.Resource.parse_xattrs resource.CacheData.Resource.xattrs in
      let existing = List.mem_assoc name xattrs in
      if not existing then raise No_attribute;
      let file_patch = File.empty
        |> File.appProperties ^= [
             CacheData.Resource.xattr_no_value_to_app_property name;
           ] in
      FilesResource.update
        ~supportsAllDrives:true
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
  let config = Context.get_ctx () |. Context.config_lens in
  let (path_in_cache, trashed) = get_path_in_cache path config in
  let fetch_link_target =
    get_resource path_in_cache trashed >>= fun resource ->
    let link_target =
      match resource.CacheData.Resource.link_target with
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

