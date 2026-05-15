module File = GapiDriveV3Model.File

let folder_mime_type = "application/vnd.google-apps.folder"
let shortcut_mime_type = "application/vnd.google-apps.shortcut"

let chars_blacklist_regexp = Str.regexp "[/\000]"
let clean_filename name = Str.global_replace chars_blacklist_regexp "_" name

let get_file_extension file_name =
  try
    let dot_index = String.rindex file_name '.' in
    String.sub file_name (dot_index + 1)
      (String.length file_name - dot_index - 1)
  with Not_found -> ""

let get_filename config name is_document get_document_format =
  let clean_name = clean_filename name in
  let document_format =
    if is_document then get_document_format config else ""
  in
  if is_document && config.Config.docs_file_extension && document_format <> ""
  then
    let current_extension = get_file_extension clean_name in
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
  let fmt = CacheData.Resource.get_format_from_mime_type mime_type config in
  match fmt with
  | "desktop" when config.Config.desktop_entry_as_html -> "html"
  | _ -> fmt

let get_remote_id_fingerprint word_length remote_id =
  if word_length > 4 then invalid_arg "Too many filename conflicts";
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

let disambiguate_filename filename full_file_extension remote_id filename_table
    =
  let rec find_first_unique_filename filename counter =
    let new_candidate =
      let fingerprint = get_remote_id_fingerprint counter remote_id in
      let filename_length = String.length filename in
      let extension_opt =
        if
          String.length full_file_extension > 0
          && ExtString.String.ends_with filename full_file_extension
        then Some full_file_extension
        else
          try
            let dot_pos = String.rindex filename '.' in
            let ext =
              String.sub filename (dot_pos + 1) (filename_length - dot_pos - 1)
            in
            Some ext
          with Not_found -> None
      in
      match extension_opt with
      | None -> Printf.sprintf "%s (%s)" filename fingerprint
      | Some extension ->
          let extension_length = String.length extension in
          let base_name =
            String.sub filename 0 (filename_length - extension_length - 1)
          in
          if String.length base_name > 0 then
            Printf.sprintf "%s (%s).%s" base_name fingerprint extension
          else Printf.sprintf "(%s).%s" fingerprint extension
    in
    if not (Hashtbl.mem filename_table new_candidate) then (
      Utils.log_with_header "Checking: %s: OK\n%!" new_candidate;
      new_candidate)
    else (
      Utils.log_with_header "Checking: %s: KO\n%!" new_candidate;
      find_first_unique_filename filename (counter + 1))
  in
  if Hashtbl.mem filename_table filename then (
    Utils.log_with_header "Filename collision detected: %s\n%!" filename;
    let unique_filename = find_first_unique_filename filename 1 in
    let name_counter = Hashtbl.find filename_table filename in
    Hashtbl.replace filename_table filename (name_counter + 1);
    unique_filename)
  else (
    Utils.log_with_header "Filename (unused): %s\n%!" filename;
    Hashtbl.add filename_table filename 0;
    filename)

let build_resource_tables config resources =
  let filename_table = Hashtbl.create Utils.hashtable_initial_size in
  let remote_id_table = Hashtbl.create (List.length resources) in
  List.iter
    (fun resource ->
      let name = Option.get resource.CacheData.Resource.name in
      let clean_name =
        get_filename config name
          (CacheData.Resource.is_document resource)
          (fun config -> get_file_extension_from_format resource config)
      in
      let filename = Filename.basename resource.CacheData.Resource.path in
      (if clean_name <> filename then
         let name_counter =
           try Hashtbl.find filename_table clean_name with Not_found -> 0
         in
         Hashtbl.replace filename_table clean_name name_counter);
      Hashtbl.add filename_table filename 0;
      Hashtbl.add remote_id_table
        (Option.get resource.CacheData.Resource.remote_id)
        resource)
    resources;
  (filename_table, remote_id_table)

let clean_document_extension config file_name resource =
  if CacheData.Resource.is_document resource then
    let document_extension = get_file_extension_from_format resource config in
    if config.Config.docs_file_extension && document_extension <> "" then
      let current_extension = get_file_extension file_name in
      if current_extension = document_extension then
        let regexp = Str.quote document_extension |> Str.regexp in
        try
          let pos =
            Str.search_backward regexp file_name (String.length file_name)
          in
          if pos > 0 then Str.string_before file_name (pos - 1) else file_name
        with Not_found -> file_name
      else file_name
    else file_name
  else file_name

let create_resource ~now path =
  let parent_path = Filename.dirname path in
  {
    CacheData.Resource.id = 0L;
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
    resource_key = None;
    target_id = None;
    target_resource_key = None;
    file_mode_bits = None;
    uid = None;
    gid = None;
    link_target = None;
    xattrs = "";
    parent_path;
    path;
    state = CacheData.Resource.State.ToDownload;
    last_update = now ();
  }

let get_unique_filename config name full_file_extension remote_id is_document
    get_document_format filename_table =
  let complete_name =
    get_filename config name is_document get_document_format
  in
  disambiguate_filename complete_name full_file_extension remote_id
    filename_table

let get_unique_filename_from_resource config resource name filename_table =
  get_unique_filename config name
    (Option.default "" resource.CacheData.Resource.full_file_extension)
    (Option.default "" resource.CacheData.Resource.remote_id)
    (CacheData.Resource.is_document resource)
    (fun config -> get_file_extension_from_format resource config)
    filename_table

let get_unique_filename_from_file config file filename_table =
  get_unique_filename config file.File.name file.File.fullFileExtension
    file.File.id
    (CacheData.Resource.is_document_mime_type file.File.mimeType)
    (fun config -> get_file_extension_from_mime_type file.File.mimeType config)
    filename_table

let recompute_path config resource name filename_table =
  let filename =
    get_unique_filename_from_resource config resource name filename_table
  in
  Filename.concat resource.CacheData.Resource.parent_path filename

let update_resource_from_file ~now ~recompute_path ?state ?link_target
    resource file =
  let path =
    match resource.CacheData.Resource.name with
    | Some cached_name ->
        if cached_name <> file.File.name then
          recompute_path resource file.File.name
        else resource.CacheData.Resource.path
    | None -> resource.CacheData.Resource.path
  in
  let parent_path = Filename.dirname path in
  let new_state = Option.default resource.CacheData.Resource.state state in
  let new_size =
    match new_state with
    | CacheData.Resource.State.Uploading | CacheData.Resource.State.ToUpload ->
        resource.CacheData.Resource.size
    | _ -> Some file.File.size
  in
  let resource_key =
    match file.File.resourceKey with "" -> None | _ as s -> Some s
  in
  let target_id =
    if file.File.mimeType = shortcut_mime_type then
      match file.File.shortcutDetails with
      | { File.ShortcutDetails.targetId; _ } when targetId <> "" ->
          Some targetId
      | _ -> None
    else None
  in
  let target_resource_key =
    if file.File.mimeType = shortcut_mime_type then
      match file.File.shortcutDetails with
      | { File.ShortcutDetails.targetResourceKey; _ }
        when targetResourceKey <> "" ->
          Some targetResourceKey
      | _ -> None
    else None
  in
  let link_target =
    if file.File.mimeType = shortcut_mime_type then link_target
    else CacheData.Resource.get_link_target file.File.appProperties
  in
  {
    resource with
    CacheData.Resource.remote_id = Some file.File.id;
    name = Some file.File.name;
    mime_type = Some file.File.mimeType;
    created_time = Some (Netdate.since_epoch file.File.createdTime);
    modified_time = Some (Netdate.since_epoch file.File.modifiedTime);
    viewed_by_me_time = Some (Netdate.since_epoch file.File.viewedByMeTime);
    file_extension = Some file.File.fileExtension;
    full_file_extension = Some file.File.fullFileExtension;
    md5_checksum = Some file.File.md5Checksum;
    size = new_size;
    can_edit = Some file.File.capabilities.File.Capabilities.canEdit;
    trashed = Some file.File.trashed;
    web_view_link = Some file.File.webViewLink;
    export_links =
      Some (CacheData.Resource.serialize_export_links file.File.exportLinks);
    version = Some file.File.version;
    resource_key;
    target_id;
    target_resource_key;
    file_mode_bits =
      CacheData.Resource.get_file_mode_bits file.File.appProperties;
    uid = CacheData.Resource.get_uid file.File.appProperties;
    gid = CacheData.Resource.get_gid file.File.appProperties;
    link_target;
    xattrs = CacheData.Resource.get_xattrs file.File.appProperties;
    last_update = now ();
    path;
    parent_path;
    state = new_state;
  }
