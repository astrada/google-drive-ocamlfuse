type t = {
  cache_dir : string;
  db_path : string;
  busy_timeout : int;
  in_memory : bool;
  autosaving_interval : int;
}

module Resource = struct
  module State = struct
    type t =
      | Synchronized
      | ToDownload
      | Downloading
      | ToUpload
      | Uploading
      | NotFound

    let to_string = function
      | Synchronized -> "Synchronized"
      | ToDownload -> "ToDownload"
      | Downloading -> "Downloading"
      | ToUpload -> "ToUpload"
      | Uploading -> "Uploading"
      | NotFound -> "NotFound"

    let of_string = function
      | "Synchronized" -> Synchronized
      | "ToDownload" -> ToDownload
      | "Downloading" -> Downloading
      | "ToUpload" -> ToUpload
      | "Uploading" -> Uploading
      | "NotFound" -> NotFound
      | s -> failwith ("Resource state unexpected: " ^ s)
  end

  type t = {
    (* rowid *)
    id : int64;
    (* remote data *)
    remote_id : string option;
    name : string option;
    mime_type : string option;
    created_time : float option;
    modified_time : float option;
    viewed_by_me_time : float option;
    file_extension : string option;
    full_file_extension : string option;
    md5_checksum : string option;
    size : int64 option;
    can_edit : bool option;
    trashed : bool option;
    web_view_link : string option;
    export_links : string option;
    version : int64 option;
    resource_key : string option;
    target_id : string option;
    target_resource_key : string option;
    (* app data stored in Drive *)
    file_mode_bits : int64 option;
    uid : int64 option;
    gid : int64 option;
    link_target : string option;
    xattrs : string;
    (* local data *)
    parent_path : string;
    path : string;
    state : State.t;
    last_update : float;
  }

  let id =
    {
      GapiLens.get = (fun x -> x.id);
      GapiLens.set = (fun v x -> { x with id = v });
    }

  let remote_id =
    {
      GapiLens.get = (fun x -> x.remote_id);
      GapiLens.set = (fun v x -> { x with remote_id = v });
    }

  let name =
    {
      GapiLens.get = (fun x -> x.name);
      GapiLens.set = (fun v x -> { x with name = v });
    }

  let mime_type =
    {
      GapiLens.get = (fun x -> x.mime_type);
      GapiLens.set = (fun v x -> { x with mime_type = v });
    }

  let created_time =
    {
      GapiLens.get = (fun x -> x.created_time);
      GapiLens.set = (fun v x -> { x with created_time = v });
    }

  let modified_time =
    {
      GapiLens.get = (fun x -> x.modified_time);
      GapiLens.set = (fun v x -> { x with modified_time = v });
    }

  let viewed_by_me_time =
    {
      GapiLens.get = (fun x -> x.viewed_by_me_time);
      GapiLens.set = (fun v x -> { x with viewed_by_me_time = v });
    }

  let file_extension =
    {
      GapiLens.get = (fun x -> x.file_extension);
      GapiLens.set = (fun v x -> { x with file_extension = v });
    }

  let full_file_extension =
    {
      GapiLens.get = (fun x -> x.full_file_extension);
      GapiLens.set = (fun v x -> { x with full_file_extension = v });
    }

  let md5_checksum =
    {
      GapiLens.get = (fun x -> x.md5_checksum);
      GapiLens.set = (fun v x -> { x with md5_checksum = v });
    }

  let size =
    {
      GapiLens.get = (fun x -> x.size);
      GapiLens.set = (fun v x -> { x with size = v });
    }

  let can_edit =
    {
      GapiLens.get = (fun x -> x.can_edit);
      GapiLens.set = (fun v x -> { x with can_edit = v });
    }

  let trashed =
    {
      GapiLens.get = (fun x -> x.trashed);
      GapiLens.set = (fun v x -> { x with trashed = v });
    }

  let web_view_link =
    {
      GapiLens.get = (fun x -> x.web_view_link);
      GapiLens.set = (fun v x -> { x with web_view_link = v });
    }

  let export_links =
    {
      GapiLens.get = (fun x -> x.export_links);
      GapiLens.set = (fun v x -> { x with export_links = v });
    }

  let version =
    {
      GapiLens.get = (fun x -> x.version);
      GapiLens.set = (fun v x -> { x with version = v });
    }

  let resource_key =
    {
      GapiLens.get = (fun x -> x.resource_key);
      GapiLens.set = (fun v x -> { x with resource_key = v });
    }

  let target_id =
    {
      GapiLens.get = (fun x -> x.target_id);
      GapiLens.set = (fun v x -> { x with target_id = v });
    }

  let target_resource_key =
    {
      GapiLens.get = (fun x -> x.target_resource_key);
      GapiLens.set = (fun v x -> { x with target_resource_key = v });
    }

  let file_mode_bits =
    {
      GapiLens.get = (fun x -> x.file_mode_bits);
      GapiLens.set = (fun v x -> { x with file_mode_bits = v });
    }

  let uid =
    {
      GapiLens.get = (fun x -> x.uid);
      GapiLens.set = (fun v x -> { x with uid = v });
    }

  let gid =
    {
      GapiLens.get = (fun x -> x.gid);
      GapiLens.set = (fun v x -> { x with gid = v });
    }

  let link_target =
    {
      GapiLens.get = (fun x -> x.link_target);
      GapiLens.set = (fun v x -> { x with link_target = v });
    }

  let xattrs =
    {
      GapiLens.get = (fun x -> x.xattrs);
      GapiLens.set = (fun v x -> { x with xattrs = v });
    }

  let parent_path =
    {
      GapiLens.get = (fun x -> x.parent_path);
      GapiLens.set = (fun v x -> { x with parent_path = v });
    }

  let path =
    {
      GapiLens.get = (fun x -> x.path);
      GapiLens.set = (fun v x -> { x with path = v });
    }

  let state =
    {
      GapiLens.get = (fun x -> x.state);
      GapiLens.set = (fun v x -> { x with state = v });
    }

  let last_update =
    {
      GapiLens.get = (fun x -> x.last_update);
      GapiLens.set = (fun v x -> { x with last_update = v });
    }

  (* file mode bits *)
  let file_mode_bits_to_kind m =
    let s_ifmt = 0o170000 in
    let file_type = Int64.to_int m land s_ifmt in
    match file_type with
    | 0o140000 -> Unix.S_SOCK
    | 0o120000 -> Unix.S_LNK
    | 0o100000 -> Unix.S_REG
    | 0o060000 -> Unix.S_BLK
    | 0o040000 -> Unix.S_DIR
    | 0o020000 -> Unix.S_CHR
    | 0o010000 -> Unix.S_FIFO
    | _ -> Unix.S_REG

  let file_mode_bits_to_perm m =
    let mask = 0o7777 in
    Int64.to_int m land mask

  (* xattrs *)
  let render_xattrs xattrs =
    let buffer = Buffer.create 64 in
    List.iter
      (fun (name, value) ->
        let s =
          Printf.sprintf "%S:%S;"
            (String.sub name 2 (String.length name - 2))
            value
        in
        Buffer.add_string buffer s)
      xattrs;
    Buffer.contents buffer

  let parse_xattrs s =
    let sb = Scanf.Scanning.from_string s in
    let xattrs = ref [] in
    while not (Scanf.Scanning.end_of_input sb) do
      let name, value = Scanf.bscanf sb "%S:%S;" (fun f l -> (f, l)) in
      xattrs := (name, value) :: !xattrs
    done;
    !xattrs

  (* app properties *)
  let find_app_property name app_properties =
    try Some (List.assoc name app_properties) with Not_found -> None

  let app_property_to_int64 p = Option.map (fun s -> Int64.of_string s) p

  let get_file_mode_bits app_properties =
    app_property_to_int64 (find_app_property "mode" app_properties)

  let file_mode_bits_to_app_property file_mode_bits =
    ("mode", Option.map_default Int64.to_string "" file_mode_bits)

  let mode_to_app_property mode = ("mode", string_of_int mode)

  let get_uid app_properties =
    app_property_to_int64 (find_app_property "uid" app_properties)

  let uid_to_app_property uid = ("uid", uid)

  let get_gid app_properties =
    app_property_to_int64 (find_app_property "gid" app_properties)

  let gid_to_app_property gid = ("gid", gid)
  let get_link_target app_properties = find_app_property "l" app_properties
  let link_target_to_app_property link_target = ("l", link_target)

  let get_xattrs app_properties =
    render_xattrs
      (List.filter
         (fun (n, _) -> ExtString.String.starts_with n "x-")
         app_properties)

  let xattr_to_app_property name value = ("x-" ^ name, value)
  let xattr_no_value_to_app_property name = ("x-" ^ name, "")

  let is_folder resource =
    match resource.mime_type with
    | Some "application/vnd.google-apps.folder" -> true
    | _ -> false

  let is_document_mime_type mime_type =
    match mime_type with
    | "application/vnd.google-apps.document"
    | "application/vnd.google-apps.drawing" | "application/vnd.google-apps.form"
    | "application/vnd.google-apps.presentation"
    | "application/vnd.google-apps.spreadsheet"
    | "application/vnd.google-apps.map"
    | "application/vnd.google-apps.fusiontable"
    | "application/vnd.google-apps.script" ->
        true
    | _ -> false

  let is_document resource =
    match resource.mime_type with
    | Some mime_type -> is_document_mime_type mime_type
    | _ -> false

  let is_symlink resource =
    resource.mime_type = Some "text/plain"
    && Option.is_some resource.link_target

  let is_shortcut resource =
    resource.mime_type = Some "application/vnd.google-apps.shortcut"

  let is_valid resource metadata_last_update =
    resource.state = State.ToUpload
    || resource.state = State.Uploading
    || resource.last_update >= metadata_last_update

  let is_large_file config resource =
    let large_file_threshold =
      Int64.mul (Int64.of_int config.Config.large_file_threshold_mb) Utils.mb
    in
    config.Config.stream_large_files
    && Option.default 0L resource.size > large_file_threshold

  let to_stream config resource =
    let to_stream =
      (not (is_document resource))
      && resource.state = State.ToDownload
      && is_large_file config resource
    in
    let to_memory_buffer = config.Config.memory_buffer_size > 0 in
    (to_stream, to_memory_buffer)

  let get_format_from_mime_type mime_type config =
    match mime_type with
    | "application/vnd.google-apps.document" -> config.Config.document_format
    | "application/vnd.google-apps.drawing" -> config.Config.drawing_format
    | "application/vnd.google-apps.form" -> config.Config.form_format
    | "application/vnd.google-apps.presentation" ->
        config.Config.presentation_format
    | "application/vnd.google-apps.spreadsheet" ->
        config.Config.spreadsheet_format
    | "application/vnd.google-apps.script" -> config.Config.apps_script_format
    | "application/vnd.google-apps.map" -> config.Config.map_format
    | "application/vnd.google-apps.fusiontable" ->
        config.Config.fusion_table_format
    | _ -> "html"

  let get_format resource config =
    match resource.mime_type with
    | Some mime_type -> get_format_from_mime_type mime_type config
    | _ -> "html"

  let get_icon_from_mime_type mime_type config =
    match mime_type with
    | "application/vnd.google-apps.document" -> config.Config.document_icon
    | "application/vnd.google-apps.drawing" -> config.Config.drawing_icon
    | "application/vnd.google-apps.form" -> config.Config.form_icon
    | "application/vnd.google-apps.presentation" ->
        config.Config.presentation_icon
    | "application/vnd.google-apps.spreadsheet" ->
        config.Config.spreadsheet_icon
    | "application/vnd.google-apps.script" -> config.Config.apps_script_icon
    | "application/vnd.google-apps.map" -> config.Config.map_icon
    | "application/vnd.google-apps.fusiontable" ->
        config.Config.fusion_table_icon
    | _ -> ""

  let get_icon resource config =
    match resource.mime_type with
    | Some mime_type -> get_icon_from_mime_type mime_type config
    | _ -> "html"

  let mime_type_of_format fmt =
    match fmt with
    | "csv" -> "text/csv"
    | "doc" | "docx" ->
        "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    | "epub" -> "application/epub+zip"
    | "htm" | "html" -> "text/html"
    | "jpg" | "jpeg" -> "image/jpeg"
    | "json" -> "application/vnd.google-apps.script+json"
    | "ods" -> "application/x-vnd.oasis.opendocument.spreadsheet"
    | "odt" -> "application/vnd.oasis.opendocument.text"
    | "odp" -> "application/vnd.oasis.opendocument.presentation"
    | "pdf" -> "application/pdf"
    | "png" -> "image/png"
    | "ppt" | "pptx" ->
        "application/vnd.openxmlformats-officedocument.presentationml.presentation"
    | "rtf" -> "application/rtf"
    | "svg" -> "image/svg+xml"
    | "tsv" -> "text/tab-separated-values"
    | "txt" -> "text/plain"
    | "xls" | "xlsx" ->
        "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    | "zip" -> "application/zip"
    | _ -> failwith ("Unsupported format: " ^ fmt)

  (* Export links *)
  let serialize_export_links export_links =
    let b = Buffer.create 512 in
    List.iter
      (fun (k, v) ->
        Buffer.add_string b k;
        Buffer.add_string b "|";
        Buffer.add_string b v;
        Buffer.add_string b ";")
      export_links;
    Buffer.contents b

  let parse_export_links serialize_export_links =
    let parse_export_link serialized_export_link =
      let pipe_index = String.index serialized_export_link '|' in
      let k = String.sub serialized_export_link 0 pipe_index in
      let v =
        String.sub serialized_export_link (pipe_index + 1)
          (String.length serialized_export_link - pipe_index - 1)
      in
      (k, v)
    in
    let rec loop accu s =
      try
        let semicolon_index = String.index s ';' in
        let kv = String.sub s 0 semicolon_index in
        let rest =
          String.sub s (semicolon_index + 1)
            (String.length s - semicolon_index - 1)
        in
        loop (parse_export_link kv :: accu) rest
      with Not_found -> accu
    in
    loop [] serialize_export_links |> List.rev
end

module Metadata = struct
  type t = {
    display_name : string;
    storage_quota_limit : int64;
    storage_quota_usage : int64;
    start_page_token : string;
    cache_size : int64;
    last_update : float;
    clean_shutdown : bool;
  }

  let display_name =
    {
      GapiLens.get = (fun x -> x.display_name);
      GapiLens.set = (fun v x -> { x with display_name = v });
    }

  let storage_quota_limit =
    {
      GapiLens.get = (fun x -> x.storage_quota_limit);
      GapiLens.set = (fun v x -> { x with storage_quota_limit = v });
    }

  let storage_quota_usage =
    {
      GapiLens.get = (fun x -> x.storage_quota_usage);
      GapiLens.set = (fun v x -> { x with storage_quota_usage = v });
    }

  let start_page_token =
    {
      GapiLens.get = (fun x -> x.start_page_token);
      GapiLens.set = (fun v x -> { x with start_page_token = v });
    }

  let cache_size =
    {
      GapiLens.get = (fun x -> x.cache_size);
      GapiLens.set = (fun v x -> { x with cache_size = v });
    }

  let last_update =
    {
      GapiLens.get = (fun x -> x.last_update);
      GapiLens.set = (fun v x -> { x with last_update = v });
    }

  let clean_shutdown =
    {
      GapiLens.get = (fun x -> x.clean_shutdown);
      GapiLens.set = (fun v x -> { x with clean_shutdown = v });
    }

  let is_valid metadata_cache_time metadata =
    let now = Unix.gettimeofday () in
    now -. metadata.last_update <= float_of_int metadata_cache_time
end

module UploadEntry = struct
  module State = struct
    type t = ToUpload | Uploading

    let to_string = function ToUpload -> "ToUpload" | Uploading -> "Uploading"

    let of_string = function
      | "ToUpload" -> ToUpload
      | "Uploading" -> Uploading
      | s -> failwith ("Upload entry state unexpected: " ^ s)
  end

  type t = {
    (* rowid *)
    id : int64;
    resource_id : int64;
    state : string;
    last_update : float;
  }

  let id =
    {
      GapiLens.get = (fun x -> x.id);
      GapiLens.set = (fun v x -> { x with id = v });
    }

  let resource_id =
    {
      GapiLens.get = (fun x -> x.resource_id);
      GapiLens.set = (fun v x -> { x with resource_id = v });
    }

  let state =
    {
      GapiLens.get = (fun x -> x.state);
      GapiLens.set = (fun v x -> { x with state = v });
    }

  let last_update =
    {
      GapiLens.get = (fun x -> x.last_update);
      GapiLens.set = (fun v x -> { x with last_update = v });
    }
end
