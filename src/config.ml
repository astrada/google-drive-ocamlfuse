open GapiUtils.Infix
open GapiLens.Infix

let application_name = "google-drive-ocamlfuse"
let version = "0.6.0"

module ConflictResolutionStrategy =
struct
  type t =
      Client
    | Server

  let to_string = function
      Client -> "client"
    | Server -> "server"

  let of_string = function
      "client" -> Client
    | "server" -> Server
    | s -> failwith ("Unsupported conflict resolution strategy: " ^ s)

end

type t = {
  (* Debug mode *)
  debug : bool;
  (* Number of seconds metadata should be cached. *)
  metadata_cache_time : int;
  (* Specifies whether local filesystem is mounted read-only. *)
  read_only : bool;
  (* umask mount option *)
  umask : int;
  (* Sqlite3 busy handler timeout in milliseconds *)
  sqlite3_busy_timeout : int;
  (* Specifies whether to download also Google Docs *)
  download_docs : bool;
  (* Text documents export format *)
  document_format : string;
  (* Text documents icon *)
  document_icon : string;
  (* Drawings export format *)
  drawing_format : string;
  (* Drawings icon *)
  drawing_icon : string;
  (* Forms export format *)
  form_format : string;
  (* Forms icon *)
  form_icon : string;
  (* Presentations export format *)
  presentation_format : string;
  (* Presentations icon *)
  presentation_icon : string;
  (* Spreadsheets export format *)
  spreadsheet_format : string;
  (* Spreadsheets icon *)
  spreadsheet_icon : string;
  (* Map export format *)
  map_format : string;
  (* Map icon *)
  map_icon : string;
  (* OAuth2 Client ID *)
  client_id : string;
  (* OAuth2 Client secret *)
  client_secret : string;
  (* OAuth2 verification code *)
  verification_code : string;
  (* Conflict resolution strategy:
   * - client: (in case of conflict) always update server (client side wins)
   * - server: (in case of conflict) always maintain server version (server side
   * wins) *)
  conflict_resolution : ConflictResolutionStrategy.t;
  (* Specifies whether to keep files with duplicated names (no overwrite) *)
  keep_duplicates : bool;
  (* Specifies whether to put file extension to Google Docs *)
  docs_file_extension : bool;
  (* Maximum cache size in megabytes *)
  max_cache_size_mb : int;
  (* Specifies whether a file overwrite should instead create a new revision *)
  new_revision : bool;
  (* Specifies whether not to log requests/responses (CURL). Set to true to
   * avoid a segmentation fault on some architectures. *)
  curl_debug_off : bool;
  (* Files removed from trash folder are permanently deleted (no recovery
   * possible) *)
  delete_forever_in_trash_folder : bool;
  (* Specifies whether large files should not be cached (they will be directly
   * streamed) *)
  stream_large_files : bool;
  (* Specifies the minimum size (in megabytes) of large files *)
  large_file_threshold_mb : int;
  (* Specifies whether to force document export even if the requested format
   * is not available *)
  force_docs_export : bool;
  (* Specifies whether to start uploading in a parallel thread. Warning! This
   * flag is EXPERIMENTAL *)
  async_upload : bool;
  (* Specifies connection timeout in milliseconds *)
  connect_timeout_ms : int;
  max_download_speed : int64;
  (* Max download speed (on a single transfer) in bytes/second. *)
  max_upload_speed : int64;
  (* Max upload speed (on a single transfer) in bytes/second. *)
  low_speed_limit : int;
  low_speed_time : int;
  (* If speed (in bytes/second) is under low_speed_limit for low_speed_time
   * (in seconds), the file transfer is considered too slow and therefore
   * terminated. *)
  max_retries : int;
  (* Specifies the maximum number of attempts if an operation fails. *)
}

let debug = {
  GapiLens.get = (fun x -> x.debug);
  GapiLens.set = (fun v x -> { x with debug = v })
}
let metadata_cache_time = {
  GapiLens.get = (fun x -> x.metadata_cache_time);
  GapiLens.set = (fun v x -> { x with metadata_cache_time = v })
}
let read_only = {
  GapiLens.get = (fun x -> x.read_only);
  GapiLens.set = (fun v x -> { x with read_only = v })
}
let umask = {
  GapiLens.get = (fun x -> x.umask);
  GapiLens.set = (fun v x -> { x with umask = v })
}
let sqlite3_busy_timeout = {
  GapiLens.get = (fun x -> x.sqlite3_busy_timeout);
  GapiLens.set = (fun v x -> { x with sqlite3_busy_timeout = v })
}
let download_docs = {
  GapiLens.get = (fun x -> x.download_docs);
  GapiLens.set = (fun v x -> { x with download_docs = v })
}
let document_format = {
  GapiLens.get = (fun x -> x.document_format);
  GapiLens.set = (fun v x -> { x with document_format = v })
}
let document_icon = {
  GapiLens.get = (fun x -> x.document_icon);
  GapiLens.set = (fun v x -> { x with document_icon = v })
}
let drawing_format = {
  GapiLens.get = (fun x -> x.drawing_format);
  GapiLens.set = (fun v x -> { x with drawing_format = v })
}
let drawing_icon = {
  GapiLens.get = (fun x -> x.drawing_icon);
  GapiLens.set = (fun v x -> { x with drawing_icon = v })
}
let form_format = {
  GapiLens.get = (fun x -> x.form_format);
  GapiLens.set = (fun v x -> { x with form_format = v })
}
let form_icon = {
  GapiLens.get = (fun x -> x.form_icon);
  GapiLens.set = (fun v x -> { x with form_icon = v })
}
let presentation_format = {
  GapiLens.get = (fun x -> x.presentation_format);
  GapiLens.set = (fun v x -> { x with presentation_format = v })
}
let presentation_icon = {
  GapiLens.get = (fun x -> x.presentation_icon);
  GapiLens.set = (fun v x -> { x with presentation_icon = v })
}
let spreadsheet_format = {
  GapiLens.get = (fun x -> x.spreadsheet_format);
  GapiLens.set = (fun v x -> { x with spreadsheet_format = v })
}
let spreadsheet_icon = {
  GapiLens.get = (fun x -> x.spreadsheet_icon);
  GapiLens.set = (fun v x -> { x with spreadsheet_icon = v })
}
let map_format = {
  GapiLens.get = (fun x -> x.map_format);
  GapiLens.set = (fun v x -> { x with map_format = v })
}
let map_icon = {
  GapiLens.get = (fun x -> x.map_icon);
  GapiLens.set = (fun v x -> { x with map_icon = v })
}
let client_id = {
  GapiLens.get = (fun x -> x.client_id);
  GapiLens.set = (fun v x -> { x with client_id = v })
}
let client_secret = {
  GapiLens.get = (fun x -> x.client_secret);
  GapiLens.set = (fun v x -> { x with client_secret = v })
}
let verification_code = {
  GapiLens.get = (fun x -> x.verification_code);
  GapiLens.set = (fun v x -> { x with verification_code = v })
}
let conflict_resolution = {
  GapiLens.get = (fun x -> x.conflict_resolution);
  GapiLens.set = (fun v x -> { x with conflict_resolution = v })
}
let keep_duplicates = {
  GapiLens.get = (fun x -> x.keep_duplicates);
  GapiLens.set = (fun v x -> { x with keep_duplicates = v })
}
let docs_file_extension = {
  GapiLens.get = (fun x -> x.docs_file_extension);
  GapiLens.set = (fun v x -> { x with docs_file_extension = v })
}
let max_cache_size_mb = {
  GapiLens.get = (fun x -> x.max_cache_size_mb);
  GapiLens.set = (fun v x -> { x with max_cache_size_mb = v })
}
let new_revision = {
  GapiLens.get = (fun x -> x.new_revision);
  GapiLens.set = (fun v x -> { x with new_revision = v })
}
let curl_debug_off = {
  GapiLens.get = (fun x -> x.curl_debug_off);
  GapiLens.set = (fun v x -> { x with curl_debug_off = v })
}
let delete_forever_in_trash_folder = {
  GapiLens.get = (fun x -> x.delete_forever_in_trash_folder);
  GapiLens.set = (fun v x -> { x with delete_forever_in_trash_folder = v })
}
let stream_large_files = {
  GapiLens.get = (fun x -> x.stream_large_files);
  GapiLens.set = (fun v x -> { x with stream_large_files = v })
}
let large_file_threshold_mb = {
  GapiLens.get = (fun x -> x.large_file_threshold_mb);
  GapiLens.set = (fun v x -> { x with large_file_threshold_mb = v })
}
let force_docs_export = {
  GapiLens.get = (fun x -> x.force_docs_export);
  GapiLens.set = (fun v x -> { x with force_docs_export = v })
}
let async_upload = {
  GapiLens.get = (fun x -> x.async_upload);
  GapiLens.set = (fun v x -> { x with async_upload = v })
}
let connect_timeout_ms = {
  GapiLens.get = (fun x -> x.connect_timeout_ms);
  GapiLens.set = (fun v x -> { x with connect_timeout_ms = v })
}
let max_download_speed = {
  GapiLens.get = (fun x -> x.max_download_speed);
  GapiLens.set = (fun v x -> { x with max_download_speed = v })
}
let max_upload_speed = {
  GapiLens.get = (fun x -> x.max_upload_speed);
  GapiLens.set = (fun v x -> { x with max_upload_speed = v })
}
let low_speed_limit = {
  GapiLens.get = (fun x -> x.low_speed_limit);
  GapiLens.set = (fun v x -> { x with low_speed_limit = v })
}
let low_speed_time = {
  GapiLens.get = (fun x -> x.low_speed_time);
  GapiLens.set = (fun v x -> { x with low_speed_time = v })
}
let max_retries = {
  GapiLens.get = (fun x -> x.max_retries);
  GapiLens.set = (fun v x -> { x with max_retries = v })
}

let umask =
  let prev_umask = Unix.umask 0 in
  let _ = Unix.umask prev_umask in
    prev_umask

let default = {
  debug = false;
  metadata_cache_time = 60;
  read_only = false;
  umask;
  sqlite3_busy_timeout = 5000;
  download_docs = true;
  document_format = "odt";
  document_icon = "";
  drawing_format = "png";
  drawing_icon = "";
  form_format = "ods";
  form_icon = "";
  presentation_format = "pdf";
  presentation_icon = "";
  spreadsheet_format = "ods";
  spreadsheet_icon = "";
  map_format = "desktop";
  map_icon = "";
  client_id = "";
  client_secret = "";
  verification_code = "";
  conflict_resolution = ConflictResolutionStrategy.Server;
  keep_duplicates = false;
  docs_file_extension = true;
  max_cache_size_mb = 512;
  new_revision = true;
  curl_debug_off = false;
  delete_forever_in_trash_folder = false;
  stream_large_files = false;
  large_file_threshold_mb = 16;
  force_docs_export = true;
  async_upload = false;
  connect_timeout_ms = 5000;
  max_download_speed = 0L;
  max_upload_speed = 0L;
  low_speed_limit = 0;
  low_speed_time = 0;
  max_retries = 10;
}

let default_debug = {
  debug = true;
  metadata_cache_time = 60;
  read_only = false;
  umask;
  sqlite3_busy_timeout = 5000;
  download_docs = true;
  document_format = "odt";
  document_icon = "";
  drawing_format = "png";
  drawing_icon = "";
  form_format = "ods";
  form_icon = "";
  presentation_format = "pdf";
  presentation_icon = "";
  spreadsheet_format = "ods";
  spreadsheet_icon = "";
  map_format = "desktop";
  map_icon = "";
  client_id = "";
  client_secret = "";
  verification_code = "";
  conflict_resolution = ConflictResolutionStrategy.Server;
  keep_duplicates = false;
  docs_file_extension = true;
  max_cache_size_mb = 512;
  new_revision = true;
  curl_debug_off = false;
  delete_forever_in_trash_folder = false;
  stream_large_files = false;
  large_file_threshold_mb = 16;
  force_docs_export = true;
  async_upload = false;
  connect_timeout_ms = 5000;
  max_download_speed = 0L;
  max_upload_speed = 0L;
  low_speed_limit = 0;
  low_speed_time = 0;
  max_retries = 10;
}

let of_table table =
  let get k = Utils.get_from_string_table table k in
    { debug = get "debug" bool_of_string default.debug;
      metadata_cache_time =
        get "metadata_cache_time" int_of_string default.metadata_cache_time;
      read_only = get "read_only" bool_of_string default.read_only;
      umask = get "umask" int_of_string default.umask;
      sqlite3_busy_timeout =
        get "sqlite3_busy_timeout" int_of_string default.sqlite3_busy_timeout;
      download_docs =
        get "download_docs" bool_of_string default.download_docs;
      document_format =
        get "document_format" Std.identity default.document_format;
      document_icon =
        get "document_icon" Std.identity default.document_icon;
      drawing_format =
        get "drawing_format" Std.identity default.drawing_format;
      drawing_icon =
        get "drawing_icon" Std.identity default.drawing_icon;
      form_format =
        get "form_format" Std.identity default.form_format;
      form_icon =
        get "form_icon" Std.identity default.form_icon;
      presentation_format =
        get "presentation_format" Std.identity default.presentation_format;
      presentation_icon =
        get "presentation_icon" Std.identity default.presentation_icon;
      spreadsheet_format =
        get "spreadsheet_format" Std.identity default.spreadsheet_format;
      spreadsheet_icon =
        get "spreadsheet_icon" Std.identity default.spreadsheet_icon;
      map_format = get "map_format" Std.identity default.map_format;
      map_icon = get "map_icon" Std.identity default.map_icon;
      client_id = get "client_id" Std.identity default.client_id;
      client_secret = get "client_secret" Std.identity default.client_secret;
      verification_code =
        get "verification_code" Std.identity default.verification_code;
      conflict_resolution =
        get "conflict_resolution" ConflictResolutionStrategy.of_string
          default.conflict_resolution;
      keep_duplicates =
        get "keep_duplicates" bool_of_string default.keep_duplicates;
      docs_file_extension =
        get "docs_file_extension" bool_of_string default.docs_file_extension;
      max_cache_size_mb =
        get "max_cache_size_mb" int_of_string default.max_cache_size_mb;
      new_revision =
        get "new_revision" bool_of_string default.new_revision;
      curl_debug_off =
        get "curl_debug_off" bool_of_string default.curl_debug_off;
      delete_forever_in_trash_folder =
        get "delete_forever_in_trash_folder" bool_of_string
          default.delete_forever_in_trash_folder;
      stream_large_files =
        get "stream_large_files" bool_of_string
          default.stream_large_files;
      large_file_threshold_mb =
        get "large_file_threshold_mb" int_of_string
          default.large_file_threshold_mb;
      force_docs_export =
        get "force_docs_export" bool_of_string default.force_docs_export;
      async_upload =
        get "async_upload" bool_of_string default.async_upload;
      connect_timeout_ms =
        get "connect_timeout_ms" int_of_string default.connect_timeout_ms;
      max_download_speed =
        get "max_download_speed" Int64.of_string default.max_download_speed;
      max_upload_speed =
        get "max_upload_speed" Int64.of_string default.max_upload_speed;
      low_speed_limit =
        get "low_speed_limit" int_of_string default.low_speed_limit;
      low_speed_time =
        get "low_speed_time" int_of_string default.low_speed_time;
      max_retries = get "max_retries" int_of_string default.max_retries;
    }

let to_table data =
  let table = Hashtbl.create 16 in
  let add = Hashtbl.add table in
    add "debug" (data.debug |> string_of_bool);
    add "metadata_cache_time" (data.metadata_cache_time |> string_of_int);
    add "read_only" (data.read_only |> string_of_bool);
    add "umask" (data.umask |> Printf.sprintf "0o%03o");
    add "sqlite3_busy_timeout" (data.sqlite3_busy_timeout |> string_of_int);
    add "download_docs" (data.download_docs |> string_of_bool);
    add "document_format" data.document_format;
    add "document_icon" data.document_icon;
    add "drawing_format" data.drawing_format;
    add "drawing_icon" data.drawing_icon;
    add "form_format" data.form_format;
    add "form_icon" data.form_icon;
    add "presentation_format" data.presentation_format;
    add "presentation_icon" data.presentation_icon;
    add "spreadsheet_format" data.spreadsheet_format;
    add "spreadsheet_icon" data.spreadsheet_icon;
    add "map_format" data.map_format;
    add "map_icon" data.map_icon;
    add "client_id" data.client_id;
    add "client_secret" data.client_secret;
    add "verification_code" data.verification_code;
    add "conflict_resolution"
      (data.conflict_resolution |> ConflictResolutionStrategy.to_string);
    add "keep_duplicates" (data.keep_duplicates |> string_of_bool);
    add "docs_file_extension" (data.docs_file_extension |> string_of_bool);
    add "max_cache_size_mb" (data.max_cache_size_mb |> string_of_int);
    add "new_revision" (data.new_revision |> string_of_bool);
    add "curl_debug_off" (data.curl_debug_off |> string_of_bool);
    add "delete_forever_in_trash_folder"
      (data.delete_forever_in_trash_folder |> string_of_bool);
    add "stream_large_files" (data.stream_large_files |> string_of_bool);
    add "large_file_threshold_mb"
      (data.large_file_threshold_mb |> string_of_int);
    add "force_docs_export" (data.force_docs_export |> string_of_bool);
    add "async_upload" (data.async_upload |> string_of_bool);
    add "connect_timeout_ms" (data.connect_timeout_ms |> string_of_int);
    add "max_download_speed" (data.max_download_speed |> Int64.to_string);
    add "max_upload_speed" (data.max_upload_speed |> Int64.to_string);
    add "low_speed_limit" (data.low_speed_limit |> string_of_int);
    add "low_speed_time" (data.low_speed_time |> string_of_int);
    add "max_retries" (data.max_retries |> string_of_int);
    table

let debug_print out_ch start_time curl info_type info =
  let time = Unix.gettimeofday () in
  let timestamp = time -. start_time in
  let nl =
    if String.length info > 0 &&
       info.[String.length info - 1] = '\n' then ""
    else "\n"
  in
    Printf.fprintf out_ch "[%f] curl: %s: %s%s%!"
      timestamp
      (GapiCurl.string_of_curl_info_type info_type)
      info
      nl

let create_gapi_config config app_dir =
  let gapi_config =
    if config.debug && (not config.curl_debug_off) then
      let out_ch = open_out (app_dir |. AppDir.curl_log_path) in
      let debug_function = debug_print out_ch (Unix.gettimeofday ()) in
        GapiConfig.default_debug
          |> GapiConfig.debug ^= Some (GapiConfig.Custom debug_function)
    else
      GapiConfig.default
  in
    gapi_config
      |> GapiConfig.application_name ^= application_name ^ " (" ^ version ^ ")"
      |> GapiConfig.connect_timeout ^= Some config.connect_timeout_ms
      |> GapiConfig.max_recv_speed ^= config.max_download_speed
      |> GapiConfig.max_send_speed ^= config.max_upload_speed
      |> GapiConfig.low_speed_limit ^= config.low_speed_limit
      |> GapiConfig.low_speed_time ^= config.low_speed_time
      (* If client_id and client_secret are not set, the authorization will
       * be handled by the GAE proxy *)
      |> GapiConfig.auth ^= GapiConfig.OAuth2
                              { GapiConfig.client_id = config.client_id;
                                client_secret = config.client_secret;
                                refresh_access_token = None }

