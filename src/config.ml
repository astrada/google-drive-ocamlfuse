open GapiUtils.Infix
open GapiLens.Infix

let application_name = "google-drive-ocamlfuse"
let version = "0.6.26"

type t = {
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
  (* Fusion tables export format *)
  fusion_table_format : string;
  (* Fusion tables icon *)
  fusion_table_icon : string;
  (* Google apps script export format *)
  apps_script_format : string;
  (* Google apps script icon *)
  apps_script_icon : string;
  (* OAuth2 Client ID *)
  client_id : string;
  (* OAuth2 Client secret *)
  client_secret : string;
  (* OAuth2 verification code *)
  verification_code : string;
  (* Specifies whether to keep files with duplicated names (no overwrite) *)
  keep_duplicates : bool;
  (* Specifies whether to put file extension to Google Docs *)
  docs_file_extension : bool;
  (* Maximum cache size in megabytes *)
  max_cache_size_mb : int;
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
  (* Specifies whether large files are read-only. Checked only if
   * [stream_large_files] is [true]. *)
  large_file_read_only : bool;
  (* Specifies whether to start uploading in a parallel thread. *)
  async_upload : bool;
  (* Specifies connection timeout in milliseconds *)
  connect_timeout_ms : int;
  (* Max download speed (on a single transfer) in bytes/second. *)
  max_download_speed : int64;
  (* Max upload speed (on a single transfer) in bytes/second. *)
  max_upload_speed : int64;
  (* If speed (in bytes/second) is under low_speed_limit for low_speed_time
   * (in seconds), the file transfer is considered too slow and therefore
   * terminated. *)
  low_speed_limit : int;
  low_speed_time : int;
  (* Specifies the maximum number of attempts if an operation fails. *)
  max_retries : int;
  (* Specifies the maximum size (in bytes) of file chunks during upload
   * operations. *)
  max_upload_chunk_size : int;
  (* Specifies the size (in bytes) of memory buffer blocks. *)
  memory_buffer_size : int;
  (* Maximum memory cache size in bytes (used if [stream_large_files] is
   * [true]. *)
  max_memory_cache_size : int;
  (* Specifies how many blocks (of [memory_buffer_size] bytes) can be
   * pre-fetched when streaming. *)
  read_ahead_buffers : int;
  (* Fetch files with no parents and make them available in lost+found. *)
  lost_and_found : bool;
  (* Fetch shared files and make them available in .shared *)
  shared_with_me : bool;
  (* Path of the directory storing application state *)
  data_directory : string;
  (* Path of the directory storing application cache *)
  cache_directory : string;
  (* Path of the directory containing log files *)
  log_directory : string;
  (* Folder id or remote path of the root folder *)
  root_folder : string;
  (* Team drive id *)
  team_drive_id : string;
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
let fusion_table_format = {
  GapiLens.get = (fun x -> x.fusion_table_format);
  GapiLens.set = (fun v x -> { x with fusion_table_format = v })
}
let fusion_table_icon = {
  GapiLens.get = (fun x -> x.fusion_table_icon);
  GapiLens.set = (fun v x -> { x with fusion_table_icon = v })
}
let apps_script_format = {
  GapiLens.get = (fun x -> x.apps_script_format);
  GapiLens.set = (fun v x -> { x with apps_script_format = v })
}
let apps_script_icon = {
  GapiLens.get = (fun x -> x.apps_script_icon);
  GapiLens.set = (fun v x -> { x with apps_script_icon = v })
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
let large_file_read_only = {
  GapiLens.get = (fun x -> x.large_file_read_only);
  GapiLens.set = (fun v x -> { x with large_file_read_only = v })
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
let max_upload_chunk_size = {
  GapiLens.get = (fun x -> x.max_upload_chunk_size);
  GapiLens.set = (fun v x -> { x with max_upload_chunk_size = v })
}
let memory_buffer_size = {
  GapiLens.get = (fun x -> x.memory_buffer_size);
  GapiLens.set = (fun v x -> { x with memory_buffer_size = v })
}
let max_memory_cache_size = {
  GapiLens.get = (fun x -> x.max_memory_cache_size);
  GapiLens.set = (fun v x -> { x with max_memory_cache_size = v })
}
let read_ahead_buffers = {
  GapiLens.get = (fun x -> x.read_ahead_buffers);
  GapiLens.set = (fun v x -> { x with read_ahead_buffers = v })
}
let lost_and_found = {
  GapiLens.get = (fun x -> x.lost_and_found);
  GapiLens.set = (fun v x -> { x with lost_and_found = v })
}
let shared_with_me = {
  GapiLens.get = (fun x -> x.shared_with_me);
  GapiLens.set = (fun v x -> { x with shared_with_me = v })
}
let data_directory = {
  GapiLens.get = (fun x -> x.data_directory);
  GapiLens.set = (fun v x -> { x with data_directory = v })
}
let cache_directory = {
  GapiLens.get = (fun x -> x.cache_directory);
  GapiLens.set = (fun v x -> { x with cache_directory = v })
}
let log_directory = {
  GapiLens.get = (fun x -> x.log_directory);
  GapiLens.set = (fun v x -> { x with log_directory = v })
}
let root_folder = {
  GapiLens.get = (fun x -> x.root_folder);
  GapiLens.set = (fun v x -> { x with root_folder = v })
}
let team_drive_id = {
  GapiLens.get = (fun x -> x.team_drive_id);
  GapiLens.set = (fun v x -> { x with team_drive_id = v })
}

let umask =
  let prev_umask = Unix.umask 0 in
  let _ = Unix.umask prev_umask in
  prev_umask

let default_max_upload_chunk_size =
  if Sys.word_size == 64 then
    Int64.to_int
      (Int64.mul
        (Int64.mul
           (Int64.mul 1024L 1024L)
           1024L)
        1024L) (* 1TB *)
  else 768 * 1024 * 1024 (* 768MB *)

let default = {
  metadata_cache_time = 60;
  read_only = false;
  umask;
  sqlite3_busy_timeout = 5000;
  download_docs = true;
  document_format = "odt";
  document_icon = "";
  drawing_format = "png";
  drawing_icon = "";
  form_format = "zip";
  form_icon = "";
  presentation_format = "pdf";
  presentation_icon = "";
  spreadsheet_format = "ods";
  spreadsheet_icon = "";
  map_format = "desktop";
  map_icon = "";
  fusion_table_format = "desktop";
  fusion_table_icon = "";
  apps_script_format = "json";
  apps_script_icon = "";
  client_id = "";
  client_secret = "";
  verification_code = "";
  keep_duplicates = false;
  docs_file_extension = true;
  max_cache_size_mb = 512;
  curl_debug_off = false;
  delete_forever_in_trash_folder = false;
  stream_large_files = false;
  large_file_threshold_mb = 16;
  large_file_read_only = false;
  async_upload = true;
  connect_timeout_ms = 5000;
  max_download_speed = 0L;
  max_upload_speed = 0L;
  low_speed_limit = 0;
  low_speed_time = 0;
  max_retries = 8;
  max_upload_chunk_size = default_max_upload_chunk_size;
  memory_buffer_size = 1048576;
  max_memory_cache_size = 10485760;
  read_ahead_buffers = 3;
  lost_and_found = false;
  shared_with_me = false;
  data_directory = "";
  cache_directory = "";
  log_directory = "";
  root_folder = "";
  team_drive_id = "";
}

let default_debug = {
  metadata_cache_time = 60;
  read_only = false;
  umask;
  sqlite3_busy_timeout = 5000;
  download_docs = true;
  document_format = "odt";
  document_icon = "";
  drawing_format = "png";
  drawing_icon = "";
  form_format = "zip";
  form_icon = "";
  presentation_format = "pdf";
  presentation_icon = "";
  spreadsheet_format = "ods";
  spreadsheet_icon = "";
  map_format = "desktop";
  map_icon = "";
  fusion_table_format = "desktop";
  fusion_table_icon = "";
  apps_script_format = "json";
  apps_script_icon = "";
  client_id = "";
  client_secret = "";
  verification_code = "";
  keep_duplicates = false;
  docs_file_extension = true;
  max_cache_size_mb = 512;
  curl_debug_off = false;
  delete_forever_in_trash_folder = false;
  stream_large_files = false;
  large_file_threshold_mb = 1;
  large_file_read_only = false;
  async_upload = true;
  connect_timeout_ms = 5000;
  max_download_speed = 0L;
  max_upload_speed = 0L;
  low_speed_limit = 0;
  low_speed_time = 0;
  max_retries = 8;
  max_upload_chunk_size = default_max_upload_chunk_size;
  memory_buffer_size = 1048576;
  max_memory_cache_size = 10485760;
  read_ahead_buffers = 3;
  lost_and_found = false;
  shared_with_me = false;
  data_directory = "";
  cache_directory = "";
  log_directory = "";
  root_folder = "";
  team_drive_id = "";
}

let of_table table =
  let get k = Utils.get_from_string_table table k in
    { metadata_cache_time =
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
      fusion_table_format =
        get "fusion_table_format" Std.identity default.fusion_table_format;
      fusion_table_icon =
        get "fusion_table_icon" Std.identity default.fusion_table_icon;
      apps_script_format =
        get "apps_script_format" Std.identity default.apps_script_format;
      apps_script_icon =
        get "apps_script_icon" Std.identity default.apps_script_icon;
      client_id = get "client_id" Std.identity default.client_id;
      client_secret = get "client_secret" Std.identity default.client_secret;
      verification_code =
        get "verification_code" Std.identity default.verification_code;
      keep_duplicates =
        get "keep_duplicates" bool_of_string default.keep_duplicates;
      docs_file_extension =
        get "docs_file_extension" bool_of_string default.docs_file_extension;
      max_cache_size_mb =
        get "max_cache_size_mb" int_of_string default.max_cache_size_mb;
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
      large_file_read_only =
        get "large_file_read_only" bool_of_string
          default.large_file_read_only;
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
      max_upload_chunk_size =
        get "max_upload_chunk_size" int_of_string
          default.max_upload_chunk_size;
      memory_buffer_size =
        get "memory_buffer_size" int_of_string
          default.memory_buffer_size;
      max_memory_cache_size =
        get "max_memory_cache_size" int_of_string
          default.max_memory_cache_size;
      read_ahead_buffers =
        get "read_ahead_buffers" int_of_string
          default.read_ahead_buffers;
      lost_and_found =
        get "lost_and_found" bool_of_string
          default.lost_and_found;
      shared_with_me =
        get "shared_with_me" bool_of_string
          default.shared_with_me;
      data_directory =
        get "data_directory" Std.identity
          default.data_directory;
      cache_directory =
        get "cache_directory" Std.identity
          default.cache_directory;
      log_directory =
        get "log_directory" Std.identity
          default.log_directory;
      root_folder =
        get "root_folder" Std.identity
          default.root_folder;
      team_drive_id =
        get "team_drive_id" Std.identity
          default.team_drive_id;
    }

let to_table data =
  let table = Hashtbl.create 16 in
  let add = Hashtbl.add table in
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
    add "fusion_table_format" data.fusion_table_format;
    add "fusion_table_icon" data.fusion_table_icon;
    add "apps_script_format" data.apps_script_format;
    add "apps_script_icon" data.apps_script_icon;
    add "client_id" data.client_id;
    add "client_secret" data.client_secret;
    add "verification_code" data.verification_code;
    add "keep_duplicates" (data.keep_duplicates |> string_of_bool);
    add "docs_file_extension" (data.docs_file_extension |> string_of_bool);
    add "max_cache_size_mb" (data.max_cache_size_mb |> string_of_int);
    add "curl_debug_off" (data.curl_debug_off |> string_of_bool);
    add "delete_forever_in_trash_folder"
      (data.delete_forever_in_trash_folder |> string_of_bool);
    add "stream_large_files" (data.stream_large_files |> string_of_bool);
    add "large_file_threshold_mb"
      (data.large_file_threshold_mb |> string_of_int);
    add "large_file_read_only"
      (data.large_file_read_only |> string_of_bool);
    add "async_upload" (data.async_upload |> string_of_bool);
    add "connect_timeout_ms" (data.connect_timeout_ms |> string_of_int);
    add "max_download_speed" (data.max_download_speed |> Int64.to_string);
    add "max_upload_speed" (data.max_upload_speed |> Int64.to_string);
    add "low_speed_limit" (data.low_speed_limit |> string_of_int);
    add "low_speed_time" (data.low_speed_time |> string_of_int);
    add "max_retries" (data.max_retries |> string_of_int);
    add "max_upload_chunk_size" (data.max_upload_chunk_size |> string_of_int);
    add "memory_buffer_size" (data.memory_buffer_size |> string_of_int);
    add "max_memory_cache_size" (data.max_memory_cache_size |> string_of_int);
    add "read_ahead_buffers" (data.read_ahead_buffers |> string_of_int);
    add "lost_and_found" (data.lost_and_found |> string_of_bool);
    add "shared_with_me" (data.shared_with_me |> string_of_bool);
    add "data_directory" data.data_directory;
    add "cache_directory" data.cache_directory;
    add "log_directory" data.log_directory;
    add "root_folder" data.root_folder;
    add "team_drive_id" data.team_drive_id;
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

let create_gapi_config config debug curl_log_path =
  let gapi_config =
    if debug && (not config.curl_debug_off) then
      let out_ch = open_out curl_log_path in
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
      |> GapiConfig.upload_chunk_size ^= config.max_upload_chunk_size
      (* If client_id and client_secret are not set, the authorization will
       * be handled by the GAE proxy *)
      |> GapiConfig.auth ^= GapiConfig.OAuth2
                              { GapiConfig.client_id = config.client_id;
                                client_secret = config.client_secret;
                                refresh_access_token = None }

