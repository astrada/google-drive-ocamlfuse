open GapiUtils.Infix
open GapiLens.Infix

let application_name = "google-drive-ocamlfuse"

let version = "0.7.22"

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
  (* Path of the directory storing application state *)
  data_directory : string;
  (* Path of the directory storing application cache *)
  cache_directory : string;
  (* Path of the directory containing log files *)
  log_directory : string;
  (* Specifies the file descriptor to log to (stdout/stderr). *)
  log_to : string;
  (* Folder id or remote path of the root folder *)
  root_folder : string;
  (* Team drive id *)
  team_drive_id : string;
  (* Specifies to cache metadata in memory and periodically save them to disk.
   *)
  metadata_memory_cache : bool;
  (* Interval (in seconds) between metadata memory cache saving. *)
  metadata_memory_cache_saving_interval : int;
  (* Specifies to download files that Drive considers abusive (malware, etc.) *)
  acknowledge_abuse : bool;
  (* Executable used to open desktop entries *)
  desktop_entry_exec : string;
  (* Use memory buffers to cache writes. *)
  write_buffers : bool;
  (* Disable trash folder. *)
  disable_trash : bool;
  (* Let Google Drive autodetect MIME types. *)
  autodetect_mime : bool;
  (* Make mv overwrite destination file keeping version history (if false, the
   * target will be trashed). *)
  mv_keep_target : bool;
  (* Enable asynchronous uploading. *)
  async_upload_queue : bool;
  (* Async upload queue thread pool size. *)
  async_upload_threads : int;
  (* Log buffer contents to ease debugging. *)
  debug_buffers : bool;
  (* Path of the JSON file that contains service account credentials. *)
  service_account_credentials_path : string;
  (* Email of the user for which the application is requesting delegated
   * access. Valid only for G Suite domains. Considered only if
   * service_account_credentials_path is specified. *)
  service_account_user_to_impersonate : string;
  (* Specifies a custom Drive API scope. *)
  scope : string;
  (* Specifies a custom Drive API redirect URI. *)
  redirect_uri : string;
  (* Specifies if transform desktop entries in HTML files, as support for
   * .desktop files is being removed from Nautilus. *)
  desktop_entry_as_html : bool;
  (* Async upload queue maximum number of entries (files) before blocking.
   * 0 means unlimited. *)
  async_upload_queue_max_length : int;
  (* Use a background thread to prefetch folder structure in advance. *)
  background_folder_fetching : bool;
}

let metadata_cache_time =
  {
    GapiLens.get = (fun x -> x.metadata_cache_time);
    GapiLens.set = (fun v x -> { x with metadata_cache_time = v });
  }

let read_only =
  {
    GapiLens.get = (fun x -> x.read_only);
    GapiLens.set = (fun v x -> { x with read_only = v });
  }

let umask =
  {
    GapiLens.get = (fun x -> x.umask);
    GapiLens.set = (fun v x -> { x with umask = v });
  }

let sqlite3_busy_timeout =
  {
    GapiLens.get = (fun x -> x.sqlite3_busy_timeout);
    GapiLens.set = (fun v x -> { x with sqlite3_busy_timeout = v });
  }

let download_docs =
  {
    GapiLens.get = (fun x -> x.download_docs);
    GapiLens.set = (fun v x -> { x with download_docs = v });
  }

let document_format =
  {
    GapiLens.get = (fun x -> x.document_format);
    GapiLens.set = (fun v x -> { x with document_format = v });
  }

let document_icon =
  {
    GapiLens.get = (fun x -> x.document_icon);
    GapiLens.set = (fun v x -> { x with document_icon = v });
  }

let drawing_format =
  {
    GapiLens.get = (fun x -> x.drawing_format);
    GapiLens.set = (fun v x -> { x with drawing_format = v });
  }

let drawing_icon =
  {
    GapiLens.get = (fun x -> x.drawing_icon);
    GapiLens.set = (fun v x -> { x with drawing_icon = v });
  }

let form_format =
  {
    GapiLens.get = (fun x -> x.form_format);
    GapiLens.set = (fun v x -> { x with form_format = v });
  }

let form_icon =
  {
    GapiLens.get = (fun x -> x.form_icon);
    GapiLens.set = (fun v x -> { x with form_icon = v });
  }

let presentation_format =
  {
    GapiLens.get = (fun x -> x.presentation_format);
    GapiLens.set = (fun v x -> { x with presentation_format = v });
  }

let presentation_icon =
  {
    GapiLens.get = (fun x -> x.presentation_icon);
    GapiLens.set = (fun v x -> { x with presentation_icon = v });
  }

let spreadsheet_format =
  {
    GapiLens.get = (fun x -> x.spreadsheet_format);
    GapiLens.set = (fun v x -> { x with spreadsheet_format = v });
  }

let spreadsheet_icon =
  {
    GapiLens.get = (fun x -> x.spreadsheet_icon);
    GapiLens.set = (fun v x -> { x with spreadsheet_icon = v });
  }

let map_format =
  {
    GapiLens.get = (fun x -> x.map_format);
    GapiLens.set = (fun v x -> { x with map_format = v });
  }

let map_icon =
  {
    GapiLens.get = (fun x -> x.map_icon);
    GapiLens.set = (fun v x -> { x with map_icon = v });
  }

let fusion_table_format =
  {
    GapiLens.get = (fun x -> x.fusion_table_format);
    GapiLens.set = (fun v x -> { x with fusion_table_format = v });
  }

let fusion_table_icon =
  {
    GapiLens.get = (fun x -> x.fusion_table_icon);
    GapiLens.set = (fun v x -> { x with fusion_table_icon = v });
  }

let apps_script_format =
  {
    GapiLens.get = (fun x -> x.apps_script_format);
    GapiLens.set = (fun v x -> { x with apps_script_format = v });
  }

let apps_script_icon =
  {
    GapiLens.get = (fun x -> x.apps_script_icon);
    GapiLens.set = (fun v x -> { x with apps_script_icon = v });
  }

let client_id =
  {
    GapiLens.get = (fun x -> x.client_id);
    GapiLens.set = (fun v x -> { x with client_id = v });
  }

let client_secret =
  {
    GapiLens.get = (fun x -> x.client_secret);
    GapiLens.set = (fun v x -> { x with client_secret = v });
  }

let verification_code =
  {
    GapiLens.get = (fun x -> x.verification_code);
    GapiLens.set = (fun v x -> { x with verification_code = v });
  }

let keep_duplicates =
  {
    GapiLens.get = (fun x -> x.keep_duplicates);
    GapiLens.set = (fun v x -> { x with keep_duplicates = v });
  }

let docs_file_extension =
  {
    GapiLens.get = (fun x -> x.docs_file_extension);
    GapiLens.set = (fun v x -> { x with docs_file_extension = v });
  }

let max_cache_size_mb =
  {
    GapiLens.get = (fun x -> x.max_cache_size_mb);
    GapiLens.set = (fun v x -> { x with max_cache_size_mb = v });
  }

let curl_debug_off =
  {
    GapiLens.get = (fun x -> x.curl_debug_off);
    GapiLens.set = (fun v x -> { x with curl_debug_off = v });
  }

let delete_forever_in_trash_folder =
  {
    GapiLens.get = (fun x -> x.delete_forever_in_trash_folder);
    GapiLens.set = (fun v x -> { x with delete_forever_in_trash_folder = v });
  }

let stream_large_files =
  {
    GapiLens.get = (fun x -> x.stream_large_files);
    GapiLens.set = (fun v x -> { x with stream_large_files = v });
  }

let large_file_threshold_mb =
  {
    GapiLens.get = (fun x -> x.large_file_threshold_mb);
    GapiLens.set = (fun v x -> { x with large_file_threshold_mb = v });
  }

let large_file_read_only =
  {
    GapiLens.get = (fun x -> x.large_file_read_only);
    GapiLens.set = (fun v x -> { x with large_file_read_only = v });
  }

let connect_timeout_ms =
  {
    GapiLens.get = (fun x -> x.connect_timeout_ms);
    GapiLens.set = (fun v x -> { x with connect_timeout_ms = v });
  }

let max_download_speed =
  {
    GapiLens.get = (fun x -> x.max_download_speed);
    GapiLens.set = (fun v x -> { x with max_download_speed = v });
  }

let max_upload_speed =
  {
    GapiLens.get = (fun x -> x.max_upload_speed);
    GapiLens.set = (fun v x -> { x with max_upload_speed = v });
  }

let low_speed_limit =
  {
    GapiLens.get = (fun x -> x.low_speed_limit);
    GapiLens.set = (fun v x -> { x with low_speed_limit = v });
  }

let low_speed_time =
  {
    GapiLens.get = (fun x -> x.low_speed_time);
    GapiLens.set = (fun v x -> { x with low_speed_time = v });
  }

let max_retries =
  {
    GapiLens.get = (fun x -> x.max_retries);
    GapiLens.set = (fun v x -> { x with max_retries = v });
  }

let max_upload_chunk_size =
  {
    GapiLens.get = (fun x -> x.max_upload_chunk_size);
    GapiLens.set = (fun v x -> { x with max_upload_chunk_size = v });
  }

let memory_buffer_size =
  {
    GapiLens.get = (fun x -> x.memory_buffer_size);
    GapiLens.set = (fun v x -> { x with memory_buffer_size = v });
  }

let max_memory_cache_size =
  {
    GapiLens.get = (fun x -> x.max_memory_cache_size);
    GapiLens.set = (fun v x -> { x with max_memory_cache_size = v });
  }

let read_ahead_buffers =
  {
    GapiLens.get = (fun x -> x.read_ahead_buffers);
    GapiLens.set = (fun v x -> { x with read_ahead_buffers = v });
  }

let lost_and_found =
  {
    GapiLens.get = (fun x -> x.lost_and_found);
    GapiLens.set = (fun v x -> { x with lost_and_found = v });
  }

let data_directory =
  {
    GapiLens.get = (fun x -> x.data_directory);
    GapiLens.set = (fun v x -> { x with data_directory = v });
  }

let cache_directory =
  {
    GapiLens.get = (fun x -> x.cache_directory);
    GapiLens.set = (fun v x -> { x with cache_directory = v });
  }

let log_directory =
  {
    GapiLens.get = (fun x -> x.log_directory);
    GapiLens.set = (fun v x -> { x with log_directory = v });
  }

let log_to =
  {
    GapiLens.get = (fun x -> x.log_to);
    GapiLens.set = (fun v x -> { x with log_to = v });
  }

let root_folder =
  {
    GapiLens.get = (fun x -> x.root_folder);
    GapiLens.set = (fun v x -> { x with root_folder = v });
  }

let team_drive_id =
  {
    GapiLens.get = (fun x -> x.team_drive_id);
    GapiLens.set = (fun v x -> { x with team_drive_id = v });
  }

let metadata_memory_cache =
  {
    GapiLens.get = (fun x -> x.metadata_memory_cache);
    GapiLens.set = (fun v x -> { x with metadata_memory_cache = v });
  }

let metadata_memory_cache_saving_interval =
  {
    GapiLens.get = (fun x -> x.metadata_memory_cache_saving_interval);
    GapiLens.set =
      (fun v x -> { x with metadata_memory_cache_saving_interval = v });
  }

let acknowledge_abuse =
  {
    GapiLens.get = (fun x -> x.acknowledge_abuse);
    GapiLens.set = (fun v x -> { x with acknowledge_abuse = v });
  }

let desktop_entry_exec =
  {
    GapiLens.get = (fun x -> x.desktop_entry_exec);
    GapiLens.set = (fun v x -> { x with desktop_entry_exec = v });
  }

let write_buffers =
  {
    GapiLens.get = (fun x -> x.write_buffers);
    GapiLens.set = (fun v x -> { x with write_buffers = v });
  }

let disable_trash =
  {
    GapiLens.get = (fun x -> x.disable_trash);
    GapiLens.set = (fun v x -> { x with disable_trash = v });
  }

let autodetect_mime =
  {
    GapiLens.get = (fun x -> x.autodetect_mime);
    GapiLens.set = (fun v x -> { x with autodetect_mime = v });
  }

let mv_keep_target =
  {
    GapiLens.get = (fun x -> x.mv_keep_target);
    GapiLens.set = (fun v x -> { x with mv_keep_target = v });
  }

let async_upload_queue =
  {
    GapiLens.get = (fun x -> x.async_upload_queue);
    GapiLens.set = (fun v x -> { x with async_upload_queue = v });
  }

let async_upload_threads =
  {
    GapiLens.get = (fun x -> x.async_upload_threads);
    GapiLens.set = (fun v x -> { x with async_upload_threads = v });
  }

let debug_buffers =
  {
    GapiLens.get = (fun x -> x.debug_buffers);
    GapiLens.set = (fun v x -> { x with debug_buffers = v });
  }

let service_account_credentials_path =
  {
    GapiLens.get = (fun x -> x.service_account_credentials_path);
    GapiLens.set = (fun v x -> { x with service_account_credentials_path = v });
  }

let service_account_user_to_impersonate =
  {
    GapiLens.get = (fun x -> x.service_account_user_to_impersonate);
    GapiLens.set =
      (fun v x -> { x with service_account_user_to_impersonate = v });
  }

let scope =
  {
    GapiLens.get = (fun x -> x.scope);
    GapiLens.set = (fun v x -> { x with scope = v });
  }

let redirect_uri =
  {
    GapiLens.get = (fun x -> x.redirect_uri);
    GapiLens.set = (fun v x -> { x with redirect_uri = v });
  }

let desktop_entry_as_html =
  {
    GapiLens.get = (fun x -> x.desktop_entry_as_html);
    GapiLens.set = (fun v x -> { x with desktop_entry_as_html = v });
  }

let async_upload_queue_max_length =
  {
    GapiLens.get = (fun x -> x.async_upload_queue_max_length);
    GapiLens.set = (fun v x -> { x with async_upload_queue_max_length = v });
  }

let background_folder_fetching =
  {
    GapiLens.get = (fun x -> x.background_folder_fetching);
    GapiLens.set = (fun v x -> { x with background_folder_fetching = v });
  }

let umask =
  let prev_umask = Unix.umask 0 in
  let _ = Unix.umask prev_umask in
  prev_umask

let default_max_upload_chunk_size =
  if Sys.word_size == 64 then
    Int64.to_int (Int64.mul (Int64.mul (Int64.mul 1024L 1024L) 1024L) 1024L)
    (* 1TB *)
  else 768 * 1024 * 1024

(* 768MB *)

let default =
  {
    metadata_cache_time = 60;
    read_only = false;
    umask;
    sqlite3_busy_timeout = 5000;
    download_docs = true;
    document_format = "desktop";
    document_icon = "";
    drawing_format = "desktop";
    drawing_icon = "";
    form_format = "desktop";
    form_icon = "";
    presentation_format = "desktop";
    presentation_icon = "";
    spreadsheet_format = "desktop";
    spreadsheet_icon = "";
    map_format = "desktop";
    map_icon = "";
    fusion_table_format = "desktop";
    fusion_table_icon = "";
    apps_script_format = "desktop";
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
    data_directory = "";
    cache_directory = "";
    log_directory = "";
    log_to = "";
    root_folder = "";
    team_drive_id = "";
    metadata_memory_cache = true;
    metadata_memory_cache_saving_interval = 30;
    acknowledge_abuse = false;
    desktop_entry_exec = "";
    write_buffers = false;
    disable_trash = false;
    autodetect_mime = true;
    mv_keep_target = false;
    async_upload_queue = false;
    async_upload_threads = 10;
    debug_buffers = false;
    service_account_credentials_path = "";
    service_account_user_to_impersonate = "";
    scope = "";
    redirect_uri = "";
    desktop_entry_as_html = false;
    async_upload_queue_max_length = 0;
    background_folder_fetching = false;
  }

let default_debug =
  {
    metadata_cache_time = 60;
    read_only = false;
    umask;
    sqlite3_busy_timeout = 5000;
    download_docs = true;
    document_format = "desktop";
    document_icon = "";
    drawing_format = "desktop";
    drawing_icon = "";
    form_format = "desktop";
    form_icon = "";
    presentation_format = "desktop";
    presentation_icon = "";
    spreadsheet_format = "desktop";
    spreadsheet_icon = "";
    map_format = "desktop";
    map_icon = "";
    fusion_table_format = "desktop";
    fusion_table_icon = "";
    apps_script_format = "desktop";
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
    data_directory = "";
    cache_directory = "";
    log_directory = "";
    log_to = "";
    root_folder = "";
    team_drive_id = "";
    metadata_memory_cache = true;
    metadata_memory_cache_saving_interval = 30;
    acknowledge_abuse = false;
    desktop_entry_exec = "";
    write_buffers = false;
    disable_trash = false;
    autodetect_mime = true;
    mv_keep_target = false;
    async_upload_queue = false;
    async_upload_threads = 10;
    debug_buffers = false;
    service_account_credentials_path = "";
    service_account_user_to_impersonate = "";
    scope = "";
    redirect_uri = "";
    desktop_entry_as_html = false;
    async_upload_queue_max_length = 0;
    background_folder_fetching = false;
  }

let of_table table =
  let get k = Utils.get_from_string_table table k in
  {
    metadata_cache_time =
      get "metadata_cache_time" int_of_string default.metadata_cache_time;
    read_only = get "read_only" bool_of_string default.read_only;
    umask = get "umask" int_of_string default.umask;
    sqlite3_busy_timeout =
      get "sqlite3_busy_timeout" int_of_string default.sqlite3_busy_timeout;
    download_docs = get "download_docs" bool_of_string default.download_docs;
    document_format = get "document_format" Std.identity default.document_format;
    document_icon = get "document_icon" Std.identity default.document_icon;
    drawing_format = get "drawing_format" Std.identity default.drawing_format;
    drawing_icon = get "drawing_icon" Std.identity default.drawing_icon;
    form_format = get "form_format" Std.identity default.form_format;
    form_icon = get "form_icon" Std.identity default.form_icon;
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
    curl_debug_off = get "curl_debug_off" bool_of_string default.curl_debug_off;
    delete_forever_in_trash_folder =
      get "delete_forever_in_trash_folder" bool_of_string
        default.delete_forever_in_trash_folder;
    stream_large_files =
      get "stream_large_files" bool_of_string default.stream_large_files;
    large_file_threshold_mb =
      get "large_file_threshold_mb" int_of_string
        default.large_file_threshold_mb;
    large_file_read_only =
      get "large_file_read_only" bool_of_string default.large_file_read_only;
    connect_timeout_ms =
      get "connect_timeout_ms" int_of_string default.connect_timeout_ms;
    max_download_speed =
      get "max_download_speed" Int64.of_string default.max_download_speed;
    max_upload_speed =
      get "max_upload_speed" Int64.of_string default.max_upload_speed;
    low_speed_limit =
      get "low_speed_limit" int_of_string default.low_speed_limit;
    low_speed_time = get "low_speed_time" int_of_string default.low_speed_time;
    max_retries = get "max_retries" int_of_string default.max_retries;
    max_upload_chunk_size =
      get "max_upload_chunk_size" int_of_string default.max_upload_chunk_size;
    memory_buffer_size =
      get "memory_buffer_size" int_of_string default.memory_buffer_size;
    max_memory_cache_size =
      get "max_memory_cache_size" int_of_string default.max_memory_cache_size;
    read_ahead_buffers =
      get "read_ahead_buffers" int_of_string default.read_ahead_buffers;
    lost_and_found = get "lost_and_found" bool_of_string default.lost_and_found;
    data_directory = get "data_directory" Std.identity default.data_directory;
    cache_directory = get "cache_directory" Std.identity default.cache_directory;
    log_directory = get "log_directory" Std.identity default.log_directory;
    log_to = get "log_to" Std.identity default.log_to;
    root_folder = get "root_folder" Std.identity default.root_folder;
    team_drive_id = get "team_drive_id" Std.identity default.team_drive_id;
    metadata_memory_cache =
      get "metadata_memory_cache" bool_of_string default.metadata_memory_cache;
    metadata_memory_cache_saving_interval =
      get "metadata_memory_cache_saving_interval" int_of_string
        default.metadata_memory_cache_saving_interval;
    acknowledge_abuse =
      get "acknowledge_abuse" bool_of_string default.acknowledge_abuse;
    desktop_entry_exec =
      get "desktop_entry_exec" Std.identity default.desktop_entry_exec;
    write_buffers = get "write_buffers" bool_of_string default.write_buffers;
    disable_trash = get "disable_trash" bool_of_string default.disable_trash;
    autodetect_mime =
      get "autodetect_mime" bool_of_string default.autodetect_mime;
    mv_keep_target = get "mv_keep_target" bool_of_string default.mv_keep_target;
    async_upload_queue =
      get "async_upload_queue" bool_of_string default.async_upload_queue;
    async_upload_threads =
      get "async_upload_threads" int_of_string default.async_upload_threads;
    debug_buffers = get "debug_buffers" bool_of_string default.debug_buffers;
    service_account_credentials_path =
      get "service_account_credentials_path" Std.identity
        default.service_account_credentials_path;
    service_account_user_to_impersonate =
      get "service_account_user_to_impersonate" Std.identity
        default.service_account_user_to_impersonate;
    scope = get "scope" Std.identity default.scope;
    redirect_uri = get "redirect_uri" Std.identity default.redirect_uri;
    desktop_entry_as_html =
      get "desktop_entry_as_html" bool_of_string default.desktop_entry_as_html;
    async_upload_queue_max_length =
      get "async_upload_queue_max_length" int_of_string
        default.async_upload_queue_max_length;
    background_folder_fetching =
      get "background_folder_fetching" bool_of_string
        default.background_folder_fetching;
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
  add "large_file_threshold_mb" (data.large_file_threshold_mb |> string_of_int);
  add "large_file_read_only" (data.large_file_read_only |> string_of_bool);
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
  add "data_directory" data.data_directory;
  add "cache_directory" data.cache_directory;
  add "log_directory" data.log_directory;
  add "log_to" data.log_to;
  add "root_folder" data.root_folder;
  add "team_drive_id" data.team_drive_id;
  add "metadata_memory_cache" (data.metadata_memory_cache |> string_of_bool);
  add "metadata_memory_cache_saving_interval"
    (data.metadata_memory_cache_saving_interval |> string_of_int);
  add "acknowledge_abuse" (data.acknowledge_abuse |> string_of_bool);
  add "desktop_entry_exec" data.desktop_entry_exec;
  add "write_buffers" (data.write_buffers |> string_of_bool);
  add "disable_trash" (data.disable_trash |> string_of_bool);
  add "autodetect_mime" (data.autodetect_mime |> string_of_bool);
  add "mv_keep_target" (data.mv_keep_target |> string_of_bool);
  add "async_upload_queue" (data.async_upload_queue |> string_of_bool);
  add "async_upload_threads" (data.async_upload_threads |> string_of_int);
  add "debug_buffers" (data.debug_buffers |> string_of_bool);
  add "service_account_credentials_path" data.service_account_credentials_path;
  add "service_account_user_to_impersonate"
    data.service_account_user_to_impersonate;
  add "scope" data.scope;
  add "redirect_uri" data.redirect_uri;
  add "desktop_entry_as_html" (data.desktop_entry_as_html |> string_of_bool);
  add "async_upload_queue_max_length"
    (data.async_upload_queue_max_length |> string_of_int);
  add "background_folder_fetching"
    (data.background_folder_fetching |> string_of_bool);
  table

let debug_print out_ch start_time curl info_type info =
  let time = Unix.gettimeofday () in
  let timestamp = time -. start_time in
  let nl =
    if String.length info > 0 && info.[String.length info - 1] = '\n' then ""
    else "\n"
  in
  Printf.fprintf out_ch "[%f] curl: %s: %s%s%!" timestamp
    (GapiCurl.string_of_curl_info_type info_type)
    info nl

let create_gapi_config config debug curl_log_path log_to =
  let gapi_config =
    if debug && not config.curl_debug_off then
      let out_ch = Utils.open_log_out_ch log_to curl_log_path in
      let debug_function = debug_print out_ch (Unix.gettimeofday ()) in
      GapiConfig.default_debug
      |> GapiConfig.debug ^= Some (GapiConfig.Custom debug_function)
    else GapiConfig.default
  in
  let gapi_auth =
    if config.service_account_credentials_path = "" then
      GapiConfig.OAuth2
        {
          GapiConfig.client_id = config.client_id;
          client_secret = config.client_secret;
          refresh_access_token = None;
        }
    else
      let service_account_credentials_json =
        Utils.with_in_channel config.service_account_credentials_path (fun ch ->
            let b = Buffer.create 512 in
            ( try
                while true do
                  Buffer.add_string b (input_line ch)
                done
              with End_of_file -> () );
            Buffer.contents b)
      in
      let user_to_impersonate =
        match config.service_account_user_to_impersonate with
        | "" -> None
        | u -> Some u
      in
      let scopes =
        match config.scope with
        | "" -> [ GapiDriveV2Service.Scope.drive ]
        | s -> [ s ]
      in
      GapiConfig.OAuth2ServiceAccount
        {
          GapiConfig.service_account_credentials_json;
          scopes;
          user_to_impersonate;
          refresh_service_account_access_token = None;
        }
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
  |> GapiConfig.auth ^= gapi_auth
