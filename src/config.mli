val application_name : string
val version : string

type t = {
  metadata_cache_time : int;
  read_only : bool;
  umask : int;
  sqlite3_busy_timeout : int;
  download_docs : bool;
  document_format : string;
  document_icon : string;
  drawing_format : string;
  drawing_icon : string;
  form_format : string;
  form_icon : string;
  presentation_format : string;
  presentation_icon : string;
  spreadsheet_format : string;
  spreadsheet_icon : string;
  map_format : string;
  map_icon : string;
  fusion_table_format : string;
  fusion_table_icon : string;
  apps_script_format : string;
  apps_script_icon : string;
  client_id : string;
  client_secret : string;
  verification_code : string;
  keep_duplicates : bool;
  docs_file_extension : bool;
  max_cache_size_mb : int;
  curl_debug_off : bool;
  delete_forever_in_trash_folder : bool;
  stream_large_files : bool;
  large_file_threshold_mb : int;
  large_file_read_only : bool;
  connect_timeout_ms : int;
  max_download_speed : int64;
  max_upload_speed : int64;
  low_speed_limit : int;
  low_speed_time : int;
  max_retries : int;
  max_upload_chunk_size : int;
  memory_buffer_size : int;
  max_memory_cache_size : int;
  read_ahead_buffers : int;
  lost_and_found : bool;
  data_directory : string;
  cache_directory : string;
  log_directory : string;
  log_to : string;
  root_folder : string;
  team_drive_id : string;
  metadata_memory_cache : bool;
  metadata_memory_cache_saving_interval : int;
  acknowledge_abuse : bool;
  desktop_entry_exec : string;
  write_buffers : bool;
  disable_trash : bool;
  autodetect_mime : bool;
  mv_keep_target : bool;
  async_upload_queue : bool;
  async_upload_threads : int;
  debug_buffers : bool;
  service_account_credentials_path : string;
  service_account_user_to_impersonate : string;
  scope : string;
  redirect_uri : string;
  desktop_entry_as_html : bool;
  async_upload_queue_max_length : int;
  background_folder_fetching : bool;
  oauth2_loopback : bool;
  oauth2_loopback_port : int;
}

val metadata_cache_time : (t, int) GapiLens.t
val read_only : (t, bool) GapiLens.t
val sqlite3_busy_timeout : (t, int) GapiLens.t
val download_docs : (t, bool) GapiLens.t
val document_format : (t, string) GapiLens.t
val document_icon : (t, string) GapiLens.t
val drawing_format : (t, string) GapiLens.t
val drawing_icon : (t, string) GapiLens.t
val form_format : (t, string) GapiLens.t
val form_icon : (t, string) GapiLens.t
val presentation_format : (t, string) GapiLens.t
val presentation_icon : (t, string) GapiLens.t
val spreadsheet_format : (t, string) GapiLens.t
val spreadsheet_icon : (t, string) GapiLens.t
val map_format : (t, string) GapiLens.t
val map_icon : (t, string) GapiLens.t
val fusion_table_format : (t, string) GapiLens.t
val fusion_table_icon : (t, string) GapiLens.t
val apps_script_format : (t, string) GapiLens.t
val apps_script_icon : (t, string) GapiLens.t
val client_id : (t, string) GapiLens.t
val client_secret : (t, string) GapiLens.t
val verification_code : (t, string) GapiLens.t
val keep_duplicates : (t, bool) GapiLens.t
val docs_file_extension : (t, bool) GapiLens.t
val max_cache_size_mb : (t, int) GapiLens.t
val curl_debug_off : (t, bool) GapiLens.t
val delete_forever_in_trash_folder : (t, bool) GapiLens.t
val stream_large_files : (t, bool) GapiLens.t
val large_file_threshold_mb : (t, int) GapiLens.t
val large_file_read_only : (t, bool) GapiLens.t
val connect_timeout_ms : (t, int) GapiLens.t
val max_download_speed : (t, int64) GapiLens.t
val max_upload_speed : (t, int64) GapiLens.t
val low_speed_limit : (t, int) GapiLens.t
val low_speed_time : (t, int) GapiLens.t
val max_retries : (t, int) GapiLens.t
val max_upload_chunk_size : (t, int) GapiLens.t
val memory_buffer_size : (t, int) GapiLens.t
val max_memory_cache_size : (t, int) GapiLens.t
val read_ahead_buffers : (t, int) GapiLens.t
val lost_and_found : (t, bool) GapiLens.t
val data_directory : (t, string) GapiLens.t
val cache_directory : (t, string) GapiLens.t
val log_directory : (t, string) GapiLens.t
val log_to : (t, string) GapiLens.t
val root_folder : (t, string) GapiLens.t
val team_drive_id : (t, string) GapiLens.t
val metadata_memory_cache : (t, bool) GapiLens.t
val metadata_memory_cache_saving_interval : (t, int) GapiLens.t
val acknowledge_abuse : (t, bool) GapiLens.t
val desktop_entry_exec : (t, string) GapiLens.t
val write_buffers : (t, bool) GapiLens.t
val disable_trash : (t, bool) GapiLens.t
val autodetect_mime : (t, bool) GapiLens.t
val mv_keep_target : (t, bool) GapiLens.t
val async_upload_queue : (t, bool) GapiLens.t
val async_upload_threads : (t, int) GapiLens.t
val debug_buffers : (t, bool) GapiLens.t
val service_account_credentials_path : (t, string) GapiLens.t
val service_account_user_to_impersonate : (t, string) GapiLens.t
val scope : (t, string) GapiLens.t
val redirect_uri : (t, string) GapiLens.t
val desktop_entry_as_html : (t, bool) GapiLens.t
val async_upload_queue_max_length : (t, int) GapiLens.t
val background_folder_fetching : (t, bool) GapiLens.t
val oauth2_loopback : (t, bool) GapiLens.t
val oauth2_loopback_port : (t, int) GapiLens.t
val umask : Unix.file_perm
val default_max_upload_chunk_size : int
val default : t
val default_debug : t
val of_table : (string, string) Hashtbl.t -> t
val to_table : t -> (string, string) Hashtbl.t
val validate : t -> t

val debug_print :
  out_channel -> float -> 'a -> Curl.curlDebugType -> string -> unit

val create_gapi_config : t -> bool -> string -> string -> GapiConfig.t
