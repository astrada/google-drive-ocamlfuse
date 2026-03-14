type t = { path : string; data : Config.t }
type load_result = {
  store : t;
  created : bool;
  migrated : bool;
  upgraded : bool;
}

exception File_not_found
exception Parse_error of string

let config_version = 1
let backup_suffix = ".bak"

let auth_table =
  [
    "client_id";
    "client_secret";
    "verification_code";
    "scope";
    "redirect_uri";
    "oauth2_loopback";
    "oauth2_loopback_port";
    "service_account_credentials_path";
    "service_account_user_to_impersonate";
  ]

let mount_table =
  [
    "metadata_cache_time";
    "read_only";
    "umask";
    "root_folder";
    "team_drive_id";
    "lost_and_found";
    "disable_trash";
    "keep_duplicates";
    "mv_keep_target";
  ]

let docs_table =
  [
    "download_docs";
    "docs_file_extension";
    "document_format";
    "drawing_format";
    "form_format";
    "presentation_format";
    "spreadsheet_format";
    "map_format";
    "fusion_table_format";
    "apps_script_format";
    "document_icon";
    "drawing_icon";
    "form_icon";
    "presentation_icon";
    "spreadsheet_icon";
    "map_icon";
    "fusion_table_icon";
    "apps_script_icon";
    "desktop_entry_exec";
    "desktop_entry_as_html";
  ]

let cache_table =
  [
    "max_cache_size_mb";
    "metadata_memory_cache";
    "metadata_memory_cache_saving_interval";
    "sqlite3_busy_timeout";
    "data_directory";
    "cache_directory";
    "log_directory";
  ]

let io_table =
  [
    "stream_large_files";
    "large_file_threshold_mb";
    "large_file_read_only";
    "memory_buffer_size";
    "max_memory_cache_size";
    "read_ahead_buffers";
    "write_buffers";
    "max_upload_chunk_size";
    "autodetect_mime";
    "acknowledge_abuse";
  ]

let network_table =
  [
    "connect_timeout_ms";
    "max_download_speed";
    "max_upload_speed";
    "low_speed_limit";
    "low_speed_time";
    "max_retries";
    "curl_debug_off";
  ]

let async_table =
  [
    "async_upload_queue";
    "async_upload_threads";
    "async_upload_queue_max_length";
    "background_folder_fetching";
  ]

let logging_table = [ "log_to"; "debug_buffers" ]

let grouped_tables =
  [
    ("auth", auth_table);
    ("mount", mount_table);
    ("docs", docs_table);
    ("cache", cache_table);
    ("io", io_table);
    ("network", network_table);
    ("async", async_table);
    ("logging", logging_table);
  ]

let path =
  {
    GapiLens.get = (fun x -> x.path);
    GapiLens.set = (fun v x -> { x with path = v });
  }

let data =
  {
    GapiLens.get = (fun x -> x.data);
    GapiLens.set = (fun v x -> { x with data = v });
  }

let bool_keys =
  [
    "read_only";
    "download_docs";
    "keep_duplicates";
    "docs_file_extension";
    "curl_debug_off";
    "delete_forever_in_trash_folder";
    "stream_large_files";
    "large_file_read_only";
    "lost_and_found";
    "metadata_memory_cache";
    "acknowledge_abuse";
    "write_buffers";
    "disable_trash";
    "autodetect_mime";
    "mv_keep_target";
    "async_upload_queue";
    "debug_buffers";
    "desktop_entry_as_html";
    "background_folder_fetching";
    "oauth2_loopback";
  ]

let int_keys =
  [
    "metadata_cache_time";
    "sqlite3_busy_timeout";
    "max_cache_size_mb";
    "large_file_threshold_mb";
    "connect_timeout_ms";
    "low_speed_limit";
    "low_speed_time";
    "max_retries";
    "max_upload_chunk_size";
    "memory_buffer_size";
    "max_memory_cache_size";
    "read_ahead_buffers";
    "metadata_memory_cache_saving_interval";
    "async_upload_threads";
    "async_upload_queue_max_length";
    "oauth2_loopback_port";
  ]

let stringified_numeric_keys =
  [ "umask"; "max_download_speed"; "max_upload_speed" ]

let is_member = List.mem

let is_legacy_line line =
  let line = String.trim line in
  let regexp =
    Str.regexp "^[A-Za-z0-9_]+=[A-Za-z0-9_./:@+-]*\\(0o[0-7]+\\)?$"
  in
  line <> "" && Str.string_match regexp line 0

let classify_file path =
  let has_config_version = ref false in
  let legacy_candidate = ref true in
  Utils.with_in_channel path (fun ch ->
      try
        while true do
          let raw_line = input_line ch in
          let line = String.trim raw_line in
          if line <> "" then (
            if ExtString.String.starts_with line "config_version" then
              has_config_version := true;
            if ExtString.String.starts_with line "#" then
              legacy_candidate := false
            else if not (is_legacy_line line) then legacy_candidate := false)
        done
      with End_of_file -> ());
  if !has_config_version then `Toml
  else if !legacy_candidate then `Legacy
  else `Toml

let parse_error path message =
  Parse_error (Printf.sprintf "Cannot parse configuration %s: %s" path message)

let parse_config_version path entries =
  let rec find_version = function
    | [] -> None
    | ("config_version", Otoml.TomlInteger version) :: _ -> Some version
    | ("config_version", _) :: _ ->
        raise (parse_error path "config_version must be an integer")
    | _ :: rest -> find_version rest
  in
  match find_version entries with
  | None -> 0
  | Some version when version < 0 ->
      raise (parse_error path "config_version must be >= 0")
  | Some version -> version

let rec upgrade_config path loaded_version config =
  if loaded_version > config_version then
    raise
      (parse_error path
         (Printf.sprintf "unsupported config_version %d" loaded_version));
  match loaded_version with
  | version when version = config_version -> (config, false)
  | 0 ->
      let upgraded_config = config in
      let _, rewritten = upgrade_config path 1 upgraded_config in
      (upgraded_config, true || rewritten)
  | version ->
      raise
        (parse_error path
           (Printf.sprintf "missing upgrade path from config_version %d" version))

let load_legacy path =
  let table = Hashtbl.create 16 in
  let add_entry line_number key value =
    if Hashtbl.mem table key then
      raise
        (parse_error path
           (Printf.sprintf "duplicate key '%s' at line %d" key line_number));
    Hashtbl.add table key value
  in
  Utils.with_in_channel path (fun ch ->
      let line_number = ref 0 in
      try
        while true do
          incr line_number;
          let raw_line = input_line ch in
          let line = String.trim raw_line in
          if line <> "" then
            try
              let eq_pos = String.index line '=' in
              let key = String.sub line 0 eq_pos |> String.trim in
              let value =
                String.sub line (eq_pos + 1) (String.length line - eq_pos - 1)
                |> String.trim
              in
              if key = "" then
                raise
                  (parse_error path
                     (Printf.sprintf "empty key at line %d" !line_number));
              add_entry !line_number key value
            with Not_found ->
              raise
                (parse_error path
                   (Printf.sprintf "invalid line %d: %s" !line_number line))
        done
      with End_of_file -> ());
  try Config.of_table table
  with Failure message -> raise (parse_error path message)

let string_of_toml_value path key = function
  | Otoml.TomlBoolean b -> string_of_bool b
  | Otoml.TomlInteger i -> string_of_int i
  | Otoml.TomlFloat f -> string_of_float f
  | Otoml.TomlString s -> s
  | _ ->
      raise
        (parse_error path
           (Printf.sprintf "unsupported TOML value for key '%s'" key))

let add_toml_entry path table key value =
  if Hashtbl.mem table key then
    raise (parse_error path (Printf.sprintf "duplicate key '%s'" key))
  else Hashtbl.add table key (string_of_toml_value path key value)

let rec add_toml_entries path table = function
  | [] -> ()
  | (key, value) :: rest -> (
      match value with
      | Otoml.TomlTable entries ->
          List.iter
            (fun (nested_key, nested_value) ->
              add_toml_entry path table nested_key nested_value)
            entries
      | _ when key = "config_version" -> ()
      | _ -> add_toml_entry path table key value);
      add_toml_entries path table rest

let load_toml path =
  let toml =
    try Utils.with_in_channel path (fun ch -> Otoml.Parser.from_channel ch)
    with e -> raise (parse_error path (Printexc.to_string e))
  in
  let table = Hashtbl.create 16 in
  let entries =
    match toml with
    | Otoml.TomlTable entries -> entries
    | _ -> raise (parse_error path "top-level TOML value must be a table")
  in
  let loaded_version = parse_config_version path entries in
  add_toml_entries path table entries;
  try
    let config = Config.of_table table in
    let upgraded_config, upgraded = upgrade_config path loaded_version config in
    (upgraded_config, upgraded)
  with Failure message -> raise (parse_error path message)

let load path =
  if not (Sys.file_exists path) then raise File_not_found;
  let config =
    match classify_file path with
    | `Legacy -> load_legacy path
    | `Toml -> load_toml path |> fst
  in
  { path; data = config }

let toml_value_of_key key value =
  if is_member key bool_keys then Otoml.TomlBoolean (bool_of_string value)
  else if is_member key int_keys then Otoml.TomlInteger (int_of_string value)
  else if is_member key stringified_numeric_keys then Otoml.TomlString value
  else Otoml.TomlString value

let build_minimal_toml config =
  let current = Config.to_table config in
  let defaults = Config.to_table Config.default in
  let changed_keys = Hashtbl.create 16 in
  let add_if_needed key value =
    if Utils.safe_find defaults key <> Some value then Hashtbl.add changed_keys key value
  in
  Hashtbl.iter add_if_needed current;
  let build_group (table_name, keys) =
    let entries =
      List.fold_left
        (fun acc key ->
          match Utils.safe_find changed_keys key with
          | None -> acc
          | Some value -> (key, toml_value_of_key key value) :: acc)
        [] keys
      |> List.rev
    in
    match entries with [] -> None | _ -> Some (table_name, Otoml.TomlTable entries)
  in
  let group_entries = List.filter_map build_group grouped_tables in
  Otoml.TomlTable
    (("config_version", Otoml.TomlInteger config_version) :: group_entries)

let save store =
  let dir = Filename.dirname store.path in
  let tmp_path = store.path ^ ".tmp" in
  let backup_path = store.path ^ backup_suffix in
  Utils.safe_makedir dir;
  Utils.with_out_channel ~mode:[ Open_creat; Open_trunc; Open_wronly ] tmp_path
    (fun ch -> Otoml.Printer.to_channel ch (build_minimal_toml store.data));
  if Sys.file_exists store.path then (
    if Sys.file_exists backup_path then Sys.remove backup_path;
    Sys.rename store.path backup_path);
  Sys.rename tmp_path store.path

let create_default ~debug ~path =
  let data = if debug then Config.default_debug else Config.default in
  let store = { path; data } in
  save store;
  store

let load_or_create ~debug path =
  if not (Sys.file_exists path) then
    let store = create_default ~debug ~path in
    { store; created = true; migrated = false; upgraded = false }
  else
    match classify_file path with
    | `Toml ->
        let data, upgraded = load_toml path in
        let store = { path; data } in
        if upgraded then save store;
        { store; created = false; migrated = false; upgraded }
    | `Legacy ->
        let store = { path; data = load_legacy path } in
        save store;
        { store; created = false; migrated = true; upgraded = false }
