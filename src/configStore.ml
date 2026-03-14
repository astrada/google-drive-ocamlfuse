type t = { path : string; data : Config.t }
type load_result = { store : t; created : bool; migrated : bool }

exception File_not_found
exception Parse_error of string

let config_version = 1
let legacy_backup_suffix = ".legacy.bak"

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
  List.iter
    (fun (key, value) ->
      if key <> "config_version" then
        if Hashtbl.mem table key then
          raise (parse_error path (Printf.sprintf "duplicate key '%s'" key))
        else Hashtbl.add table key (string_of_toml_value path key value))
    entries;
  try Config.of_table table
  with Failure message -> raise (parse_error path message)

let load path =
  if not (Sys.file_exists path) then raise File_not_found;
  let config =
    match classify_file path with
    | `Legacy -> load_legacy path
    | `Toml -> load_toml path
  in
  { path; data = config }

let build_minimal_toml config =
  let current = Config.to_table config in
  let defaults = Config.to_table Config.default in
  let entries = ref [ ("config_version", Otoml.TomlInteger config_version) ] in
  let add_if_needed key value =
    let default_value = Utils.safe_find defaults key in
    if default_value <> Some value then
      let toml_value =
        if is_member key bool_keys then Otoml.TomlBoolean (bool_of_string value)
        else if is_member key int_keys then
          Otoml.TomlInteger (int_of_string value)
        else if is_member key stringified_numeric_keys then
          Otoml.TomlString value
        else Otoml.TomlString value
      in
      entries := (key, toml_value) :: !entries
  in
  Hashtbl.iter add_if_needed current;
  Otoml.TomlTable (List.rev !entries)

let save store =
  let dir = Filename.dirname store.path in
  let tmp_path = store.path ^ ".tmp" in
  Utils.safe_makedir dir;
  Utils.with_out_channel ~mode:[ Open_creat; Open_trunc; Open_wronly ] tmp_path
    (fun ch -> Otoml.Printer.to_channel ch (build_minimal_toml store.data));
  Sys.rename tmp_path store.path

let backup_legacy_file path =
  let backup_path = path ^ legacy_backup_suffix in
  if Sys.file_exists backup_path then Sys.remove backup_path;
  Sys.rename path backup_path

let create_default ~debug ~path =
  let data = if debug then Config.default_debug else Config.default in
  let store = { path; data } in
  save store;
  store

let load_or_create ~debug path =
  if not (Sys.file_exists path) then
    let store = create_default ~debug ~path in
    { store; created = true; migrated = false }
  else
    match classify_file path with
    | `Toml -> { store = load path; created = false; migrated = false }
    | `Legacy ->
        let store = { path; data = load_legacy path } in
        backup_legacy_file path;
        save store;
        { store; created = false; migrated = true }
