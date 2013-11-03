open GapiUtils.Infix
open GapiLens.Infix

let application_name = "google-drive-ocamlfuse"
let version = "0.5.1"

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
  (* Drawings export format *)
  drawing_format : string;
  (* Forms export format *)
  form_format : string;
  (* Presentations export format *)
  presentation_format : string;
  (* Spreadsheets export format *)
  spreadsheet_format : string;
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
let drawing_format = {
  GapiLens.get = (fun x -> x.drawing_format);
  GapiLens.set = (fun v x -> { x with drawing_format = v })
}
let form_format = {
  GapiLens.get = (fun x -> x.form_format);
  GapiLens.set = (fun v x -> { x with form_format = v })
}
let presentation_format = {
  GapiLens.get = (fun x -> x.presentation_format);
  GapiLens.set = (fun v x -> { x with presentation_format = v })
}
let spreadsheet_format = {
  GapiLens.get = (fun x -> x.spreadsheet_format);
  GapiLens.set = (fun v x -> { x with spreadsheet_format = v })
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

let umask =
  let prev_umask = Unix.umask 0 in
  let _ = Unix.umask prev_umask in
    prev_umask

let default = {
  debug = false;
  metadata_cache_time = 60;
  read_only = false;
  umask;
  sqlite3_busy_timeout = 500;
  download_docs = true;
  document_format = "odt";
  drawing_format = "png";
  form_format = "ods";
  presentation_format = "pdf";
  spreadsheet_format = "ods";
  client_id = "";
  client_secret = "";
  verification_code = "";
  conflict_resolution = ConflictResolutionStrategy.Server;
  keep_duplicates = false;
  docs_file_extension = true;
  max_cache_size_mb = 512;
}

let default_debug = {
  debug = true;
  metadata_cache_time = 60;
  read_only = false;
  umask;
  sqlite3_busy_timeout = 500;
  download_docs = true;
  document_format = "odt";
  drawing_format = "png";
  form_format = "ods";
  presentation_format = "pdf";
  spreadsheet_format = "ods";
  client_id = "";
  client_secret = "";
  verification_code = "";
  conflict_resolution = ConflictResolutionStrategy.Server;
  keep_duplicates = false;
  docs_file_extension = true;
  max_cache_size_mb = 512;
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
      drawing_format =
        get "drawing_format" Std.identity default.drawing_format;
      form_format =
        get "form_format" Std.identity default.form_format;
      presentation_format =
        get "presentation_format" Std.identity default.presentation_format;
      spreadsheet_format =
        get "spreadsheet_format" Std.identity default.spreadsheet_format;
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
    add "drawing_format" data.drawing_format;
    add "form_format" data.form_format;
    add "presentation_format" data.presentation_format;
    add "spreadsheet_format" data.spreadsheet_format;
    add "client_id" data.client_id;
    add "client_secret" data.client_secret;
    add "verification_code" data.verification_code;
    add "conflict_resolution"
      (data.conflict_resolution |> ConflictResolutionStrategy.to_string);
    add "keep_duplicates" (data.keep_duplicates |> string_of_bool);
    add "docs_file_extension" (data.docs_file_extension |> string_of_bool);
    add "max_cache_size_mb" (data.max_cache_size_mb |> string_of_int);
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
    if config.debug then
      let out_ch = open_out (app_dir |. AppDir.curl_log_path) in
      let debug_function = debug_print out_ch (Unix.gettimeofday ()) in
        GapiConfig.default_debug
        |> GapiConfig.debug ^= Some (GapiConfig.Custom debug_function)
    else
      GapiConfig.default
  in
    gapi_config
    |> GapiConfig.application_name ^= application_name ^ " (" ^ version ^ ")"
    (* If client_id and client_secret are not set, the authorization will
     * be handled by the GAE proxy *)
    |> GapiConfig.auth ^= GapiConfig.OAuth2
                            { GapiConfig.client_id = config.client_id;
                              client_secret = config.client_secret }

