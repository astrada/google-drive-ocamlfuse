open GapiUtils.Infix
open GapiLens.Infix

(* Helpers *)
let fail rc =
  failwith ("Sqlite3 error: " ^ (Sqlite3.Rc.to_string rc))

let expect exptected rc =
  if rc <> exptected then fail rc

let fail_if_not_ok = expect Sqlite3.Rc.OK

let get_result rc result =
  fail_if_not_ok rc;
  !result

let wrap_exec_not_null_no_headers
      db ?(callback = (fun _ -> Some ())) sql =
  let result = ref None in
  let cb row = result := callback row in
  let rc = Sqlite3.exec_not_null_no_headers db ~cb sql in
  get_result rc result

let wrap_exec
      db ?(callback = (fun _ _ -> Some ())) sql =
  let result = ref None in
  let cb row headers = result := callback row headers in
  let rc = Sqlite3.exec db ~cb sql in
  get_result rc result

let reset_stmt stmt =
  Sqlite3.reset stmt |> fail_if_not_ok

let finalize_stmt stmt =
  Sqlite3.finalize stmt |> fail_if_not_ok

let final_step stmt =
  Utils.with_retry (fun () ->
    Sqlite3.step stmt |> expect Sqlite3.Rc.DONE)
    "final_step"
(* END Helpers *)

(* Query helpers *)
let bind to_data stmt name value =
  let sql_value = match value with
      None -> Sqlite3.Data.NULL
    | Some v -> to_data v in
  Sqlite3.bind stmt
    (Sqlite3.bind_parameter_index stmt name)
    sql_value
  |> fail_if_not_ok

let bind_text = bind (fun v -> Sqlite3.Data.TEXT v)
let bind_int = bind (fun v -> Sqlite3.Data.INT v)
let bind_float = bind (fun v -> Sqlite3.Data.FLOAT v)
let bind_bool = bind (fun v -> Sqlite3.Data.INT (if v then 1L else 0L))

let data_to_int64 = function
    Sqlite3.Data.NULL -> None
  | Sqlite3.Data.INT v -> Some v
  | _ -> failwith "data_to_int64: data does not contain an INT value"

let data_to_bool = function
    Sqlite3.Data.NULL -> None
  | Sqlite3.Data.INT v -> Some (if v > 0L then true else false)
  | _ -> failwith "data_to_bool: data does not contain an INT value"

let data_to_string = function
    Sqlite3.Data.NULL -> None
  | Sqlite3.Data.TEXT v -> Some v
  | _ -> failwith "data_to_string: data does not contain a TEXT value"

let data_to_float = function
    Sqlite3.Data.NULL -> None
  | Sqlite3.Data.FLOAT v -> Some v
  | _ -> failwith "data_to_float: data does not contain a FLOAT value"

let get_next_row stmt row_to_data =
  let rc = Sqlite3.step stmt in
  match rc with
      Sqlite3.Rc.ROW ->
        Some (Sqlite3.row_data stmt |> row_to_data)
    | Sqlite3.Rc.DONE -> None
    | _ -> fail rc

let select_first_row stmt bind_parameters row_to_data =
  bind_parameters stmt;
  get_next_row stmt row_to_data

let select_all_rows stmt bind_parameters row_to_data =
  bind_parameters stmt;
  let rec loop rows =
    let row = get_next_row stmt row_to_data in
      match row with
          None -> rows
        | Some r -> loop (r :: rows)
  in
  loop []
(* END Query helpers *)

(* Prepare SQL *)
let prepare_begin_tran_stmt db =
  Sqlite3.prepare db "BEGIN IMMEDIATE TRANSACTION;"

let prepare_commit_tran_stmt db =
  Sqlite3.prepare db "COMMIT TRANSACTION;"

let prepare_rollback_tran_stmt db =
  Sqlite3.prepare db "ROLLBACK TRANSACTION;"

module ResourceStmts =
struct
  let api_fields_without_id =
    "remote_id, \
     name, \
     mime_type, \
     created_time, \
     modified_time, \
     viewed_by_me_time, \
     file_extension, \
     md5_checksum, \
     size, \
     can_edit, \
     trashed, \
     web_view_link, \
     version, "
  let app_properties_fields =
    "file_mode_bits, \
     parent_path_hash, \
     local_name, \
     uid, \
     gid, \
     link_target, \
     xattrs, "
  let app_fields =
    "parent_path, \
     path, \
     state, \
     last_update"

  let fields_without_id = api_fields_without_id ^
                          app_properties_fields ^
                          app_fields
  let fields = "id, " ^ fields_without_id

  let prepare_insert_stmt db =
    let sql =
      "INSERT INTO resource (" ^ fields_without_id ^ ") \
       VALUES ( \
         :remote_id, \
         :name, \
         :mime_type, \
         :created_time, \
         :modified_time, \
         :viewed_by_me_time, \
         :file_extension, \
         :md5_checksum, \
         :size, \
         :can_edit, \
         :trashed, \
         :web_view_link, \
         :version, \
         :file_mode_bits, \
         :parent_path_hash, \
         :local_name, \
         :uid, \
         :gid, \
         :link_target, \
         :xattrs, \
         :parent_path, \
         :path, \
         :state, \
         :last_update \
       );"
    in
      Sqlite3.prepare db sql

  let prepare_update_stmt db =
    let sql =
      "UPDATE resource \
       SET \
         remote_id = :remote_id, \
         name = :name, \
         mime_type = :mime_type, \
         created_time = :created_time, \
         modified_time = :modified_time, \
         viewed_by_me_time = :viewed_by_me_time, \
         file_extension = :file_extension, \
         md5_checksum = :md5_checksum, \
         size = :size, \
         can_edit = :can_edit, \
         trashed = :trashed, \
         web_view_link = :web_view_link, \
         version = :version, \
         file_mode_bits = :file_mode_bits, \
         parent_path_hash = :parent_path_hash, \
         local_name = :local_name, \
         uid = :uid, \
         gid = :gid, \
         link_target = :link_target, \
         xattrs = :xattrs, \
         parent_path = :parent_path, \
         path = :path, \
         state = :state, \
         last_update = :last_update \
       WHERE id = :id;"
    in
      Sqlite3.prepare db sql

  let prepare_update_state_stmt db =
    let sql =
      "UPDATE resource \
       SET \
         state = :state \
       WHERE id = :id;"
    in
      Sqlite3.prepare db sql

  let prepare_delete_all_with_parent_path db =
    let sql =
      "DELETE \
       FROM resource \
       WHERE parent_path LIKE :parent_path \
         AND trashed = :trashed"
    in
      Sqlite3.prepare db sql

  let prepare_trash_all_with_parent_path db =
    let sql =
      "UPDATE resource \
       SET trashed = 1 \
       WHERE parent_path LIKE :parent_path \
         AND trashed = 0"
    in
      Sqlite3.prepare db sql

  let prepare_invalidate_stmt db =
    let sql =
      "UPDATE resource \
       SET \
         state = 'ToDownload' \
       WHERE id = :id \
         AND state <> 'ToUpload' \
         AND state <> 'Uploading';"
    in
      Sqlite3.prepare db sql

  let prepare_invalidate_trash_bin_stmt db =
    let sql =
      "UPDATE resource \
       SET \
         state = 'ToDownload' \
       WHERE path LIKE '/' \
         AND trashed = 1;"
    in
      Sqlite3.prepare db sql

  let prepare_trash_stmt db =
    let sql =
      "UPDATE resource \
       SET \
         trashed = 1 \
       WHERE id = :id;"
    in
      Sqlite3.prepare db sql

  let prepare_delete_stmt db =
    let sql =
      "DELETE \
       FROM resource \
       WHERE id = :id;"
    in
      Sqlite3.prepare db sql

  let prepare_delete_all_with_path_stmt db =
    let sql =
      "DELETE \
       FROM resource \
       WHERE path = :path \
         AND trashed = :trashed;"
    in
      Sqlite3.prepare db sql

  let prepare_delete_not_found_with_path_stmt db =
    let sql =
      "DELETE \
       FROM resource \
       WHERE path = :path \
         AND state = 'NotFound';"
    in
      Sqlite3.prepare db sql

  let prepare_delete_with_parent_path_stmt db =
    let sql =
      "DELETE \
       FROM resource \
       WHERE parent_path = :parent_path \
         AND trashed = :trashed \
         AND state <> 'NotFound';"
    in
      Sqlite3.prepare db sql

  let prepare_select_with_path_stmt db =
    let sql =
      "SELECT " ^ fields ^ " \
       FROM resource \
       WHERE path = :path \
         AND trashed = :trashed;"
    in
      Sqlite3.prepare db sql

  let prepare_select_with_remote_id_stmt db =
    let sql =
      "SELECT " ^ fields ^ " \
       FROM resource \
       WHERE remote_id = :remote_id;"
    in
      Sqlite3.prepare db sql

  let prepare_select_with_parent_path_stmt db =
    let sql =
      "SELECT " ^ fields ^ " \
       FROM resource \
       WHERE parent_path = :parent_path \
         AND trashed = :trashed \
         AND state IN ('Synchronized', 'ToDownload');"
    in
      Sqlite3.prepare db sql

  let prepare_select_order_by_last_update db =
    let sql =
      "SELECT " ^ fields ^ " \
       FROM resource \
       WHERE state = 'Synchronized' \
         AND size > 0 \
       ORDER BY last_update;"
    in
      Sqlite3.prepare db sql

end

module MetadataStmts =
struct
  let api_fields_without_id =
    "display_name, \
     storage_quota_limit, \
     storage_quota_usage, \
     start_page_token, "
  let app_fields =
    "cache_size, \
     last_update"

  let fields_without_id = api_fields_without_id ^ app_fields
  let fields = "id, " ^ fields_without_id

  let prepare_insert_stmt db =
    let sql =
      "INSERT OR REPLACE INTO metadata (" ^ fields ^ ") \
       VALUES ( \
         1, \
         :display_name, \
         :storage_quota_limit, \
         :storage_quota_usage, \
         :start_page_token, \
         :cache_size, \
         :last_update \
       );"
    in
      Sqlite3.prepare db sql

  let prepare_select_stmt db =
    let sql =
      "SELECT " ^ fields_without_id ^ " \
       FROM metadata \
       WHERE id = 1;"
    in
      Sqlite3.prepare db sql

end
(* END Prepare SQL *)

(* Open/close db *)
type t = {
  cache_dir : string;
  db_path : string;
  busy_timeout : int;
}

let create_cache app_dir config =
  let cache_dir = app_dir.AppDir.cache_dir in
  let db_path = Filename.concat cache_dir "cache.db" in
  let busy_timeout = config.Config.sqlite3_busy_timeout in
  { cache_dir;
    db_path;
    busy_timeout;
  }

let open_db cache =
  let db = Sqlite3.db_open cache.db_path in
  Sqlite3.busy_timeout db cache.busy_timeout;
  db

let close_db db =
  Utils.with_retry (fun () ->
    if not (Sqlite3.db_close db) then raise (Failure "close_db"))
    "close_db"

let with_db cache f =
  let db = open_db cache in
  Utils.try_finally
    (fun () -> f db)
    (fun () -> close_db db)

let with_transaction cache f =
  with_db cache
    (fun db ->
       let begin_tran_stmt = prepare_begin_tran_stmt db in
       let commit_tran_stmt = prepare_commit_tran_stmt db in
       final_step begin_tran_stmt;
       let result = f db in
       final_step commit_tran_stmt;
       finalize_stmt commit_tran_stmt;
       finalize_stmt begin_tran_stmt;
       result)
(* END Open/close db *)

module Resource =
struct
  module State =
  struct
    type t =
        Synchronized
      | ToDownload
      | Downloading
      | ToUpload
      | Uploading
      | NotFound

    let to_string = function
        Synchronized -> "Synchronized"
      | ToDownload -> "ToDownload"
      | Downloading -> "Downloading"
      | ToUpload -> "ToUpload"
      | Uploading -> "Uploading"
      | NotFound -> "NotFound"

    let of_string = function
        "Synchronized" -> Synchronized
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
    md5_checksum : string option;
    size : int64 option;
    can_edit : bool option;
    trashed : bool option;
    web_view_link : string option;
    version : int64 option;
    (* app data stored in Drive *)
    file_mode_bits : int64 option;
    parent_path_hash : string option;
    local_name : string option;
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

  let id = {
    GapiLens.get = (fun x -> x.id);
    GapiLens.set = (fun v x -> { x with id = v })
  }
  let remote_id = {
    GapiLens.get = (fun x -> x.remote_id);
    GapiLens.set = (fun v x -> { x with remote_id = v })
  }
  let name = {
    GapiLens.get = (fun x -> x.name);
    GapiLens.set = (fun v x -> { x with name = v })
  }
  let mime_type = {
    GapiLens.get = (fun x -> x.mime_type);
    GapiLens.set = (fun v x -> { x with mime_type = v })
  }
  let created_time = {
    GapiLens.get = (fun x -> x.created_time);
    GapiLens.set = (fun v x -> { x with created_time = v })
  }
  let modified_time = {
    GapiLens.get = (fun x -> x.modified_time);
    GapiLens.set = (fun v x -> { x with modified_time = v })
  }
  let viewed_by_me_time = {
    GapiLens.get = (fun x -> x.viewed_by_me_time);
    GapiLens.set = (fun v x -> { x with viewed_by_me_time = v })
  }
  let file_extension = {
    GapiLens.get = (fun x -> x.file_extension);
    GapiLens.set = (fun v x -> { x with file_extension = v })
  }
  let md5_checksum = {
    GapiLens.get = (fun x -> x.md5_checksum);
    GapiLens.set = (fun v x -> { x with md5_checksum = v })
  }
  let size = {
    GapiLens.get = (fun x -> x.size);
    GapiLens.set = (fun v x -> { x with size = v })
  }
  let can_edit = {
    GapiLens.get = (fun x -> x.can_edit);
    GapiLens.set = (fun v x -> { x with can_edit = v })
  }
  let trashed = {
    GapiLens.get = (fun x -> x.trashed);
    GapiLens.set = (fun v x -> { x with trashed = v })
  }
  let web_view_link = {
    GapiLens.get = (fun x -> x.web_view_link);
    GapiLens.set = (fun v x -> { x with web_view_link = v })
  }
  let version = {
    GapiLens.get = (fun x -> x.version);
    GapiLens.set = (fun v x -> { x with version = v })
  }
  let file_mode_bits = {
    GapiLens.get = (fun x -> x.file_mode_bits);
    GapiLens.set = (fun v x -> { x with file_mode_bits = v })
  }
  let parent_path_hash = {
    GapiLens.get = (fun x -> x.parent_path_hash);
    GapiLens.set = (fun v x -> { x with parent_path_hash = v })
  }
  let local_name = {
    GapiLens.get = (fun x -> x.local_name);
    GapiLens.set = (fun v x -> { x with local_name = v })
  }
  let uid = {
    GapiLens.get = (fun x -> x.uid);
    GapiLens.set = (fun v x -> { x with uid = v })
  }
  let gid = {
    GapiLens.get = (fun x -> x.gid);
    GapiLens.set = (fun v x -> { x with gid = v })
  }
  let link_target = {
    GapiLens.get = (fun x -> x.link_target);
    GapiLens.set = (fun v x -> { x with link_target = v })
  }
  let xattrs = {
    GapiLens.get = (fun x -> x.xattrs);
    GapiLens.set = (fun v x -> { x with xattrs = v })
  }
  let parent_path = {
    GapiLens.get = (fun x -> x.parent_path);
    GapiLens.set = (fun v x -> { x with parent_path = v })
  }
  let path = {
    GapiLens.get = (fun x -> x.path);
    GapiLens.set = (fun v x -> { x with path = v })
  }
  let state = {
    GapiLens.get = (fun x -> x.state);
    GapiLens.set = (fun v x -> { x with state = v })
  }
  let last_update = {
    GapiLens.get = (fun x -> x.last_update);
    GapiLens.set = (fun v x -> { x with last_update = v })
  }

  (* file mode bits *)
  let file_mode_bits_to_kind m =
    let s_ifmt = 0o170000 in
    let file_type = (Int64.to_int m) land s_ifmt in
    match file_type with
        0o140000 -> Unix.S_SOCK
      | 0o120000 -> Unix.S_LNK
      | 0o100000 -> Unix.S_REG
      | 0o060000 -> Unix.S_BLK
      | 0o040000 -> Unix.S_DIR
      | 0o020000 -> Unix.S_CHR
      | 0o010000 -> Unix.S_FIFO
      | _ -> Unix.S_REG

  let file_mode_bits_to_perm m =
    let mask = 0o7777 in
    (Int64.to_int m) land mask

  (* xattrs *)
  let render_xattrs xattrs =
    let buffer = Buffer.create 64 in
    List.iter
      (fun (name, value) ->
         let s = Printf.sprintf "%S:%S;" name value in
         Buffer.add_string buffer s)
      xattrs;
    Buffer.contents buffer

  let parse_xattrs s =
    let sb = Scanf.Scanning.from_string s in
    let xattrs = ref [] in
    while (not (Scanf.Scanning.end_of_input sb)) do
      let (name, value) = Scanf.bscanf sb "%S:%S;" (fun f l -> (f, l)) in
      xattrs := (name, value) :: !xattrs
    done;
    !xattrs

  (* app properties *)
  let find_app_property name app_properties =
    try
      Some (List.assoc name app_properties)
    with Not_found ->
      None

  let app_property_to_int64 p = Option.map (fun s -> Int64.of_string s) p

  let get_file_mode_bits app_properties =
    app_property_to_int64
      (find_app_property "mode" app_properties)

  let file_mode_bits_to_app_property file_mode_bits =
    ("mode", Option.map_default Int64.to_string "" file_mode_bits)

  let mode_to_app_property mode =
    ("mode", string_of_int mode)

  let get_uid app_properties =
    app_property_to_int64
      (find_app_property "uid" app_properties)

  let uid_to_app_property uid =
    ("uid", string_of_int uid)

  let get_gid app_properties =
    app_property_to_int64
      (find_app_property "gid" app_properties)

  let gid_to_app_property gid =
    ("gid", string_of_int gid)

  let get_link_target app_properties =
    find_app_property "l" app_properties

  let link_target_to_app_property link_target =
    ("l", link_target)

  let get_xattrs app_properties =
    render_xattrs
      (List.filter
         (fun (n, _) -> ExtString.String.starts_with "x-" n)
         app_properties)

  let xattr_to_app_property name value =
    ("x-" ^ name, value)

  let xattr_no_value_to_app_property name =
    ("x-" ^ name, "")

  (* Queries *)
  let bind_resource_parameters stmt resource =
    bind_text stmt ":remote_id" resource.remote_id;
    bind_text stmt ":name" resource.name;
    bind_text stmt ":mime_type" resource.mime_type;
    bind_float stmt ":created_time" resource.created_time;
    bind_float stmt ":modified_time" resource.modified_time;
    bind_float stmt ":viewed_by_me_time" resource.viewed_by_me_time;
    bind_text stmt ":file_extension" resource.file_extension;
    bind_text stmt ":md5_checksum" resource.md5_checksum;
    bind_int stmt ":size" resource.size;
    bind_bool stmt ":can_edit" resource.can_edit;
    bind_bool stmt ":trashed" resource.trashed;
    bind_text stmt ":web_view_link" resource.web_view_link;
    bind_int stmt ":version" resource.version;
    bind_int stmt ":file_mode_bits" resource.file_mode_bits;
    bind_text stmt ":parent_path_hash" resource.parent_path_hash;
    bind_text stmt ":local_name" resource.local_name;
    bind_int stmt ":uid" resource.uid;
    bind_int stmt ":gid" resource.gid;
    bind_text stmt ":link_target" resource.link_target;
    bind_text stmt ":xattrs" (Some resource.xattrs);
    bind_text stmt ":parent_path" (Some resource.parent_path);
    bind_text stmt ":path" (Some resource.path);
    bind_text stmt ":state" (Some (State.to_string resource.state));
    bind_float stmt ":last_update" (Some resource.last_update)

  let step_insert_resource db stmt resource =
    reset_stmt stmt;
    bind_resource_parameters stmt resource;
    final_step stmt;
    resource |> id ^= Sqlite3.last_insert_rowid db

  let insert_resource cache resource =
    let delete_stmt db =
      let stmt = ResourceStmts.prepare_delete_all_with_path_stmt db in
      bind_text stmt ":path" (Some resource.path);
      bind_bool stmt ":trashed" resource.trashed;
      finalize_stmt stmt in
    let insert_stmt db =
      let stmt = ResourceStmts.prepare_insert_stmt db in
      let result = step_insert_resource db stmt resource in
      finalize_stmt stmt;
      result
    in
    with_transaction cache
      (fun db ->
         delete_stmt db;
         insert_stmt db)

  let update_resource cache resource =
    with_transaction cache
      (fun db ->
         let stmt = ResourceStmts.prepare_update_stmt db in
         bind_resource_parameters stmt resource;
         bind_int stmt ":id" (Some resource.id);
         final_step stmt;
         finalize_stmt stmt)

  let update_resource_state cache state id =
    with_transaction cache
      (fun db ->
         let stmt = ResourceStmts.prepare_update_state_stmt db in
         bind_text stmt ":state" (Some (State.to_string state));
         bind_int stmt ":id" (Some id);
         final_step stmt;
         finalize_stmt stmt)

  let _delete_resource stmt id =
    reset_stmt stmt;
    bind_int stmt ":id" (Some id);
    final_step stmt

  let delete_resource cache resource =
    with_transaction cache
      (fun db ->
         let stmt = ResourceStmts.prepare_delete_stmt db in
         _delete_resource stmt resource.id;
         finalize_stmt stmt)

  let delete_not_found_resource_with_path cache path =
    with_transaction cache
      (fun db ->
         let stmt = ResourceStmts.prepare_delete_not_found_with_path_stmt db in
         reset_stmt stmt;
         bind_text stmt ":path" (Some path);
         final_step stmt;
         finalize_stmt stmt)

  let _delete_resources_with_parent_path db parent_path trashed =
    let stmt = ResourceStmts.prepare_delete_with_parent_path_stmt db in
    bind_text stmt ":parent_path" (Some parent_path);
    bind_bool stmt ":trashed" (Some trashed);
    final_step stmt;
    finalize_stmt stmt

  let delete_resources cache resources =
    with_transaction cache
      (fun db ->
         let stmt = ResourceStmts.prepare_delete_stmt db in
         List.iter
           (fun resource ->
              _delete_resource stmt resource.id)
           resources;
         finalize_stmt stmt)

  let insert_resources cache resources parent_path trashed =
    with_transaction cache
      (fun db ->
         _delete_resources_with_parent_path db parent_path trashed;
         let stmt = ResourceStmts.prepare_insert_stmt db in
         let results =
           List.map
             (step_insert_resource db stmt)
             resources in
         finalize_stmt stmt;
         results)

  let invalidate_resources cache ids =
    with_transaction cache
      (fun db ->
         let stmt = ResourceStmts.prepare_invalidate_stmt db in
         List.iter
           (fun id ->
              reset_stmt stmt;
              bind_int stmt ":id" (Some id);
              final_step stmt)
           ids;
         finalize_stmt stmt)

  let invalidate_trash_bin cache =
    with_transaction cache
      (fun db ->
         let stmt = ResourceStmts.prepare_invalidate_trash_bin_stmt db in
         reset_stmt stmt;
         final_step stmt;
         finalize_stmt stmt)

  let trash_resources cache resources =
    with_transaction cache
      (fun db ->
         let stmt = ResourceStmts.prepare_trash_stmt db in
         List.iter
           (fun resource ->
              reset_stmt stmt;
              bind_int stmt ":id" (Some resource.id);
              final_step stmt)
           resources;
         finalize_stmt stmt)

  let delete_all_with_parent_path cache parent_path trashed =
    with_transaction cache
      (fun db ->
         let stmt = ResourceStmts.prepare_delete_all_with_parent_path db in
         reset_stmt stmt;
         bind_text stmt ":parent_path" (Some (parent_path ^ "%"));
         bind_bool stmt ":trashed" (Some trashed);
         final_step stmt;
         finalize_stmt stmt)

  let trash_all_with_parent_path cache parent_path =
    with_transaction cache
      (fun db ->
         let stmt = ResourceStmts.prepare_trash_all_with_parent_path db in
         reset_stmt stmt;
         bind_text stmt ":parent_path" (Some (parent_path ^ "%"));
         final_step stmt;
         finalize_stmt stmt)

  let row_to_resource row_data =
    { id = row_data.(0) |> data_to_int64 |> Option.get;
      remote_id = row_data.(1) |> data_to_string;
      name = row_data.(2) |> data_to_string;
      mime_type = row_data.(3) |> data_to_string;
      created_time = row_data.(4) |> data_to_float;
      modified_time = row_data.(5) |> data_to_float;
      viewed_by_me_time = row_data.(6) |> data_to_float;
      file_extension = row_data.(7) |> data_to_string;
      md5_checksum = row_data.(8) |> data_to_string;
      size = row_data.(9) |> data_to_int64;
      can_edit = row_data.(10) |> data_to_bool;
      trashed = row_data.(11) |> data_to_bool;
      web_view_link = row_data.(12) |> data_to_string;
      version = row_data.(13) |> data_to_int64;
      file_mode_bits = row_data.(14) |> data_to_int64;
      parent_path_hash = row_data.(15) |> data_to_string;
      local_name = row_data.(16) |> data_to_string;
      uid = row_data.(17) |> data_to_int64;
      gid = row_data.(18) |> data_to_int64;
      link_target = row_data.(19) |> data_to_string;
      xattrs = row_data.(20) |> data_to_string |> Option.get;
      parent_path = row_data.(21) |> data_to_string |> Option.get;
      path = row_data.(22) |> data_to_string |> Option.get;
      state = row_data.(23) |> data_to_string |> Option.get |> State.of_string;
      last_update = row_data.(24) |> data_to_float |> Option.get;
    }

  let select_resource cache prepare bind =
    with_transaction cache
      (fun db ->
         let stmt = prepare db in
         let result =
           select_first_row stmt bind row_to_resource in
         finalize_stmt stmt;
         result)

  let select_resource_with_path cache path trashed =
    select_resource cache
      ResourceStmts.prepare_select_with_path_stmt
      (fun stmt ->
         bind_text stmt ":path" (Some path);
         bind_bool stmt ":trashed" (Some trashed))

  let select_resource_with_remote_id cache remote_id =
    select_resource cache
      ResourceStmts.prepare_select_with_remote_id_stmt
      (fun stmt -> bind_text stmt ":remote_id" (Some remote_id))

  let select_resources_with_parent_path cache parent_path trashed =
    with_transaction cache
      (fun db ->
         let stmt = ResourceStmts.prepare_select_with_parent_path_stmt db in
         let results =
           select_all_rows stmt
             (fun stmt ->
                bind_text stmt ":parent_path" (Some parent_path);
                bind_bool stmt ":trashed" (Some trashed))
             row_to_resource in
         finalize_stmt stmt;
         results)

  let select_resources_order_by_last_update cache =
    with_transaction cache
      (fun db ->
         let stmt = ResourceStmts.prepare_select_order_by_last_update db in
         let results =
           select_all_rows stmt
             (fun _ -> ())
             row_to_resource in
         finalize_stmt stmt;
         results)
  (* END Queries *)

  let is_folder resource =
    match resource.mime_type with
        Some "application/vnd.google-apps.folder" -> true
      | _ -> false

  let is_document_mime_type mime_type =
    match mime_type with
        "application/vnd.google-apps.document"
      | "application/vnd.google-apps.drawing"
      | "application/vnd.google-apps.form"
      | "application/vnd.google-apps.presentation"
      | "application/vnd.google-apps.spreadsheet"
      | "application/vnd.google-apps.map"
      | "application/vnd.google-apps.fusiontable" -> true
      | _ -> false

  let is_document resource =
    match resource.mime_type with
        Some mime_type -> is_document_mime_type mime_type
      | _ -> false

  let is_symlink resource =
    Option.is_some resource.link_target

  let is_valid resource metadata_last_update =
    resource.last_update >= metadata_last_update

  let get_format_from_mime_type mime_type config =
    match mime_type with
        "application/vnd.google-apps.document" ->
          config.Config.document_format
      | "application/vnd.google-apps.drawing" ->
          config.Config.drawing_format
      | "application/vnd.google-apps.form" ->
          config.Config.form_format
      | "application/vnd.google-apps.presentation" ->
          config.Config.presentation_format
      | "application/vnd.google-apps.spreadsheet" ->
          config.Config.spreadsheet_format
      | "application/vnd.google-apps.map" ->
          config.Config.map_format
      | "application/vnd.google-apps.fusiontable" ->
          config.Config.fusion_table_format
      | _ -> "html"

  let get_format resource config =
    match resource.mime_type with
        Some mime_type -> get_format_from_mime_type mime_type config
      | _ -> "html"

  let get_icon_from_mime_type mime_type config =
    match mime_type with
        "application/vnd.google-apps.document" ->
          config.Config.document_icon
      | "application/vnd.google-apps.drawing" ->
          config.Config.drawing_icon
      | "application/vnd.google-apps.form" ->
          config.Config.form_icon
      | "application/vnd.google-apps.presentation" ->
          config.Config.presentation_icon
      | "application/vnd.google-apps.spreadsheet" ->
          config.Config.spreadsheet_icon
      | "application/vnd.google-apps.map" ->
          config.Config.map_icon
      | "application/vnd.google-apps.fusiontable" ->
          config.Config.fusion_table_icon
      | _ -> ""

  let get_icon resource config =
    match resource.mime_type with
        Some mime_type -> get_icon_from_mime_type mime_type config
      | _ -> "html"

  let mime_type_of_format fmt =
    match fmt with
        "doc"
      | "docx" -> "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
      | "htm"
      | "html" -> "text/html"
      | "jpg"
      | "jpeg" -> "image/jpeg"
      | "ods" -> "application/x-vnd.oasis.opendocument.spreadsheet"
      | "odt" -> "application/vnd.oasis.opendocument.text"
      | "pdf" -> "application/pdf"
      | "png" -> "image/png"
      | "ppt"
      | "pptx" -> "application/vnd.openxmlformats-officedocument.presentationml.presentation"
      | "rtf" -> "application/rtf"
      | "svg" -> "image/svg+xml"
      | "txt" -> "text/plain"
      | "xls"
      | "xlsx" -> "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
      | _ -> failwith ("Unsupported format: " ^ fmt)

end

module Metadata =
struct
  type t = {
    display_name : string;
    storage_quota_limit : int64;
    storage_quota_usage : int64;
    start_page_token : string;
    cache_size : int64;
    last_update : float;
  }

  let display_name = {
    GapiLens.get = (fun x -> x.display_name);
    GapiLens.set = (fun v x -> { x with display_name = v })
  }
  let storage_quota_limit = {
    GapiLens.get = (fun x -> x.storage_quota_limit);
    GapiLens.set = (fun v x -> { x with storage_quota_limit = v })
  }
  let storage_quota_usage = {
    GapiLens.get = (fun x -> x.storage_quota_usage);
    GapiLens.set = (fun v x -> { x with storage_quota_usage = v })
  }
  let start_page_token = {
    GapiLens.get = (fun x -> x.start_page_token);
    GapiLens.set = (fun v x -> { x with start_page_token = v })
  }
  let cache_size = {
    GapiLens.get = (fun x -> x.cache_size);
    GapiLens.set = (fun v x -> { x with cache_size = v })
  }
  let last_update = {
    GapiLens.get = (fun x -> x.last_update);
    GapiLens.set = (fun v x -> { x with last_update = v })
  }

  (* Queries *)
  let save_metadata stmt metadata =
    bind_text stmt ":display_name" (Some metadata.display_name);
    bind_int stmt ":storage_quota_limit" (Some metadata.storage_quota_limit);
    bind_int stmt ":storage_quota_usage" (Some metadata.storage_quota_usage);
    bind_text stmt ":start_page_token" (Some metadata.start_page_token);
    bind_int stmt ":cache_size" (Some metadata.cache_size);
    bind_float stmt ":last_update" (Some metadata.last_update);
    final_step stmt

  let insert_metadata cache resource =
    with_transaction cache
      (fun db ->
         let stmt = MetadataStmts.prepare_insert_stmt db in
         save_metadata stmt resource;
         finalize_stmt stmt)

  let row_to_metadata row_data =
    { display_name = row_data.(0) |> data_to_string |> Option.get;
      storage_quota_limit = row_data.(1) |> data_to_int64 |> Option.get;
      storage_quota_usage = row_data.(2) |> data_to_int64 |> Option.get;
      start_page_token = row_data.(3) |> data_to_string |> Option.get;
      cache_size = row_data.(4) |> data_to_int64 |> Option.get;
      last_update = row_data.(5) |> data_to_float |> Option.get;
    }

  let select_metadata cache =
    with_transaction cache
      (fun db ->
         let stmt = MetadataStmts.prepare_select_stmt db in
         let result =
           select_first_row stmt (fun _ -> ()) row_to_metadata in
         finalize_stmt stmt;
         result)
  (* END Queries *)

  let is_valid metadata_cache_time metadata =
    let now = Unix.gettimeofday () in
    now -. metadata.last_update <= float_of_int metadata_cache_time

end

(* Resource content *)
let get_content_path cache resource =
  Filename.concat cache.cache_dir (Option.get resource.Resource.remote_id)
(* END Resource content *)

let delete_files_from_cache cache resources =
  let remove_file path =
    try
      if Sys.file_exists path then begin
        let stats = Unix.LargeFile.stat path in
        let size = stats.Unix.LargeFile.st_size in
        Sys.remove path;
        size
      end else 0L
    with e -> Utils.log_exception e; 0L
  in
  List.fold_left
    (fun total_size resource ->
       let content_path = get_content_path cache resource in
       Utils.log_with_header
         "BEGIN: Removing file (%s: resource %Ld) from cache\n%!"
         content_path resource.Resource.id;
       let size = remove_file content_path in
       let new_size = Int64.add total_size size in
       Utils.log_with_header
         "END: Removing file (%s: resource %Ld) from cache\n%!"
         content_path resource.Resource.id;
       new_size)
    0L
    resources

(* Setup *)
let setup_db cache =
  with_db cache
    (fun db ->
      wrap_exec_not_null_no_headers db
        "BEGIN IMMEDIATE TRANSACTION; \
         CREATE TABLE IF NOT EXISTS resource ( \
            id INTEGER PRIMARY KEY, \
            remote_id TEXT NULL, \
            name TEXT NULL, \
            mime_type TEXT NULL, \
            created_time REAL NULL, \
            modified_time REAL NULL, \
            viewed_by_me_time REAL NULL, \
            file_extension TEXT NULL, \
            md5_checksum TEXT NULL, \
            size INTEGER NULL, \
            can_edit INTEGER NULL, \
            trashed INTEGER NULL, \
            web_view_link TEXT NULL, \
            version INTEGER NULL, \
            file_mode_bits INTEGER NULL, \
            parent_path_hash TEXT NULL, \
            local_name TEXT NULL, \
            uid INTEGER NULL, \
            gid INTEGER NULL, \
            link_target TEXT NULL, \
            xattrs TEXT NOT NULL, \
            parent_path TEXT NOT NULL, \
            path TEXT NOT NULL, \
            state TEXT NOT NULL, \
            last_update REAL NOT NULL \
         ); \
         CREATE INDEX IF NOT EXISTS path_index ON resource (path, trashed); \
         CREATE INDEX IF NOT EXISTS parent_path_index ON resource (parent_path, trashed); \
         CREATE INDEX IF NOT EXISTS remote_id_index ON resource (remote_id); \
         CREATE INDEX IF NOT EXISTS last_update_index ON resource (last_update); \
         CREATE TABLE IF NOT EXISTS metadata ( \
            id INTEGER PRIMARY KEY, \
            display_name TEXT NOT NULL, \
            storage_quota_limit INTEGER NOT NULL, \
            storage_quota_usage INTEGER NOT NULL, \
            start_page_token TEXT NOT NULL, \
            cache_size INTEGER NOT NULL, \
            last_update REAL NOT NULL \
         ); \
         COMMIT TRANSACTION;" |> ignore)
(* END Setup *)

let clean_up_cache cache =
  if Sys.file_exists cache.cache_dir &&
     Sys.is_directory cache.cache_dir then begin
    Array.iter
      (fun file ->
         try
           Sys.remove (Filename.concat cache.cache_dir file)
         with e ->
           Utils.log_with_header "Error removing file %s: %s\n%!"
             file (Printexc.to_string e))
      (Sys.readdir cache.cache_dir)
  end

