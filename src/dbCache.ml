open GapiUtils.Infix
open GapiLens.Infix

(* Helpers *)
let fail rc =
  failwith ("Sqlite3 error: " ^ (Sqlite3.Rc.to_string rc))

let expect expected rc =
  if rc <> expected then fail rc

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
     full_file_extension, \
     md5_checksum, \
     size, \
     can_edit, \
     trashed, \
     web_view_link, \
     version, "
  let app_properties_fields =
    "file_mode_bits, \
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
         :full_file_extension, \
         :md5_checksum, \
         :size, \
         :can_edit, \
         :trashed, \
         :web_view_link, \
         :version, \
         :file_mode_bits, \
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

  let prepare_insert_with_id_stmt db =
    let sql =
      "INSERT INTO resource (" ^ fields ^ ") \
       VALUES ( \
         :id, \
         :remote_id, \
         :name, \
         :mime_type, \
         :created_time, \
         :modified_time, \
         :viewed_by_me_time, \
         :file_extension, \
         :full_file_extension, \
         :md5_checksum, \
         :size, \
         :can_edit, \
         :trashed, \
         :web_view_link, \
         :version, \
         :file_mode_bits, \
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
         full_file_extension = :full_file_extension, \
         md5_checksum = :md5_checksum, \
         size = :size, \
         can_edit = :can_edit, \
         trashed = :trashed, \
         web_view_link = :web_view_link, \
         version = :version, \
         file_mode_bits = :file_mode_bits, \
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

  let prepare_update_state_and_size_stmt db =
    let sql =
      "UPDATE resource \
       SET \
         state = :state, \
         size = :size \
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
         AND state <> 'Uploading' \
         AND state <> 'NotFound';"
    in
      Sqlite3.prepare db sql

  let prepare_invalidate_all_stmt db =
    let sql =
      "UPDATE resource \
       SET \
         state = 'ToDownload' \
       WHERE state <> 'ToUpload' \
         AND state <> 'Uploading' \
         AND state <> 'NotFound';"
    in
      Sqlite3.prepare db sql

  let prepare_invalidate_trash_bin_stmt db =
    let sql =
      "UPDATE resource \
       SET \
         state = 'ToDownload' \
       WHERE path LIKE '/' \
         AND trashed = 1 \
         AND state <> 'NotFound';"
    in
      Sqlite3.prepare db sql

  let prepare_invalidate_path_stmt db =
    let sql =
      "UPDATE resource \
       SET \
         state = 'ToDownload' \
       WHERE path LIKE :path \
         AND state <> 'ToUpload' \
         AND state <> 'Uploading' \
         AND state <> 'NotFound';"
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

  let prepare_update_all_timestamps_stmt db =
    let sql =
      "UPDATE resource \
       SET \
         last_update = :last_update;"
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

  let prepare_delete_all_stmt db =
    let sql =
      "DELETE \
       FROM resource;"
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
         AND state <> 'NotFound';"
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

  let prepare_select_all_resources db =
    let sql =
      "SELECT " ^ fields ^ " \
       FROM resource;"
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
     last_update, \
     clean_shutdown"

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
         :last_update, \
         :clean_shutdown \
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

  let prepare_update_cache_size_stmt db =
    let sql =
      "UPDATE metadata \
       SET \
         cache_size = cache_size + :delta \
       WHERE id = 1;"
    in
    Sqlite3.prepare db sql

  let prepare_set_clean_shutdown_stmt db =
    let sql =
      "UPDATE metadata \
       SET \
         clean_shutdown = :flag \
       WHERE id = 1;"
    in
    Sqlite3.prepare db sql

end
(* END Prepare SQL *)

(* Open/close db *)
let open_db cache =
  let db = Sqlite3.db_open cache.CacheData.db_path in
  Sqlite3.busy_timeout db cache.CacheData.busy_timeout;
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

  (* Queries *)
  let bind_resource_parameters stmt resource =
    let open CacheData.Resource in
    bind_text stmt ":remote_id" resource.remote_id;
    bind_text stmt ":name" resource.name;
    bind_text stmt ":mime_type" resource.mime_type;
    bind_float stmt ":created_time" resource.created_time;
    bind_float stmt ":modified_time" resource.modified_time;
    bind_float stmt ":viewed_by_me_time" resource.viewed_by_me_time;
    bind_text stmt ":file_extension" resource.file_extension;
    bind_text stmt ":full_file_extension" resource.full_file_extension;
    bind_text stmt ":md5_checksum" resource.md5_checksum;
    bind_int stmt ":size" resource.size;
    bind_bool stmt ":can_edit" resource.can_edit;
    bind_bool stmt ":trashed" resource.trashed;
    bind_text stmt ":web_view_link" resource.web_view_link;
    bind_int stmt ":version" resource.version;
    bind_int stmt ":file_mode_bits" resource.file_mode_bits;
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
    resource |> CacheData.Resource.id ^= Sqlite3.last_insert_rowid db

  let insert_resource cache resource =
    let delete_stmt db =
      let stmt = ResourceStmts.prepare_delete_all_with_path_stmt db in
      bind_text stmt ":path" (Some resource.CacheData.Resource.path);
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
         bind_text stmt ":state" (Some (CacheData.Resource.State.to_string state));
         bind_int stmt ":id" (Some id);
         final_step stmt;
         finalize_stmt stmt)

  let update_resource_state_and_size cache state size id =
    with_transaction cache
      (fun db ->
         let stmt = ResourceStmts.prepare_update_state_and_size_stmt db in
         bind_text stmt ":state" (Some (CacheData.Resource.State.to_string state));
         bind_int stmt ":size" (Some size);
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
         _delete_resource stmt resource.CacheData.Resource.id;
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
              _delete_resource stmt resource.CacheData.Resource.id)
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

  let flush_resources cache resources =
    with_transaction cache
      (fun db ->
         let delete_stmt = ResourceStmts.prepare_delete_all_stmt db in
         final_step delete_stmt;
         finalize_stmt delete_stmt;
         let insert_stmt = ResourceStmts.prepare_insert_with_id_stmt db in
         List.iter
           (fun resource -> 
              reset_stmt insert_stmt;
              bind_int insert_stmt ":id" (Some resource.CacheData.Resource.id);
              bind_resource_parameters insert_stmt resource;
              final_step insert_stmt)
           resources;
         finalize_stmt insert_stmt)

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

  let invalidate_path cache path =
    with_transaction cache
      (fun db ->
         let stmt = ResourceStmts.prepare_invalidate_path_stmt db in
         reset_stmt stmt;
         bind_text stmt ":path" (Some path);
         final_step stmt;
         finalize_stmt stmt)

  let invalidate_all cache =
    with_transaction cache
      (fun db ->
         let stmt = ResourceStmts.prepare_invalidate_all_stmt db in
         reset_stmt stmt;
         final_step stmt;
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
              bind_int stmt ":id" (Some resource.CacheData.Resource.id);
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

  let update_all_timestamps cache last_update =
    with_transaction cache
      (fun db ->
         let stmt = ResourceStmts.prepare_update_all_timestamps_stmt db in
         bind_float stmt ":last_update" (Some last_update);
         final_step stmt;
         finalize_stmt stmt)

  let row_to_resource row_data =
    { CacheData.Resource.id = row_data.(0) |> data_to_int64 |> Option.get;
      remote_id = row_data.(1) |> data_to_string;
      name = row_data.(2) |> data_to_string;
      mime_type = row_data.(3) |> data_to_string;
      created_time = row_data.(4) |> data_to_float;
      modified_time = row_data.(5) |> data_to_float;
      viewed_by_me_time = row_data.(6) |> data_to_float;
      file_extension = row_data.(7) |> data_to_string;
      full_file_extension = row_data.(8) |> data_to_string;
      md5_checksum = row_data.(9) |> data_to_string;
      size = row_data.(10) |> data_to_int64;
      can_edit = row_data.(11) |> data_to_bool;
      trashed = row_data.(12) |> data_to_bool;
      web_view_link = row_data.(13) |> data_to_string;
      version = row_data.(14) |> data_to_int64;
      file_mode_bits = row_data.(15) |> data_to_int64;
      uid = row_data.(16) |> data_to_int64;
      gid = row_data.(17) |> data_to_int64;
      link_target = row_data.(18) |> data_to_string;
      xattrs = row_data.(19) |> data_to_string |> Option.get;
      parent_path = row_data.(20) |> data_to_string |> Option.get;
      path = row_data.(21) |> data_to_string |> Option.get;
      state = row_data.(22) |> data_to_string |> Option.get |>
              CacheData.Resource.State.of_string;
      last_update = row_data.(23) |> data_to_float |> Option.get;
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

  let select_first_resource_with_remote_id cache remote_id =
    select_resource cache
      ResourceStmts.prepare_select_with_remote_id_stmt
      (fun stmt -> bind_text stmt ":remote_id" (Some remote_id))

  let select_resources_with_remote_id cache remote_id =
    with_transaction cache
      (fun db ->
         let stmt = ResourceStmts.prepare_select_with_remote_id_stmt db in
         let results =
           select_all_rows stmt
             (fun stmt -> bind_text stmt ":remote_id" (Some remote_id))
             row_to_resource in
         finalize_stmt stmt;
         results)

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

  let select_all_resources cache =
    with_transaction cache
      (fun db ->
         let stmt = ResourceStmts.prepare_select_all_resources db in
         let results =
           select_all_rows stmt
             (fun _ -> ())
             row_to_resource in
         finalize_stmt stmt;
         results)
  (* END Queries *)

end

module Metadata =
struct
  (* Queries *)
  let save_metadata stmt metadata =
    let open CacheData.Metadata in
    bind_text stmt ":display_name" (Some metadata.display_name);
    bind_int stmt ":storage_quota_limit" (Some metadata.storage_quota_limit);
    bind_int stmt ":storage_quota_usage" (Some metadata.storage_quota_usage);
    bind_text stmt ":start_page_token" (Some metadata.start_page_token);
    bind_int stmt ":cache_size" (Some metadata.cache_size);
    bind_float stmt ":last_update" (Some metadata.last_update);
    bind_bool stmt ":clean_shutdown" (Some metadata.clean_shutdown);
    final_step stmt

  let insert_metadata cache metadata =
    with_transaction cache
      (fun db ->
         let stmt = MetadataStmts.prepare_insert_stmt db in
         save_metadata stmt metadata;
         finalize_stmt stmt)

  let row_to_metadata row_data =
    { CacheData.Metadata.display_name = row_data.(0) |> data_to_string |> Option.get;
      storage_quota_limit = row_data.(1) |> data_to_int64 |> Option.get;
      storage_quota_usage = row_data.(2) |> data_to_int64 |> Option.get;
      start_page_token = row_data.(3) |> data_to_string |> Option.get;
      cache_size = row_data.(4) |> data_to_int64 |> Option.get;
      last_update = row_data.(5) |> data_to_float |> Option.get;
      clean_shutdown = row_data.(6) |> data_to_bool |> Option.get;
    }

  let select_metadata cache =
    with_transaction cache
      (fun db ->
         let stmt = MetadataStmts.prepare_select_stmt db in
         let result =
           select_first_row stmt (fun _ -> ()) row_to_metadata in
         finalize_stmt stmt;
         result)

  let update_cache_size cache delta =
    with_transaction cache
      (fun db ->
         let stmt = MetadataStmts.prepare_update_cache_size_stmt db in
         bind_int stmt ":delta" (Some delta);
         final_step stmt;
         finalize_stmt stmt)

  let set_clean_shutdown cache flag =
    with_transaction cache
      (fun db ->
         let stmt = MetadataStmts.prepare_set_clean_shutdown_stmt db in
         bind_bool stmt ":flag" (Some flag);
         final_step stmt;
         finalize_stmt stmt)
  (* END Queries *)

end

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
            full_file_extension TEXT NULL, \
            md5_checksum TEXT NULL, \
            size INTEGER NULL, \
            can_edit INTEGER NULL, \
            trashed INTEGER NULL, \
            web_view_link TEXT NULL, \
            version INTEGER NULL, \
            file_mode_bits INTEGER NULL, \
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
            last_update REAL NOT NULL, \
            clean_shutdown INTEGER NULL \
         ); \
         UPDATE resource \
         SET state = 'ToDownload' \
         WHERE state = 'Downloading'; \
         UPDATE resource \
         SET state = 'ToUpload' \
         WHERE state = 'Uploading'; \
         COMMIT TRANSACTION;" |> ignore)
(* END Setup *)

let check_clean_shutdown cache =
  let db_metadata = Metadata.select_metadata cache in
  Option.map_default
    (fun m -> m.CacheData.Metadata.clean_shutdown)
    true
    db_metadata

let set_clean_shutdown cache =
  Metadata.set_clean_shutdown cache true

let reset_clean_shutdown cache =
  Metadata.set_clean_shutdown cache false

