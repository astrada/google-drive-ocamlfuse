open GapiUtils.Infix
open GapiLens.Infix

(* Helpers *)
let escape_sql sql =
  ExtString.String.replace_chars
    (function '\'' -> "''" | c -> String.make 1 c) sql

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
  Sqlite3.step stmt |> expect Sqlite3.Rc.DONE
(* END Helpers *)

(* Query helpers *)
let bind to_data stmt name value =
  Option.may
    (fun v ->
       Sqlite3.bind stmt
         (Sqlite3.bind_parameter_index stmt name)
         (to_data v)
       |> fail_if_not_ok)
    value

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
  Sqlite3.prepare db "BEGIN TRANSACTION;"

let prepare_commit_tran_stmt db =
  Sqlite3.prepare db "COMMIT TRANSACTION;"

let prepare_rollback_tran_stmt db =
  Sqlite3.prepare db "ROLLBACK TRANSACTION;"

module ResourceStmts =
struct
  let fields_without_id =
    "etag, \
     remote_id, \
     title, \
     mime_type, \
     created_date, \
     modified_date, \
     last_viewed_by_me_date, \
     parent_remote_ids, \
     download_url, \
     export_links, \
     file_extension, \
     md5_checksum, \
     file_size, \
     editable, \
     trashed, \
     alternate_link, \
     parent_path, \
     path, \
     state, \
     change_id, \
     last_update"

  let fields = "id, " ^ fields_without_id

  let prepare_insert_stmt db =
    let sql =
      "INSERT INTO resource (" ^ fields_without_id ^ ") \
       VALUES ( \
         :etag, \
         :remote_id, \
         :title, \
         :mime_type, \
         :created_date, \
         :modified_date, \
         :last_viewed_by_me_date, \
         :parent_remote_ids, \
         :download_url, \
         :export_links, \
         :file_extension, \
         :md5_checksum, \
         :file_size, \
         :editable, \
         :trashed, \
         :alternate_link, \
         :parent_path, \
         :path, \
         :state, \
         :change_id, \
         :last_update
       );"
    in
      Sqlite3.prepare db sql

  let prepare_update_stmt db =
    let sql =
      "UPDATE resource \
       SET \
         etag = :etag, \
         remote_id = :remote_id, \
         title = :title, \
         mime_type = :mime_type, \
         created_date = :created_date, \
         modified_date = :modified_date, \
         last_viewed_by_me_date = :last_viewed_by_me_date, \
         parent_remote_ids = :parent_remote_ids, \
         download_url = :download_url, \
         export_links = :export_links, \
         file_extension = :file_extension, \
         md5_checksum = :md5_checksum, \
         file_size = :file_size, \
         editable = :editable, \
         trashed = :trashed, \
         alternate_link = :alternate_link, \
         parent_path = :parent_path, \
         path = :path, \
         state = :state, \
         change_id = :change_id, \
         last_update = :last_update \
       WHERE id = :id;"
    in
      Sqlite3.prepare db sql

  let prepare_update_all_change_ids db =
    let sql =
      "UPDATE resource \
       SET change_id = :change_id \
       WHERE state <> 'NotFound'"
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
         state = 'ToDownload', \
         change_id = :change_id \
       WHERE id = :id \
         AND state <> 'ToUpload';"
    in
      Sqlite3.prepare db sql

  let prepare_invalidate_trash_bin_stmt db =
    let sql =
      "UPDATE resource \
       SET \
         state = 'ToDownload', \
         change_id = :change_id \
       WHERE path LIKE '/' \
         AND trashed = 1;"
    in
      Sqlite3.prepare db sql

  let prepare_trash_stmt db =
    let sql =
      "UPDATE resource \
       SET \
         trashed = 1, \
         change_id = :change_id \
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
         AND (change_id < :change_id \
           OR state <> 'NotFound');"
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
         AND state IN ('InSync', 'ToDownload');"
    in
      Sqlite3.prepare db sql

  let prepare_select_order_by_last_update db =
    let sql =
      "SELECT " ^ fields ^ " \
       FROM resource \
       WHERE state = 'InSync' \
         AND file_size > 0 \
       ORDER BY last_update;"
    in
      Sqlite3.prepare db sql

end

module MetadataStmts =
struct
  let fields_without_id =
    "etag, \
     username, \
     quota_bytes_total, \
     quota_bytes_used, \
     largest_change_id, \
     remaining_change_ids, \
     root_folder_id, \
     permission_id, \
     cache_size, \
     last_update"

  let fields =
    "id, " ^ fields_without_id

  let prepare_insert_stmt db =
    let sql =
      "INSERT OR REPLACE INTO metadata (" ^ fields ^ ") \
       VALUES ( \
         1, \
         :etag, \
         :username, \
         :quota_bytes_total, \
         :quota_bytes_used, \
         :largest_change_id, \
         :remaining_change_ids, \
         :root_folder_id, \
         :permission_id, \
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
  let rec try_close n =
    if n > 4 then begin
      let thread_id = Utils.get_thread_id () in
      Utils.log_message "Thread id=%d: Error: cannot close db\n%!" thread_id;
      Printf.eprintf "Error: cannot close sqlite db.\n\
        Please restart the program.\n%!";
      exit 1
    end else if not (Sqlite3.db_close db) then begin
      Unix.sleep 1;
      try_close (succ n)
    end
  in
  try_close 0

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
        InSync
      | ToDownload
      | ToUpload
      | NotFound

    let to_string = function
        InSync -> "InSync"
      | ToDownload -> "ToDownload"
      | ToUpload -> "ToUpload"
      | NotFound -> "NotFound"

    let of_string = function
        "InSync" -> InSync
      | "ToDownload" -> ToDownload
      | "ToUpload" -> ToUpload
      | "NotFound" -> NotFound
      | s -> failwith ("Resource state unexpected: " ^ s)

  end

  type t = {
    (* rowid *)
    id : int64;
    (* remote data *)
    etag : string option;
    remote_id : string option;
    title : string option;
    mime_type : string option;
    created_date : float option;
    modified_date : float option;
    last_viewed_by_me_date : float option;
    parent_remote_ids : string option;
    download_url : string option;
    export_links : string option;
    file_extension : string option;
    md5_checksum : string option;
    file_size : int64 option;
    editable : bool option;
    trashed : bool option;
    alternate_link : string option;
    (* local data *)
    parent_path : string;
    path : string;
    state : State.t;
    change_id : int64;
    last_update : float;
  }

  let id = {
    GapiLens.get = (fun x -> x.id);
    GapiLens.set = (fun v x -> { x with id = v })
  }
  let etag = {
    GapiLens.get = (fun x -> x.etag);
    GapiLens.set = (fun v x -> { x with etag = v })
  }
  let remote_id = {
    GapiLens.get = (fun x -> x.remote_id);
    GapiLens.set = (fun v x -> { x with remote_id = v })
  }
  let title = {
    GapiLens.get = (fun x -> x.title);
    GapiLens.set = (fun v x -> { x with title = v })
  }
  let mime_type = {
    GapiLens.get = (fun x -> x.mime_type);
    GapiLens.set = (fun v x -> { x with mime_type = v })
  }
  let created_date = {
    GapiLens.get = (fun x -> x.created_date);
    GapiLens.set = (fun v x -> { x with created_date = v })
  }
  let modified_date = {
    GapiLens.get = (fun x -> x.modified_date);
    GapiLens.set = (fun v x -> { x with modified_date = v })
  }
  let last_viewed_by_me_date = {
    GapiLens.get = (fun x -> x.last_viewed_by_me_date);
    GapiLens.set = (fun v x -> { x with last_viewed_by_me_date = v })
  }
  let parent_remote_ids = {
    GapiLens.get = (fun x -> x.parent_remote_ids);
    GapiLens.set = (fun v x -> { x with parent_remote_ids = v })
  }
  let download_url = {
    GapiLens.get = (fun x -> x.download_url);
    GapiLens.set = (fun v x -> { x with download_url = v })
  }
  let export_links = {
    GapiLens.get = (fun x -> x.export_links);
    GapiLens.set = (fun v x -> { x with export_links = v })
  }
  let file_extension = {
    GapiLens.get = (fun x -> x.file_extension);
    GapiLens.set = (fun v x -> { x with file_extension = v })
  }
  let md5_checksum = {
    GapiLens.get = (fun x -> x.md5_checksum);
    GapiLens.set = (fun v x -> { x with md5_checksum = v })
  }
  let file_size = {
    GapiLens.get = (fun x -> x.file_size);
    GapiLens.set = (fun v x -> { x with file_size = v })
  }
  let editable = {
    GapiLens.get = (fun x -> x.editable);
    GapiLens.set = (fun v x -> { x with editable = v })
  }
  let trashed = {
    GapiLens.get = (fun x -> x.trashed);
    GapiLens.set = (fun v x -> { x with trashed = v })
  }
  let alternate_link = {
    GapiLens.get = (fun x -> x.alternate_link);
    GapiLens.set = (fun v x -> { x with alternate_link = v })
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
  let change_id = {
    GapiLens.get = (fun x -> x.change_id);
    GapiLens.set = (fun v x -> { x with change_id = v })
  }
  let last_update = {
    GapiLens.get = (fun x -> x.last_update);
    GapiLens.set = (fun v x -> { x with last_update = v })
  }

  (* Export links *)
  let render_export_links export_links =
    let buffer = Buffer.create 64 in
    List.iter
      (fun (fmt, link) ->
         let s = Printf.sprintf "%S:%S;" fmt link in
         Buffer.add_string buffer s)
      export_links;
    Buffer.contents buffer

  let parse_export_links s =
    let sb = Scanf.Scanning.from_string s in
    let export_links = ref [] in
    while (not (Scanf.Scanning.end_of_input sb)) do
      let (fmt, link) = Scanf.bscanf sb "%S:%S;" (fun f l -> (f, l)) in
      export_links := (fmt, link) :: !export_links
    done;
    !export_links

  (* Parent remote ids *)
  let render_parent_remote_ids parents =
    let buffer = Buffer.create 64 in
    List.iter
      (fun parent ->
         let s =
           Printf.sprintf "%S;" parent.GapiDriveV2Model.ParentReference.id in
         Buffer.add_string buffer s)
      parents;
    Buffer.contents buffer

  let parse_parent_remote_ids s =
    let sb = Scanf.Scanning.from_string s in
    let remote_ids = ref [] in
    while (not (Scanf.Scanning.end_of_input sb)) do
      let id = Scanf.bscanf sb "%S;" (fun x -> x) in
      remote_ids := id :: !remote_ids
    done;
    !remote_ids

  (* Queries *)
  let bind_resource_parameters stmt resource =
    bind_text stmt ":etag" resource.etag;
    bind_text stmt ":remote_id" resource.remote_id;
    bind_text stmt ":title" resource.title;
    bind_text stmt ":mime_type" resource.mime_type;
    bind_float stmt ":created_date" resource.created_date;
    bind_float stmt ":modified_date" resource.modified_date;
    bind_float stmt ":last_viewed_by_me_date" resource.last_viewed_by_me_date;
    bind_text stmt ":parent_remote_ids" resource.parent_remote_ids;
    bind_text stmt ":download_url" resource.download_url;
    bind_text stmt ":export_links" resource.export_links;
    bind_text stmt ":file_extension" resource.file_extension;
    bind_text stmt ":md5_checksum" resource.md5_checksum;
    bind_int stmt ":file_size" resource.file_size;
    bind_bool stmt ":editable" resource.editable;
    bind_bool stmt ":trashed" resource.trashed;
    bind_text stmt ":alternate_link" resource.alternate_link;
    bind_text stmt ":parent_path" (Some resource.parent_path);
    bind_text stmt ":path" (Some resource.path);
    bind_text stmt ":state" (Some (State.to_string resource.state));
    bind_int stmt ":change_id" (Some resource.change_id);
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
    with_db cache
      (fun db ->
         let stmt = ResourceStmts.prepare_update_stmt db in
         bind_resource_parameters stmt resource;
         bind_int stmt ":id" (Some resource.id);
         final_step stmt;
         finalize_stmt stmt)

  let _delete_resource stmt id =
    reset_stmt stmt;
    bind_int stmt ":id" (Some id);
    final_step stmt

  let delete_resource cache resource =
    with_db cache
      (fun db ->
         let stmt = ResourceStmts.prepare_delete_stmt db in
         _delete_resource stmt resource.id;
         finalize_stmt stmt)

  let delete_not_found_resource_with_path cache path =
    with_db cache
      (fun db ->
         let stmt = ResourceStmts.prepare_delete_not_found_with_path_stmt db in
         reset_stmt stmt;
         bind_text stmt ":path" (Some path);
         final_step stmt;
         finalize_stmt stmt)

  let _delete_resources_with_parent_path db parent_path change_id trashed =
    let stmt = ResourceStmts.prepare_delete_with_parent_path_stmt db in
    bind_text stmt ":parent_path" (Some parent_path);
    bind_int stmt ":change_id" (Some change_id);
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

  let insert_resources cache resources parent_path change_id trashed =
    with_transaction cache
      (fun db ->
         _delete_resources_with_parent_path db parent_path change_id trashed;
         let stmt = ResourceStmts.prepare_insert_stmt db in
         let results =
           List.map
             (step_insert_resource db stmt)
             resources in
         finalize_stmt stmt;
         results)

  let invalidate_resources cache ids change_id =
    with_transaction cache
      (fun db ->
         let all_stmt = ResourceStmts.prepare_update_all_change_ids db in
         bind_int all_stmt ":change_id" (Some change_id);
         final_step all_stmt;
         finalize_stmt all_stmt;
         let stmt = ResourceStmts.prepare_invalidate_stmt db in
         List.iter
           (fun id ->
              reset_stmt stmt;
              bind_int stmt ":change_id" (Some change_id);
              bind_int stmt ":id" (Some id);
              final_step stmt)
           ids;
         finalize_stmt stmt)

  let invalidate_trash_bin cache change_id =
    with_db cache
      (fun db ->
         let stmt = ResourceStmts.prepare_invalidate_trash_bin_stmt db in
         reset_stmt stmt;
         bind_int stmt ":change_id" (Some change_id);
         final_step stmt;
         finalize_stmt stmt)

  let trash_resources cache resources change_id =
    with_transaction cache
      (fun db ->
         let stmt = ResourceStmts.prepare_trash_stmt db in
         List.iter
           (fun resource ->
              reset_stmt stmt;
              bind_int stmt ":change_id" (Some change_id);
              bind_int stmt ":id" (Some resource.id);
              final_step stmt)
           resources;
         finalize_stmt stmt)

  let delete_all_with_parent_path cache parent_path trashed =
    with_db cache
      (fun db ->
         let stmt = ResourceStmts.prepare_delete_all_with_parent_path db in
         reset_stmt stmt;
         bind_text stmt ":parent_path" (Some (parent_path ^ "%"));
         bind_bool stmt ":trashed" (Some trashed);
         final_step stmt;
         finalize_stmt stmt)

  let trash_all_with_parent_path cache parent_path =
    with_db cache
      (fun db ->
         let stmt = ResourceStmts.prepare_trash_all_with_parent_path db in
         reset_stmt stmt;
         bind_text stmt ":parent_path" (Some (parent_path ^ "%"));
         final_step stmt;
         finalize_stmt stmt)

  let row_to_resource row_data =
    { id = row_data.(0) |> data_to_int64 |> Option.get;
      etag = row_data.(1) |> data_to_string;
      remote_id = row_data.(2) |> data_to_string;
      title = row_data.(3) |> data_to_string;
      mime_type = row_data.(4) |> data_to_string;
      created_date = row_data.(5) |> data_to_float;
      modified_date = row_data.(6) |> data_to_float;
      last_viewed_by_me_date = row_data.(7) |> data_to_float;
      parent_remote_ids = row_data.(8) |> data_to_string;
      download_url = row_data.(9) |> data_to_string;
      export_links = row_data.(10) |> data_to_string;
      file_extension = row_data.(11) |> data_to_string;
      md5_checksum = row_data.(12) |> data_to_string;
      file_size = row_data.(13) |> data_to_int64;
      editable = row_data.(14) |> data_to_bool;
      trashed = row_data.(15) |> data_to_bool;
      alternate_link = row_data.(16) |> data_to_string;
      parent_path = row_data.(17) |> data_to_string |> Option.get;
      path = row_data.(18) |> data_to_string |> Option.get;
      state = row_data.(19) |> data_to_string |> Option.get |> State.of_string;
      change_id = row_data.(20) |> data_to_int64 |> Option.get;
      last_update = row_data.(21) |> data_to_float |> Option.get;
    }

  let select_resource cache prepare bind =
    with_db cache
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
    with_db cache
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
    with_db cache
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
      | "application/vnd.google-apps.spreadsheet" -> true
      | _ -> false

  let is_document resource =
    match resource.mime_type with
        Some mime_type -> is_document_mime_type mime_type
      | _ -> false

  let is_valid resource largest_change_id =
    resource.change_id >= largest_change_id

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
      | _ -> "html"

  let get_format resource config =
    match resource.mime_type with
        Some mime_type -> get_format_from_mime_type mime_type config
      | _ -> "html"

  let mime_type_of_format fmt =
    match fmt with
        "doc" -> "application/msword"
      | "html" -> "text/html"
      | "jpeg" -> "image/jpeg"
      | "ods" -> "application/x-vnd.oasis.opendocument.spreadsheet"
      | "odt" -> "application/vnd.oasis.opendocument.text"
      | "pdf" -> "application/pdf"
      | "png" -> "image/png"
      | "pptx" -> "application/vnd.openxmlformats-officedocument.presentationml.presentation"
      | "rtf" -> "application/rtf"
      | "svg" -> "image/svg+xml"
      | "txt" -> "text/plain"
      | "xls" -> "application/vnd.ms-excel"
      | _ -> failwith ("Unsupported format: " ^ fmt)

end

module Metadata =
struct
  type t = {
    etag : string;
    username : string;
    quota_bytes_total : int64;
    quota_bytes_used : int64;
    largest_change_id : int64;
    remaining_change_ids : int64;
    root_folder_id : string;
    permission_id : string;
    cache_size : int64;
    last_update : float;
  }

  let etag = {
    GapiLens.get = (fun x -> x.etag);
    GapiLens.set = (fun v x -> { x with etag = v })
  }
  let username = {
    GapiLens.get = (fun x -> x.username);
    GapiLens.set = (fun v x -> { x with username = v })
  }
  let quota_bytes_total = {
    GapiLens.get = (fun x -> x.quota_bytes_total);
    GapiLens.set = (fun v x -> { x with quota_bytes_total = v })
  }
  let quota_bytes_used = {
    GapiLens.get = (fun x -> x.quota_bytes_used);
    GapiLens.set = (fun v x -> { x with quota_bytes_used = v })
  }
  let largest_change_id = {
    GapiLens.get = (fun x -> x.largest_change_id);
    GapiLens.set = (fun v x -> { x with largest_change_id = v })
  }
  let remaining_change_ids = {
    GapiLens.get = (fun x -> x.remaining_change_ids);
    GapiLens.set = (fun v x -> { x with remaining_change_ids = v })
  }
  let root_folder_id = {
    GapiLens.get = (fun x -> x.root_folder_id);
    GapiLens.set = (fun v x -> { x with root_folder_id = v })
  }
  let permission_id = {
    GapiLens.get = (fun x -> x.permission_id);
    GapiLens.set = (fun v x -> { x with permission_id = v })
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
    bind_text stmt ":etag" (Some metadata.etag);
    bind_text stmt ":username" (Some metadata.username);
    bind_int stmt ":quota_bytes_total" (Some metadata.quota_bytes_total);
    bind_int stmt ":quota_bytes_used" (Some metadata.quota_bytes_used);
    bind_int stmt ":largest_change_id" (Some metadata.largest_change_id);
    bind_int stmt ":remaining_change_ids" (Some metadata.remaining_change_ids);
    bind_text stmt ":root_folder_id" (Some metadata.root_folder_id);
    bind_text stmt ":permission_id" (Some metadata.permission_id);
    bind_int stmt ":cache_size" (Some metadata.cache_size);
    bind_float stmt ":last_update" (Some metadata.last_update);
    final_step stmt

  let insert_metadata cache resource =
    with_db cache
      (fun db ->
         let stmt = MetadataStmts.prepare_insert_stmt db in
         save_metadata stmt resource;
         finalize_stmt stmt)

  let row_to_metadata row_data =
    { etag = row_data.(0) |> data_to_string |> Option.get;
      username = row_data.(1) |> data_to_string |> Option.get;
      quota_bytes_total = row_data.(2) |> data_to_int64 |> Option.get;
      quota_bytes_used = row_data.(3) |> data_to_int64 |> Option.get;
      largest_change_id = row_data.(4) |> data_to_int64 |> Option.get;
      remaining_change_ids = row_data.(5) |> data_to_int64 |> Option.get;
      root_folder_id = row_data.(6) |> data_to_string |> Option.get;
      permission_id = row_data.(7) |> data_to_string |> Option.get;
      cache_size = row_data.(8) |> data_to_int64 |> Option.get;
      last_update = row_data.(9) |> data_to_float |> Option.get;
    }

  let select_metadata cache =
    with_db cache
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
       Utils.log_message "Removing file (%s: resource %Ld) from cache...%!"
         content_path resource.Resource.id;
       let size = remove_file content_path in
       let new_size = Int64.add total_size size in
       Utils.log_message "done\n%!";
       new_size)
    0L
    resources

(* Setup *)
let setup_db cache =
  with_db cache
    (fun db ->
      wrap_exec_not_null_no_headers db
        "BEGIN TRANSACTION; \
         CREATE TABLE IF NOT EXISTS resource ( \
            id INTEGER PRIMARY KEY, \
            etag TEXT NULL, \
            remote_id TEXT NULL, \
            title TEXT NULL, \
            mime_type TEXT NULL, \
            created_date REAL NULL, \
            modified_date REAL NULL, \
            last_viewed_by_me_date REAL NULL, \
            parent_remote_ids TEXT NULL, \
            download_url TEXT NULL, \
            export_links TEXT NULL, \
            file_extension TEXT NULL, \
            md5_checksum TEXT NULL, \
            file_size INTEGER NULL, \
            editable INTEGER NULL, \
            trashed INTEGER NULL, \
            alternate_link TEXT NULL, \
            parent_path TEXT NOT NULL, \
            path TEXT NOT NULL, \
            state TEXT NOT NULL, \
            change_id INTEGER NOT NULL, \
            last_update REAL NOT NULL \
         ); \
         CREATE INDEX IF NOT EXISTS path_index ON resource (path, trashed); \
         CREATE INDEX IF NOT EXISTS parent_path_index ON resource (parent_path, trashed); \
         CREATE INDEX IF NOT EXISTS remote_id_index ON resource (remote_id); \
         CREATE INDEX IF NOT EXISTS last_update_index ON resource (last_update); \
         CREATE TABLE IF NOT EXISTS metadata ( \
            id INTEGER PRIMARY KEY, \
            etag TEXT NOT NULL, \
            username TEXT NOT NULL, \
            quota_bytes_total INTEGER NOT NULL, \
            quota_bytes_used INTEGER NOT NULL, \
            largest_change_id INTEGER NOT NULL, \
            remaining_change_ids INTEGER NOT NULL, \
            root_folder_id TEXT NOT NULL, \
            permission_id TEXT NOT NULL, \
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
           Utils.log_message "Error removing file %s: %s\n%!"
             file (Printexc.to_string e))
      (Sys.readdir cache.cache_dir)
  end

