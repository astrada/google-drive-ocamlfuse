open Utils.Infix
open GapiLens.Infix

(* Record:
 * id: INTEGER PRIMARY KEY (int64)
 *
 * resourceId : TEXT (string)
 * category : TEXT (string) : folder, document, spreadsheet, pdf, file, ...
 * remoteId : TEXT (string)
 * extension : TEXT (string)
 * changestamp : INT (int)
 *
 * path : TEXT (string) <- TODO: normalize path
 * state : TEXT (string) : in_sync, to_upload, to_download, conflict, error?
 *
 * Indexes: path, resourceId, remoteId
 *)
type t = {
  cache_dir : string;
  db : Sqlite3.db;
  begin_tran_stmt : Sqlite3.stmt;
  commit_tran_stmt : Sqlite3.stmt;
  insert_stmt : Sqlite3.stmt;
  update_stmt : Sqlite3.stmt;
  delete_stmt : Sqlite3.stmt;
  select_path_stmt : Sqlite3.stmt;
}

module ResourceState =
struct
  type t =
      InSync
    | ToDownload
    | ToDelete
    | Conflict
    | Error

  let to_string = function
      InSync -> "InSync"
    | ToDownload -> "ToDownload"
    | ToDelete -> "ToDelete"
    | Conflict -> "Conflict"
    | Error -> "Error"

  let of_string = function
      "InSync" -> InSync
    | "ToDownload" -> ToDownload
    | "ToDelete" -> ToDelete
    | "Conflict" -> Conflict
    | "Error" -> Error
    | s -> failwith ("Resource state unexpected: " ^ s)

end

type resource = {
  (* rowid *)
  id : int64;
  (* remote data *)
  resource_id : string option;
  category : string option;
  remote_id : string option;
  extension : string option;
  changestamp : int64 option;
  (* local data *)
  path : string;
  state : ResourceState.t;
  last_update : float;
}

let id = {
  GapiLens.get = (fun x -> x.id);
  GapiLens.set = (fun v x -> { x with id = v })
}
let resource_id = {
  GapiLens.get = (fun x -> x.resource_id);
  GapiLens.set = (fun v x -> { x with resource_id = v })
}
let category = {
  GapiLens.get = (fun x -> x.category);
  GapiLens.set = (fun v x -> { x with category = v })
}
let remote_id = {
  GapiLens.get = (fun x -> x.remote_id);
  GapiLens.set = (fun v x -> { x with remote_id = v })
}
let extension = {
  GapiLens.get = (fun x -> x.extension);
  GapiLens.set = (fun v x -> { x with extension = v })
}
let changestamp = {
  GapiLens.get = (fun x -> x.changestamp);
  GapiLens.set = (fun v x -> { x with changestamp = v })
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

(* Prepare SQL *)
let prepare_begin_tran_stmt db =
  Sqlite3.prepare db "BEGIN TRANSACTION;"

let prepare_commit_tran_stmt db =
  Sqlite3.prepare db "COMMIT TRANSACTION;"

let prepare_insert_stmt db =
  let sql =
    "INSERT INTO resource ( \
       resource_id, \
       category, \
       remote_id, \
       changestamp, \
       path, \
       state, \
       last_update \
     ) \
     VALUES ( \
       :resource_id, \
       :category, \
       :remote_id, \
       :changestamp, \
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
        resource_id = :resource_id, \
        category = :category, \
        remote_id = :remote_id, \
        changestamp = :changestamp, \
        path = :path, \
        state = :state, \
        last_update = :last_update \
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

let prepare_select_path_stmt db =
  let sql =
    "SELECT \
       id, \
       resource_id, \
       category, \
       remote_id, \
       changestamp, \
       path, \
       state, \
       last_update \
     FROM resource \
     WHERE path = :path;"
  in
    Sqlite3.prepare db sql
(* END Prepare SQL *)

(* Queries *)
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

let save_resource stmt resource =
  Sqlite3.reset stmt |> fail_if_not_ok;
  bind_int stmt "id" (Some resource.id);
  bind_text stmt "resource_id" resource.resource_id;
  bind_text stmt "category" resource.category;
  bind_text stmt "remote_id" resource.remote_id;
  bind_int stmt "changestamp" resource.changestamp;
  bind_text stmt "path" (Some resource.path);
  bind_text stmt "state" (Some (ResourceState.to_string resource.state));
  bind_float stmt "last_update" (Some resource.last_update);
  Sqlite3.step stmt |> expect Sqlite3.Rc.DONE

let insert_resource cache resource =
  let stmt = cache.insert_stmt in
    save_resource stmt resource;
    resource |> id ^= Sqlite3.last_insert_rowid cache.db

let update_resource cache resource =
  let stmt = cache.update_stmt in
    save_resource stmt resource

let delete_resource cache resource =
  let stmt = cache.delete_stmt in
    Sqlite3.reset stmt |> fail_if_not_ok;
    bind_int stmt "id" (Some resource.id);
    Sqlite3.step stmt |> expect Sqlite3.Rc.DONE

let insert_resources resources cache =
  Sqlite3.reset cache.begin_tran_stmt |> fail_if_not_ok;
  Sqlite3.step cache.begin_tran_stmt |> expect Sqlite3.Rc.DONE;
  let results =
    List.map
      (insert_resource cache)
      resources in
  Sqlite3.reset cache.commit_tran_stmt |> fail_if_not_ok;
  Sqlite3.step cache.begin_tran_stmt |> expect Sqlite3.Rc.DONE;
  results

let data_to_int64 = function
    Sqlite3.Data.NULL -> None
  | Sqlite3.Data.INT v -> Some v
  | _ -> failwith "data_to_int64: data does not contain an INT value"

let data_to_string = function
    Sqlite3.Data.NULL -> None
  | Sqlite3.Data.TEXT v -> Some v
  | _ -> failwith "data_to_string: data does not contain a TEXT value"

let data_to_float = function
    Sqlite3.Data.NULL -> None
  | Sqlite3.Data.FLOAT v -> Some v
  | _ -> failwith "data_to_float: data does not contain a FLOAT value"

let row_to_resource row_data =
  { id = row_data.(0) |> data_to_int64 |> Option.get;
    resource_id = row_data.(1) |> data_to_string;
    category = row_data.(2) |> data_to_string;
    remote_id = row_data.(3) |> data_to_string;
    extension = row_data.(4) |> data_to_string;
    changestamp = row_data.(5) |> data_to_int64;
    path = row_data.(6) |> data_to_string |> Option.get;
    state = row_data.(7)
              |> data_to_string |> Option.get |> ResourceState.of_string;
    last_update = row_data.(8) |> data_to_float |> Option.get;
  }

let select_resource_with_path path cache =
  let stmt = cache.select_path_stmt in
  Sqlite3.reset stmt |> fail_if_not_ok;
  bind_text stmt "path" (Some path);
  let rc = Sqlite3.step stmt in
    match rc with
        Sqlite3.Rc.ROW ->
          Some (Sqlite3.row_data stmt |> row_to_resource)
      | Sqlite3.Rc.DONE -> None
      | _ -> fail rc
(* END Queries *)

(* Setup *)
let setup_db app_dir =
  let cache_dir = app_dir.AppDir.cache_dir in
  let filename = Filename.concat cache_dir "cache.db" in
  let db = Sqlite3.db_open filename in
  let _ = wrap_exec_not_null_no_headers db
    "CREATE TABLE IF NOT EXISTS resource ( \
        id PRIMARY INTEGER KEY, \
        resource_id TEXT NULL, \
        category TEXT NULL, \
        remote_id TEXT NULL, \
        changestamp INTEGER NULL, \
        path TEXT NOT NULL, \
        state TEXT NOT NULL, \
        last_update REAL NOT NULL \
     ); \
     CREATE INDEX IF NOT EXISTS path_index ON resource (path); \
     CREATE INDEX IF NOT EXISTS resource_id_index ON resource (resource_id); \
     CREATE INDEX IF NOT EXISTS remote_id_index ON resource (remote_id);" in
  let begin_tran_stmt = prepare_begin_tran_stmt db in
  let commit_tran_stmt = prepare_commit_tran_stmt db in
  let insert_stmt = prepare_insert_stmt db in
  let update_stmt = prepare_update_stmt db in
  let delete_stmt = prepare_delete_stmt db in
  let select_path_stmt = prepare_insert_stmt db in
    { cache_dir;
      db;
      begin_tran_stmt;
      commit_tran_stmt;
      insert_stmt;
      update_stmt;
      delete_stmt;
      select_path_stmt;
    }
(* END Setup *)

(* Teardown *)
let close_db cache =
  try
    Sqlite3.finalize cache.begin_tran_stmt |> ignore;
    Sqlite3.finalize cache.commit_tran_stmt |> ignore;
    Sqlite3.finalize cache.insert_stmt |> ignore;
    Sqlite3.finalize cache.update_stmt |> ignore;
    Sqlite3.finalize cache.delete_stmt |> ignore;
    Sqlite3.finalize cache.select_path_stmt |> ignore;
    (* TODO: handle busy db (close_db returns false) *)
    Sqlite3.db_close cache.db
  with _ -> false
(* END Teardown *)

