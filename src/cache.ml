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

let get_next_row stmt row_to_data =
  let rc = Sqlite3.step stmt in
    match rc with
        Sqlite3.Rc.ROW ->
          Some (Sqlite3.row_data stmt |> row_to_data)
      | Sqlite3.Rc.DONE -> None
      | _ -> fail rc

let select_first_row stmt bind_parameters row_to_data =
  Sqlite3.reset stmt |> fail_if_not_ok;
  bind_parameters stmt;
  get_next_row stmt row_to_data

let select_all_rows stmt bind_parameters row_to_data =
  Sqlite3.reset stmt |> fail_if_not_ok;
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
(* END Prepare SQL *)

module ResourceStmts =
struct
  type t = {
    insert_stmt : Sqlite3.stmt;
    update_stmt : Sqlite3.stmt;
    delete_stmt : Sqlite3.stmt;
    delete_with_parent_path_stmt : Sqlite3.stmt;
    select_with_path_stmt : Sqlite3.stmt;
    select_with_parent_path_stmt : Sqlite3.stmt;
  }

  let prepare_insert_stmt db =
    let sql =
      "INSERT INTO resource ( \
         resource_id, \
         kind, \
         md5_checksum, \
         size, \
         last_viewed, \
         last_modified, \
         parent_path, \
         path, \
         state, \
         changestamp, \
         last_update \
       ) \
       VALUES ( \
         :resource_id, \
         :kind, \
         :md5_checksum, \
         :size, \
         :last_viewed, \
         :last_modified, \
         :parent_path, \
         :path, \
         :state, \
         :changestamp, \
         :last_update \
       );"
    in
      Sqlite3.prepare db sql

  let prepare_update_stmt db =
    let sql =
      "UPDATE resource \
        SET \
          resource_id = :resource_id, \
          kind = :kind, \
          md5_checksum = :md5_checksum, \
          last_viewed = :last_viewed, \
          last_modified = :last_modified, \
          parent_path = :parent_path, \
          path = :path, \
          state = :state, \
          changestamp = :changestamp, \
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

  let prepare_delete_with_parent_path_stmt db =
    let sql =
      "DELETE \
       FROM resource \
       WHERE parent_path = :parent_path;"
    in
      Sqlite3.prepare db sql

  let prepare_select_with_path_stmt db =
    let sql =
      "SELECT \
         id, \
         resource_id, \
         kind, \
         md5_checksum, \
         size, \
         last_viewed, \
         last_modified, \
         parent_path, \
         path, \
         state, \
         changestamp, \
         last_update \
       FROM resource \
       WHERE path = :path;"
    in
      Sqlite3.prepare db sql

  let prepare_select_with_parent_path_stmt db =
    let sql =
      "SELECT \
         id, \
         resource_id, \
         kind, \
         md5_checksum, \
         size, \
         last_viewed, \
         last_modified, \
         parent_path, \
         path, \
         state, \
         changestamp, \
         last_update \
       FROM resource \
       WHERE parent_path = :parent_path \
         AND state <> 'NotFound';"
    in
      Sqlite3.prepare db sql

  let create db = {
    insert_stmt = prepare_insert_stmt db;
    update_stmt = prepare_update_stmt db;
    delete_stmt = prepare_delete_stmt db;
    delete_with_parent_path_stmt = prepare_delete_with_parent_path_stmt db;
    select_with_path_stmt = prepare_select_with_path_stmt db;
    select_with_parent_path_stmt = prepare_select_with_parent_path_stmt db;
  }

  let finalize stmts =
    Sqlite3.finalize stmts.insert_stmt |> ignore;
    Sqlite3.finalize stmts.update_stmt |> ignore;
    Sqlite3.finalize stmts.delete_stmt |> ignore;
    Sqlite3.finalize stmts.delete_with_parent_path_stmt |> ignore;
    Sqlite3.finalize stmts.select_with_path_stmt |> ignore;
    Sqlite3.finalize stmts.select_with_parent_path_stmt |> ignore

end

module MetadataStmts =
struct
  type t = {
    insert_stmt : Sqlite3.stmt;
    select_stmt : Sqlite3.stmt;
  }

  let prepare_insert_stmt db =
    let sql =
      "INSERT OR REPLACE INTO metadata ( \
         id, \
         largest_changestamp, \
         remaining_changestamps, \
         quota_bytes_total, \
         quota_bytes_used, \
         last_update \
       ) \
       VALUES ( \
         1, \
         :largest_changestamp, \
         :remaining_changestamps, \
         :quota_bytes_total, \
         :quota_bytes_used, \
         :last_update \
       );"
    in
      Sqlite3.prepare db sql

  let prepare_select_stmt db =
    let sql =
      "SELECT \
         largest_changestamp, \
         remaining_changestamps, \
         quota_bytes_total, \
         quota_bytes_used, \
         last_update \
       FROM metadata \
       WHERE id = 1;"
    in
      Sqlite3.prepare db sql

  let create db = {
    insert_stmt = prepare_insert_stmt db;
    select_stmt = prepare_select_stmt db;
  }

  let finalize stmts =
    Sqlite3.finalize stmts.insert_stmt |> ignore;
    Sqlite3.finalize stmts.select_stmt |> ignore

end

type t = {
  cache_dir : string;
  db : Sqlite3.db;
  (* Query cache *)
  begin_tran_stmt : Sqlite3.stmt;
  commit_tran_stmt : Sqlite3.stmt;
  resource_stmts : ResourceStmts.t;
  metadata_stmts : MetadataStmts.t;
}

module Resource =
struct
  module State =
  struct
    type t =
        InSync
      | ToDownload
      | ToDelete
      | Conflict
      | NotFound

    let to_string = function
        InSync -> "InSync"
      | ToDownload -> "ToDownload"
      | ToDelete -> "ToDelete"
      | Conflict -> "Conflict"
      | NotFound -> "NotFound"

    let of_string = function
        "InSync" -> InSync
      | "ToDownload" -> ToDownload
      | "ToDelete" -> ToDelete
      | "Conflict" -> Conflict
      | "NotFound" -> NotFound
      | s -> failwith ("Resource state unexpected: " ^ s)

  end

  (* TODO:
   * add table of directory content (?)
   *)
  type t = {
    (* rowid *)
    id : int64;
    (* remote data *)
    resource_id : string option;
    kind : string option;
    md5_checksum : string option;
    size : int64 option;
    last_viewed : float option;
    last_modified : float option;
    (* local data *)
    parent_path : string;
    path : string;
    state : State.t;
    changestamp : int64;
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
  let kind = {
    GapiLens.get = (fun x -> x.kind);
    GapiLens.set = (fun v x -> { x with kind = v })
  }
	let md5_checksum = {
		GapiLens.get = (fun x -> x.md5_checksum);
		GapiLens.set = (fun v x -> { x with md5_checksum = v })
	}
	let size = {
		GapiLens.get = (fun x -> x.size);
		GapiLens.set = (fun v x -> { x with size = v })
	}
	let last_viewed = {
		GapiLens.get = (fun x -> x.last_viewed);
		GapiLens.set = (fun v x -> { x with last_viewed = v })
	}
	let last_modified = {
		GapiLens.get = (fun x -> x.last_modified);
		GapiLens.set = (fun v x -> { x with last_modified = v })
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
  let changestamp = {
    GapiLens.get = (fun x -> x.changestamp);
    GapiLens.set = (fun v x -> { x with changestamp = v })
  }
  let last_update = {
    GapiLens.get = (fun x -> x.last_update);
    GapiLens.set = (fun v x -> { x with last_update = v })
  }

  (* Queries *)
  let bind_resource_parameters stmt resource =
    bind_text stmt ":resource_id" resource.resource_id;
    bind_text stmt ":kind" resource.kind;
    bind_text stmt ":md5_checksum" resource.md5_checksum;
    bind_int stmt ":size" resource.size;
    bind_float stmt ":last_viewed" resource.last_viewed;
    bind_float stmt ":last_modified" resource.last_modified;
    bind_text stmt ":parent_path" (Some resource.parent_path);
    bind_text stmt ":path" (Some resource.path);
    bind_text stmt ":state" (Some (State.to_string resource.state));
    bind_int stmt ":changestamp" (Some resource.changestamp);
    bind_float stmt ":last_update" (Some resource.last_update)

  let insert_resource cache resource =
    let stmt = cache.resource_stmts.ResourceStmts.insert_stmt in
      Sqlite3.reset stmt |> fail_if_not_ok;
      bind_resource_parameters stmt resource;
      Sqlite3.step stmt |> expect Sqlite3.Rc.DONE;
      resource |> id ^= Sqlite3.last_insert_rowid cache.db

  let update_resource cache resource =
    let stmt = cache.resource_stmts.ResourceStmts.update_stmt in
      Sqlite3.reset stmt |> fail_if_not_ok;
      bind_resource_parameters stmt resource;
      bind_int stmt ":id" (Some resource.id);
      Sqlite3.step stmt |> expect Sqlite3.Rc.DONE

  let delete_resource cache resource =
    let stmt = cache.resource_stmts.ResourceStmts.delete_stmt in
      Sqlite3.reset stmt |> fail_if_not_ok;
      bind_int stmt ":id" (Some resource.id);
      Sqlite3.step stmt |> expect Sqlite3.Rc.DONE

  let delete_resources cache parent_path =
    let stmt =
      cache.resource_stmts.ResourceStmts.delete_with_parent_path_stmt
    in
      Sqlite3.reset stmt |> fail_if_not_ok;
      bind_text stmt ":parent_path" (Some parent_path);
      Sqlite3.step stmt |> expect Sqlite3.Rc.DONE

  let insert_resources cache resources parent_path =
    Sqlite3.reset cache.begin_tran_stmt |> fail_if_not_ok;
    Sqlite3.step cache.begin_tran_stmt |> expect Sqlite3.Rc.DONE;
    delete_resources cache parent_path;
    let results =
      List.map
        (insert_resource cache)
        resources in
    Sqlite3.reset cache.commit_tran_stmt |> fail_if_not_ok;
    Sqlite3.step cache.begin_tran_stmt |> expect Sqlite3.Rc.DONE;
    results

  let row_to_resource row_data =
    { id = row_data.(0) |> data_to_int64 |> Option.get;
      resource_id = row_data.(1) |> data_to_string;
      kind = row_data.(2) |> data_to_string;
      md5_checksum = row_data.(3) |> data_to_string;
      size = row_data.(4) |> data_to_int64;
      last_viewed = row_data.(5) |> data_to_float;
      last_modified = row_data.(6) |> data_to_float;
      parent_path = row_data.(7) |> data_to_string |> Option.get;
      path = row_data.(8) |> data_to_string |> Option.get;
      state = row_data.(9) |> data_to_string |> Option.get |> State.of_string;
      changestamp = row_data.(10) |> data_to_int64 |> Option.get;
      last_update = row_data.(11) |> data_to_float |> Option.get;
    }

  let select_resource_with_path cache path =
    let stmt = cache.resource_stmts.ResourceStmts.select_with_path_stmt in
      select_first_row stmt
        (fun stmt -> bind_text stmt ":path" (Some path))
        row_to_resource

  let select_resources_with_parent_path cache parent_path =
    let stmt =
      cache.resource_stmts.ResourceStmts.select_with_parent_path_stmt in
      select_all_rows stmt
        (fun stmt -> bind_text stmt ":parent_path" (Some parent_path))
        row_to_resource
  (* END Queries *)

  let is_folder resource =
    match resource.kind with
        Some "folder" -> true
      | _ -> false

  let is_valid resource largest_changestamp =
    resource.changestamp >= largest_changestamp

end

module Metadata =
struct
  type t = {
    largest_changestamp : int64;
    remaining_changestamps : int64;
    quota_bytes_total : int64;
    quota_bytes_used : int64;
    last_update : float;
  }

	let largest_changestamp = {
		GapiLens.get = (fun x -> x.largest_changestamp);
		GapiLens.set = (fun v x -> { x with largest_changestamp = v })
	}
	let remaining_changestamps = {
		GapiLens.get = (fun x -> x.remaining_changestamps);
		GapiLens.set = (fun v x -> { x with remaining_changestamps = v })
	}
	let quota_bytes_total = {
		GapiLens.get = (fun x -> x.quota_bytes_total);
		GapiLens.set = (fun v x -> { x with quota_bytes_total = v })
	}
	let quota_bytes_used = {
		GapiLens.get = (fun x -> x.quota_bytes_used);
		GapiLens.set = (fun v x -> { x with quota_bytes_used = v })
	}
	let last_update = {
		GapiLens.get = (fun x -> x.last_update);
		GapiLens.set = (fun v x -> { x with last_update = v })
	}

  (* Queries *)
  let save_metadata stmt metadata =
    Sqlite3.reset stmt |> fail_if_not_ok;
    bind_int stmt ":largest_changestamp" (Some metadata.largest_changestamp);
    bind_int stmt ":remaining_changestamps"
      (Some metadata.remaining_changestamps);
    bind_int stmt ":quota_bytes_total" (Some metadata.quota_bytes_total);
    bind_int stmt ":quota_bytes_used" (Some metadata.quota_bytes_used);
    bind_float stmt ":last_update" (Some metadata.last_update);
    Sqlite3.step stmt |> expect Sqlite3.Rc.DONE

  let insert_metadata cache resource =
    let stmt = cache.metadata_stmts.MetadataStmts.insert_stmt in
      save_metadata stmt resource

  let row_to_metadata row_data =
    { largest_changestamp = row_data.(0) |> data_to_int64 |> Option.get;
      remaining_changestamps = row_data.(1) |> data_to_int64 |> Option.get;
      quota_bytes_total = row_data.(2) |> data_to_int64 |> Option.get;
      quota_bytes_used = row_data.(3) |> data_to_int64 |> Option.get;
      last_update = row_data.(4) |> data_to_float |> Option.get;
    }

  let select_metadata cache =
    let stmt = cache.metadata_stmts.MetadataStmts.select_stmt in
      select_first_row stmt (fun _ -> ()) row_to_metadata
  (* END Queries *)

  let is_valid metadata_cache_time metadata =
    let now = Unix.gettimeofday () in
      now -. metadata.last_update <= float_of_int metadata_cache_time

end

(* Resource XML entry *)
let get_xml_entry_path cache id =
  let filename = Printf.sprintf "%Ld.xml" id in
    Filename.concat cache.cache_dir filename

let save_xml_entry cache id xml_string =
  let path = get_xml_entry_path cache id in
  let ch = open_out path in
    try
      output_string ch xml_string;
      close_out ch
    with e ->
      close_out ch;
      raise e

(* END Resource XML entry *)

(* Setup *)
let setup_db app_dir =
  let cache_dir = app_dir.AppDir.cache_dir in
  let filename = Filename.concat cache_dir "cache.db" in
  let db = Sqlite3.db_open filename in
  let _ = wrap_exec_not_null_no_headers db
    "CREATE TABLE IF NOT EXISTS resource ( \
        id INTEGER PRIMARY KEY, \
        resource_id TEXT NULL, \
        kind TEXT NULL, \
        md5_checksum TEXT NULL, \
        size INTEGER NULL, \
        last_viewed REAL NULL, \
        last_modified REAL NULL, \
        parent_path TEXT NOT NULL, \
        path TEXT NOT NULL, \
        state TEXT NOT NULL, \
        changestamp INTEGER NULL, \
        last_update REAL NOT NULL \
     ); \
     CREATE INDEX IF NOT EXISTS path_index ON resource (path); \
     CREATE INDEX IF NOT EXISTS parent_path_index ON resource (parent_path); \
     CREATE INDEX IF NOT EXISTS resource_id_index ON resource (resource_id); \
     CREATE TABLE IF NOT EXISTS metadata ( \
        id INTEGER PRIMARY KEY, \
        largest_changestamp INTEGER NOT NULL, \
        remaining_changestamps INTEGER NOT NULL, \
        quota_bytes_total INTEGER NOT NULL, \
        quota_bytes_used INTEGER NOT NULL, \
        last_update REAL NOT NULL \
     );" in
    { cache_dir;
      db;
      begin_tran_stmt = prepare_begin_tran_stmt db;
      commit_tran_stmt = prepare_commit_tran_stmt db;
      resource_stmts = ResourceStmts.create db;
      metadata_stmts = MetadataStmts.create db;
    }
(* END Setup *)

(* Teardown *)
let close_db cache =
  try
    Sqlite3.finalize cache.begin_tran_stmt |> ignore;
    Sqlite3.finalize cache.commit_tran_stmt |> ignore;
    ResourceStmts.finalize cache.resource_stmts;
    MetadataStmts.finalize cache.metadata_stmts;
    (* TODO: handle busy db (close_db returns false) *)
    Sqlite3.db_close cache.db
  with _ -> false
(* END Teardown *)

