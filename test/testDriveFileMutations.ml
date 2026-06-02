open OUnit
open GapiMonad

let session =
  {
    GapiConversation.Session.curl = GapiCurl.Initialized;
    config = GapiConfig.default;
    auth = GapiConversation.Session.NoAuth;
    cookies = [];
    etag = "";
  }

let run_session m = fst (m session)

let dummy_cache =
  {
    CacheData.cache_dir = "/tmp";
    db_path = "/tmp/test-cache.db";
    busy_timeout = 0;
    in_memory = true;
    autosaving_interval = 0;
  }

let default_runtime ?(config = Config.default) () =
  { DriveFileMutations.cache = dummy_cache; config }

let buffer_of_string s =
  let len = String.length s in
  let buffer = Bigarray.Array1.create Bigarray.char Bigarray.c_layout len in
  for i = 0 to len - 1 do
    buffer.{i} <- s.[i]
  done;
  buffer

let string_of_int64_list values =
  String.concat "," (List.map Int64.to_string values)

let index_of = DriveTestSupport.Trace.index_of

module FakePorts = struct
  let resources = Hashtbl.create 32
  let local_files = Hashtbl.create 32
  let trace = ref []
  let shrink_cache_calls = ref []

  let reset () =
    Hashtbl.reset resources;
    Hashtbl.reset local_files;
    trace := [];
    shrink_cache_calls := []

  let record event = trace := !trace @ [ event ]
  let key path trashed = Printf.sprintf "%b:%s" trashed path

  let add_resource resource =
    let trashed = Option.default false resource.CacheData.Resource.trashed in
    Hashtbl.replace resources
      (key resource.CacheData.Resource.path trashed)
      resource

  let content_path resource =
    "/tmp/" ^ Option.get resource.CacheData.Resource.remote_id

  let set_local_file resource size =
    Hashtbl.replace local_files (content_path resource) size

  let find_resource path trashed =
    try Some (Hashtbl.find resources (key path trashed))
    with Not_found -> None

  let find_by_id id =
    Hashtbl.to_seq_values resources
    |> List.of_seq
    |> List.find_opt (fun resource -> resource.CacheData.Resource.id = id)

  let replace_resource resource =
    let old_keys =
      Hashtbl.to_seq resources |> List.of_seq
      |> List.filter_map (fun (k, existing) ->
          if existing.CacheData.Resource.id = resource.CacheData.Resource.id
          then Some k
          else None)
    in
    List.iter (Hashtbl.remove resources) old_keys;
    add_resource resource

  let get_path_in_cache = Drive.get_path_in_cache

  let get_resource path trashed =
    record (Printf.sprintf "get_resource:%s:%b" path trashed);
    match find_resource path trashed with
    | Some resource -> SessionM.return resource
    | None -> Utils.raise_m Drive.File_not_found

  let ensure_local_content resource =
    record ("ensure:" ^ resource.CacheData.Resource.path);
    SessionM.return (content_path resource)

  let flush_memory_buffers resource =
    record ("flush:" ^ resource.CacheData.Resource.path)

  let write_to_memory_buffers _resource content_path buf offset =
    let bytes = Bigarray.Array1.dim buf in
    record
      (Printf.sprintf "write_to_memory:%s:%Ld:%d" content_path offset bytes);
    bytes

  let write_to_file content_path buf offset =
    let bytes = Bigarray.Array1.dim buf in
    record (Printf.sprintf "write_to_file:%s:%Ld:%d" content_path offset bytes);
    let top_offset = Int64.add offset (Int64.of_int bytes) in
    let current_size =
      Hashtbl.find_opt local_files content_path |> Option.default 0L
    in
    Hashtbl.replace local_files content_path (max current_size top_offset);
    bytes

  let truncate_local_file content_path size =
    record (Printf.sprintf "truncate:%s:%Ld" content_path size);
    Hashtbl.replace local_files content_path size

  let file_exists path = Hashtbl.mem local_files path

  let update_cached_resource _cache resource =
    record
      (Printf.sprintf "update:%s:%s" resource.CacheData.Resource.path
         (CacheData.Resource.State.to_string resource.CacheData.Resource.state));
    replace_resource resource

  let update_cached_resource_state _cache state id =
    record
      (Printf.sprintf "update_state:%Ld:%s" id
         (CacheData.Resource.State.to_string state));
    match find_by_id id with
    | None -> ()
    | Some resource -> replace_resource { resource with state }

  let shrink_cache ?(file_size = 0L) () =
    record ("shrink:" ^ Int64.to_string file_size);
    shrink_cache_calls := !shrink_cache_calls @ [ file_size ]
end

module FileMutations = DriveFileMutations.Make (FakePorts)

let make_resource ?(id = 1L) ?(state = CacheData.Resource.State.Synchronized)
    ?(size = 0L) ?(mime_type = "text/plain") path remote_id =
  let resource = Drive.create_resource path in
  {
    resource with
    id;
    remote_id = Some remote_id;
    name = Some (Filename.basename path);
    mime_type = Some mime_type;
    size = Some size;
    trashed = Some false;
    version = Some 1L;
    can_edit = Some true;
    modified_time = Some 0.;
    created_time = Some 0.;
    viewed_by_me_time = Some 0.;
    state;
  }

let test_write_direct_mode_updates_size_and_uses_file_sink () =
  FakePorts.reset ();
  FakePorts.add_resource (make_resource ~size:0L "/file.txt" "file-id");
  let runtime = default_runtime () in
  let bytes =
    run_session
      (FileMutations.write runtime "/file.txt" (buffer_of_string "abc") 0L)
  in
  let resource = Option.get (FakePorts.find_resource "/file.txt" false) in
  assert_equal 3 bytes;
  assert_equal (Some 3L) resource.CacheData.Resource.size;
  assert_equal CacheData.Resource.State.ToUpload
    resource.CacheData.Resource.state;
  assert_equal ~printer:string_of_int64_list [ 3L ]
    !FakePorts.shrink_cache_calls;
  assert_bool "expected direct file sink"
    (List.mem "write_to_file:/tmp/file-id:0:3" !FakePorts.trace);
  assert_bool "unexpected memory-buffer sink"
    (not (List.mem "write_to_memory:/tmp/file-id:0:3" !FakePorts.trace))

let test_write_buffered_overwrite_only_marks_state () =
  FakePorts.reset ();
  FakePorts.add_resource (make_resource ~size:10L "/file.txt" "file-id");
  let config = { Config.default with write_buffers = true } in
  let runtime = default_runtime ~config () in
  let bytes =
    run_session
      (FileMutations.write runtime "/file.txt" (buffer_of_string "abc") 2L)
  in
  let resource = Option.get (FakePorts.find_resource "/file.txt" false) in
  assert_equal 3 bytes;
  assert_equal (Some 10L) resource.CacheData.Resource.size;
  assert_equal CacheData.Resource.State.ToUpload
    resource.CacheData.Resource.state;
  assert_equal [] !FakePorts.shrink_cache_calls;
  assert_bool "expected memory-buffer sink"
    (List.mem "write_to_memory:/tmp/file-id:2:3" !FakePorts.trace);
  assert_bool "unexpected direct file sink"
    (not (List.mem "write_to_file:/tmp/file-id:2:3" !FakePorts.trace))

let test_truncate_flushes_before_local_truncate_and_grows_size () =
  FakePorts.reset ();
  let resource = make_resource ~size:4L "/file.txt" "file-id" in
  FakePorts.add_resource resource;
  FakePorts.set_local_file resource 4L;
  let runtime = default_runtime () in
  run_session (FileMutations.truncate runtime "/file.txt" 10L);
  let updated = Option.get (FakePorts.find_resource "/file.txt" false) in
  let flush_index = index_of "flush:/file.txt" !FakePorts.trace in
  let truncate_index = index_of "truncate:/tmp/file-id:10" !FakePorts.trace in
  assert_bool "expected flush before truncate" (flush_index < truncate_index);
  assert_equal (Some 10L) updated.CacheData.Resource.size;
  assert_equal CacheData.Resource.State.ToUpload
    updated.CacheData.Resource.state;
  assert_equal ~printer:string_of_int64_list [ 6L ]
    !FakePorts.shrink_cache_calls

let test_truncate_shrinks_with_negative_delta () =
  FakePorts.reset ();
  let resource = make_resource ~size:10L "/file.txt" "file-id" in
  FakePorts.add_resource resource;
  FakePorts.set_local_file resource 10L;
  let runtime = default_runtime () in
  run_session (FileMutations.truncate runtime "/file.txt" 4L);
  let updated = Option.get (FakePorts.find_resource "/file.txt" false) in
  assert_equal (Some 4L) updated.CacheData.Resource.size;
  assert_equal CacheData.Resource.State.ToUpload
    updated.CacheData.Resource.state;
  assert_equal ~printer:string_of_int64_list [ -6L ]
    !FakePorts.shrink_cache_calls

let test_truncate_missing_local_file_skips_local_mutation_but_marks_dirty () =
  FakePorts.reset ();
  FakePorts.add_resource (make_resource ~size:10L "/file.txt" "file-id");
  let runtime = default_runtime () in
  run_session (FileMutations.truncate runtime "/file.txt" 4L);
  let updated = Option.get (FakePorts.find_resource "/file.txt" false) in
  assert_equal (Some 4L) updated.CacheData.Resource.size;
  assert_equal CacheData.Resource.State.ToUpload
    updated.CacheData.Resource.state;
  assert_equal ~printer:string_of_int64_list [ -6L ]
    !FakePorts.shrink_cache_calls;
  assert_bool "unexpected local truncate"
    (not (List.mem "truncate:/tmp/file-id:4" !FakePorts.trace))

let suite =
  "DriveFileMutations test"
  >::: [
         "test_write_direct_mode_updates_size_and_uses_file_sink"
         >:: test_write_direct_mode_updates_size_and_uses_file_sink;
         "test_write_buffered_overwrite_only_marks_state"
         >:: test_write_buffered_overwrite_only_marks_state;
         "test_truncate_flushes_before_local_truncate_and_grows_size"
         >:: test_truncate_flushes_before_local_truncate_and_grows_size;
         "test_truncate_shrinks_with_negative_delta"
         >:: test_truncate_shrinks_with_negative_delta;
         "test_truncate_missing_local_file_skips_local_mutation_but_marks_dirty"
         >:: test_truncate_missing_local_file_skips_local_mutation_but_marks_dirty;
       ]
