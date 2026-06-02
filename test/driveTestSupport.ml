let session =
  {
    GapiConversation.Session.curl = GapiCurl.Initialized;
    config = GapiConfig.default;
    auth = GapiConversation.Session.NoAuth;
    cookies = [];
    etag = "";
  }

let run_session request = fst (request session)

let dummy_cache =
  {
    CacheData.cache_dir = "/tmp";
    db_path = "/tmp/test-cache.db";
    busy_timeout = 0;
    in_memory = true;
    autosaving_interval = 0;
  }

let base_runtime ?(cache = dummy_cache) ?(config = Config.default) () =
  { DriveRuntime.cache; config }

let cache_only_runtime ?(cache = dummy_cache) () = { DriveRuntime.cache }
let config_only_runtime ?(config = Config.default) () = { DriveRuntime.config }

let make_resource ?(id = 1L) ?remote_id ?name ?(mime_type = "text/plain") ?size
    ?(trashed = false) ?(state = CacheData.Resource.State.Synchronized)
    ?(can_edit = true) ?(version = 1L) ?(modified_time = 0.)
    ?(created_time = 0.) ?(viewed_by_me_time = 0.) path =
  let resource = Drive.create_resource path in
  {
    resource with
    id;
    remote_id;
    name = Some (Option.default (Filename.basename path) name);
    mime_type = Some mime_type;
    size;
    trashed = Some trashed;
    version = Some version;
    can_edit = Some can_edit;
    modified_time = Some modified_time;
    created_time = Some created_time;
    viewed_by_me_time = Some viewed_by_me_time;
    state;
  }

let make_file ?(id = "rid-updated") ?(name = "file.txt")
    ?(mime_type = "text/plain") ?(size = 2L) ?(version = 2L) () =
  {
    GapiDriveV3Model.File.empty with
    id;
    name;
    mimeType = mime_type;
    size;
    version;
  }

module Trace = struct
  type t = string list ref

  let create () = ref []
  let reset trace = trace := []
  let record trace event = trace := !trace @ [ event ]
  let events trace = !trace

  let index_of value values =
    let rec loop i = function
      | [] -> raise Not_found
      | x :: xs -> if x = value then i else loop (i + 1) xs
    in
    loop 0 values

  let assert_before earlier later events =
    OUnit.assert_bool
      (Printf.sprintf "expected %s before %s" earlier later)
      (index_of earlier events < index_of later events)

  let assert_no_event prefix events =
    OUnit.assert_bool
      (Printf.sprintf "unexpected event with prefix %s" prefix)
      (not (List.exists (String.starts_with ~prefix) events))
end
