open GapiMonad
open GapiMonad.SessionM.Infix

exception File_not_found = DriveMutations.File_not_found

type runtime = { cache : CacheData.t; config : Config.t }

module type PORTS = sig
  val get_content_path : CacheData.t -> CacheData.Resource.t -> string

  val select_first_resource_with_remote_id :
    CacheData.t -> string -> CacheData.Resource.t option

  val file_exists : string -> bool
  val check_md5_checksum : CacheData.Resource.t -> CacheData.t -> bool

  val update_cached_resource_state :
    CacheData.t -> CacheData.Resource.State.t -> int64 -> unit

  val update_cache_size_for_documents :
    CacheData.t -> CacheData.Resource.t -> string -> (int64 -> int64) -> unit

  val shrink_cache : ?file_size:int64 -> unit -> unit

  val with_resource_lock :
    CacheData.Resource.t -> unit SessionM.m -> unit SessionM.m

  val create_desktop_entry : CacheData.Resource.t -> string -> Config.t -> unit

  val create_html_with_redirect :
    CacheData.Resource.t -> string -> Config.t -> unit

  val download_export_link_to_file : string -> string -> unit SessionM.m

  val export_document_to_file :
    string -> file_id:string -> mime_type:string -> unit SessionM.m

  val download_media_to_file : string -> CacheData.Resource.t -> unit SessionM.m
  val create_empty_file : string -> unit
  val wait_exponential_backoff : int -> unit
  val handle_download_exception : exn -> unit SessionM.m
end

let is_desktop_format config resource =
  CacheData.Resource.get_format resource config = "desktop"

module Make (P : PORTS) = struct
  let update_state runtime state resource =
    P.update_cached_resource_state runtime.cache state
      resource.CacheData.Resource.id

  let shrink_cache_before_downloading resource =
    let file_size = Option.default 0L resource.CacheData.Resource.size in
    P.shrink_cache ~file_size ();
    SessionM.return ()

  let materialize_from_drive runtime content_path resource =
    let file_id = resource.CacheData.Resource.remote_id |> Option.get in
    if CacheData.Resource.is_document resource then
      let fmt = CacheData.Resource.get_format resource runtime.config in
      let mime_type = CacheData.Resource.mime_type_of_format fmt in
      let export_links =
        CacheData.Resource.parse_export_links
          (Option.default "" resource.CacheData.Resource.export_links)
      in
      try
        let export_link = List.assoc mime_type export_links in
        P.download_export_link_to_file content_path export_link
      with Not_found ->
        P.export_document_to_file content_path ~file_id ~mime_type
    else if Option.default 0L resource.CacheData.Resource.size > 0L then
      P.download_media_to_file content_path resource
    else (
      Utils.log_with_header
        "BEGIN: Creating resource without content (path=%s)\n%!" content_path;
      P.create_empty_file content_path;
      SessionM.return ())

  let materialize runtime resource content_path =
    SessionM.return () >>= fun () ->
    Utils.log_with_header "BEGIN: Downloading resource (id=%Ld) to %s\n%!"
      resource.CacheData.Resource.id content_path;
    (if is_desktop_format runtime.config resource then (
       shrink_cache_before_downloading resource >>= fun () ->
       P.update_cache_size_for_documents runtime.cache resource content_path
         Int64.neg;
       if runtime.config.Config.desktop_entry_as_html then
         P.create_html_with_redirect resource content_path runtime.config
       else P.create_desktop_entry resource content_path runtime.config;
       SessionM.return ())
     else
       shrink_cache_before_downloading resource >>= fun () ->
       update_state runtime CacheData.Resource.State.Downloading resource;
       P.update_cache_size_for_documents runtime.cache resource content_path
         Int64.neg;
       Utils.try_with_m (materialize_from_drive runtime content_path resource)
         (fun e ->
           update_state runtime CacheData.Resource.State.ToDownload resource;
           P.handle_download_exception e))
    >>= fun () ->
    P.update_cache_size_for_documents runtime.cache resource content_path
      Std.identity;
    Utils.log_with_header "END: Downloading resource (id=%Ld) to %s\n%!"
      resource.CacheData.Resource.id content_path;
    update_state runtime CacheData.Resource.State.Synchronized resource;
    SessionM.return ()

  let materialize_with_lock runtime resource content_path =
    P.with_resource_lock resource (materialize runtime resource content_path)

  let reloaded_resource runtime resource =
    match resource.CacheData.Resource.remote_id with
    | None -> Some resource
    | Some remote_id ->
        P.select_first_resource_with_remote_id runtime.cache remote_id

  let download_if_not_updated runtime resource content_path reloaded_resource =
    if P.check_md5_checksum reloaded_resource runtime.cache then (
      update_state runtime CacheData.Resource.State.Synchronized resource;
      SessionM.return ())
    else materialize_with_lock runtime resource content_path

  let rec check_state runtime resource content_path n =
    match reloaded_resource runtime resource with
    | None -> Utils.raise_m File_not_found
    | Some current_resource -> (
        match current_resource.CacheData.Resource.state with
        | CacheData.Resource.State.Synchronized
        | CacheData.Resource.State.ToUpload | CacheData.Resource.State.Uploading
          ->
            if P.file_exists content_path then SessionM.return ()
            else materialize_with_lock runtime resource content_path
        | CacheData.Resource.State.ToDownload ->
            download_if_not_updated runtime resource content_path
              current_resource
        | CacheData.Resource.State.Downloading ->
            if n > 300 then (
              Utils.log_with_header
                "Still downloading resource (id=%Ld) after about 5 hours: \
                 start downloading again\n\
                 %!"
                resource.CacheData.Resource.id;
              download_if_not_updated runtime resource content_path
                current_resource)
            else (
              Utils.log_with_header
                "Already downloading resource (id=%Ld): check number %d\n%!"
                resource.CacheData.Resource.id n;
              P.wait_exponential_backoff (min n 6);
              check_state runtime resource content_path (n + 1))
        | CacheData.Resource.State.NotFound -> Utils.raise_m File_not_found)

  let download_resource runtime resource =
    let content_path = P.get_content_path runtime.cache resource in
    check_state runtime resource content_path 0 >>= fun () ->
    SessionM.return content_path
end
