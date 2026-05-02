open GapiLens.Infix
open GapiMonad
open GapiMonad.SessionM.Infix
module File = GapiDriveV3Model.File

type runtime = { cache : CacheData.t; config : Config.t }

module type PORTS = sig
  val get_content_path : CacheData.t -> CacheData.Resource.t -> string

  val create_file_resource :
    ?content_type:string -> string -> GapiMediaResource.t

  val media_content_type : GapiMediaResource.t -> string
  val media_content_length : GapiMediaResource.t -> int64

  val update_cached_resource_state_and_size :
    CacheData.t -> CacheData.Resource.State.t -> int64 -> int64 -> unit

  val build_resource_keys_header_from_resource :
    CacheData.Resource.t -> GapiCore.Header.t list

  val now : unit -> Netdate.t

  val remote_update :
    media_source:GapiMediaResource.t option ->
    custom_headers:GapiCore.Header.t list ->
    fileId:string ->
    File.t ->
    File.t SessionM.m

  val update_resource_from_file :
    ?state:CacheData.Resource.State.t ->
    CacheData.Resource.t ->
    File.t ->
    CacheData.Resource.t

  val select_first_resource_with_remote_id :
    CacheData.t -> string -> CacheData.Resource.t option

  val update_cached_resource : CacheData.t -> CacheData.Resource.t -> unit
  val shrink_cache : unit -> unit
end

module Make (P : PORTS) = struct
  let content_type_for_upload runtime resource content_path =
    if
      CacheData.Resource.is_document resource
      && runtime.config.Config.editable_docs
    then
      let fmt = CacheData.Resource.get_format resource runtime.config in
      CacheData.Resource.mime_type_of_format fmt
    else if runtime.config.Config.autodetect_mime then ""
    else
      let file_source = P.create_file_resource content_path in
      let resource_mime_type =
        resource |. CacheData.Resource.mime_type |> Option.get
      in
      let content_type = P.media_content_type file_source in
      if resource_mime_type <> "" then resource_mime_type else content_type

  let next_state_after_upload resource =
    match resource.CacheData.Resource.state with
    | CacheData.Resource.State.Uploading ->
        Some CacheData.Resource.State.Synchronized
    | _ -> None

  let upload runtime resource =
    let content_path = P.get_content_path runtime.cache resource in
    let content_type = content_type_for_upload runtime resource content_path in
    let file_source = P.create_file_resource ~content_type content_path in
    let size = P.media_content_length file_source in
    P.update_cached_resource_state_and_size runtime.cache
      CacheData.Resource.State.Uploading size resource.CacheData.Resource.id;
    let remote_id = resource |. CacheData.Resource.remote_id |> Option.get in
    let media_source = if size = 0L then None else Some file_source in
    Utils.log_with_header
      "BEGIN: Uploading file (id=%Ld, path=%s, cache path=%s, content type=%s, \
       content_length=%Ld).\n\
       %!"
      resource.CacheData.Resource.id resource.CacheData.Resource.path
      content_path
      (if content_type = "" then "autodetect" else content_type)
      size;
    let file_patch = File.empty |> File.modifiedTime ^= P.now () in
    let custom_headers = P.build_resource_keys_header_from_resource resource in
    P.remote_update ~media_source ~custom_headers ~fileId:remote_id file_patch
    >>= fun file ->
    let resource = P.update_resource_from_file resource file in
    Utils.log_with_header
      "END: Uploading file (id=%Ld, path=%s, cache path=%s, content type=%s).\n\
       %!"
      resource.CacheData.Resource.id resource.CacheData.Resource.path
      content_path file.File.mimeType;
    let reloaded_resource =
      P.select_first_resource_with_remote_id runtime.cache file.File.id
    in
    let resource = Option.default resource reloaded_resource in
    let state = next_state_after_upload resource in
    let updated_resource = P.update_resource_from_file ?state resource file in
    P.update_cached_resource runtime.cache updated_resource;
    P.shrink_cache ();
    SessionM.return ()
end
