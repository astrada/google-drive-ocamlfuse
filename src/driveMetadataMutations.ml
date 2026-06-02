open GapiLens.Infix
open GapiMonad
open GapiMonad.SessionM.Infix
open GapiDriveV3Model

type runtime = DriveRuntime.base = { cache : CacheData.t; config : Config.t }

module type PORTS = sig
  val build_resource_keys_header_from_resource :
    CacheData.Resource.t -> GapiCore.Header.t list

  val remote_update :
    custom_headers:GapiCore.Header.t list ->
    fileId:string ->
    File.t ->
    File.t SessionM.m

  val update_remote_resource :
    runtime ->
    string ->
    ?update_file_in_cache:(string -> unit) ->
    (CacheData.Resource.t -> File.t option SessionM.m) ->
    unit SessionM.m

  val update_file_times : string -> float -> float -> unit
end

module Make (P : PORTS) = struct
  let remote_id resource = resource.CacheData.Resource.remote_id |> Option.get

  let patch_remote resource file_patch =
    let remote_id = remote_id resource in
    let custom_headers = P.build_resource_keys_header_from_resource resource in
    P.remote_update ~custom_headers ~fileId:remote_id file_patch

  let utime runtime path atime mtime =
    let touch resource =
      let remote_id = remote_id resource in
      Utils.log_with_header
        "BEGIN: Updating file mtime (remote id=%s, mtime=%f)\n%!" remote_id
        mtime;
      let file_patch =
        File.empty |> File.modifiedTime ^= Netdate.create mtime
      in
      patch_remote resource file_patch >>= fun patched_file ->
      Utils.log_with_header
        "END: Updating file mtime (remote id=%s, mtime=%f)\n%!" remote_id mtime;
      SessionM.return (Some patched_file)
    in
    P.update_remote_resource runtime path
      ~update_file_in_cache:(fun content_path ->
        P.update_file_times content_path atime mtime)
      touch

  let chmod runtime path mode =
    let chmod resource =
      let remote_id = remote_id resource in
      Utils.log_with_header "BEGIN: Updating mode (remote id=%s, mode=%o)\n%!"
        remote_id mode;
      let file_patch =
        File.empty
        |> File.appProperties
           ^= [ CacheData.Resource.mode_to_app_property mode ]
      in
      patch_remote resource file_patch >>= fun patched_file ->
      Utils.log_with_header "END: Updating mode (remote id=%s, mode=%o)\n%!"
        remote_id mode;
      SessionM.return (Some patched_file)
    in
    P.update_remote_resource runtime path chmod

  let id_to_string id =
    let id64 = Int64.of_int id in
    let minus_one_32_unsigned = Int64.pred (Int64.shift_left 1L 32) in
    if id64 = Int64.minus_one || id64 = minus_one_32_unsigned then ""
    else string_of_int id

  let chown_app_properties uid gid =
    let uid_string = id_to_string uid in
    let gid_string = id_to_string gid in
    let app_properties =
      if gid_string = "" then []
      else [ CacheData.Resource.gid_to_app_property gid_string ]
    in
    let app_properties =
      if uid_string = "" then app_properties
      else CacheData.Resource.uid_to_app_property uid_string :: app_properties
    in
    (uid_string, gid_string, app_properties)

  let chown runtime path uid gid =
    let chown resource =
      let remote_id = remote_id resource in
      let uid_string, gid_string, app_properties =
        chown_app_properties uid gid
      in
      Utils.log_with_header
        "BEGIN: Updating owner (remote id=%s, uid=%s gid=%s)\n%!" remote_id
        uid_string gid_string;
      let file_patch = File.empty |> File.appProperties ^= app_properties in
      patch_remote resource file_patch >>= fun patched_file ->
      Utils.log_with_header
        "End: Updating owner (remote id=%s, uid=%d gid=%d)\n%!" remote_id uid
        gid;
      SessionM.return (Some patched_file)
    in
    P.update_remote_resource runtime path chown
end
