open GapiLens.Infix
open GapiMonad
open GapiMonad.SessionM.Infix
open GapiDriveV3Model

exception Existing_attribute = DriveMutations.Existing_attribute
exception Invalid_operation = DriveMutations.Invalid_operation
exception No_attribute = DriveMutations.No_attribute
exception Permission_denied = DriveMutations.Permission_denied

type runtime = { cache : CacheData.t; config : Config.t }

module type PORTS = sig
  val max_attribute_length : int
  val json_length : string -> int
  val get_path_in_cache : string -> Config.t -> string * bool
  val get_resource : string -> bool -> CacheData.Resource.t SessionM.m

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
    (CacheData.Resource.t -> File.t option SessionM.m) ->
    unit SessionM.m
end

module Make (P : PORTS) = struct
  let parse_xattrs resource =
    CacheData.Resource.parse_xattrs resource.CacheData.Resource.xattrs

  let remote_id resource = resource.CacheData.Resource.remote_id |> Option.get

  let get_xattr runtime path name =
    let path_in_cache, trashed = P.get_path_in_cache path runtime.config in
    P.get_resource path_in_cache trashed >>= fun resource ->
    let xattrs = parse_xattrs resource in
    let value =
      try List.assoc name xattrs with Not_found -> raise No_attribute
    in
    SessionM.return value

  let list_xattr runtime path =
    let path_in_cache, trashed = P.get_path_in_cache path runtime.config in
    P.get_resource path_in_cache trashed >>= fun resource ->
    let xattrs = parse_xattrs resource in
    let keys = List.map (fun (n, _) -> n) xattrs in
    SessionM.return keys

  let set_xattr runtime path name value xflags =
    let setxattr resource =
      let remote_id = remote_id resource in
      Utils.log_with_header
        "BEGIN: Setting xattr (remote id=%s, name=%s value=%s xflags=%s)\n%!"
        remote_id name value
        (Utils.xattr_flags_to_string xflags);
      let xattrs = parse_xattrs resource in
      let existing = List.mem_assoc name xattrs in
      (match xflags with
      | Fuse.CREATE -> if existing then raise Existing_attribute
      | Fuse.REPLACE -> if not existing then raise No_attribute
      | Fuse.AUTO -> ());
      let attribute_length = P.json_length name + P.json_length value in
      if attribute_length > P.max_attribute_length then raise Invalid_operation;
      let file_patch =
        File.empty
        |> File.appProperties
           ^= [ CacheData.Resource.xattr_to_app_property name value ]
      in
      let custom_headers =
        P.build_resource_keys_header_from_resource resource
      in
      P.remote_update ~custom_headers ~fileId:remote_id file_patch
      >>= fun patched_file ->
      Utils.log_with_header
        "END: Setting xattr (remote id=%s, name=%s value=%s xflags=%s)\n%!"
        remote_id name value
        (Utils.xattr_flags_to_string xflags);
      SessionM.return (Some patched_file)
    in
    P.update_remote_resource runtime path setxattr

  let remove_xattr runtime path name =
    let removexattr resource =
      let remote_id = remote_id resource in
      Utils.log_with_header "BEGIN: Removing xattr (remote id=%s, name=%s)\n%!"
        remote_id name;
      let xattrs = parse_xattrs resource in
      let existing = List.mem_assoc name xattrs in
      if not existing then raise No_attribute;
      let file_patch =
        File.empty
        |> File.appProperties
           ^= [ CacheData.Resource.xattr_no_value_to_app_property name ]
      in
      let custom_headers =
        P.build_resource_keys_header_from_resource resource
      in
      P.remote_update ~custom_headers ~fileId:remote_id file_patch
      >>= fun patched_file ->
      Utils.log_with_header "END: Removing xattr (remote id=%s, name=%s)\n%!"
        remote_id name;
      SessionM.return (Some patched_file)
    in
    P.update_remote_resource runtime path removexattr
end
