open GapiLens.Infix
open GapiMonad
open GapiMonad.SessionM.Infix

exception File_not_found = DriveMutations.File_not_found
exception Invalid_operation = DriveMutations.Invalid_operation

type runtime = {
  cache : CacheData.t;
  config : Config.t;
  mountpoint_path : string;
  mountpoint_stats : Unix.LargeFile.stats;
}

module type PORTS = sig
  include DrivePortFragments.PATH_LOOKUP
  include DrivePortFragments.RESOURCE_LOOKUP

  val get_resource_with_id :
    string -> CacheData.t -> CacheData.Resource.t SessionM.m

  val update_cached_resource : CacheData.t -> CacheData.Resource.t -> unit
  val materialize_for_stat : CacheData.Resource.t -> string SessionM.m
  val file_exists : string -> bool
  val stat_file : string -> Unix.LargeFile.stats
  val is_file_read_only : CacheData.Resource.t -> bool
  val is_lost_and_found_root : string -> bool -> Config.t -> bool
  val is_shared_with_me_root : string -> bool -> Config.t -> bool
end

module Make (P : PORTS) = struct
  let root_directory = "/"
  let trash_directory = "/.Trash"
  let f_bsize = 4096L

  let normalized_mountpoint_path runtime =
    if ExtString.String.ends_with runtime.mountpoint_path Filename.dir_sep then
      Filename.chop_suffix runtime.mountpoint_path Filename.dir_sep
    else runtime.mountpoint_path

  let fetch_link_target runtime path_in_cache trashed =
    P.get_resource path_in_cache trashed >>= fun resource ->
    match resource.CacheData.Resource.link_target with
    | Some link_target -> SessionM.return link_target
    | None -> (
        match resource.CacheData.Resource.target_id with
        | None -> raise Invalid_operation
        | Some tid ->
            P.get_resource_with_id tid runtime.cache >>= fun link_resource ->
            let link_target =
              normalized_mountpoint_path runtime
              ^ link_resource.CacheData.Resource.path
            in
            let updated_resource =
              resource |> CacheData.Resource.link_target ^= Some link_target
            in
            P.update_cached_resource runtime.cache updated_resource;
            SessionM.return link_target)

  let read_link runtime path =
    let path_in_cache, trashed = P.get_path_in_cache path runtime.config in
    fetch_link_target runtime path_in_cache trashed

  let get_attr runtime path =
    let path_in_cache, trashed = P.get_path_in_cache path runtime.config in
    let mountpoint_stats = runtime.mountpoint_stats in
    let read_only_mountpoint_stats =
      {
        mountpoint_stats with
        Unix.LargeFile.st_perm =
          mountpoint_stats.Unix.LargeFile.st_perm land 0o555;
      }
    in
    let request_resource =
      P.get_resource path_in_cache trashed >>= fun resource ->
      (if
         CacheData.Resource.is_document resource
         && runtime.config.Config.download_docs
       then
         Utils.try_with_m (P.materialize_for_stat resource) (function
           | File_not_found -> SessionM.return ""
           | e -> Utils.raise_m e)
       else SessionM.return "")
      >>= fun content_path -> SessionM.return (resource, content_path)
    in
    if path = root_directory then SessionM.return mountpoint_stats
    else if
      (path = trash_directory && not runtime.config.Config.disable_trash)
      || P.is_shared_with_me_root path trashed runtime.config
    then SessionM.return read_only_mountpoint_stats
    else if P.is_lost_and_found_root path trashed runtime.config then
      SessionM.return mountpoint_stats
    else
      request_resource >>= fun (resource, content_path) ->
      let stat =
        if content_path <> "" && P.file_exists content_path then
          Some (P.stat_file content_path)
        else None
      in
      let st_kind =
        if CacheData.Resource.is_folder resource then Unix.S_DIR
        else if CacheData.Resource.is_shortcut resource then Unix.S_LNK
        else
          Option.map_default CacheData.Resource.file_mode_bits_to_kind
            Unix.S_REG resource.CacheData.Resource.file_mode_bits
      in
      let st_perm =
        let default_perm =
          if CacheData.Resource.is_folder resource then 0o777 else 0o666
        in
        let perm =
          Option.map_default CacheData.Resource.file_mode_bits_to_perm
            default_perm resource.CacheData.Resource.file_mode_bits
        in
        let mask =
          if
            CacheData.Resource.is_symlink resource
            || CacheData.Resource.is_shortcut resource
          then 0o777
          else
            lnot runtime.config.Config.umask
            land if P.is_file_read_only resource then 0o555 else 0o777
        in
        perm land mask
      in
      (* To avoid potential performance issues, counting the number of subdirs
       * (as st_nlink is usually equals to 2 + subdir number), let set the
       * value to 1, as it can be used to mean "I don't know the subdirectory
       * count" (https://github.com/cryptomator/fuse-nio-adapter/issues/34).
       * See also:
       * https://bugzilla.kernel.org/show_bug.cgi?id=196405#c5
       *)
      let st_nlink = 1 in
      let st_uid =
        Option.map_default Int64.to_int mountpoint_stats.Unix.LargeFile.st_uid
          resource.CacheData.Resource.uid
      in
      let st_gid =
        Option.map_default Int64.to_int mountpoint_stats.Unix.LargeFile.st_gid
          resource.CacheData.Resource.gid
      in
      let st_size_m =
        if
          CacheData.Resource.is_symlink resource
          || CacheData.Resource.is_shortcut resource
        then
          fetch_link_target runtime path_in_cache trashed >>= fun link_target ->
          SessionM.return (link_target |> String.length |> Int64.of_int)
        else
          SessionM.return
            (match stat with
            | None ->
                if CacheData.Resource.is_folder resource then f_bsize
                else Option.default 0L resource.CacheData.Resource.size
            | Some st -> st.Unix.LargeFile.st_size)
      in
      st_size_m >>= fun st_size ->
      let st_atime =
        match stat with
        | None -> resource.CacheData.Resource.viewed_by_me_time |> Option.get
        | Some st -> st.Unix.LargeFile.st_atime
      in
      let is_to_upload =
        resource.CacheData.Resource.state = CacheData.Resource.State.ToUpload
      in
      let st_mtime =
        match stat with
        | Some st when is_to_upload -> st.Unix.LargeFile.st_mtime
        | _ -> resource.CacheData.Resource.modified_time |> Option.get
      in
      let st_ctime =
        match stat with
        | Some st when is_to_upload -> st.Unix.LargeFile.st_ctime
        | _ -> st_mtime
      in
      SessionM.return
        {
          mountpoint_stats with
          Unix.LargeFile.st_kind;
          st_perm;
          st_nlink;
          st_uid;
          st_gid;
          st_size;
          st_atime;
          st_mtime;
          st_ctime;
        }

  let opendir runtime path =
    let path_in_cache, trashed = P.get_path_in_cache path runtime.config in
    P.get_resource path_in_cache trashed >>= fun _ -> SessionM.return ()
end
