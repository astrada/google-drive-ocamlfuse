let handle_exception e label param =
  match e with
  | Drive.File_not_found ->
      Utils.log_with_header "File not found: %s %s\n%!" label param;
      raise (Unix.Unix_error (Unix.ENOENT, label, param))
  | Drive.Permission_denied ->
      Utils.log_with_header "Permission denied: %s %s\n%!" label param;
      raise (Unix.Unix_error (Unix.EACCES, label, param))
  | Drive.Directory_not_empty ->
      Utils.log_with_header "Directory not empty: %s %s\n%!" label param;
      raise (Unix.Unix_error (Unix.ENOTEMPTY, label, param))
  | Drive.IO_error ->
      Utils.log_with_header "Input/output error: %s %s\n%!" label param;
      raise (Unix.Unix_error (Unix.EIO, label, param))
  | Drive.No_attribute ->
      raise (Unix.Unix_error (Unix.EUNKNOWNERR 61, label, param))
  | Drive.Existing_attribute ->
      raise (Unix.Unix_error (Unix.EEXIST, label, param))
  | Drive.Invalid_operation ->
      raise (Unix.Unix_error (Unix.EINVAL, label, param))
  | Unix.Unix_error _ as e ->
      Utils.log_exception e;
      raise e
  | Oauth2.InvalidRefreshToken as e -> raise e
  | e ->
      Utils.log_exception e;
      raise (Unix.Unix_error (Unix.EIO, label, param))

let with_drive_op ?(log_exception = false) ~label ~param f =
  try f ()
  with e ->
    if log_exception then Utils.log_exception e;
    handle_exception e label param

let drive_path_op ~name ?(label = name) path op =
  Utils.log_with_header "%s %s\n%!" name path;
  with_drive_op ~label ~param:path (fun () -> op path)

let init_filesystem () =
  Utils.log_with_header "init_filesystem\n%!";
  with_drive_op ~log_exception:true ~label:"init_filesystem" ~param:""
    Drive.init_filesystem

let statfs path =
  Utils.log_with_header "statfs %s\n%!" path;
  with_drive_op ~log_exception:true ~label:"statfs" ~param:path Drive.statfs

let getattr path _file_info =
  drive_path_op ~name:"getattr" ~label:"stat" path Drive.get_attr

let readdir path offset file_info flags =
  let file_handle = GdfuseFuseNative.file_handle_as_int file_info in
  Utils.log_with_header "readdir %s %Ld %d %d\n%!" path offset file_handle
    flags.Fuse.readdir_flags_raw;
  let dir_list =
    with_drive_op ~label:"readdir" ~param:path (fun () -> Drive.read_dir path)
  in
  Filename.current_dir_name :: Filename.parent_dir_name :: dir_list
  |> GdfuseFuseNative.dir_entries_of_names

let opendir path file_info =
  let flags = GdfuseFuseNative.flags_of_file_info file_info in
  Utils.log_with_header "opendir %s %s\n%!" path (Utils.flags_to_string flags);
  let handle =
    with_drive_op ~label:"opendir" ~param:path (fun () ->
        Drive.opendir path flags)
  in
  GdfuseFuseNative.file_info_update_of_handle handle

let releasedir path file_info =
  let flags = GdfuseFuseNative.flags_of_file_info file_info in
  Utils.log_with_header "releasedir %s %s\n%!" path
    (Utils.flags_to_string flags)

let fsyncdir path ds file_info =
  let file_handle = GdfuseFuseNative.file_handle_as_int file_info in
  Utils.log_with_header "fsyncdir %s %b %d\n%!" path ds file_handle

let utimens path atime mtime _file_info =
  let atime = GdfuseFuseNative.float_of_timestamp "utimens" path atime in
  let mtime = GdfuseFuseNative.float_of_timestamp "utimens" path mtime in
  Utils.log_with_header "utimens %s %f %f\n%!" path atime mtime;
  with_drive_op ~label:"utimens" ~param:path (fun () ->
      Drive.utime path atime mtime)

let fopen path file_info =
  let flags = GdfuseFuseNative.flags_of_file_info file_info in
  Utils.log_with_header "fopen %s %s\n%!" path (Utils.flags_to_string flags);
  let handle =
    with_drive_op ~label:"fopen" ~param:path (fun () ->
        let handle = Drive.fopen path flags in
        if List.mem Unix.O_TRUNC flags then Drive.truncate path 0L;
        handle)
  in
  GdfuseFuseNative.file_info_update_of_handle handle

let read path buf offset file_info =
  let file_descr = GdfuseFuseNative.file_handle_as_int file_info in
  let buf_len = Bigarray.Array1.dim buf in
  Utils.log_with_header "read %s [%d bytes] %Ld %d\n%!" path buf_len offset
    file_descr;
  let result =
    with_drive_op ~label:"read" ~param:path (fun () ->
        Drive.read path buf offset file_descr)
  in
  if !Utils.debug_buffers then
    Utils.log_buffer
      (Printf.sprintf "read %s [%d bytes] %Ld %d" path buf_len offset file_descr)
      buf result;
  result

let write path buf offset file_info =
  let file_descr = GdfuseFuseNative.file_handle_as_int file_info in
  let buf_len = Bigarray.Array1.dim buf in
  Utils.log_with_header "write %s [%d bytes] %Ld %d\n%!" path buf_len offset
    file_descr;
  if !Utils.debug_buffers then
    Utils.log_buffer
      (Printf.sprintf "write %s [%d bytes] %Ld %d" path buf_len offset
         file_descr)
      buf buf_len;
  with_drive_op ~label:"write" ~param:path (fun () ->
      Drive.write path buf offset file_descr)

let mknod path mode =
  Utils.log_with_header "mknod %s %o\n%!" path mode;
  with_drive_op ~label:"mknod" ~param:path (fun () -> Drive.mknod path mode)

let mkdir path mode =
  Utils.log_with_header "mkdir %s %o\n%!" path mode;
  with_drive_op ~label:"mkdir" ~param:path (fun () -> Drive.mkdir path mode)

let unlink path = drive_path_op ~name:"unlink" path Drive.unlink
let rmdir path = drive_path_op ~name:"rmdir" path Drive.rmdir

let rename path new_path flags =
  Utils.log_with_header "rename %s %s %d\n%!" path new_path
    flags.Fuse.rename_flags_raw;
  GdfuseFuseNative.reject_unsupported_rename_flags path flags;
  with_drive_op ~label:"rename" ~param:path (fun () ->
      Drive.rename path new_path)

let truncate path size _file_info =
  Utils.log_with_header "truncate %s %Ld\n%!" path size;
  with_drive_op ~label:"truncate" ~param:path (fun () ->
      Drive.truncate path size)

let release path file_info =
  let flags = GdfuseFuseNative.flags_of_file_info file_info in
  let hnd = GdfuseFuseNative.file_handle_as_int file_info in
  Utils.log_with_header "release %s %s\n%!" path (Utils.flags_to_string flags);
  with_drive_op ~label:"release" ~param:path (fun () ->
      Drive.release path flags hnd)

let flush path file_info =
  let file_descr = GdfuseFuseNative.file_handle_as_int file_info in
  Utils.log_with_header "flush %s %d\n%!" path file_descr;
  with_drive_op ~label:"flush" ~param:path (fun () ->
      Drive.flush path file_descr)

let fsync path ds file_info =
  let file_descr = GdfuseFuseNative.file_handle_as_int file_info in
  Utils.log_with_header "fsync %s %b %d\n%!" path ds file_descr;
  with_drive_op ~label:"fsync" ~param:path (fun () ->
      Drive.fsync path ds file_descr)

let chmod path mode _file_info =
  Utils.log_with_header "chmod %s %o\n%!" path mode;
  with_drive_op ~label:"chmod" ~param:path (fun () -> Drive.chmod path mode)

let chown path uid gid _file_info =
  Utils.log_with_header "chown %s %d %d\n%!" path uid gid;
  with_drive_op ~label:"chown" ~param:path (fun () -> Drive.chown path uid gid)

let getxattr path name =
  Utils.log_with_header "getxattr %s %s\n%!" path name;
  with_drive_op ~label:"getxattr" ~param:path (fun () ->
      Drive.get_xattr path name)

let setxattr path name value xflags =
  Utils.log_with_header "setxattr %s %s %s %s\n%!" path name value
    (Utils.xattr_flags_to_string xflags);
  with_drive_op ~label:"setxattr" ~param:path (fun () ->
      Drive.set_xattr path name value xflags)

let listxattr path = drive_path_op ~name:"listxattr" path Drive.list_xattr

let removexattr path name =
  Utils.log_with_header "removexattr %s %s\n%!" path name;
  with_drive_op ~label:"removexattr" ~param:path (fun () ->
      Drive.remove_xattr path name)

let readlink path = drive_path_op ~name:"readlink" path Drive.read_link

let symlink target linkpath =
  Utils.log_with_header "symlink %s %s\n%!" target linkpath;
  with_drive_op ~label:"symlink" ~param:target (fun () ->
      Drive.symlink target linkpath)

let start_filesystem mountpoint fuse_args =
  Utils.log_with_header "Starting filesystem %s\n%!" mountpoint;
  let fuse_argv =
    Sys.argv.(0) :: (fuse_args @ [ mountpoint ]) |> Array.of_list
  in
  let operations : Fuse.operations =
    {
      Fuse.default_operations with
      init = init_filesystem;
      statfs;
      getattr;
      readdir;
      opendir;
      releasedir;
      fsyncdir;
      utimens;
      fopen;
      read;
      write;
      mknod;
      mkdir;
      unlink;
      rmdir;
      rename;
      truncate;
      release;
      flush;
      fsync;
      chmod;
      chown;
      getxattr;
      setxattr;
      listxattr;
      removexattr;
      readlink;
      symlink;
    }
  in
  Fuse.main fuse_argv operations
