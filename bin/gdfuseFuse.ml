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
  try f () with
  | e ->
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

let getattr path = drive_path_op ~name:"getattr" ~label:"stat" path Drive.get_attr

let readdir path hnd =
  Utils.log_with_header "readdir %s %d\n%!" path hnd;
  let dir_list =
    with_drive_op ~label:"readdir" ~param:path (fun () -> Drive.read_dir path)
  in
  Filename.current_dir_name :: Filename.parent_dir_name :: dir_list

let opendir path flags =
  Utils.log_with_header "opendir %s %s\n%!" path (Utils.flags_to_string flags);
  with_drive_op ~label:"opendir" ~param:path (fun () -> Drive.opendir path flags)

let releasedir path flags _hnd =
  Utils.log_with_header "releasedir %s %s\n%!" path
    (Utils.flags_to_string flags)

let fsyncdir path ds hnd =
  Utils.log_with_header "fsyncdir %s %b %d\n%!" path ds hnd

let utime path atime mtime =
  Utils.log_with_header "utime %s %f %f\n%!" path atime mtime;
  with_drive_op ~label:"utime" ~param:path (fun () ->
      Drive.utime path atime mtime)

let fopen path flags =
  Utils.log_with_header "fopen %s %s\n%!" path (Utils.flags_to_string flags);
  with_drive_op ~label:"fopen" ~param:path (fun () -> Drive.fopen path flags)

let read path buf offset file_descr =
  let buf_len = Bigarray.Array1.dim buf in
  Utils.log_with_header "read %s [%d bytes] %Ld %d\n%!" path buf_len offset
    file_descr;
  let result =
    with_drive_op ~label:"read" ~param:path (fun () ->
        Drive.read path buf offset file_descr)
  in
  if !Utils.debug_buffers then
    Utils.log_buffer
      (Printf.sprintf "read %s [%d bytes] %Ld %d" path buf_len offset
         file_descr)
      buf result;
  result

let write path buf offset file_descr =
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

let rename path new_path =
  Utils.log_with_header "rename %s %s\n%!" path new_path;
  with_drive_op ~label:"rename" ~param:path (fun () -> Drive.rename path new_path)

let truncate path size =
  Utils.log_with_header "truncate %s %Ld\n%!" path size;
  with_drive_op ~label:"truncate" ~param:path (fun () ->
      Drive.truncate path size)

let release path flags hnd =
  Utils.log_with_header "release %s %s\n%!" path (Utils.flags_to_string flags);
  with_drive_op ~label:"release" ~param:path (fun () ->
      Drive.release path flags hnd)

let flush path file_descr =
  Utils.log_with_header "flush %s %d\n%!" path file_descr;
  with_drive_op ~label:"flush" ~param:path (fun () ->
      Drive.flush path file_descr)

let fsync path ds file_descr =
  Utils.log_with_header "fsync %s %b %d\n%!" path ds file_descr;
  with_drive_op ~label:"fsync" ~param:path (fun () ->
      Drive.fsync path ds file_descr)

let chmod path mode =
  Utils.log_with_header "chmod %s %o\n%!" path mode;
  with_drive_op ~label:"chmod" ~param:path (fun () -> Drive.chmod path mode)

let chown path uid gid =
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
  Fuse.main fuse_argv
    {
      Fuse.default_operations with
      Fuse.init = init_filesystem;
      statfs;
      getattr;
      readdir;
      opendir;
      releasedir;
      fsyncdir;
      utime;
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
