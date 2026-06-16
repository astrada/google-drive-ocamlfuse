let default_entry_flags = { Fuse.fill_dir_plus = false }

let dir_entry_of_name entry_name =
  {
    Fuse.entry_name;
    entry_stats = None;
    entry_offset = None;
    entry_flags = default_entry_flags;
  }

let dir_entries_of_names names = List.map dir_entry_of_name names

let file_info_update_of_handle = function
  | None -> Fuse.default_file_info_update
  | Some file_handle ->
      {
        Fuse.default_file_info_update with
        Fuse.fi_update_fh = Some (Int64.of_int file_handle);
      }

let int_of_file_handle file_handle =
  let min_file_handle = Int64.of_int min_int in
  let max_file_handle = Int64.of_int max_int in
  if
    Int64.compare file_handle min_file_handle < 0
    || Int64.compare file_handle max_file_handle > 0
  then raise (Unix.Unix_error (Unix.EOVERFLOW, "file handle", ""))
  else Int64.to_int file_handle

let file_handle_as_int file_info = int_of_file_handle file_info.Fuse.fi_fh
let flags_of_file_info file_info = file_info.Fuse.fi_flags

let float_of_timestamp label path = function
  | Fuse.Time { Fuse.tv_sec; tv_nsec } ->
      Int64.to_float tv_sec +. (float tv_nsec /. 1_000_000_000.0)
  | Fuse.Now | Fuse.Omit -> raise (Unix.Unix_error (Unix.EINVAL, label, path))

let reject_unsupported_rename_flags path flags =
  if flags.Fuse.rename_flags_raw <> 0 then
    raise (Unix.Unix_error (Unix.EINVAL, "rename", path))
