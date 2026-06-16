val dir_entry_of_name : string -> Fuse.dir_entry
val dir_entries_of_names : string list -> Fuse.dir_entry list
val file_info_update_of_handle : int option -> Fuse.file_info_update
val int_of_file_handle : int64 -> int
val file_handle_as_int : Fuse.file_info -> int
val flags_of_file_info : Fuse.file_info -> Unix.open_flag list
val float_of_timestamp : string -> string -> Fuse.timestamp -> float
val reject_unsupported_rename_flags : string -> Fuse.rename_flags -> unit
