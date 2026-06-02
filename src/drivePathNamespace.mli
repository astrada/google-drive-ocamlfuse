val root_directory : string
val trash_directory : string
val trash_directory_name_length : int
val trash_directory_base_path : string
val lost_and_found_directory : string
val shared_with_me_directory : string
val is_in_trash_directory : string -> Config.t -> bool
val is_lost_and_found_root : string -> bool -> Config.t -> bool
val is_lost_and_found : string -> bool -> Config.t -> bool
val is_shared_with_me_root : string -> bool -> Config.t -> bool
val is_shared_with_me : string -> bool -> Config.t -> bool
val get_path_in_cache : string -> Config.t -> string * bool
