let root_directory = "/"
let trash_directory = "/.Trash"
let trash_directory_name_length = String.length trash_directory
let trash_directory_base_path = "/.Trash/"
let lost_and_found_directory = "/lost+found"
let shared_with_me_directory = "/.shared"

let is_in_trash_directory path config =
  if path = trash_directory || config.Config.disable_trash then false
  else ExtString.String.starts_with path trash_directory_base_path

let is_lost_and_found_root path trashed config =
  if trashed || not config.Config.lost_and_found then false
  else path = lost_and_found_directory

let is_lost_and_found path trashed config =
  if trashed || not config.Config.lost_and_found then false
  else ExtString.String.starts_with path lost_and_found_directory

let is_shared_with_me_root path trashed _config =
  if trashed then false else path = shared_with_me_directory

let is_shared_with_me path trashed _config =
  if trashed then false
  else ExtString.String.starts_with path shared_with_me_directory

let get_path_in_cache path config =
  if path = root_directory then (root_directory, false)
  else if path = trash_directory && not config.Config.disable_trash then
    (root_directory, true)
  else if is_in_trash_directory path config then
    let path_in_cache = Str.string_after path trash_directory_name_length in
    (path_in_cache, true)
  else (path, false)
