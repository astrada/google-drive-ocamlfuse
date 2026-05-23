exception Error of string

type t = {
  mount_timeout_seconds : float;
  unmount_timeout_seconds : float;
  fs_timeout_seconds : float;
  keep_local : bool;
  log_excerpt_lines : int;
}

let env name = try Some (Sys.getenv name) with Not_found -> None

let parse_positive_float name default =
  match env name with
  | None -> default
  | Some value -> (
      try
        let parsed = float_of_string (String.trim value) in
        if parsed <= 0.0 then
          raise (Error (Printf.sprintf "%s must be greater than 0" name));
        parsed
      with Failure _ ->
        raise
          (Error
             (Printf.sprintf "%s must be a positive number, got %S" name value))
      )

let parse_positive_int name default =
  match env name with
  | None -> default
  | Some value -> (
      try
        let parsed = int_of_string (String.trim value) in
        if parsed <= 0 then
          raise (Error (Printf.sprintf "%s must be greater than 0" name));
        parsed
      with Failure _ ->
        raise
          (Error
             (Printf.sprintf "%s must be a positive integer, got %S" name value))
      )

let parse_bool name default =
  match env name with
  | None -> default
  | Some value -> (
      match String.lowercase_ascii (String.trim value) with
      | "1" | "true" | "yes" | "on" -> true
      | "0" | "false" | "no" | "off" -> false
      | _ ->
          raise
            (Error
               (Printf.sprintf
                  "%s must be a boolean (true/false, yes/no, on/off, 1/0), got \
                   %S"
                  name value)))

let load () =
  {
    mount_timeout_seconds =
      parse_positive_float "GDFUSE_E2E_MOUNT_TIMEOUT_SECONDS" 30.0;
    unmount_timeout_seconds =
      parse_positive_float "GDFUSE_E2E_UNMOUNT_TIMEOUT_SECONDS" 10.0;
    fs_timeout_seconds =
      parse_positive_float "GDFUSE_E2E_FS_TIMEOUT_SECONDS" 30.0;
    keep_local = parse_bool "GDFUSE_E2E_KEEP_LOCAL" false;
    log_excerpt_lines = parse_positive_int "GDFUSE_E2E_LOG_EXCERPT_LINES" 80;
  }

let describe settings =
  Printf.sprintf
    "mount_timeout=%.1fs; unmount_timeout=%.1fs; fs_timeout=%.1fs; \
     keep_local=%b; log_excerpt_lines=%d"
    settings.mount_timeout_seconds settings.unmount_timeout_seconds
    settings.fs_timeout_seconds settings.keep_local settings.log_excerpt_lines
