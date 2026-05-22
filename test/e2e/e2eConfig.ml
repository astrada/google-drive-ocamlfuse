exception Error of string

type t = {
  client_id : string;
  client_secret : string;
  refresh_token : string;
  test_folder_path : string;
}

let template_prefix = "replace-with-"

let is_template_value value =
  ExtString.String.starts_with value ~prefix:template_prefix

let trim = String.trim

let parse_string_field json key =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt key fields with
      | Some (`String value) ->
          let value = trim value in
          if value = "" then
            raise (Error (Printf.sprintf "%s must not be empty" key));
          if is_template_value value then
            raise
              (Error
                 (Printf.sprintf
                    "%s still contains the template placeholder value" key));
          value
      | Some _ -> raise (Error (Printf.sprintf "%s must be a string" key))
      | None -> raise (Error (Printf.sprintf "missing required key %s" key)))
  | _ -> raise (Error "configuration must be a JSON object")

let validate_test_folder_path path =
  if Filename.is_relative path then
    raise (Error "test_folder_path must be an absolute Drive path")
  else path

let validate_safe_area path =
  let normalized = Utils.normalize_absolute_path path in
  if normalized = "/" then
    raise (Error "test_folder_path must not be the Drive root");
  normalized

let load path =
  if not (Sys.file_exists path) then
    raise
      (Error
         (Printf.sprintf
            "missing %s; copy test/e2e/config.template.json to this path and \
             fill in local OAuth credentials"
            path));
  let json =
    try Yojson.Safe.from_file path with
    | Yojson.Json_error message ->
        raise (Error (Printf.sprintf "cannot parse %s: %s" path message))
    | Sys_error message ->
        raise (Error (Printf.sprintf "cannot read %s: %s" path message))
  in
  let client_id = parse_string_field json "client_id" in
  let client_secret = parse_string_field json "client_secret" in
  let refresh_token = parse_string_field json "refresh_token" in
  let test_folder_path =
    parse_string_field json "test_folder_path"
    |> validate_test_folder_path |> validate_safe_area
  in
  { client_id; client_secret; refresh_token; test_folder_path }

let redacted value =
  if value = "" then "<empty>"
  else Printf.sprintf "<redacted:%d chars>" (String.length value)

let describe config =
  Printf.sprintf
    "{ client_id = %S; client_secret = %s; refresh_token = %s; \
     test_folder_path = %S }"
    config.client_id
    (redacted config.client_secret)
    (redacted config.refresh_token)
    config.test_folder_path
