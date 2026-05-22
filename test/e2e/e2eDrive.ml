open GapiLens.Infix
module File = GapiDriveV3Model.File
module FileList = GapiDriveV3Model.FileList
module FilesResource = GapiDriveV3Service.FilesResource

exception Error of string

type t = {
  config : E2eConfig.t;
  gapi_config : GapiConfig.t;
  mutable access_token : string;
}

let folder_mime_type = "application/vnd.google-apps.folder"

let file_fields =
  "id,name,mimeType,parents,trashed,capabilities(canAddChildren,canTrash)"

let std_params fields =
  {
    GapiService.StandardParameters.default with
    GapiService.StandardParameters.fields;
  }

let file_std_params = std_params file_fields

let file_list_std_params =
  std_params ("files(" ^ file_fields ^ "),nextPageToken")

let create_gapi_config config =
  let gdfuse_config =
    {
      Config.default with
      client_id = config.E2eConfig.client_id;
      client_secret = config.client_secret;
    }
  in
  Config.create_gapi_config gdfuse_config false "" ""

let extract_access_token response =
  let oauth2_access_token =
    response |. GapiAuthResponse.oauth2_access_token |. GapiLens.option_get
  in
  oauth2_access_token.GapiAuthResponse.OAuth2.access_token

let refresh_access_token t =
  try
    let response, _ =
      GapiConversation.with_curl t.gapi_config (fun session ->
          GapiOAuth2.refresh_access_token
            ~client_id:t.config.E2eConfig.client_id
            ~client_secret:t.config.client_secret
            ~refresh_token:t.config.refresh_token session)
    in
    let access_token = extract_access_token response in
    if access_token = "" then
      raise (Error "OAuth refresh returned an empty access token");
    t.access_token <- access_token
  with
  | Error _ as e -> raise e
  | GapiOAuth2.InvalidGrant _ ->
      raise (Error "OAuth refresh token is invalid or expired")
  | GapiOAuth2.InvalidRequest _ ->
      raise (Error "OAuth refresh request was rejected")
  | GapiOAuth2.DeletedClient _ -> raise (Error "OAuth client has been deleted")
  | e ->
      raise
        (Error
           (Printf.sprintf "cannot refresh OAuth access token: %s"
              (Printexc.to_string e)))

let create config =
  let t =
    { config; gapi_config = create_gapi_config config; access_token = "" }
  in
  refresh_access_token t;
  t

let auth_context t =
  GapiConversation.Session.OAuth2
    {
      GapiConversation.Session.oauth2_token = t.access_token;
      refresh_token = t.config.E2eConfig.refresh_token;
    }

let run t request =
  let run_once () =
    let result, _ =
      GapiConversation.with_curl ~auth_context:(auth_context t) t.gapi_config
        request
    in
    result
  in
  try run_once ()
  with GapiRequest.Unauthorized _ | GapiRequest.RefreshTokenFailed _ ->
    refresh_access_token t;
    run_once ()

let preflight t =
  try
    ignore
      (run t
         (FilesResource.get ~supportsAllDrives:true ~std_params:file_std_params
            ~fileId:"root"))
  with e ->
    raise
      (Error
         (Printf.sprintf "Drive API preflight failed: %s" (Printexc.to_string e)))

let escape_apostrophe =
  let regexp = Str.regexp (Str.quote "'") in
  fun value -> Str.global_replace regexp "\\'" value

let folder_query ~parent_id ~name =
  Printf.sprintf
    "name='%s' and '%s' in parents and trashed=false and mimeType='%s'"
    (escape_apostrophe name) parent_id folder_mime_type

let find_folder t ~parent_id ~name =
  let file_list =
    run t
      (FilesResource.list ~supportsAllDrives:true
         ~std_params:file_list_std_params
         ~q:(folder_query ~parent_id ~name)
         ~pageSize:10)
  in
  match file_list.FileList.files with
  | [] -> None
  | [ file ] -> Some file
  | _ ->
      raise
        (Error
           (Printf.sprintf
              "safe-area path is ambiguous: multiple folders named %S under \
               Drive parent %s"
              name parent_id))

let create_folder t ~parent_id ~name =
  let file =
    {
      File.empty with
      File.name;
      mimeType = folder_mime_type;
      parents = [ parent_id ];
    }
  in
  run t
    (FilesResource.create ~enforceSingleParent:true ~supportsAllDrives:true
       ~std_params:file_std_params file)

let path_segments path =
  String.split_on_char '/' path |> List.filter (fun segment -> segment <> "")

let find_or_create_folder t ~parent_id ~name =
  match find_folder t ~parent_id ~name with
  | Some folder -> folder
  | None ->
      Printf.printf "Creating e2e Drive folder %S under parent %s\n%!" name
        parent_id;
      create_folder t ~parent_id ~name

let resolve_or_create_path t path =
  let rec loop parent_id = function
    | [] -> parent_id
    | segment :: rest ->
        let folder = find_or_create_folder t ~parent_id ~name:segment in
        loop folder.File.id rest
  in
  loop "root" (path_segments path)

let create_run_root t ~safe_parent_id ~run_id =
  let name = "google-drive-ocamlfuse-e2e-" ^ run_id in
  let file =
    {
      File.empty with
      File.name;
      mimeType = folder_mime_type;
      parents = [ safe_parent_id ];
      appProperties = [ ("google_drive_ocamlfuse_e2e_run_id", run_id) ];
    }
  in
  run t
    (FilesResource.create ~enforceSingleParent:true ~supportsAllDrives:true
       ~std_params:file_std_params file)

let trash_file t ~file_id =
  let patch = { File.empty with File.trashed = true } in
  ignore
    (run t
       (FilesResource.update ~enforceSingleParent:true ~supportsAllDrives:true
          ~std_params:file_std_params ~fileId:file_id patch))
