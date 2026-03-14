type inputs = {
  persisted : Config.t;
  created : bool;
  migrated : bool;
  upgraded : bool;
  cli_client_id : string;
  cli_client_secret : string;
  cli_service_account_credentials_path : string;
  cli_service_account_user_to_impersonate : string;
  cli_log_to : string;
  cli_scope : string;
  cli_redirect_uri : string;
  cli_docs_mode : string;
  cli_port : int;
  device : bool;
  multi_threading : bool;
}

type result = {
  runtime_config : Config.t;
  persisted_config : Config.t;
  should_persist : bool;
  clear_cache : bool;
}

let apply_docs_mode docs_mode config =
  if docs_mode = "libreoffice" then
    {
      config with
      Config.download_docs = true;
      document_format = "odt";
      drawing_format = "png";
      form_format = "zip";
      presentation_format = "odp";
      spreadsheet_format = "ods";
      apps_script_format = "json";
    }
  else if docs_mode = "msoffice" then
    {
      config with
      Config.download_docs = true;
      document_format = "docx";
      drawing_format = "png";
      form_format = "zip";
      presentation_format = "pptx";
      spreadsheet_format = "xlsx";
      apps_script_format = "json";
    }
  else if docs_mode = "desktop" then
    {
      config with
      Config.download_docs = true;
      document_format = "desktop";
      drawing_format = "desktop";
      form_format = "desktop";
      presentation_format = "desktop";
      spreadsheet_format = "desktop";
      apps_script_format = "desktop";
    }
  else if docs_mode = "off" then
    { config with Config.download_docs = false }
  else if docs_mode <> "" then failwith ("Unsupported docsmode: " ^ docs_mode)
  else config

let resolve inputs =
  let persisted = inputs.persisted in
  let client_id =
    if inputs.cli_client_id = "" then persisted.Config.client_id
    else inputs.cli_client_id
  in
  let client_secret =
    if inputs.cli_client_secret = "" then persisted.Config.client_secret
    else inputs.cli_client_secret
  in
  let service_account_credentials_path =
    if inputs.cli_service_account_credentials_path = "" then
      persisted.Config.service_account_credentials_path
    else inputs.cli_service_account_credentials_path
  in
  let service_account_user_to_impersonate =
    if inputs.cli_service_account_user_to_impersonate = "" then
      persisted.Config.service_account_user_to_impersonate
    else inputs.cli_service_account_user_to_impersonate
  in
  let scope =
    if inputs.device then Drive.device_scope
    else if inputs.cli_scope = "" then persisted.Config.scope
    else inputs.cli_scope
  in
  let redirect_uri =
    if inputs.cli_redirect_uri = "" then persisted.Config.redirect_uri
    else inputs.cli_redirect_uri
  in
  let log_to =
    if inputs.cli_log_to = "" then persisted.Config.log_to else inputs.cli_log_to
  in
  let sqlite3_busy_timeout =
    if inputs.multi_threading && persisted.Config.sqlite3_busy_timeout = 500 then
      5000
    else persisted.Config.sqlite3_busy_timeout
  in
  let base_runtime_config =
    {
      persisted with
      Config.client_id;
      client_secret;
      sqlite3_busy_timeout;
      service_account_credentials_path;
      service_account_user_to_impersonate;
      log_to;
      scope;
      redirect_uri;
      oauth2_loopback_port = inputs.cli_port;
    }
  in
  let runtime_config = apply_docs_mode inputs.cli_docs_mode base_runtime_config |> Config.validate in
  let persisted_config =
    {
      persisted with
      Config.client_id;
      client_secret;
    }
  in
  let should_persist =
    inputs.created || inputs.migrated || inputs.upgraded
    || persisted_config.Config.client_id <> persisted.Config.client_id
    || persisted_config.Config.client_secret <> persisted.Config.client_secret
  in
  let clear_cache = inputs.cli_docs_mode <> "" && base_runtime_config <> runtime_config in
  { runtime_config; persisted_config; should_persist; clear_cache }
