open GapiUtils.Infix
open GapiLens.Infix
open GapiMonad
open GapiMonad.SessionM.Infix

let scope = [GapiDriveV2Service.Scope.drive]

(* Gapi request wrapper *)
let do_request go =
  let update_state session =
    let context = Context.get_ctx () in
    let state = context |. Context.state_lens in
    let access_token = session
      |. GapiConversation.Session.auth
      |. GapiConversation.Session.oauth2
      |. GapiLens.option_get
      |. GapiConversation.Session.oauth2_token in
    if state.State.last_access_token <> access_token then begin
      context
      |> (Context.state_lens |-- State.last_access_token) ^= access_token
      |> Context.save_state_from_context
    end in
  let interact session =
    let (_, session') as result = go session in
    update_state session';
    result in
  let rec try_request n =
    try
      let context = Context.get_ctx () in
      let state = context |. Context.state_lens in
      let config = context.Context.gapi_config in
      let curl_state = context.Context.curl_state in
      let auth_context =
        GapiConversation.Session.OAuth2
          { GapiConversation.Session.oauth2_token =
              state.State.last_access_token;
            refresh_token = state.State.refresh_token
          }
      in
        GapiConversation.with_session
          ~auth_context
          config
          curl_state
          interact
    with
        Failure message as e ->
          let check_curl_error () =
            ExtString.String.exists message "CURLE_OPERATION_TIMEOUTED" ||
            ExtString.String.exists message "CURLE_COULDNT_RESOLVE_HOST" ||
            ExtString.String.exists message "CURLE_COULDNT_RESOLVE_PROXY" ||
            ExtString.String.exists message "CURLE_COULDNT_CONNECT" ||
            ExtString.String.exists message "CURLE_SSL_CONNECT_ERROR" ||
            ExtString.String.exists message "CURLE_SEND_ERROR" ||
            ExtString.String.exists message "CURLE_RECV_ERROR"
          in
          Utils.log_message "Error during request: %s\n%!" message;
          if check_curl_error () && n < !Utils.max_retries then begin
            let n' = n + 1 in
            Utils.log_message "Retrying (%d/%d)\n%!" n' !Utils.max_retries;
            GapiUtils.wait_exponential_backoff n;
            (* Retry on timeout *)
            try_request n'
          end else begin
            Utils.log_message "Giving up\n%!";
            raise e
          end
      | GapiRequest.Unauthorized _
      | GapiRequest.RefreshTokenFailed _ ->
          if n > 0 then failwith "Cannot access resource: \
                                  Refreshing token was not enough";
          GaeProxy.refresh_access_token ();
          (* Retry with refreshed token *)
          try_request (n + 1)
      | GapiService.ServiceError e ->
          Utils.log_message "ServiceError\n%!";
          let message =
            e |> GapiError.RequestError.to_data_model
              |> GapiJson.data_model_to_json
              |> Yojson.Safe.to_string
          in
            failwith message
  in
    try_request 0

(* Get access token using the installed apps flow or print authorization URL
 * if headleass mode is on *)
let get_access_token headless =
  let context = Context.get_ctx () in
  let config_lens = context |. Context.config_lens in
  let client_id = config_lens |. Config.client_id in
  let client_secret = config_lens |. Config.client_secret in
  let verification_code = config_lens |. Config.verification_code in
  let redirect_uri = "urn:ietf:wg:oauth:2.0:oob" in
  let code = 
    if verification_code = "" then
      let url = GapiOAuth2.authorization_code_url
                  ~redirect_uri
                  ~scope
                  ~response_type:"code"
                  client_id in
      if headless then begin
        Printf.printf
          "Please, open the following URL in a web browser: %s\n%!"
          url;
      end else Utils.start_browser url;
      Printf.printf "Please enter the verification code: %!";
      input_line stdin
    else
      verification_code
  in
  try
    let (response, _) =
      do_request
        (fun session ->
           GapiOAuth2.get_access_token
             ~client_id
             ~client_secret
             ~code
             ~redirect_uri
             session) in
    Printf.printf "Access token retrieved correctly.\n%!";
    let { GapiAuthResponse.OAuth2.access_token;
          refresh_token;
          _ } =
      response
      |. GapiAuthResponse.oauth2_access_token
      |. GapiLens.option_get in
    let now = GapiDate.now () in
    let current_state = context |. Context.state_lens in
      context
      |> Context.state_lens ^=
        { current_state with
              State.auth_request_date = now;
              refresh_token;
              last_access_token = access_token;
              access_token_date = now;
        }
      |> Context.save_state_from_context
  with e ->
    prerr_endline "Cannot retrieve auth tokens.";
    Printexc.to_string e |> prerr_endline;
    exit 1

