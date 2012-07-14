open GapiUtils.Infix
open GapiLens.Infix
open GapiMonad
open GapiMonad.SessionM.Infix

let scope = [GapiDriveV2Service.Scope.drive]

(* Gapi request wrapper *)
let do_request interact =
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
            refresh_token = state.State.refresh_token }
      in
        GapiConversation.with_session
          ~auth_context
          config
          curl_state
          interact
    with
        Failure message as e ->
          if ExtString.String.exists message "CURLE_OPERATION_TIMEOUTED" then
            (* Retry on timeout *)
            try_request (n + 1)
          else raise e
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
              |> Json_io.string_of_json
          in
            failwith message
  in
    try_request 0

(* Get access token using the installed apps flow *)
let get_access_token () =
  let context = Context.get_ctx () in
  let client_id = context |. Context.config_lens |. Config.client_id in
  let redirect_uri = "urn:ietf:wg:oauth:2.0:oob" in
  let url = GapiOAuth2.authorization_code_url
              ~redirect_uri
              ~scope
              ~response_type:"code"
              client_id in
  Utils.start_browser url;
  Printf.printf "Please enter the verification code: %!";
  let code = input_line stdin in
  let client_secret = context |. Context.config_lens |. Config.client_secret in
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

