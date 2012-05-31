open Utils.Infix
open GapiLens.Infix
open GapiMonad.SessionM.Infix
open GdataDocumentsV3Model
open GdataDocumentsV3Service

let do_request
      initial_context
      interact =
  let rec try_request context =
    try
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
          if ExtString.String.exists
               message "CURLE_OPERATION_TIMEOUTED" then
            (* Retry on timeout *)
            try_request context
          else begin
            Utils.log_message "Failure %s\n%!" message;
            raise e
          end
      | GapiRequest.RefreshTokenFailed _ ->
          let updated_context =
            try
              GaeProxy.refresh_access_token context
            with e ->
              Utils.log_message "Exception: %s\n%!" (Printexc.to_string e);
              raise e
          in
            Utils.log_message "done.\n%!";
            Context.StateFileStore.save updated_context.Context.state_store;
            (* Retry with refreshed token *)
            try_request updated_context
      | GapiService.ServiceError e ->
          Utils.log_message "ServiceError\n%!";
          let message =
            e |> GapiError.RequestError.to_data_model
              |> GapiJson.data_model_to_json
              |> Json_io.string_of_json
          in
            failwith message
  in
    try_request initial_context

let get_dir_list context =
    do_request
      context
      (fun session ->
         let parameters = QueryParameters.default
           |> QueryParameters.showfolders ^= true in
         let (feed, session) =
           query_folder_contents
             ~parameters
             root_folder_id
             session
         in
           List.map
             (fun entry ->
                entry |. Document.Entry.title |. GdataAtom.Title.value)
             feed.Document.Feed.entries)

