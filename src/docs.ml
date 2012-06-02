open Utils.Infix
open GapiLens.Infix
open GapiMonad
open GapiMonad.SessionM.Infix
open GdataDocumentsV3Model
open GdataDocumentsV3Service

let do_request interact =
  let rec try_request () =
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
            try_request ()
          else begin
            Utils.log_message "Failure %s\n%!" message;
            raise e
          end
      | GapiRequest.RefreshTokenFailed _ ->
          begin try
            GaeProxy.refresh_access_token ();
            Utils.log_message "done.\n%!";
            (* Retry with refreshed token *)
            try_request ()
          with e ->
            Utils.log_message "Exception: %s\n%!" (Printexc.to_string e);
            raise e
          end
      | GapiService.ServiceError e ->
          Utils.log_message "ServiceError\n%!";
          let message =
            e |> GapiError.RequestError.to_data_model
              |> GapiJson.data_model_to_json
              |> Json_io.string_of_json
          in
            failwith message
  in
    try_request ()

let get_folder_id path =
  root_folder_id

let get_dir_list path =
  let folder_id =
    if path = "/" then root_folder_id
    else get_folder_id path in

  let get_feed =
    let parameters = QueryParameters.default
      |> QueryParameters.showfolders ^= true
    in
      query_folder_contents ~parameters folder_id >>= fun feed ->
          SessionM.return feed
  in

  let (feed, _) =
    do_request get_feed in
  let dir_list =
    List.map
      (fun entry ->
         entry |. Document.Entry.title |. GdataAtom.Title.value)
      feed.Document.Feed.entries
  in
    dir_list

