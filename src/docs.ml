open Utils.Infix
open GapiLens.Infix
open GapiMonad
open GapiMonad.SessionM.Infix
open GdataDocumentsV3Model
open GdataDocumentsV3Service

(* Gapi request wrapper *)
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
(* END Gapi request wrapper *)

(* Resources *)
let cache_lookup path map =
  let cache = Context.get_ctx () |. Context.cache in
  let resource = Cache.select_resource_with_path path cache in
    Option.map map resource

let get_resource_id parent_folder_id title =
  let parameters = QueryParameters.default
    |> QueryParameters.showfolders ^= true
    |> QueryParameters.title ^= title
    |> QueryParameters.title_exact ^= true
  in
    query_folder_contents ~parameters parent_folder_id >>= fun feed ->
      SessionM.return
        (feed
           |. Document.Feed.entries
           |. GapiLens.head
           |. Document.Entry.resourceId)

(* END Resources *)

(* Folders *)
let root_directory = "/"

let rec get_folder_id path =
  if path = root_directory then
    SessionM.return root_folder_id
  else
    let cached_id =
      cache_lookup path
        (fun resource -> resource.Cache.resource_id |> Option.get)
    in
      match cached_id with
          Some id -> SessionM.return id
        | None ->
            let parent_path = Filename.dirname path in
            let title = Filename.basename path in
              get_folder_id parent_path >>= fun parent_folder_id ->
              get_resource_id parent_folder_id title

let get_dir_list path =
  let get_feed =
    let parameters = QueryParameters.default
      |> QueryParameters.showfolders ^= true
    in
      get_folder_id path >>= fun folder_id ->
      query_folder_contents ~parameters folder_id >>= fun feed ->
        SessionM.return feed
  in

  let feed = do_request get_feed |> fst in
  let dir_list =
    List.map
      (fun entry ->
         entry |. Document.Entry.title |. GdataAtom.Title.value)
      feed.Document.Feed.entries
  in
    dir_list
(* END Folders *)

