open Utils.Infix
open GapiLens.Infix

exception ServerError of string

let gae_proxy_url = "http://localhost:8080"

let gae_proxy_request page query_string context =
  let page_url = Printf.sprintf "%s/%s?%s" gae_proxy_url page query_string in
    GapiConversation.with_curl
      context.Context.gapi_config
      (fun session ->
         let (tokens, _) =
           GapiConversation.request
             GapiCore.HttpMethod.GET
             session
             page_url
             (fun pipe code headers session ->
                let response = GapiConversation.read_all pipe in
                if code <> 200 then begin
                  Utils.log_message "fail\n%!";
                  raise (ServerError (Printf.sprintf
                                        "Server response: %s (code=%d)"
                                        response code));
                end else begin
                  match response with
                      "Not_found" ->
                        Utils.log_message "not found, retrying\n%!";
                        raise Not_found
                    | "access_denied"
                    | "ConflictError"
                    | "Exception" as error_code ->
                        Utils.log_message "fail (error_code=%s)\n%!" error_code;
                        raise (ServerError ("error_code " ^ error_code))
                    | "Missing_request_id" ->
                        failwith "Bug! Missing_request_id"
                    | "Missing_refresh_token" ->
                        failwith "Bug! Missing_refresh_token"
                    | _ -> ()
                end;
                Utils.log_message "ok\n%!";
                let json = Json_io.json_of_string response in
                let obj = Json_type.Browse.objekt json in
                  Json_type.Browse.make_table obj
             )
         in
           tokens)

let get_tokens context =
  Utils.log_message "Getting tokens from GAE proxy...";
  let request_id = context |. Context.request_id_lens in
  let query_string =
    Netencoding.Url.mk_url_encoded_parameters [("requestid", request_id)] in
  let table = gae_proxy_request "gettokens" query_string context in
  let open Json_type.Browse in
    context
    |> Context.state_lens ^= {
       State.auth_request_id =
         field table "request_id" |> string;
       auth_request_date =
         field table "refresh_date" |> string |> GapiDate.of_string;
       refresh_token = field table "refresh_token" |> string;
       last_access_token = field table "access_token" |> string;
       access_token_date =
         field table "refresh_date" |> string |> GapiDate.of_string;
     }

let start_server_polling context =
  let rec loop n =
    if n = 24 then failwith "Cannot retrieve auth tokens: Timeout expired";
    try
      get_tokens context
    with Not_found ->
      Unix.sleep 5;
      loop (succ n)
  in
    loop 0

let refresh_access_token context =
  Utils.log_message "Refreshing access token...";
  let token = context |. Context.refresh_token_lens in
  let query_string =
    Netencoding.Url.mk_url_encoded_parameters [("token", token)] in
  let table = gae_proxy_request "refreshtoken" query_string context in
  let current_state = context |. Context.state_lens in
  let open Json_type.Browse in
    context
    |> Context.state_lens ^= {
       current_state with
           State.last_access_token = field table "access_token" |> string;
           access_token_date =
             field table "refresh_date" |> string |> GapiDate.of_string;
     }

