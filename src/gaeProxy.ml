open GapiUtils.Infix
open GapiLens.Infix

exception ServerError of string

let gae_proxy_url = "https://gd-ocaml-auth.appspot.com"

let gae_proxy_request page query_string =
  let context = Context.get_ctx () in
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
              let json = Yojson.Safe.from_string response in
              let fields =
                match json with
                    `Assoc xs -> xs
                  | _ ->
                      failwith ("Unexpected response from proxy: " ^ response)
              in
              let table = Hashtbl.create 8 in
              List.iter
                (fun (n, v) -> Hashtbl.add table n v)
                fields;
              table)
       in
       tokens)

let get_string_field table name =
  match Hashtbl.find table name with
      `String s -> s
    | _ -> failwith ("Cannot get " ^ name ^ " field from JSON response")

let get_tokens () =
  Utils.log_message "Getting tokens from GAE proxy...";
  let context = Context.get_ctx () in
  let request_id = context |. Context.request_id_lens in
  let query_string =
    Netencoding.Url.mk_url_encoded_parameters [("requestid", request_id)] in
  let table = gae_proxy_request "gettokens" query_string in
  let get_string = get_string_field table in
  let current_state = context |. Context.state_lens in
  context
    |> Context.state_lens ^=
         { current_state with
               State.auth_request_id = get_string "request_id";
               auth_request_date =
                 get_string "refresh_date" |> GapiDate.of_string;
               refresh_token = get_string "refresh_token";
               last_access_token = get_string "access_token";
               access_token_date =
                 get_string "refresh_date" |> GapiDate.of_string;
         }
    |> Context.save_state_from_context

let start_server_polling () =
  let rec loop n =
    if n = 24 then failwith "Cannot retrieve auth tokens: Timeout expired";
    try
      get_tokens ();
      Printf.printf "Access token retrieved correctly.\n%!"
    with Not_found ->
      Unix.sleep 5;
      loop (succ n)
  in
    loop 0

let refresh_access_token () =
  Utils.log_message "Refreshing access token...";
  let context = Context.get_ctx () in
  let token = context |. Context.refresh_token_lens in
  let query_string =
    Netencoding.Url.mk_url_encoded_parameters [("token", token)] in
  let table = gae_proxy_request "refreshtoken" query_string in
  let get_string = get_string_field table in
  let current_state = context |. Context.state_lens in
  context
    |> Context.state_lens ^=
      { current_state with
            State.last_access_token = get_string "access_token";
            access_token_date = get_string "refresh_date" |> GapiDate.of_string;
      }
    |> Context.save_state_from_context

