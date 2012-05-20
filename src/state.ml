open Utils.Infix

type t = {
  auth_request_id : string;
  auth_request_date : GapiDate.t;
  user_id : string;
  refresh_token : string;
  last_access_token : string;
}

let of_table table =
  let get = Hashtbl.find table in
    { auth_request_id = get "auth_request_id";
      auth_request_date = get "auth_request_date" |> GapiDate.of_string;
      user_id = get "user_id";
      refresh_token = get "refresh_token";
      last_access_token = get "last_access_token";
    }

let to_table data =
  let add = Hashtbl.add table in
    add "auth_request_id" data.auth_request_id;
    GapiDate.to_string data.auth_request_date |> add "auth_request_date";
    add "user_id" data.user_id;
    add "refresh_token" data.refresh_token;
    add "last_access_token" data.last_access_token

