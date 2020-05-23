open GapiUtils.Infix

type t = {
  (* GAE proxy request id *)
  auth_request_id : string;
  (* GAE proxy request date *)
  auth_request_date : GapiDate.t;
  (* OAuth2 refresh token *)
  refresh_token : string;
  (* OAuth2 last obtained access token *)
  last_access_token : string;
  (* last access token date *)
  access_token_date : GapiDate.t;
  (* last known version *)
  saved_version : string;
}

let auth_request_id =
  {
    GapiLens.get = (fun x -> x.auth_request_id);
    GapiLens.set = (fun v x -> { x with auth_request_id = v });
  }

let auth_request_date =
  {
    GapiLens.get = (fun x -> x.auth_request_date);
    GapiLens.set = (fun v x -> { x with auth_request_date = v });
  }

let refresh_token =
  {
    GapiLens.get = (fun x -> x.refresh_token);
    GapiLens.set = (fun v x -> { x with refresh_token = v });
  }

let last_access_token =
  {
    GapiLens.get = (fun x -> x.last_access_token);
    GapiLens.set = (fun v x -> { x with last_access_token = v });
  }

let access_token_date =
  {
    GapiLens.get = (fun x -> x.access_token_date);
    GapiLens.set = (fun v x -> { x with access_token_date = v });
  }

let saved_version =
  {
    GapiLens.get = (fun x -> x.saved_version);
    GapiLens.set = (fun v x -> { x with saved_version = v });
  }

let empty =
  {
    auth_request_id = "";
    auth_request_date = GapiDate.epoch;
    refresh_token = "";
    last_access_token = "";
    access_token_date = GapiDate.epoch;
    saved_version = "";
  }

let of_table table =
  let get k = Utils.get_from_string_table table k in
  {
    auth_request_id = get "auth_request_id" Std.identity empty.auth_request_id;
    auth_request_date =
      get "auth_request_date" GapiDate.of_string empty.auth_request_date;
    refresh_token = get "refresh_token" Std.identity empty.refresh_token;
    last_access_token =
      get "last_access_token" Std.identity empty.last_access_token;
    access_token_date =
      get "access_token_date" GapiDate.of_string empty.access_token_date;
    saved_version = get "saved_version" Std.identity empty.saved_version;
  }

let to_table data =
  let table = Hashtbl.create 8 in
  let add = Hashtbl.add table in
  add "auth_request_id" data.auth_request_id;
  GapiDate.to_string data.auth_request_date |> add "auth_request_date";
  add "refresh_token" data.refresh_token;
  add "last_access_token" data.last_access_token;
  GapiDate.to_string data.access_token_date |> add "access_token_date";
  add "saved_version" data.saved_version;
  table
