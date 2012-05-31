open Utils.Infix

type t = {
  auth_request_id : string;
  auth_request_date : GapiDate.t;
  refresh_token : string;
  last_access_token : string;
  access_token_date : GapiDate.t;
}

let auth_request_id = {
  GapiLens.get = (fun x -> x.auth_request_id);
  GapiLens.set = (fun v x -> { x with auth_request_id = v })
}
let auth_request_date = {
  GapiLens.get = (fun x -> x.auth_request_date);
  GapiLens.set = (fun v x -> { x with auth_request_date = v })
}
let refresh_token = {
  GapiLens.get = (fun x -> x.refresh_token);
  GapiLens.set = (fun v x -> { x with refresh_token = v })
}
let last_access_token = {
  GapiLens.get = (fun x -> x.last_access_token);
  GapiLens.set = (fun v x -> { x with last_access_token = v })
}
let access_token_date = {
  GapiLens.get = (fun x -> x.access_token_date);
  GapiLens.set = (fun v x -> { x with access_token_date = v })
}

let empty = {
  auth_request_id = "";
  auth_request_date = GapiDate.epoch;
  refresh_token = "";
  last_access_token = "";
  access_token_date = GapiDate.epoch;
}

let of_table table =
  let get = Hashtbl.find table in
    { auth_request_id = get "auth_request_id";
      auth_request_date = get "auth_request_date" |> GapiDate.of_string;
      refresh_token = get "refresh_token";
      last_access_token = get "last_access_token";
      access_token_date = get "access_token_date" |> GapiDate.of_string;
    }

let to_table data =
  let table = Hashtbl.create 8 in
  let add = Hashtbl.add table in
    add "auth_request_id" data.auth_request_id;
    GapiDate.to_string data.auth_request_date |> add "auth_request_date";
    add "refresh_token" data.refresh_token;
    add "last_access_token" data.last_access_token;
    GapiDate.to_string data.access_token_date |> add "access_token_date";
    table

