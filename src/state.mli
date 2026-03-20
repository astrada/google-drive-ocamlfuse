type t = {
  auth_request_id : string;
  auth_request_date : GapiDate.t;
  refresh_token : string;
  last_access_token : string;
  access_token_date : GapiDate.t;
  saved_version : string;
}

val auth_request_id : (t, string) GapiLens.t
val auth_request_date : (t, GapiDate.t) GapiLens.t
val refresh_token : (t, string) GapiLens.t
val last_access_token : (t, string) GapiLens.t
val access_token_date : (t, GapiDate.t) GapiLens.t
val saved_version : (t, string) GapiLens.t
val empty : t
val of_table : (string, string) Hashtbl.t -> t
val to_table : t -> (string, string) Hashtbl.t
