exception File_not_found = DriveMutations.File_not_found
exception IO_error = DriveMutations.IO_error
exception Invalid_operation = DriveMutations.Invalid_operation
exception Permission_denied = DriveMutations.Permission_denied

module type PORTS = sig
  val max_retries : unit -> int
  val wait_exponential_backoff : int -> unit
end

module Make (P : PORTS) = struct
  let match_service_error reason = function
    | GapiService.ServiceError (_, e) -> (
        match e.GapiError.RequestError.errors with
        | [] -> false
        | e :: _ -> e.GapiError.SingleError.reason = reason)
    | _ -> false

  let handle_default_exceptions = function
    | GapiService.ServiceError (_, e) -> (
        let message =
          e |> GapiError.RequestError.to_data_model
          |> GapiJson.data_model_to_json |> Yojson.Safe.to_string
        in
        Utils.log_with_header "Service error: %s.\n%!" message;
        match e.GapiError.RequestError.errors with
        | [] -> Utils.raise_m IO_error
        | e :: _ -> (
            match e.GapiError.SingleError.reason with
            | "userRateLimitExceeded" | "rateLimitExceeded" | "backendError"
            | "downloadQuotaExceeded" ->
                Utils.raise_m Utils.Temporary_error
            | "insufficientFilePermissions" | "insufficientPermissions" ->
                Utils.raise_m Permission_denied
            | _ -> Utils.raise_m IO_error))
    | GapiRequest.PermissionDenied _ ->
        Utils.log_with_header "Server error: Permission denied.\n%!";
        Utils.raise_m Permission_denied
    | GapiRequest.RequestTimeout _ ->
        Utils.log_with_header "Server error: Request Timeout.\n%!";
        Utils.raise_m Utils.Temporary_error
    | GapiRequest.PreconditionFailed _ | GapiRequest.Conflict _ ->
        Utils.log_with_header "Server error: Conflict.\n%!";
        Utils.raise_m Utils.Temporary_error
    | GapiRequest.Forbidden _ ->
        Utils.log_with_header "Server error: Forbidden.\n%!";
        Utils.raise_m IO_error
    | GapiRequest.Gone _ ->
        Utils.log_with_header "Server error: Gone.\n%!";
        Utils.raise_m IO_error
    | GapiRequest.BadRequest _ ->
        Utils.log_with_header "Server error: bad request.\n%!";
        Utils.raise_m Utils.Temporary_error
    | Buffering.Invalid_block -> Utils.raise_m Invalid_operation
    | GapiRequest.NotFound _ ->
        Utils.log_with_header "Server error: not found.\n%!";
        Utils.raise_m File_not_found
    | e -> Utils.raise_m e

  let try_with_default f s = Utils.try_with_m f handle_default_exceptions s

  let with_retry_default f =
    let rec loop n =
      Utils.try_with_m f (fun e s ->
          try handle_default_exceptions e s
          with Utils.Temporary_error ->
            if n >= P.max_retries () then Utils.raise_m IO_error s
            else (
              P.wait_exponential_backoff n;
              let n' = n + 1 in
              Utils.log_with_header "Retrying (%d/%d).\n%!" n'
                (P.max_retries ());
              loop n' s))
    in
    loop 0
end

module Default = Make (struct
  let max_retries () = !Utils.max_retries
  let wait_exponential_backoff = GapiUtils.wait_exponential_backoff
end)

let match_service_error = Default.match_service_error
let handle_default_exceptions = Default.handle_default_exceptions
let try_with_default = Default.try_with_default
let with_retry_default = Default.with_retry_default
