open OUnit
module RequestHandling = DriveRequestHandling

let session =
  {
    GapiConversation.Session.curl = GapiCurl.Initialized;
    config = GapiConfig.default;
    auth = GapiConversation.Session.NoAuth;
    cookies = [];
    etag = "";
  }

let run_session request = fst (request session)
let pipe () = GapiPipe.OcamlnetPipe.create ()

let single_error reason =
  { GapiError.SingleError.empty with GapiError.SingleError.reason }

let request_error errors =
  { GapiError.RequestError.errors; code = 500; message = "error" }

let service_error errors =
  GapiService.ServiceError (session, request_error errors)

let assert_raises_m expected request =
  assert_raises expected (fun () -> ignore (run_session request))

module FakePorts = struct
  let max_retries_value = ref 2
  let waits = ref []

  let reset () =
    max_retries_value := 2;
    waits := []

  let max_retries () = !max_retries_value
  let wait_exponential_backoff n = waits := !waits @ [ n ]
end

module Ops = RequestHandling.Make (FakePorts)

let test_match_service_error_matches_first_reason () =
  assert_bool "expected matching reason"
    (Ops.match_service_error "backendError"
       (service_error [ single_error "backendError" ]))

let test_match_service_error_rejects_different_reason () =
  assert_bool "expected non-matching reason"
    (not
       (Ops.match_service_error "backendError"
          (service_error [ single_error "rateLimitExceeded" ])))

let test_match_service_error_rejects_empty_service_error () =
  assert_bool "expected empty service errors to be ignored"
    (not (Ops.match_service_error "backendError" (service_error [])))

let test_match_service_error_rejects_non_service_error () =
  assert_bool "expected non-service error to be ignored"
    (not (Ops.match_service_error "backendError" (Failure "not service")))

let test_service_retry_reasons_map_to_temporary_error () =
  List.iter
    (fun reason ->
      assert_raises_m Utils.Temporary_error
        (Ops.handle_default_exceptions (service_error [ single_error reason ])))
    [
      "userRateLimitExceeded";
      "rateLimitExceeded";
      "backendError";
      "downloadQuotaExceeded";
    ]

let test_service_permission_reasons_map_to_permission_denied () =
  List.iter
    (fun reason ->
      assert_raises_m DriveMutations.Permission_denied
        (Ops.handle_default_exceptions (service_error [ single_error reason ])))
    [ "insufficientFilePermissions"; "insufficientPermissions" ]

let test_service_unknown_and_empty_errors_map_to_io_error () =
  assert_raises_m DriveMutations.IO_error
    (Ops.handle_default_exceptions
       (service_error [ single_error "unknownReason" ]));
  assert_raises_m DriveMutations.IO_error
    (Ops.handle_default_exceptions (service_error []))

let test_request_exceptions_map_to_drive_exceptions () =
  assert_raises_m DriveMutations.Permission_denied
    (Ops.handle_default_exceptions (GapiRequest.PermissionDenied session));
  assert_raises_m Utils.Temporary_error
    (Ops.handle_default_exceptions (GapiRequest.RequestTimeout session));
  assert_raises_m Utils.Temporary_error
    (Ops.handle_default_exceptions (GapiRequest.PreconditionFailed session));
  assert_raises_m Utils.Temporary_error
    (Ops.handle_default_exceptions (GapiRequest.Conflict session));
  assert_raises_m Utils.Temporary_error
    (Ops.handle_default_exceptions
       (GapiRequest.BadRequest (session, 400, pipe ())));
  assert_raises_m DriveMutations.IO_error
    (Ops.handle_default_exceptions
       (GapiRequest.Forbidden (session, 403, pipe ())));
  assert_raises_m DriveMutations.IO_error
    (Ops.handle_default_exceptions (GapiRequest.Gone session));
  assert_raises_m DriveMutations.Invalid_operation
    (Ops.handle_default_exceptions Buffering.Invalid_block);
  assert_raises_m DriveMutations.File_not_found
    (Ops.handle_default_exceptions
       (GapiRequest.NotFound (session, 404, pipe ())))

let test_unknown_exception_propagates () =
  assert_raises_m (Failure "passthrough")
    (Ops.handle_default_exceptions (Failure "passthrough"))

let test_try_with_default_returns_success () =
  let request session = ("ok", session) in
  assert_equal "ok" (run_session (Ops.try_with_default request))

let test_try_with_default_translates_failure () =
  let request _session = raise (GapiRequest.PermissionDenied session) in
  assert_raises_m DriveMutations.Permission_denied
    (Ops.try_with_default request)

let test_with_retry_default_retries_temporary_failures_until_success () =
  FakePorts.reset ();
  let attempts = ref 0 in
  let request session =
    incr attempts;
    if !attempts <= 2 then raise (GapiRequest.RequestTimeout session)
    else ("ok", session)
  in
  assert_equal "ok" (run_session (Ops.with_retry_default request));
  assert_equal 3 !attempts;
  assert_equal [ 0; 1 ] !FakePorts.waits

let test_with_retry_default_exhaustion_raises_io_error () =
  FakePorts.reset ();
  FakePorts.max_retries_value := 1;
  let attempts = ref 0 in
  let request session =
    incr attempts;
    raise (GapiRequest.RequestTimeout session)
  in
  assert_raises_m DriveMutations.IO_error (Ops.with_retry_default request);
  assert_equal 2 !attempts;
  assert_equal [ 0 ] !FakePorts.waits

let test_with_retry_default_does_not_retry_non_temporary_failures () =
  FakePorts.reset ();
  let attempts = ref 0 in
  let request session =
    incr attempts;
    raise (GapiRequest.PermissionDenied session)
  in
  assert_raises_m DriveMutations.Permission_denied
    (Ops.with_retry_default request);
  assert_equal 1 !attempts;
  assert_equal [] !FakePorts.waits

let suite =
  "DriveRequestHandling tests"
  >::: [
         "test_match_service_error_matches_first_reason"
         >:: test_match_service_error_matches_first_reason;
         "test_match_service_error_rejects_different_reason"
         >:: test_match_service_error_rejects_different_reason;
         "test_match_service_error_rejects_empty_service_error"
         >:: test_match_service_error_rejects_empty_service_error;
         "test_match_service_error_rejects_non_service_error"
         >:: test_match_service_error_rejects_non_service_error;
         "test_service_retry_reasons_map_to_temporary_error"
         >:: test_service_retry_reasons_map_to_temporary_error;
         "test_service_permission_reasons_map_to_permission_denied"
         >:: test_service_permission_reasons_map_to_permission_denied;
         "test_service_unknown_and_empty_errors_map_to_io_error"
         >:: test_service_unknown_and_empty_errors_map_to_io_error;
         "test_request_exceptions_map_to_drive_exceptions"
         >:: test_request_exceptions_map_to_drive_exceptions;
         "test_unknown_exception_propagates"
         >:: test_unknown_exception_propagates;
         "test_try_with_default_returns_success"
         >:: test_try_with_default_returns_success;
         "test_try_with_default_translates_failure"
         >:: test_try_with_default_translates_failure;
         "test_with_retry_default_retries_temporary_failures_until_success"
         >:: test_with_retry_default_retries_temporary_failures_until_success;
         "test_with_retry_default_exhaustion_raises_io_error"
         >:: test_with_retry_default_exhaustion_raises_io_error;
         "test_with_retry_default_does_not_retry_non_temporary_failures"
         >:: test_with_retry_default_does_not_retry_non_temporary_failures;
       ]
