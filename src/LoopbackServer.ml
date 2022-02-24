open GapiLens.Infix
module S = Tiny_httpd

let build_page title h3 body =
  Printf.sprintf
    "<!DOCTYPE html>\n\
     <html lang=\"en\">\n\
     <head>\n\
     <title>%s</title>\n\
     </head>\n\
     <body>\n\
     <h3>%s</h3>\n\
     %s\n\
     </body>\n\
     </html>"
    title h3 body

let start port =
  let server = S.create ~port () in
  S.add_route_handler server
    S.Route.(exact "oauth2callback" @/ return)
    (fun req ->
      let query = S.Request.query req in
      let response =
        try
          let verification_code =
            List.find (fun (key, _) -> key = "code") query |> snd
          in
          Context.update_ctx (Context.verification_code ^= verification_code);
          S.Response.make_string
            (Ok
               (build_page "gdfuse oauth2 flow" "Success!"
                  (Printf.sprintf
                     "<p>Verification code: %s</p><p>You may close the \
                      browser. Please, check the console output of the \
                      application.</p>"
                     verification_code)))
        with Not_found ->
          S.Response.make_string
            (Ok
               (build_page "gdfuse oauth2 flow" "Error!"
                  (Printf.sprintf "<p>Cannot get the verification code</p>")))
      in
      S.stop server;
      response);
  Utils.log_with_header
    "Starting OAuth2 local web server. Listening on http://%s:%d\n%!"
    (S.addr server) (S.port server);
  Thread.create
    (fun () -> match S.run server with Ok () -> () | Error e -> raise e)
    ()
  |> ignore
