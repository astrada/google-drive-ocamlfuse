### Authorization

You have to create your own OAuth2 client ID and client secret, so that you
may pass them on the command line (the first time you run the application),
e.g.:

    $ google-drive-ocamlfuse -id 12345678.apps.googleusercontent.com -secret abcde12345

A browser will be started pointing to an authorization page, and when you
allow access to Google Drive, a verification code will be generated. An HTTP
server is embedded in the application to listen on port 8080 by default (but
you can change the port with the `-port` command line option). The HTTP server
will receive the verification code, after you have authorized the access to
your Google Drive. If everything goes fine, the application will notify you
that it has retrieved the tokens from Google.

    Access token retrieved correctly.

You can create OAuth2 credentials from the Google Cloud Console.
[Here](https://support.google.com/cloud/answer/6158849?hl=en) you can find how
to set up OAuth2. When creating a new client ID, you must select `Desktop` as
`Application type`. You must also [activate the Google Drive
API](https://cloud.google.com/service-usage/docs/enable-disable).

### Revoking access

You can revoke access to Google Drive from
[here](https://myaccount.google.com/connections?filters=3,4&hl=en).

### Service accounts & device mode

If you want you can use a [service account](https://github.com/astrada/google-drive-ocamlfuse/wiki/Service-Accounts) that is useful to access the users' Drive of a G Suite domain.

You can also use a [simpler authorization flow](https://github.com/astrada/google-drive-ocamlfuse/wiki/OAuth2-for-Devices) specific for TVs and limited-input devices, but this way you won't have access to all the files in your Drive.
