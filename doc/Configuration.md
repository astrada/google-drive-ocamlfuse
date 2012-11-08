The configuration file is saved in `~/.gdfuse/default/config` (or `~/.gdfuse/label/config` if a label was specified on the command line). The parser is very simple and only accepts lines in the form `key=value`, without spaces, comments, or anything else.

### Content

Specifies if debug mode is turned on: if `true`, logs verbose output to `~/.gdfuse/default/gdfuse.log`, and logs every curl request to `~/.gdfuse/default/curl.log`:

    debug=true

Specifies the interval in seconds between queries to detect server-side changes:

    metadata_cache_time=60

Specifies if the filesystem is to be mounted read-only:

    read_only=false

Specifies the umask (i.e. the bitmask of  the  permissions  that  are  not present) mount option:

    umask=0o002

Specifies the Sqlite3 busy handler timeout in milliseconds:

    sqlite3_busy_timeout=500

Specifies whether to download Google Docs (these files are read-only, even if `read_only=false`):

    download_docs=true

Text document [export format](https://developers.google.com/google-apps/documents-list/#valid_download_formats_for_text_documents):

    document_format=odt
    
Drawings [export format](https://developers.google.com/google-apps/documents-list/#valid_download_formats_for_drawings):

    drawing_format=png

Forms [export format](https://developers.google.com/google-apps/documents-list/#valid_formats_for_spreadsheets):

    form_format=ods

Presentations [export format](https://developers.google.com/google-apps/documents-list/#valid_formats_for_presentations):

    presentation_format=pdf

Spreadsheets [export format](https://developers.google.com/google-apps/documents-list/#valid_formats_for_spreadsheets):

    spreadsheet_format=ods

OAuth2 client ID (optional):

    client_id=

OAuth2 client secret (optional):

    client_secret=

Conflict resolution strategy. In case of conflict (update on both sides), if
we set the `client` value, the application will always update the server
resource (client side wins). Otherwise, setting `server`, the application will
always maintain the server version of the resource (server side wins):

    conflict_resolution=server

Google Drive supports multiple files with the same name. This flag specifies
the behavior of `mv`: When set to `false`, `mv` behaves in the standard way,
overwriting (trashing) the target file if it exists. When set to `true`, `mv`
will always keep the target file:

    keep_duplicates=false

