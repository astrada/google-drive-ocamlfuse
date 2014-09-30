The configuration file is saved in `~/.gdfuse/default/config` (or `~/.gdfuse/label/config` if a label was specified on the command line). The parser is very simple and only accepts lines in the form `key=value`, without spaces, comments, or anything else.

### Content

Specifies if debug mode is turned on: if `true`, logs verbose output to `~/.gdfuse/default/gdfuse.log`, and logs every curl request to `~/.gdfuse/default/curl.log`:

    debug=false

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

Text document [export format](https://developers.google.com/google-apps/documents-list/#valid_download_formats_for_text_documents). `desktop` format creates a shortcut to the document that will be opened in the web browser for edit:

    document_format=odt
    
Drawings [export format](https://developers.google.com/google-apps/documents-list/#valid_download_formats_for_drawings). `desktop` format creates a shortcut to the document that will be opened in the web browser for edit:

    drawing_format=png

Forms [export format](https://developers.google.com/google-apps/documents-list/#valid_formats_for_spreadsheets). `desktop` format creates a shortcut to the document that will be opened in the web browser for edit:

    form_format=ods

Presentations [export format](https://developers.google.com/google-apps/documents-list/#valid_formats_for_presentations). `desktop` format creates a shortcut to the document that will be opened in the web browser for edit:

    presentation_format=pdf

Spreadsheets [export format](https://developers.google.com/google-apps/documents-list/#valid_formats_for_spreadsheets). `desktop` format creates a shortcut to the document that will be opened in the web browser for edit:

    spreadsheet_format=ods

OAuth2 client ID (optional):

    client_id=

OAuth2 client secret (optional):

    client_secret=

OAuth2 verification code (optional). Useful when authorizing on a different machine.

    verification_code=

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

Specifies whether to display file extensions for Google Docs:

    docs_file_extension=true

Specifies the maximum cache size in MB:

    max_cache_size_mb=512

Specifies whether overwriting a file generates a new revision:

    new_revision=true

Specifies whether to permanently turn off CURL logging (even in debug mode). Set it to `true` to avoid a segmentation fault on some architectures:

    curl_debug_off=false

Specifies whether files removed from trash folder are permanently deleted (**WARNING**: permanently deleted files *cannot be recovered*, set it to `true` at your own risk):

    delete_forever_in_trash_folder=false

Specifies whether to directly stream large files (so that are no more cached):

    stream_large_files=false

Specifies the threshold (in megabytes) to detect large files:

    large_file_threshold_mb=16
