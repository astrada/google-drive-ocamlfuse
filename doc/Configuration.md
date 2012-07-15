The configuration file is saved in `~/.gdfuse/default/config` (or `~/.gdfuse/label/config` if a label was specified on the command line). The parser is very simple and only accepts lines in the form `key=value`, without spaces, comments, or anything else.

### Content

Specifies if debug mode is turned on: if `true`, logs verbose output to `~/.gdfuse/default/gdfuse.log`, and logs every curl request to `~/.gdfuse/default/curl.log`:

    debug=true

Specifies the interval in seconds between queries to detect server-side changes:

    metadata_cache_time=60

Specifies if the filesystem is to be mounted read-only (at this moment, write support is not yet implemented):

    read_only=true

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

OAuth2 client ID (optional)

    client_id=

OAuth2 client secret (optional)

    client_secret=

