The configuration file is saved in `~/.gdfuse/default/config` (or `~/.gdfuse/label/config` if a label was specified on the command line). The parser is very simple and only accepts lines in the form `key=value`, without spaces, comments, or anything else.

### Content

Specifies the interval in seconds between queries to detect server-side changes:

    metadata_cache_time=60

Specifies if the filesystem is to be mounted read-only:

    read_only=false

Specifies the umask (i.e. the bitmask of  the  permissions  that  are  not present) mount option:

    umask=0o002

Specifies the Sqlite3 busy handler timeout in milliseconds:

    sqlite3_busy_timeout=5000

Specifies whether to download Google Docs (these files are read-only, even if `read_only=false`):

    download_docs=true

Text document [[export format|Exportable-formats#valid-download-formats-for-text-documents]]:

    document_format=odt

If `document_format=desktop`, specifies the icon of the desktop link (default is no icon):

    document_icon=

Drawings [[export format|Exportable-formats#valid-download-formats-for-drawings]]:

    drawing_format=png

If `drawing_format=desktop`, specifies the icon of the desktop link (default is no icon):

    drawing_icon=

Forms [[export format|Exportable-formats#valid-formats-for-spreadsheets]]:

    form_format=ods

If `form_format=desktop`, specifies the icon of the desktop link (default is no icon):

    form_icon=

Presentations [[export format|Exportable-formats#valid-formats-for-presentations]]:

    presentation_format=pdf

If `presentation_format=desktop`, specifies the icon of the desktop link (default is no icon):

    presentation_icon=

Spreadsheets [[export format|Exportable-formats#valid-formats-for-spreadsheets]]:

    spreadsheet_format=ods

If `spreadsheet_format=desktop`, specifies the icon of the desktop link (default is no icon):

    spreadsheet_icon=

Maps export format (the only valid format is `desktop`):

    map_format=desktop

If `map_format=desktop`, specifies the icon of the desktop link (default is no icon):

    map_icon=

Fusion table export format (the only valid format is `desktop`):

    fusion_table_format=desktop

If `fusion_table_format=desktop`, specifies the icon of the desktop link (default is no icon):

    fusion_table_format=

Google Apps Script [[export format|Exportable-formats#valid-formats-for-google-apps-scripts]]:

    apps_script_format=json

If `apps_script_format=desktop`, specifies the icon of the desktop link (default is no icon):

    apps_script_icon=

OAuth2 client ID (optional):

    client_id=

OAuth2 client secret (optional):

    client_secret=

OAuth2 verification code (optional). Useful when authorizing on a different machine.

    verification_code=

Google Drive supports multiple files with the same name. This flag specifies
the behavior of `mv`: When set to `false`, `mv` behaves in the standard way,
overwriting (trashing) the target file if it exists. When set to `true`, `mv`
will always keep the target file:

    keep_duplicates=false

Specifies whether to display file extensions for Google Docs:

    docs_file_extension=true

Specifies the maximum cache size in MB:

    max_cache_size_mb=512

Specifies whether to permanently turn off CURL logging (even in debug mode). Set it to `true` to avoid a segmentation fault on some architectures:

    curl_debug_off=false

Specifies whether files removed from trash folder are permanently deleted (**WARNING**: permanently deleted files *cannot be recovered*, set it to `true` at your own risk):

    delete_forever_in_trash_folder=false

Specifies whether to directly stream large files, so that are no more cached (please note that this does not affect file uploading):

    stream_large_files=false

Specifies the threshold (in megabytes) to detect large files:

    large_file_threshold_mb=16

Specifies if large files should be read-only, to avoid accidental uploads of very big files (checked only if `stream_large_files=true`):

    large_file_read_only=false

Specifies whether to upload files in a concurrent thread:

    async_upload=true

Specifies connection timeout in milliseconds:

    connect_timeout_ms=5000

Specifies maximum download speed per connection in bytes/second (`0` = unlimited):

    max_download_speed=0

Specifies maximum upload speed per connection in bytes/second (`0` = unlimited):

    max_upload_speed=0

Specifies when a transfer is too slow. If speed (in bytes/second) is under `low_speed_limit` for `low_speed_time` (in seconds), the file transfer will be terminated (`0` = don't check):

    low_speed_limit=0
    low_speed_time=0

Specifies maximum number of retries after an error occurred (during a Drive API request):

    max_retries=8

Specifies the maximum size in bytes of an upload chunk:

    max_upload_chunk_size=1099511627776 (* 1TB for 64-bit machines *)
    max_upload_chunk_size=805306368 (* 768MB for 32-bit machines *)

Specifies the minimum size of download buffers (used only if `stream_large_files=true`):

    memory_buffer_size=1048576 (* 1MB *)

Specifies the maximum memory occupation of read-ahead buffers, before it starts to de-allocate the oldest ones (used only if `stream_large_files=true`):

    max_memory_cache_size=10485760 (* 10MB *)

Specifies how many blocks (of `memory_buffer_size` bytes) to download in parallel (used only if `stream_large_files=true`):

    read_ahead_buffers=3

When set to `true`, creates a new directory `/lost+found` where you can access unorganized files (those you can get filtering with `is:unorganized owner:me` in the web interface):

    lost_and_found=false

When set to `true`, creates a new directory `/.shared` where you can access all files that are shared with you (those you can get in the `Shared with me` section of the web interface):

    shared_with_me=false

### Document export formats

`desktop` format creates a shortcut to the document that will be opened in the web browser for edit.

If you don't want to export a specific kind of docs, just remove the format portion. For example, if you have this line in your `config` file:

    drawing_format=

no drawings will be exported.

Note that if you change any export format, you will have to clear the cache (using `-cc` command line option). Note also that this feature works only if you set `docs_file_extension=true`.
