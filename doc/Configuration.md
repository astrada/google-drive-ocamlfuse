By default the configuration file is first created in `~/.gdfuse/default/config` (or `~/.gdfuse/label/config` if a label was specified on the command line). If the program is launched with the `-xdgbd` flag, however, the file is instead created in `~/.config/gdfuse/default/config` or `~/.config/gdfuse/label/config`; see [Configuration path priority](#configuration-path-priority) for more details.

The parser is very simple and only accepts lines in the form `key=value`, without spaces, comments, or anything else.

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

    document_format=desktop

If `document_format=desktop`, specifies the icon of the desktop link (default is no icon):

    document_icon=

Drawings [[export format|Exportable-formats#valid-download-formats-for-drawings]]:

    drawing_format=desktop

If `drawing_format=desktop`, specifies the icon of the desktop link (default is no icon):

    drawing_icon=

Forms [[export format|Exportable-formats#valid-formats-for-spreadsheets]]:

    form_format=desktop

If `form_format=desktop`, specifies the icon of the desktop link (default is no icon):

    form_icon=

Presentations [[export format|Exportable-formats#valid-formats-for-presentations]]:

    presentation_format=desktop

If `presentation_format=desktop`, specifies the icon of the desktop link (default is no icon):

    presentation_icon=

Spreadsheets [[export format|Exportable-formats#valid-formats-for-spreadsheets]]:

    spreadsheet_format=desktop

If `spreadsheet_format=desktop`, specifies the icon of the desktop link (default is no icon):

    spreadsheet_icon=

Maps export format (the only valid format is `desktop`):

    map_format=desktop

If `map_format=desktop`, specifies the icon of the desktop link (default is no icon):

    map_icon=

Fusion table export format (the only valid format is `desktop`):

    fusion_table_format=desktop

If `fusion_table_format=desktop`, specifies the icon of the desktop link (default is no icon):

    fusion_table_icon=

Google Apps Script [[export format|Exportable-formats#valid-formats-for-google-apps-scripts]]:

    apps_script_format=desktop

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

[Removed in 0.7.0] Specifies whether to upload files in a concurrent thread:

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

Specifies the minimum size of download/write buffers (used only if `stream_large_files=true` or `write_buffers=true`):

    memory_buffer_size=1048576 (* 1MB *)

Specifies the maximum memory occupation of read-ahead/write buffers, before it starts to de-allocate the oldest ones (used only if `stream_large_files=true` or `write_buffers=true`):

    max_memory_cache_size=10485760 (* 10MB *)

Specifies how many blocks (of `memory_buffer_size` bytes) to download in parallel (used only if `stream_large_files=true`):

    read_ahead_buffers=3

When set to `true`, creates a new directory `/lost+found` where you can access unorganized files (those you can get filtering with `is:unorganized owner:me` in the web interface):

    lost_and_found=false

[Removed in version 0.7.20] When set to `true`, creates a new read-only directory `/.shared` where you can access all files that are shared with you (those you can get in the `Shared with me` section of the web interface):

    shared_with_me=false

Enable XDG Base Directory support (to turn it on use `-xdgbd` command line option; see [Configuration path priority](#configuration-path-priority) for more information):

    xdg_base_directory=false

Path of the directory storing application state (if blank uses the default):

    data_directory=

Path of the directory storing application cache (if blank uses the default):

    cache_directory=

Path of the directory containing log files (if blank uses the default):

    log_directory=

[Since 0.6.20] Specifies a Google Drive folder to use as root, if you don't want to expose your entire drive from your mountpoint. You can use a folder id or a full remote path:

    root_folder=

[Since 0.6.23] Specifies the Team Drive id, if you want to mount a [[Team Drive|Team Drives]]:

    team_drive_id=

[Since 0.7.0] Enables in-memory metadata cache:

    metadata_memory_cache=true

[Since 0.7.0] Specifies every how many seconds the metadata cache is saved to sqlite3 db:

    metadata_memory_cache_saving_interval=30

[Since 0.7.0] Permits the download of files marked by Google Drive as abusive:

    acknowledge_abuse=false

[Since 0.7.0] Customize the executable to run when you specify a `desktop` export format:

    desktop_entry_exec=

[Since 0.7.1] Enable caching file writes in memory:

    write_buffers=false

[Since 0.7.4] Disable trash bin access:

    disable_trash=false

[Since 0.7.8] Let Google Drive autodetect MIME types:

    autodetect_mime=true

[Since 0.7.9] Keep target history/metadata when moving/renaming files:

    mv_keep_target=false

[Since 0.7.10] Enable async upload queue:

    async_upload_queue=false

[Since 0.7.10] Size of the async upload thread pool (how many concurrent uploads can start):

    async_upload_threads=10

[Since 0.7.10] Logs buffer contents for reads/writes (for debug purposes):

    debug_buffers=false

[Since 0.7.11] Path of the JSON file that contains service account credentials (downloaded during account [creation](https://developers.google.com/identity/protocols/OAuth2ServiceAccount#creatinganaccount)):

    service_account_credentials_path=

[Since 0.7.11] Email of the user for which the application is requesting delegated access. Works only for G Suite domains. It is ignored if service_account_credentials_path is not specified:

    service_account_user_to_impersonate

[Since 0.7.12] Specifies where the logs are going to be written. It can be `stdout`, `stderr`, an absolute path, or blank to keep the standard logging behavior:

    log_to=

[Since 0.7.12] Specifies a custom Drive API [scope](https://developers.google.com/drive/api/v3/about-auth#OAuth2Authorizing):

    scope=

[Since 0.7.12] Specifies a custom `redirect_uri` for the OAuth2 [flow](https://developers.google.com/identity/protocols/OAuth2InstalledApp#step-2-send-a-request-to-googles-oauth-20-server):

    redirect_uri=

[Since 0.7.14] If set to `true`, instead of creating `.desktop` link files, it produces `.html` files with a redirect to the Google Document edit page:

    desktop_entry_as_html=false

[Since 0.7.18] Specifies the maximum number of entries (files) of the async upload queue. `0` (default) means unlimited:

    async_upload_queue_max_length=0

[Since 0.7.21] Specifies whether to fetch folder data in a background thread:

    background_folder_fetching=false

### Document export formats

`desktop` format creates a shortcut to the document that will be opened in the web browser for edit. If `desktop_entry_as_html=true` instead of `.desktop` files you will get `.html` files.

If you don't want to export a specific kind of docs, just remove the format portion. For example, if you have this line in your `config` file:

    drawing_format=

no drawings will be exported.

Note that if you change any export format, you will have to clear the cache (using `-cc` command line option). Note also that this feature works only if you set `docs_file_extension=true`.

### Configuration path priority

The application reads or initializes its configuration according to the following logic:

1. `$XDG_CONFIG_HOME/gdfuse/` is read if already existing;
2. Otherwise `$HOME/.gdfuse/` is read if already existing;
3. Otherwise `$XDG_CONFIG_HOME/gdfuse/` is created, but only if the `-xdgbd` flag has been passed to the command;
4. Otherwise `$HOME/.gdfuse/` is created.

This is done in compliance with the [XDG Base Directory Specification](http://standards.freedesktop.org/basedir-spec/latest/).

The `xdg_base_directory` option in the `default/config` (or `label/config`) file acts independently from the mechanism above: if set to `true` (default with the `-xdgbd` flag), it tells the program to also use the `$XDG_DATA_HOME` and `$XDG_CACHE_HOME` paths; if set to `false`, all the user's data will stay under the `gdfuse` directory, wherever it is located.