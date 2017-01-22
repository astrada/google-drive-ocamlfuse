0.6.12
======

This version includes XDG Base Directory support. To turn it on, use `-xdgbd` command line option. It is needed only once, then it will be stored in configuration file (`xdg_base_directory` option). While enabled, the application stores config in `$XDG_CONFIG_HOME/gdfuse/[label]` (or `.config/gdfuse/[label]` if `$XDG_CONFIG_HOME` is not set), state in `$XDG_DATA_HOME/gdfuse/[label]` (or `.local/share/gdfuse/[label]` if `$XDG_DATA_HOME` is not set), and cache in `$XDG_CACHE_HOME/gdfuse/[label]` (or `.cache/gdfuse/[label]` if `$XDG_CACHE_HOME` is not set). You can also set custom paths (`data_directory`, `cache_directory`, `log_directory`) in the configuration file, and a custom configuration file path with `-config` command line option.

0.6.11
======

* Access to unorganized files (those you can find searching for `is:unorganized owner:me` from the web interface): Activating `lost_and_found` config option, you will get a `/lost+found` directory where you will see all your unorganized files.
* Access to "Shared with me" files. Activating `shared_with_me` config option, you will get a `/.shared` directory where you will see all files shared with you, even if you didn't add them to your Drive.

0.6.x
=====

* Multi-threading is now enabled by default.
* Permissions and ownership are persisted: Unix file mode, uid&gid are saved to Drive's appProperties.
* Soft links (but not hard links) are supported: Unix file type is saved to Drive's appProperties too, so you can keep soft links, sockets, FIFOs, devices, etc. Note that if the linked file is outside of Google Drive it will not be uploaded, because a symbolic link is just a placeholder that stores the path to the linked file. (Note also that hard links are still not supported.)
* xattrs are supported: xattrs are saved to appProperties too, but due to some limitations of Drive's appProperties, you cannot have more than about 25 extended attributes per file and the concatenation of attribute's name and value cannot exceed about 125 bytes.
* Streaming is optimized using large blocks (to get around the 128KB FUSE limit) and read-ahead buffers
* Conflicting name handling changed: duplicates name get a suffix based on Drive API file ID (so they are more reproducible than a sequential number)
* Switch to Drive API v3

### New streaming options

These new options are available if `stream_large_files=true`. Basically, files to stream are split in blocks of `memory_buffer_size` bytes (if it's above 0), and `read_ahead_buffers` specifies how many parallel threads are spun to pre-fetch next blocks. For example, using the default values (where `memory_buffer_size` is 1MB and `read_ahead_buffers` is 3), when you access the first bytes of a file, google-drive-ocamlfuse starts downloading the first block (of 1MB) and spins 3 new parallel threads to fetch blocks 2, 3, and 4. Then, when you access the second block, google-drive-ocamlfuse starts downloading block 5 (blocks 3, and 4 are already downloaded/downloading), and so on. Finally `max_memory_cache_size` specifies how many blocks can be kept in memory at the same time. So (using the default value of 10MB), in the example, when you access the 8th block, my app starts downloading block 11, and deallocates block 1. Note that `max_memory_cache_size` is global and not per file, to keep memory usage under control.
