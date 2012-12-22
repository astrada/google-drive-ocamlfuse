This application is still to be tested thoroughly, so there are still probably bugs to discover and fix. To be extra sure, if you want, you can mount the filesystem in read-only mode, modifying the configuration (see the [[Configuration]] wiki page), to avoid any write attempt to the server. Anyway, the `rm` command will simply trash your file, so you should always be able to rollback any changes. If you have problems, you can turn on debug logging:

        $ google-drive-ocamlfuse -debug mountpoint

In `~/.gdfuse/default` you can find `curl.log` that will track every request to the Google Drive API, and `gdfuse.log` that will log FUSE operations and cache management. If something goes wrong, you can try cleaning the cache, removing all the files in `~/.gdfuse/default/cache`, to start from scratch (or
you may remove everything in `~/.gdfuse/default` to restart with default configuration and reauthorize the application).

Note that in order to reduce latency, the application will query the server and check for changes only every 60 seconds (configurable). So, if you make a change to your documents (server side), you won't see it immediately in the mounted filesystem.

Note also that, at least for now, Google Documents will be exported read-only.