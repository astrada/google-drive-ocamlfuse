This application is still under testing, so there are probably bugs to
discover and fix. To be extra sure, if you want, you can mount the filesystem
in read-only mode, modifying the configuration (see the
[documentation](https://github.com/astrada/google-drive-ocamlfuse/wiki/Configuration)),
to avoid any write attempt to the server. Anyway, the `rm` command will simply
trash your file, so you should always be able to rollback any changes.

If you have problems, you can turn on debug logging:

    $ google-drive-ocamlfuse -debug mountpoint

In `~/.gdfuse/default` you can find `curl.log` that will track every request
to the Google Drive API, and `gdfuse.log` that will log FUSE operations and
cache management.

Please note that the `-debug` option changes the process behaviour by keeping it in foreground mode (so it's easier to see if the process is still active or if it stops abruptly).

If something goes wrong, you can try clearing the cache, with this command:

    $ google-drive-ocamlfuse -cc

If something still doesn't work, try starting from scratch removing everything
in `~/.gdfuse/default`. In this case you will need to reauthorize the
application.

Note that in order to reduce latency, the application will query the server
and check for changes only every 60 seconds (configurable). So, if you make a
change to your documents (server side), you won't see it immediately in the
mounted filesystem.