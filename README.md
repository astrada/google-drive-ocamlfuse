FUSE filesystem over Google Drive
=================================

**google-drive-ocamlfuse** is a FUSE filesystem backed by Google Drive,
written in OCaml.

### Features

* Offline Google Documents access (read-only)
* Duplicate file handling

Getting started
---------------

### Requirements

* [OCaml][] >= 3.12.0
* [Findlib][] >= 1.2.7
* [ocamlfuse][] >= 2.7.1
* [gapi-ocaml][] >= 0.2
* [sqlite3-ocaml][] >= 1.6.1

[OCaml]: http://caml.inria.fr/ocaml/release.en.html
[Findlib]: http://projects.camlcity.org/projects/findlib.html/
[ocamlfuse]: http://sourceforge.net/projects/ocamlfuse/
[gapi-ocaml]: http://forge.ocamlcore.org/projects/gapi-ocaml/
[sqlite3-ocaml]: https://bitbucket.org/mmottl/sqlite3-ocaml

It's better to use the last [CVS
snapshot](http://sourceforge.net/scm/?type=cvs&group_id=121959) of
`ocamlfuse`, because it contains some bugfixes. I've uploaded the snapshot on
[OASIS DB](http://oasis.ocamlcore.org/dev/view/ocamlfuse/latest) and added
OASIS support, to ease compilation and installation.

### Configuration and installation

To build the executable, run

    $ ocaml setup.ml -configure
    $ ocaml setup.ml -build

To install it, run (as root, if your user doesn't have enough privileges)

    $ ocaml setup.ml -install

To uninstall anything that was previously installed, execute

    $ ocaml setup.ml -uninstall

### Usage

The first time, you can run `google-drive-ocamlfuse` without parameters:

    $ google-drive-ocamlfuse

This command will create the default application directory
(`~/.gdfuse/default`), containing the configuration file `config` (see the
[wiki
page](https://github.com/astrada/google-drive-ocamlfuse/wiki/Configuration)
for more details about configuration). And it will start a web browser to
obtain authorization to access your Google Drive. This will let you modify
default configuration before mounting the filesystem.

Then you can mount the filesystem:

    $ google-drive-ocamlfuse mountpoint

If you have more than one account, you can run:

    $ google-drive-ocamlfuse -label label [mountpoint]

Using `label` to distinguish different accounts. The program will use the
directory `~/.gdfuse/label` to host configuration, application state, and file
cache. No file is shared among different accounts, so you can have a different
configuration for each one.

### Troubleshooting

This application is still to be tested thoroughly, so there are still probably
bugs to discover and fix. To be extra sure, if you want, you can mount the
filesystem in read-only mode, modifying the configuration (see the
[documentation](https://github.com/astrada/google-drive-ocamlfuse/wiki/Configuration)),
to avoid any write attempt to the server. Anyway, the `rm` command will simply
trash your file, so you should always be able to rollback any changes. If you
have problems, you can turn on debug logging:

    $ google-drive-ocamlfuse -debug mountpoint

In `~/.gdfuse/default` you can find `curl.log` that will track every request
to the Google Drive API, and `gdfuse.log` that will log FUSE operations and
cache management. If something goes wrong, you can try cleaning the cache,
removing all the files in `~/.gdfuse/default/cache`, to start from scratch (or
you may remove everything in `~/.gdfuse/default` to restart with default
configuration and reauthorize the application).

Note that in order to reduce latency, the application will query the server
and check for changes only every 60 seconds (configurable). So, if you make a
change to your documents (server side), you won't see it immediately in the
mounted filesystem.

Note also that, at least for now, Google Documents will be exported read-only.

### Upgrading from previous versions

If you are upgrading from previous versions, you should clean every file in
`~/.gdfuse/default`:

    $ rm -rf ~/.gdfuse/default

because previous versions used a different cache schema and/or a different
Google API (the last version uses the new Drive API v2).

### Support

If you have questions, suggestions or want to report a problem, you can post
to this [mailing
list](https://lists.forge.ocamlcore.org/mailman/listinfo/gdfuse-devel). Or you
may want to open an
[issue](https://github.com/astrada/google-drive-ocamlfuse/issues) on github.

