FUSE filesystem over Google Drive
=================================

**google-drive-ocamlfuse** is a FUSE filesystem backed by Google Drive,
written in OCaml.

Getting started
---------------

### Requirements

* [OCaml][] >= 3.12.0
* [Findlib][] >= 1.2.7
* [ocamlfuse][] >= 2.7.1
* [gapi-ocaml][] >= 0.1.11
* [sqlite3-ocaml][] >= 1.6.1

[OCaml]: http://caml.inria.fr/ocaml/release.en.html
[Findlib]: http://projects.camlcity.org/projects/findlib.html/
[ocamlfuse]: http://sourceforge.net/projects/ocamlfuse/
[gapi-ocaml]: http://forge.ocamlcore.org/projects/gapi-ocaml/
[sqlite3-ocaml]: https://bitbucket.org/mmottl/sqlite3-ocaml

It's better to use the last [CVS
snapshot](http://sourceforge.net/scm/?type=cvs&group_id=121959) of
`ocamlfuse`, because it contains some bugfixes. I've uploaded the snapshot on
[github](https://github.com/downloads/astrada/ocamlfuse/ocamlfuse-2.7.1-cvs~oasis1.tar.gz)
and added OASIS support, to ease compilation and installation.

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
(`~/.gdfuse/default`), containing the configuration file `config` (See the
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
cache. No file is shared among different accounts, so you can have different
configuration for each one.

### Troubleshooting

This application is still in early stage of development, so there are probably
many bugs to discover and fix. So far, the filesystem is read-only, so it is
safe to use, because it won't try to write to your Google Drive. If you have
problems, you can turn on debug logging:

    $ google-drive-ocamlfuse -debug mountpoint

In `~/.gdfuse/default` you can find `curl.log` that will track every request
to the Google Docs API, and `gdfuse.log` that will log filesystem operations
and cache management. If something goes wrong, you can clean the cache,
removing all the files in `~/.gdfuse/default/cache`, to start from scratch (or
you may remove everything in `~/.gdfuse/default` to restart with default
configuration and reauthorize the application).

Note that in order to reduce latency, the application will query the server
and check for changes only every 60 seconds (configurable). So if you make a
change to your documents, you won't see it immediately in the mounted
filesystem.

