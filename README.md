FUSE filesystem over Google Drive
=================================

**google-drive-ocamlfuse** is a FUSE filesystem backed by Google Drive,
written in OCaml. It lets you mount your Google Drive on Linux.

### Features

* Full read/write access to ordinary files and folders
* Read-only access to Google Docs, Sheets, and Slides (exported to
  configurable formats)
* Multiple account support
* Duplicate file handling
* Access to trash (`.Trash` directory)

### Resources

* [Homepage](http://gdfuse.forge.ocamlcore.org/)
* [Wiki](https://github.com/astrada/google-drive-ocamlfuse/wiki): includes
  installation instructions, and more details about configuration, and
  authorization

### Authorization

Please be sure to have a look at the
[authorization](https://github.com/astrada/google-drive-ocamlfuse/wiki/Authorization)
page, to understand how the authorization process works, and to discover all
the available options.

Getting started
---------------

### Installation

I've uploaded .deb packages to my [PPA](https://launchpad.net/~alessandro-strada/+archive/ppa),
for Ubuntu 13.10, 13.04, 12.10, 12.04. In order to to install it, use the commands below:

    $ sudo add-apt-repository ppa:alessandro-strada/ppa
    $ sudo apt-get update
    $ sudo apt-get install google-drive-ocamlfuse

For other installation options, please refer to the [wiki](https://github.com/astrada/google-drive-ocamlfuse/wiki/Installation).

How to build
------------

### Requirements

* [OCaml][] >= 3.12.0
* [Findlib][] >= 1.2.7
* [ocamlfuse][] >= 2.7.1
* [gapi-ocaml][] >= 0.2.1
* [sqlite3-ocaml][] >= 1.6.1

[OCaml]: http://caml.inria.fr/ocaml/release.en.html
[Findlib]: http://projects.camlcity.org/projects/findlib.html/
[ocamlfuse]: http://sourceforge.net/projects/ocamlfuse/
[gapi-ocaml]: http://forge.ocamlcore.org/projects/gapi-ocaml/
[sqlite3-ocaml]: https://bitbucket.org/mmottl/sqlite3-ocaml

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

To unmount the filesystem, issue this command:

    $ fusermount -u mountpoint

### Troubleshooting

This application is still under testing, so there are probably bugs to
discover and fix. To be extra sure, if you want, you can mount the filesystem
in read-only mode, modifying the configuration (see the
[documentation](https://github.com/astrada/google-drive-ocamlfuse/wiki/Configuration)),
to avoid any write attempt to the server. Anyway, the `rm` command will simply
trash your file, so you should always be able to rollback any changes. If you
have problems, you can turn on debug logging:

    $ google-drive-ocamlfuse -debug mountpoint

In `~/.gdfuse/default` you can find `curl.log` that will track every request
to the Google Drive API, and `gdfuse.log` that will log FUSE operations and
cache management. If something goes wrong, you can try clearing the cache,
with this command:

    $ google-drive-ocamlfuse -cc

If something still doesn't work, try starting from scratch removing everything
in `~/.gdfuse/default`. In this case you will need to reauthorize the
application.

Note that in order to reduce latency, the application will query the server
and check for changes only every 60 seconds (configurable). So, if you make a
change to your documents (server side), you won't see it immediately in the
mounted filesystem.

Note also that Google Documents will be exported read-only.

### Support

If you have questions, suggestions or want to report a problem, you can post
to this [mailing
list](https://lists.forge.ocamlcore.org/mailman/listinfo/gdfuse-devel). Or you
may want to open an
[issue](https://github.com/astrada/google-drive-ocamlfuse/issues) on github.
