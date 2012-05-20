FUSE filesystem over Google Drive
=================================

**google-drive-ocamlfuse** is a FUSE filesystem backed by Google Drive,
written in OCaml.

Getting started
---------------

### Requirements

This library was developed with the following dependencies ([Unofficial OCaml
packages for Debian](http://ocaml.debian.net/debian/ocaml-3.12.1/)):

* [OCaml][] = 3.12.1
* [Findlib][] = 1.2.7
* [gapi-ocaml][] = 0.1.8

[OCaml]: http://caml.inria.fr/ocaml/release.en.html
[Findlib]: http://projects.camlcity.org/projects/findlib.html/
[gapi-ocaml]: http://forge.ocamlcore.org/projects/gapi-ocaml

### Configuration and installation

To build the executable, run

    $ ocaml setup.ml -configure
    $ ocaml setup.ml -build

To install it, run (as root, if your user doesn't have enough privileges)

    $ ocaml setup.ml -install

To uninstall anything that was previously installed, execute

    $ ocaml setup.ml -uninstall

### Usage

The first time, you should launch this command:

    $ gdfuse -setup

to build the configuration directory and to authorize access to your Google
Drive.

Then you can mount the filesystem:

    $ gdfuse mountpoint

