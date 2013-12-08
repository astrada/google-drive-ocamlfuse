Here you can find instructions on how to install `google-drive-ocamlfuse`. You can choose one of the following methods.

## PPA repository

I've set up a [PPA repository](https://launchpad.net/~alessandro-strada/+archive/ppa) where I've uploaded the .deb packages for Ubuntu 12.04, 12.10, 13.04, and 13.10 (i386 and amd64). To install the software using this method, run the following commands:

        $ sudo add-apt-repository ppa:alessandro-strada/ppa
        $ sudo apt-get update
        $ sudo apt-get install google-drive-ocamlfuse

## Archlinux

`google-drive-ocamlfuse` is available in the [AUR](https://aur.archlinux.org/packages/google-drive-ocamlfuse/) (thanks to [mlq](http://pwmt.org/) for uploading the package). To install it, run:

        $ yaourt -S google-drive-ocamlfuse

## Installing from source

If you are using a different distribution or you want to build the package from source, you may want to use OPAM (an OCaml package manager). If you are on a Debian Jessie, check out these instructions (contributed by Martin Gallant): [[How to install from source on Debian Jessie]].

### Installing with OPAM

1. Install `OPAM` (http://opam.ocaml.org/doc/Quick_Install.html)
2. Install the C tools and libraries: `m4`, `curl`, `fuse`, and `sqlite3`. These dependencies are not managed by OPAM, so you have to install them in order to compile the executable. If you are using a .deb based distribution, you can install the libraries with this command:

        $ sudo apt-get install m4 libcurl4-gnutls-dev libfuse-dev libsqlite3-dev

3. Update OPAM

        $ opam update

4. Install `google-drive-ocamlfuse`

        $ opam install google-drive-ocamlfuse