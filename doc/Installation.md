Here you can find instructions on how to install `google-drive-ocamlfuse`. You can choose one of the following methods.

## PPA repository

I've set up a [PPA repository](https://launchpad.net/~alessandro-strada/+archive/ppa) where I've uploaded  .deb packages for Ubuntu 14.04, 16.04, and 16.10 (i386, amd64, and armhf). To install the software using this method, run the following commands:

        $ sudo add-apt-repository ppa:alessandro-strada/ppa
        $ sudo apt-get update
        $ sudo apt-get install google-drive-ocamlfuse

## PPA repository (beta versions)

This [PPA repository](https://launchpad.net/~alessandro-strada/+archive/ubuntu/google-drive-ocamlfuse-beta) hosts versions from [beta branch](https://github.com/astrada/google-drive-ocamlfuse/tree/beta). These are experimental versions, to test new functionalities. If you want to install them, run the following commands:

        $ sudo add-apt-repository ppa:alessandro-strada/google-drive-ocamlfuse-beta
        $ sudo apt-get update
        $ sudo apt-get install google-drive-ocamlfuse

## Archlinux

`google-drive-ocamlfuse` is available in the [AUR](https://aur.archlinux.org/packages/google-drive-ocamlfuse/) (thanks to [mlq](http://pwmt.org/) for uploading the package). To install it, run:

        $ yaourt -S google-drive-ocamlfuse

## Installing from source

If you are using a different distribution or you want to build the package from source, you may want to use OPAM (an OCaml package manager). If you are on a Debian Jessie, check out these instructions (contributed by Martin Gallant): [[How to install from source on Debian Jessie]].

### Installing with OPAM

1. Install `OPAM` (http://opam.ocaml.org/doc/Install.html)
2. Install the C tools and libraries: `m4`, `curl`, `fuse`, and `sqlite3`. These dependencies are not managed by OPAM, so you have to install them in order to compile the executable. If you are using a .deb based distribution, you can install the libraries with this command:

        $ sudo apt-get install m4 libcurl4-gnutls-dev libfuse-dev libsqlite3-dev zlib1g-dev libncurses5-dev

3. Starting OPAM

        $ opam init

4. Update OPAM

        $ opam update

5. Install `google-drive-ocamlfuse`

        $ opam install google-drive-ocamlfuse

### Installing with OPAM on AWS Linux

Contributed by [ngr](https://github.com/ngr).

    # Connect CentOS repository. It will work for Amazon-Linux.
    sudo wget http://download.opensuse.org/repositories/home:ocaml/CentOS_7/home:ocaml.repo -P /etc/yum.repos.d/

    # Install Ocaml and required dependencies.
    sudo yum install opam ocaml gcc gcc-c++ m4 make ocamldoc sqlite-devel libcurl-devel fuse-devel zlib-devel ocaml-camlp4-devel ncurses-devel

    opam init
    opam install google-drive-ocamlfuse
