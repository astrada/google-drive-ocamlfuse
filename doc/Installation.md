Here you can find instructions on how to install `google-drive-ocamlfuse`. You can choose one of the following methods.

## PPA repository

I've set up a [PPA repository](https://launchpad.net/~alessandro-strada/+archive/ppa) where I've uploaded  .deb packages for Ubuntu 14.04, 16.04, 17.04, and 17.10 (i386, amd64, armhf, and arm64). To install the software using this method, run the following commands:

    sudo add-apt-repository ppa:alessandro-strada/ppa
    sudo apt-get update
    sudo apt-get install google-drive-ocamlfuse

## PPA repository (beta versions)

This [PPA repository](https://launchpad.net/~alessandro-strada/+archive/ubuntu/google-drive-ocamlfuse-beta) hosts versions from [beta branch](https://github.com/astrada/google-drive-ocamlfuse/tree/beta). These are experimental versions, to test new functionalities. If you want to install them, run the following commands:

    sudo add-apt-repository ppa:alessandro-strada/google-drive-ocamlfuse-beta
    sudo apt-get update
    sudo apt-get install google-drive-ocamlfuse

## Docker

There is Docker [image](https://hub.docker.com/r/maltokyo/docker-google-drive-ocamlfuse) maintained by [maltokyo](https://github.com/maltokyo).

## Archlinux

`google-drive-ocamlfuse` is available in the [AUR](https://aur.archlinux.org/packages/google-drive-ocamlfuse/) (thanks to [mlq](http://pwmt.org/) for uploading the package). To install it, run:

    yaourt -S google-drive-ocamlfuse

## Debian Stretch

1. Run `sudo apt install software-properties-common dirmngr`

2. As root, create the file `/etc/apt/sources.list.d/alessandro-strada-ubuntu-ppa-bionic.list` and write the following lines:

    `deb http://ppa.launchpad.net/alessandro-strada/ppa/ubuntu xenial main`  
    `deb-src http://ppa.launchpad.net/alessandro-strada/ppa/ubuntu xenial main`  

3. Then you need to add the key otherwise the command **apt update** will not take the new sourcelist into account

    `sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys AD5F235DF639B041`  

    If you get a message like this: `gpg: keyserver receive failed: Server indicated a failure` Then do:
    `sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys AD5F235DF639B041`

4. Run `sudo apt update`
5. Run `sudo apt install google-drive-ocamlfuse`

## Installing from source

If you are using a different distribution or you want to build the package from source, you may want to use OPAM (an OCaml package manager). If you are on a Debian Jessie, check out these instructions (contributed by Martin Gallant): [[How to install from source on Debian Jessie]].

### Installing with OPAM

1. Install `OPAM` (http://opam.ocaml.org/doc/Install.html)

1. Start OPAM

        opam init

1. Update OPAM

        opam update

1. Install the external dependencies:

        opam install depext
        opam depext google-drive-ocamlfuse

1. Install `google-drive-ocamlfuse`

        opam install google-drive-ocamlfuse

### Installing with OPAM on AWS Linux

Contributed by [ngr](https://github.com/ngr).

    # Connect CentOS repository. It will work for Amazon-Linux.
    sudo wget http://download.opensuse.org/repositories/home:ocaml/CentOS_7/home:ocaml.repo -P /etc/yum.repos.d/

    # Install Ocaml and required dependencies.
    sudo yum install opam ocaml gcc gcc-c++ m4 make ocamldoc ocaml-camlp4-devel ncurses-devel

    opam init
    opam update
    opam install depext
    opam depext google-drive-ocamlfuse
    opam install google-drive-ocamlfuse

### Installing with OPAM on macOs

If you have problem installing on macOS, with an error like this:

    [ERROR] The compilation of conf-gmp-powm-sec failed at "sh -exc cc -c $CFLAGS -I/usr/local/include test.c".

You should install gmp this way (before retrying installing google-drive-ocamlfuse):

    env CFLAGS="-I/opt/local/include/" opam install conf-gmp-powm-sec.1

See issue [#357](https://github.com/astrada/google-drive-ocamlfuse/issues/357).

### Installing with OPAM on Debian/Raspbian

Installing OPAM on Debian/Raspbian can be a little difficult, because the default settings are to use the legacy resolver `aspcud` that doesn't work well with OPAM 2.0. The workaround is to use the `mccs` solver (disinstalling `aspcud`), like this:

    sudo apt install opam mccs
    sudo apt remove aspcud
    opam init --solver=mccs
    opam switch create 4.09.0
    eval $(opam env)
    opam depext google-drive-ocamlfuse
    opam install google-drive-ocamlfuse
