Here you can find instructions on how to install `google-drive-ocamlfuse`. You can choose one of the following methods.

## PPA repository

I've set up a [PPA repository](https://launchpad.net/~alessandro-strada/+archive/ppa) where I've uploaded the .deb packages for Ubuntu 12.04, 12.10, and 13.04 (i386 and amd64). To install the software using this method, run the following commands:

        $ sudo add-apt-repository ppa:alessandro-strada/ppa
        $ sudo apt-get update
        $ sudo apt-get install google-drive-ocamlfuse

## Installing the binaries

I've uploaded some binary packages that target the distributions I've installed on my machine.

### Prerequisites

This project uses these C libraries: `curl`, `fuse`, and `sqlite3`. If you are using a .deb based distribution, you can install the libraries with this command:

        $ sudo apt-get install libcurl3-gnutls libfuse2 libsqlite3-0

### Ubuntu 12.04 32-bit

Version 0.4: https://forge.ocamlcore.org/frs/download.php/1244/google-drive-ocamlfuse-0.4-bin-ubuntu12.04-32bit.tar.gz

### Ubuntu 12.04 64-bit

Version 0.4: https://forge.ocamlcore.org/frs/download.php/1240/google-drive-ocamlfuse-0.4-bin-ubuntu12.04-64bit.tar.gz  

Version 0.3.1: https://forge.ocamlcore.org/frs/download.php/1087/google-drive-ocamlfuse-0.3.1-bin-ubuntu12.04-64bit.tar.gz

### Ubuntu 12.10 32-bit

Version 0.4: https://forge.ocamlcore.org/frs/download.php/1241/google-drive-ocamlfuse-0.4-bin-ubuntu12.10-32bit.tar.gz

Version 0.3.2: https://forge.ocamlcore.org/frs/download.php/1164/google-drive-ocamlfuse-0.3.2-bin-ubuntu12.10-32bit.tar.gz

### Ubuntu 12.10 64-bit

Version 0.3.2: https://forge.ocamlcore.org/frs/download.php/1163/google-drive-ocamlfuse-0.3.2-bin-ubuntu12.10-64bit.tar.gz

### Ubuntu 13.04 32-bit

Version 0.4: https://forge.ocamlcore.org/frs/download.php/1245/google-drive-ocamlfuse-0.4-bin-ubuntu13.04-32bit.tar.gz

### Ubuntu 13.04 64-bit

Version 0.4: https://forge.ocamlcore.org/frs/download.php/1217/google-drive-ocamlfuse-0.4-bin-ubuntu13.04-64bit.tar.gz

Version 0.3.4: https://forge.ocamlcore.org/frs/download.php/1213/google-drive-ocamlfuse-0.3.4-bin-ubuntu13.04-64bit.tar.gz

Version 0.3.3: https://forge.ocamlcore.org/frs/download.php/1193/google-drive-ocamlfuse-0.3.3-bin-ubuntu13.04-64bit.tar.gz

Version 0.3.2: https://forge.ocamlcore.org/frs/download.php/1177/google-drive-ocamlfuse-0.3.2-bin-ubuntu13.04-64bit.tar.gz

## Installing from source

If you are using a different distribution or you want to build the package from source, you may want to use OPAM (an OCaml package manager).

### Installing with OPAM

1. Install `OPAM` (http://opam.ocamlpro.com/doc/Quick_Install.html)
2. Install the C tools and libraries: `m4`, `curl`, `fuse`, and `sqlite3`. These dependencies are not managed by OPAM, so you have to install them in order to compile the executable. If you are using a .deb based distribution, you can install the libraries with this command:

        $ sudo apt-get install m4 libcurl4-gnutls-dev libfuse-dev libsqlite3-dev

3. Update OPAM

        $ opam update

4. Install `google-drive-ocamlfuse`

        $ opam install google-drive-ocamlfuse