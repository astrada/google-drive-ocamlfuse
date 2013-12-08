[Martin Gallant](https://plus.google.com/u/0/106520267009287985667) contributed this checklist to install `google-drive-ocamlfuse` on a fresh Debian Jessie VM with only the base packages installed.

These utilities get me started from a base Debian Jessie install:

    sudo apt-get install sudo ssh

The wiki says I need to do this, I did not check if this was all necessary:

    sudo apt-get install m4 libcurl4-gnutls-dev libfuse-dev libsqlite3-dev

I needed the following as well:

    sudo apt-get install opam ocaml
    sudo apt-get install make
    sudo apt-get install fuse

Debian has a special user group to allow fuse access. Log out, and log back in after this to make change effective:

    sudo adduser *user* fuse

I think this is a bug in Jessie, as this was already done in all my other Debian boxes:

    sudo chown root.fuser /dev/fuse
    sudo chmod 660 /dev/fuse

Get the packages installed. I used the default on all installer prompts:

    opam init
    opam update
    opam install google-drive-ocamlfuse
    . /home/*user*/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
