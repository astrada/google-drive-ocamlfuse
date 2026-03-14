[Martin Gallant](https://plus.google.com/u/0/106520267009287985667) contributed this checklist to install `google-drive-ocamlfuse` on a fresh Debian Jessie VM with only the base packages installed. (Updated for Jessie 8.2 installed via netinstall. See [issue #145](https://github.com/astrada/google-drive-ocamlfuse/issues/145).)

These utilities get me started from a base Debian Jessie install:

    sudo apt-get install sudo ssh

Then install project dependencies:

    sudo apt-get install opam ocaml make fuse-dev camlp4-extra build-essential pkg-config

(Starting with debian 8, the /dev/fuse device is [world-writable by default](https://wiki.debian.org/SystemGroups#FUSE) so these next few steps are only necessary on Debian 7.)

Create `fuse` group (if not already present):

    sudo groupadd fuse

Debian has a special user group to allow fuse access. Log out, and log back in after this to make change effective:

    sudo usermod -a -G fuse *user*

I think this is a bug in Jessie, as this was already done in all my other Debian boxes:

    sudo chown root:fuse /dev/fuse
    sudo chmod 660 /dev/fuse

Get the packages installed. I used the default on all installer prompts:

    opam init
    opam update
    opam install depext
    eval `opam config env`
    opam depext google-drive-ocamlfuse
    opam install google-drive-ocamlfuse
    . /home/*user*/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

Have fun!

    google-drive-ocamlfuse ~/your/mount/point/here