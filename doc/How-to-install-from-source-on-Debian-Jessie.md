[Martin Gallant](https://plus.google.com/u/0/106520267009287985667) contributed this checklist to install `google-drive-ocamlfuse` on a fresh Debian Jessie VM with only the base packages installed. (Updated for Jessie 8.2 installed via netinstall. See [issue #145](https://github.com/astrada/google-drive-ocamlfuse/issues/145).)

These utilities get me started from a base Debian Jessie install:

    sudo apt-get install sudo ssh

Then install project dependencies:

    sudo apt-get install m4 libcurl4-gnutls-dev libfuse-dev libsqlite3-dev opam ocaml make fuse camlp4-extra build-essential pkg-config zlib1g zlib1g-dev

Create `fuse` group (if not already present):

    sudo groupadd fuse

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

*Note*: if you are using `root` to install opam, init script will be located in `/root/.opam/opam-init/init.sh`, and you will need to `chmod +x` it, because otherwise you will get a permission denied error.