There is a way to automount the filesystem at boot, although it's not very straightforward. After completing the authorization process, you can follow the following steps, to enable mounting your Google Drive using the `mount` command. Be sure to replace `$USERNAME` with your actual username.

1) Create a shell script named `gdfuse` in `/usr/bin` (as root) with this content:

```bash
#!/bin/bash

su $USERNAME -l -c "google-drive-ocamlfuse -label $1 $*"
exit 0
```

2) Give it the exec permission:

    $ sudo chmod +x /usr/bin/gdfuse

3) Create a mountpoint (e.g. `/mnt/gdrive`) and give ownership to `$USERNAME`:

    $ sudo mkdir /mnt/gdrive
    $ sudo chown $USERNAME.$USERNAME /mnt/gdrive

4) Edit `/etc/fstab` adding a line like this:

    gdfuse#default  /mnt/gdrive     fuse    uid=1000,gid=1000     0       0

If `uid` and `gid` of your user are different from 1000, modify the above line accordingly.

And then you can use `mount` to mount your Google Drive:

    $ sudo mount /mnt/gdrive

If you have another account you can mount it specifying the label after the `#` character. E.g.:

    gdfuse#account2  /mnt/gdrive2     fuse    uid=1000,gid=1000     0       0
