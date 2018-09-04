There is a way to automount the filesystem at boot, although it's not very straightforward. After completing the authorization process, you can follow the following steps, to enable mounting your Google Drive using the `mount` command. Be sure to replace `$USERNAME` with your actual username.

1) Create a shell script named `gdfuse` in `/usr/bin` (as root) with this content:

```bash
#!/bin/bash

su $USERNAME -l -c "google-drive-ocamlfuse -label $1 $*"
exit 0
```

2) Give it the exec permission:
```
$ sudo chmod +x /usr/bin/gdfuse
```
3) Create a mountpoint (e.g. `~/gdrive`):
```
$ mkdir ~/gdrive
```
4) Edit `/etc/fstab` adding a line like this. Be sure to replace `$USERNAME` with your actual username.
```
gdfuse#default  /home/$USERNAME/gdrive     fuse    uid=1000,gid=1000,allow_other,user,_netdev     0       0
```

If `uid` and `gid` of your user are different from 1000, modify the above line accordingly. Note that the "user" option implies noexec, nosuid and nodev. To be able to execute scripts etc. in gdrive, append ",exec" to "user".

The `_netdev` option is to make sure the filesystem is mounted after the network becomes available (see `man 8 mount`)

And then you can use `mount` to mount your Google Drive. Maybe you will be asked for your password.

    $ mount ~/gdrive

If you have another account you can mount it specifying the label after the `#` character. E.g.:

    gdfuse#account2  /home/$USERNAME/gdrive2     fuse    uid=1000,gid=1000,_netdev     0       0

## Mount using pam_mount

First go through the authorization process. Install the package `libpam-mount` or its equivalent. Edit the file `/etc/security/pam_mount.conf.xml` and uncomment the following line (as root):

    <luserconf name=".pam_mount.conf.xml" />

Create a file `.pam_mount.conf.xml` in your home directory with the following line in the `Volume definitions` stanza:

    <volume fstype="fuse" path="gdfuse#default" mountpoint="~/GoogleDrive" options="nosuid,nodev" />

Create a shell script `gdfuse` in `/usr/local/bin/` (as root) with the following content:

    #!/bin/bash
    
    google-drive-ocamlfuse -label $1 $*

And make it executable:

    $ sudo chmod +x /usr/local/bin/gdfuse

Create a directory `GoogleDrive` in the root of your home directory:

    $ mkdir ~/GoogleDrive

Now log out and back in again and your Google Drive should be mounted on `~/GoogleDrive`.

## Mount from login scripts

By inserting the following line into ~/.profile the shell will test wether something has already been mounted on your target mountpoint, and if not, will execute the mount.

    $ mount | grep "${HOME}/GoogleDrive" >/dev/null || /usr/bin/google-drive-ocamlfuse "${HOME}/GoogleDrive"
