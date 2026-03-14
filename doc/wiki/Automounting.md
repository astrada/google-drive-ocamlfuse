# Mount using fstab

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

# Mount using fstab with no password required

The following is a variation of the above.

As Linux shell scripts can't be set the SUID flag, while normal application can, a small C program can be written to a) determine the current username, b) become root, c) switch to the current user and mount the filesystem.

Attached is the C source code: [google-drive-ocamlfuse-mounter.txt](https://github.com/astrada/google-drive-ocamlfuse/files/8602705/google-drive-ocamlfuse-mounter.txt).
After renaming .txt to .c, compile and install with:
```
$ gcc google-drive-ocamlfuse-mounter.c
$ sudo mv a.out /usr/bin/google-drive-ocamlfuse-mounter
$ sudo chown root:root /usr/bin/google-drive-ocamlfuse-mounter
$ sudo chmod 4755 /usr/bin/google-drive-ocamlfuse-mounter
```

Then create a mount directory with rw permissions for a specific user only:
```
$ sudo mkdir /mnt/google-drive-tony
$ sudo chown tony:tony /mnt/google-drive-tony
$ sudo chmod 0700 /mnt/google-drive-tony
```

Then edit /etc/fstab and add:
```
/usr/bin/google-drive-ocamlfuse-mounter#default  /mnt/google-drive-tony  fuse    uid=1000,gid=1000,user,_netdev,comment=x-gvfs-show  0       0
```
Fix the `uid` and `gid` parameters according to yours.
The `comment=x-gvfs-show` option is there so Thunar (the default file manager in Xfce) can list this fstab entry, and mount / unmount it too.

There shouldn't be any security issue with such a configuration, as users can only mount directories where they have filesystem permissions (so you could as well have another /etc/fstab entry pointing to /mnt/google-drive-john with proper permissions, etc.).

# Mount using systemd

## User Unit

In systemd, a user unit can be created and managed by a non-priviledged user.

1) Create the unit file as `nano ~/.config/systemd/user/google-drive-ocamlfuse.service` (be sure to replace instances of {label} and {mountpoint}):

```bash
[Unit]
Description=FUSE filesystem over Google Drive
After=network.target

[Service]
ExecStart=google-drive-ocamlfuse -label {label} {mountpoint}
ExecStop=fusermount -u {mountpoint}
Restart=always
Type=forking

[Install]
WantedBy=default.target
```


2. To mount drive:
```
systemctl start --user google-drive-ocamlfuse.service
```
3. To unmount drive:
```
systemctl stop --user google-drive-ocamlfuse.service
```
4. To automount on boot:
```
systemctl enable --user google-drive-ocamlfuse.service
```

## System Unit

System units must be managed by a privileged user.

1) Create unit file `sudo nano /etc/systemd/system/google-drive-ocamlfuse.service` content (be sure to replace instances of {username}, {label}, and {mountpoint}):

```bash
[Unit]
Description=FUSE filesystem over Google Drive
After=network.target

[Service]
User={username}
Group={username}
ExecStart=google-drive-ocamlfuse -label {label} {mountpoint}
ExecStop=fusermount -u {mountpoint}
Restart=always
Type=forking

[Install]
WantedBy=multi-user.target
```

2. To mount drive:
```
    sudo systemctl start google-drive-ocamlfuse.service
```
3. To unmount drive:
```
    sudo systemctl stop google-drive-ocamlfuse.service
```
4. To automount on boot:
```
    sudo systemctl enable google-drive-ocamlfuse.service
```
# Mount using pam_mount

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

By inserting the following line into ~/.profile the shell will test whether something has already been mounted on your target mountpoint, and if not, will execute the mount.

    $ mount | grep "${HOME}/GoogleDrive" >/dev/null || /usr/bin/google-drive-ocamlfuse "${HOME}/GoogleDrive"&

# Mount when using WiFi

When attempting automount when using WiFi, the mount will fail if using the procedures above since there is no internet connection.  As such, you must confirm a connection is present before automounting.

1)  Create this script and put it in /home/**_username_**/bin (username changed to your profile name).  You'll also need to change the "/home/username/GoogleDrive" in the script below to whatever your mount point is.

```
#!/bin/bash

while true; do
  # check to see if there is a connection by pinging a Google server
  if ping -q -c 1 -W 1 8.8.8.8 >/dev/null; then
    # if connected, mount the drive and break the loop
    google-drive-ocamlfuse /home/username/GoogleDrive; break;
  else
    # if not connected, wait for one second and then check again
    sleep 1
  fi
done
```

2)  Make the script executable either through the CLI or a file manager which allows you to change permissions.

3)  Use AutoStart/Startup Applications to start the script after login.