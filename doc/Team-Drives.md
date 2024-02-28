*Team Drive* top level folders are considered as root folders, so if you have more than one Team Drive, or if you want to access both *My Drive* and one or more Team Drives, you should create separate labels.

Create a new label, for example:

    google-drive-ocamlfuse -label myTeamDrive

Authorize access to Google Drive. Now open a web browser, visit the Team Drive, and copy the Team Drive ID. You can get the ID by clicking on your Team Drive and copying the ID from the URL. It is the string after `https://drive.google.com/drive/folders/`. For example:

    https://drive.google.com/drive/u/1/folders/0XS2-_aJgHL29KfOVHAS22
                                               ^------- copy from here

Note: If you need to change the root folder/team drive without creating a new label, you can always remount with `-cc`.

Open the config file in `~/.gdfuse/<label>/config` and look for the `team_drive_id` setting. Add the ID you just copied from above (e.g., `team_drive_id=0XS2-_aJgHL29KfOVHAS22`). Save the file.

Then, mount the drive with:

    google-drive-ocamlfuse -label myTeamDrive /mountPoint

The team drive will be mounted there (instead of the usual *My Drive*).

If you still get the contents of *My Drive* instead of the team drive, try to clear the cache by adding the option `-cc` to the mount.

    google-drive-ocamlfuse -cc -label myTeamDrive /mountPoint

Note: if you want to access the backups produced by [Backup & sync](https://www.google.com/drive/download/backup-and-sync/) that you can find under `Computers`, you should put the folder id of the computer you want to access in `root_folder` and leave `team_drive_id` blank.