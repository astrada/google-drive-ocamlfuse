Team Drives top level folders are considered as root folders, so if you have more than one Team Drive, or if you want to access also "My Drive", you should create separate labels.

* Create a new label, i.e. `google-drive-ocamlfuse -label myTeamDrive ...` and authorize.
* Add the `team_drive_id` config before the first time you mount. You can get the id from the web interface, clicking on your team drive and copying the id you can find in the url (after `https://drive.google.com/drive/folders/`). If you need to change the root folder/team drive without creating a new label, you can always remount with `-cc`.
* Once mounted with `google-drive-ocamlfuse -label myTeamDrive /mountPoint`, the team drive is mounted there (instead of your usual 'My Drive').