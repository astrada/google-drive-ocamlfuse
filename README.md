## google-drive-ocamlfuse

### FUSE filesystem for Google Drive

`google-drive-ocamlfuse` is a FUSE-based file system for Google Drive, written in OCaml.
It lets you mount your Google Drive on Linux. The project is hosted on [GitHub](https://github.com/astrada/google-drive-ocamlfuse/),
where you can find the latest development version. Project documentation is hosted [here](https://github.com/astrada/google-drive-ocamlfuse/wiki).
There are Ubuntu packages in this [PPA](https://launchpad.net/~alessandro-strada/+archive/ubuntu/ppa).

### Features

* Full read/write access to ordinary files and folders
* Read-only access to Google Docs, Sheets, and Slides (exported to
  configurable formats)
* Multiple account support
* Duplicate file handling
* Access to trash (`.Trash` directory)
* Unix permissions and ownership
* Symbolic links
* Read-ahead buffers when streaming
* Accessing content shared with you (requires [configuration](https://github.com/astrada/google-drive-ocamlfuse/blob/beta/doc/Configuration.md))
* Team Drive [Support](https://github.com/astrada/google-drive-ocamlfuse/wiki/Team-Drives)
* Service Account [Support](https://github.com/astrada/google-drive-ocamlfuse/wiki/Service-Accounts)
* OAuth2 for Devices [Support](https://github.com/astrada/google-drive-ocamlfuse/wiki/OAuth2-for-Devices)
