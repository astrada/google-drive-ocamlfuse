First, you must [set up OAuth
2.0](https://support.google.com/cloud/answer/6158849?hl=en):

1. [Activate](https://cloud.google.com/service-usage/docs/enable-disable) the
   `Google Drive API`.
1. Create an OAuth client ID.
1. Choose `Desktop` as `Application type`.
1. Set the `Name` to anything you like.

This way you will get a `Client ID` and `Client secret` that you can use to
access your Drive. To authorize `google-drive-ocamlfuse`, pass the client ID
and the client secret on the command line, e.g.:

    google-drive-ocamlfuse -id xxxxxxxxxx.apps.googleusercontent.com -secret XXX-YYY-ZZZ

This command will create the default application directory
(`~/.gdfuse/default`), containing the configuration file `config` (see the
[wiki
page](https://github.com/astrada/google-drive-ocamlfuse/wiki/Configuration)
for more details about configuration). And it will start a web browser to
obtain authorization to access your Google Drive. This way, you can modify the
default configuration before mounting the filesystem.

Then, you can choose a local directory to mount your Google Drive (e.g.: `~/GoogleDrive`).

Create the mount point, if it doesn't exists:

    mkdir ~/GoogleDrive

Then, you can mount the filesystem:

    google-drive-ocamlfuse ~/GoogleDrive

If you have more than one account, you can run:

    google-drive-ocamlfuse -label [label] ~/GoogleDrive

Using `label` to distinguish different accounts. The program will use the
directory `~/.gdfuse/[label]` to host the configuration, the application
state, and the file cache. No file is shared among different accounts, so you
can have a different configuration for each one.

To unmount the filesystem, issue this command:

    fusermount -u ~/GoogleDrive

Options
-------

Run `google-drive-ocamlfuse -help` to get all the command options available. To find more details about `-o` mount options, you can refer to this [page](http://manpages.ubuntu.com/manpages/zesty/man8/mount.fuse.8.html). Non-standard mount option `gdfroot` can be used to specify a custom path to the configuration directory (default is `$HOME/.gdfuse`).

#### Since 0.7.5
`-docsmode`: This option can be used to quickly set Google Docs config options. Supported values are:
* `libreoffice`: sets
  * `download_docs=true`
  * `document_format=odt`
  * `drawing_format=png`
  * `form_format=zip`
  * `presentation_format=odp`
  * `spreadsheet_format=ods`
  * `apps_script_format=json`
* `msoffice`: sets
  * `download_docs=true`
  * `document_format=docx`
  * `drawing_format=png`
  * `form_format=zip`
  * `presentation_format=pptx`
  * `spreadsheet_format=xlsx`
  * `apps_script_format=json`
* `desktop`: sets
  * `download_docs=true`
  * `document_format=desktop`
  * `drawing_format=desktop`
  * `form_format=desktop`
  * `presentation_format=desktop`
  * `spreadsheet_format=desktop`
  * `apps_script_format=desktop`
* `off`: sets
  * `download_docs=false`

#### Since 0.7.11
* `-serviceaccountpath`: This option can be used to specify the path of the JSON file containing the credentials of a service account
* `-serviceaccountuser`: This option can be used to specify the email of a G Suite user to impersonate (with the service account specified by the `-serviceaccountpath` option)

#### Since 0.7.12
* `-log_to`: Specifies where the logs are going to be written. It can be `stdout`, `stderr`, or an absolute path
* `-scope`: Specifies a custom Drive API [scope](https://developers.google.com/drive/api/v3/about-auth#OAuth2Authorizing)
* `-redirect_uri`: Specifies a custom `redirect_uri` for the OAuth2 [flow](https://developers.google.com/identity/protocols/OAuth2InstalledApp#step-2-send-a-request-to-googles-oauth-20-server)

#### Since 0.7.13
* `-device`: Use [OAuth2 for Devices](https://github.com/astrada/google-drive-ocamlfuse/wiki/OAuth2-for-Devices) authorization flow

#### Since 0.7.32
* `-port`: This option can be used to specify a port for the embedded HTTP
  server, that will receive the verification code from Google during
  authorization. The default is 8080.

