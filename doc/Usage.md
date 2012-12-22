The first time, you can run `google-drive-ocamlfuse` without parameters:

          $ google-drive-ocamlfuse

This command will create the default application directory (`~/.gdfuse/default`), containing the configuration file `config` (see the [[Configuration]] page) for more details about configuration). And it will start a web browser to obtain authorization to access your Google Drive. This will let you modify default configuration before mounting the filesystem.

Then you can mount the filesystem:

        $ google-drive-ocamlfuse mountpoint

If you have more than one account, you can run:

        $ google-drive-ocamlfuse -label label [mountpoint]

Using `label` to distinguish different accounts. The program will use the directory `~/.gdfuse/label` to host configuration, application state, and file cache. No file is shared among different accounts, so you can have a different configuration for each one.