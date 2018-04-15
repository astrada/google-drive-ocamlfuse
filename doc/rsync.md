_notes on [rsync](https://rsync.samba.org/) usage with `google-drive-ocamlfuse` mounts_

### Temporary files and file mimeType

`rsync` copies the files using temporary random names by default (e.g. `.filename.pdf.0MN9tN`), but the content type is assigned derived the file extension, and when the file is uploaded, the extension is wrong. There's a [known google drive api issue](http://stackoverflow.com/questions/14629839/unable-to-update-mimetype-using-google-drive-api) on updating mimeType so the mimeType stays wrong.

This doesn't affect the actual file content but it impacts how google drive manages the file (i.e.: a pdf file identified as `application/octet-stream` won't show any preview and won't open in google drive leaving download as the only option)

The workaround is to use rsync option `--inplace` that avoids creating temporary files (see issue [#21](https://github.com/astrada/google-drive-ocamlfuse/issues/21))

### Improve transfer speed

* you can set a very high timeout in `metadata_cache_time=` so your cache doesn't risk to be invalidated
* using `rsync` with `-W` (no delta operations - copy Whole file) speeds it up and handles corrupted files better than cp (see issue [#48](https://github.com/astrada/google-drive-ocamlfuse/issues/48))
* `rsync` is single-theaded, but you can test [parallel](http://www.gnu.org/software/parallel/)
* increasing sqlite3_busy_timeout and max_cache_size_mb seems to mitigate "Device or resource busy" issues (see issue [#48](https://github.com/astrada/google-drive-ocamlfuse/issues/48))

### Examples

```
rsync -trluhv --delete --stats --inplace /folder/to/upload/* /folder/mounted/with/google-drive-ocamlfuse/
rsync -rlvW --inplace --size-only /folder/to/upload/* /folder/mounted/with/google-drive-ocamlfuse/
```
