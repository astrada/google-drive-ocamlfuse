# `Drive.download_resource`

## Purpose

`Drive.download_resource` is the shared helper that ensures a
`CacheData.Resource.t` has usable local content in the cache directory.

The production helper in `src/drive.ml` is a thin adapter over
`DriveDownloads`:

```ocaml
let download_resource resource =
  DownloadOps.download_resource (drive_download_runtime ()) resource
```

`DriveDownloads` owns the local-content materialization state machine.
`DriveDownloadPorts` connects that policy to production cache access,
filesystem checks and writes, Drive API export/download calls, per-resource
locking, backoff, and exception mapping.

Its job is not just "download bytes from Drive". Depending on the resource and
its current cache state, it may instead:

- reuse an already valid cached file
- trust an existing cached file after an MD5 check
- export a Google document into a local file
- synthesize a local desktop entry or HTML redirect
- create an empty local file without contacting Drive

The function returns the local cache path that callers should read from or
write to.

This is the bridge between metadata-oriented resource handling and the on-disk
content cache used by `read`, `write`, `truncate`, and some rename flows.

It is distinct from the streaming read path:

- `stream_resource` reads byte ranges directly into the FUSE buffer
- `download_resource` materializes or validates a full local cache file

See `docs/agent-docs/drive-read.md` for the top-level `Drive.read` policy that
decides when this helper is used instead of streaming.

## Signature And Return Value

```ocaml
val download_resource : CacheData.Resource.t -> string GapiMonad.SessionM.m
```

The input is an already resolved resource row.

In practice, the helper assumes that row already has a remote id. It is for
existing remote resources, not brand-new local placeholders.

The return value is always the content path:

```ocaml
let content_path = Cache.get_content_path cache resource
```

That path is keyed by the resource's remote id in the cache directory. The main
question is whether the function needs to create or refresh the file at that
path before returning it.

Inside the core, the operation is parameterized by the current runtime:

```ocaml
DriveDownloads.download_resource :
  DriveDownloads.runtime ->
  CacheData.Resource.t ->
  string GapiMonad.SessionM.m
```

## Typical Call Pattern

Most callers do not invoke `download_resource` directly. They wrap it with
`with_retry`:

```ocaml
with_retry download_resource resource
```

That outer retry layer handles transient download failures by reloading fresh
metadata for the resource and retrying with updated state.

Common callers are:

- `read`, when the file is not being streamed
- `write`, before modifying local content
- `truncate`, before truncating the local cache file
- one rename-replace path, before copying source content into the target file
- `get_attr`, for some document cases where local stat data depends on the
  materialized file

## High-Level Contract

`DriveDownloads.download_resource` should be read as an idempotent "ensure
local content exists" helper, not as an unconditional network operation.

At a high level it does this:

1. compute the local cache path
2. reload current resource state from cache when needed
3. decide whether the existing local file can be reused
4. if not, serialize a refresh through a per-file mutex
5. materialize the local file in one of several resource-specific ways
6. return `content_path`

Two consequences matter:

- callers can safely invoke it even when the cache file already exists
- a successful return does not imply that bytes were fetched from Drive during
  this call

## Per-File Locking

Actual file materialization is serialized by a mutex stored in
`Context.file_locks`, keyed by remote id. `DriveDownloads` reaches that mutex
through the production `DriveDownloadPorts.with_resource_lock` port.

The lock path is:

1. look up or create the mutex for `resource.remote_id`
2. run `do_download` under that mutex

This prevents multiple threads from downloading or regenerating the same cache
file concurrently.

The function does not lock all resources globally. Coordination happens per
remote file id.

## State-Based Reuse In `check_state`

Before touching the network or rewriting the local file, the function reloads
the current cached row by remote id and switches on its state.

That reload matters because another thread may already have downloaded,
uploaded, or otherwise updated the same resource after the caller obtained the
original `resource` value.

### `Synchronized`, `ToUpload`, `Uploading`

For these states, the rule is:

- if `content_path` already exists, return it immediately
- otherwise perform the locked materialization path

This is why `download_resource` works for both read and write flows:

- `Synchronized` means cached content is ready to reuse
- `ToUpload` and `Uploading` mean local content is already authoritative for
  the current write/upload lifecycle, so it should not be overwritten just
  because the resource is dirty

### `ToDownload`

For `ToDownload`, the function does not blindly re-download.

It first calls `check_md5_checksum`:

- if the cached file exists and its MD5 matches the resource metadata, the
  function marks the resource `Synchronized` and reuses the file
- otherwise it performs the locked materialization path

So `ToDownload` means "content may need refresh", not "must always hit the
network".

### `Downloading`

If another worker has already put the resource in `Downloading`, the function
waits with exponential backoff and polls again.

After more than 300 checks, it treats the in-progress download as effectively
stuck and falls back to the same "MD5 recheck or re-download" logic used for
`ToDownload`.

The log message describes this threshold as roughly 5 hours.

### `NotFound`

If the reloaded state is `NotFound`, the function raises `File_not_found`.

At that point there is no content to materialize locally.

## The Materialization Path

When the state check decides a refresh is needed, `DriveDownloads` runs the
materialization request through the per-file lock port.

That path always starts by logging and reserving cache space through:

```ocaml
shrink_cache ~file_size ()
```

where `file_size` is taken from `resource.size` with a default of `0L`.
The production port delegates this accounting and possible eviction work to
`DriveCacheMaintenance`.

After that, the behavior splits into four main branches.

### 1. Desktop-Format Documents

If `is_desktop_format resource config` is true, the function does not fetch
file bytes from Drive.

Instead it synthesizes a small local representation:

- `create_html_with_redirect` when `config.desktop_entry_as_html = true`
- otherwise `create_desktop_entry`

This is the branch used for document formats configured as desktop entries
rather than exported content.

Two important nuances:

- it still counts as local content materialization
- it does not set the resource state to `Downloading` before writing the file

After the file is created, the function still finishes by marking the resource
`Synchronized`.

### 2. Exportable Google Documents

If the resource is a Google document but not in desktop-entry mode, the
function exports it into a local file.

The export flow is:

1. choose the configured export format with `CacheData.Resource.get_format`
2. map that format to a MIME type
3. try to find a matching cached export link
4. if found, download through `GapiService.get`
5. otherwise fall back to `FilesResource.export`

So document downloads are not ordinary media downloads. They are format-aware
exports.

### 3. Ordinary Non-Empty Files

For non-document resources with `size > 0`, the function performs a media
download into `content_path`.

Before the API call it sets the resource state to `Downloading`.

The actual transfer uses the production `download_media_to_file` port, backed
by `download_media`, which normally issues
`FilesResource.get ... ~media_download`.

There is one important exception path: if Drive rejects the request with
`cannotDownloadAbusiveFile` and `config.acknowledge_abuse = true`, the helper
retries the request with `acknowledgeAbuse = true`.

If the API download fails after the state flip, the error handler restores the
resource state to `ToDownload` before re-raising through the normal exception
mapping.

### 4. Zero-Byte Non-Document Files

If the resource is not a document and its size is `0`, the function does not
call Drive at all.

It simply creates an empty local file at `content_path`.

That keeps the local cache semantics consistent for empty files without paying
for a useless network round trip.

## Cache Size Accounting For Documents

`DriveDownloads` calls the production `update_cache_size_for_documents` port
both before and after materialization.
That port is implemented by `DriveCacheMaintenance`.

That helper only matters when:

- `resource.size = Some 0L`
- a local cache file exists

This is mainly about Google documents and desktop-entry representations, where
the remote object may report size zero but the local exported or synthesized
file occupies real bytes in the cache.

The flow is:

- subtract any previous local file size before regenerating the content
- add the new local file size afterwards

Ordinary nonzero-size files are accounted for by `shrink_cache`, so this helper
is specifically about the size mismatch between some remote resources and their
local representations.

## Final State Transition

After a successful materialization path, the function:

1. updates document-specific cache-size accounting if needed
2. logs completion
3. sets the resource state to `Synchronized`
4. returns `content_path`

That final `Synchronized` write happens for both:

- true downloads from Drive
- locally synthesized representations such as desktop entries

## Why Dirty States Reuse Existing Files

One subtle but important design choice is that `ToUpload` and `Uploading`
re-use the local file if it already exists.

That prevents `download_resource` from clobbering locally modified content when
a write path asks to "ensure content exists" before applying further changes.

This is why `write` and `truncate` can safely call the helper even after the
resource has already become dirty.

## Maintenance Notes

### The Function Returns A Path, Not A Byte Source

Callers should treat the result as "the local file to use now", not as proof
that a network transfer occurred.

### `ToDownload` Depends On Metadata Quality

The MD5 fast path only works when the resource metadata contains a checksum. If
the checksum is absent, `check_md5_checksum` returns false and the function will
materialize content again.

### Desktop And Exported Documents Are Special

For Google documents, the local cache file may be:

- an exported document in a configured format
- a `.desktop` file
- an HTML redirect file

So "downloaded content" can mean "locally generated representation of a remote
document", not raw Drive bytes.

### The Locking Boundary Is Narrow

Only the materialization path runs under the per-file mutex. The outer
state-check loop intentionally stays outside that critical section so threads
can notice that another worker has already finished the work.

### Test Boundary

`test/testDriveDownloads.ml` exercises the materialization state machine through
fake ports. The tests cover existing-file reuse, dirty-state reuse, MD5 shortcut
reuse, `Downloading` polling and stuck recovery, `NotFound`, desktop and HTML
document representations, export-link and export-call document downloads,
ordinary media downloads, zero-byte file creation, failure rollback to
`ToDownload`, and resources without a remote id.
