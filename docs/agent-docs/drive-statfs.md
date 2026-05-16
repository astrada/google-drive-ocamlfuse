# `Drive.statfs`

## Purpose

`Drive.statfs` is the FUSE-facing filesystem-capacity reporting path. It
collects the current metadata/config runtime and delegates quota math and
`statvfs` construction to `DriveFilesystemStats`.

It does not inspect an individual file or directory path. Instead, it reports a
synthetic `statvfs` view for the whole mounted filesystem based on the global
metadata snapshot returned by `Drive.get_metadata`.

So this function is best understood as a quota-reporting adapter, not as a
path-sensitive lookup.

## Signature And FUSE Boundary

```ocaml
val statfs : unit -> Fuse.Unix_util.statvfs
```

The production FUSE adapter wires:

```ocaml
let statfs path =
  Utils.log_with_header "statfs %s\n%!" path;
  with_drive_op ~log_exception:true ~label:"statfs" ~param:path Drive.statfs
```

Two details matter here:

- FUSE passes a `path`, but `Drive.statfs` itself ignores path entirely
- the adapter uses `log_exception:true`, unlike many of the simpler path ops

So the visible path is only part of logging and error labeling at the boundary,
not an input to the reported filesystem numbers.

## Implementation Boundary

The public wrapper is:

```ocaml
let statfs () =
  let metadata = get_metadata () in
  DriveFilesystemStats.statfs (drive_filesystem_stats_runtime metadata)
```

`drive_filesystem_stats_runtime` reads the current config from `Context` and
passes it together with the metadata snapshot:

```ocaml
type runtime = {
  metadata : CacheData.Metadata.t;
  config : Config.t;
}
```

The pure implementation in `DriveFilesystemStats` then performs the existing
quota and block conversion:

```ocaml
let statfs runtime =
  let limit =
    if
      runtime.metadata.CacheData.Metadata.storage_quota_limit = 0L
      || runtime.config.Config.team_drive_id <> ""
    then Int64.max_int
    else runtime.metadata.CacheData.Metadata.storage_quota_limit
  in
  let f_blocks = Int64.div limit f_bsize in
  let free_bytes =
    Int64.sub limit runtime.metadata.CacheData.Metadata.storage_quota_usage
  in
  let f_bfree = Int64.div free_bytes f_bsize in
  {
    Fuse.Unix_util.f_bsize;
    f_blocks;
    f_bfree;
    f_bavail = f_bfree;
    f_files = f_blocks;
    f_ffree = f_bfree;
    f_namemax = 256L;
    f_frsize = 0L;
    f_favail = 0L;
    f_fsid = 0L;
    f_flag = 0L;
  }
```

That is the whole reporting control flow.

## Metadata Dependency

The first line in the public wrapper is:

```ocaml
let metadata = get_metadata ()
```

So `Drive.statfs` inherits all of `Drive.get_metadata`'s freshness behavior
before it calls `DriveFilesystemStats`.

That means a `statfs` call can:

- use cached metadata if it is still valid
- trigger a metadata refresh
- trigger Drive change-feed reconciliation indirectly

even though the visible result looks like a simple filesystem-capacity query.

See `docs/agent-docs/drive-get-metadata.md` for the full refresh logic behind
that dependency.

## Where The Quota Numbers Come From

The quota fields come from `CacheData.Metadata.t`:

- `storage_quota_limit`
- `storage_quota_usage`

Those are populated by `get_metadata` from:

```ocaml
AboutResource.get
```

using the Drive `storageQuota(limit,usage)` fields.

So the reported `statfs` capacity is fundamentally account-level Drive metadata,
not something derived from the local cache directory.

## The Team-Drive / Zero-Limit Special Case

The most important policy branch is
`DriveFilesystemStats.quota_limit`:

```ocaml
if metadata.storage_quota_limit = 0L || config.team_drive_id <> ""
then Int64.max_int
else metadata.storage_quota_limit
```

So `statfs` reports an effectively unbounded filesystem in two situations:

- Drive returned a quota limit of `0`
- the mount is operating in team-drive mode

This is not a real capacity probe. It is a deliberate fallback policy to avoid
reporting a small or meaningless quota in those modes.

## Block Math

`DriveFilesystemStats` uses the module-level constant:

```ocaml
let f_bsize = 4096L
```

All reported space figures are derived from that fixed block size.

The calculations are:

- `f_blocks = limit / f_bsize`
- `free_bytes = limit - storage_quota_usage`
- `f_bfree = free_bytes / f_bsize`
- `f_bavail = f_bfree`

So the visible block counts are a straightforward translation from Drive quota
bytes into synthetic 4 KiB blocks.

## Synthetic File Counts

The inode-like counts are also synthetic:

- `f_files = f_blocks`
- `f_ffree = f_bfree`

These numbers do not come from a real file-count query against Drive.

They are simply mirrored from the space-based block math, which makes the
reported filesystem shape internally consistent enough for consumers that expect
these fields to exist.

## Fixed And Placeholder Fields

Several returned fields are hard-coded:

- `f_namemax = 256L`
- `f_frsize = 0L`
- `f_favail = 0L`
- `f_fsid = 0L`
- `f_flag = 0L`

The source comment marks the trailing group as ignored.

So `DriveFilesystemStats.statfs` is intentionally minimal: it fills the fields
that the repository cares about and leaves the rest as placeholders.

## What `Drive.statfs` Does Not Use

`Drive.statfs` and `DriveFilesystemStats.statfs` do not use:

- the visible `path`
- per-resource lookup
- local cache size
- `Config.max_cache_size_mb`

This last point is easy to misunderstand because `cache_size` exists in
metadata and the repository also manages a local cache directory limit.

But `statfs` reports Drive quota usage, not local cache occupancy or local
cache limits.

## Relationship To `Drive.get_attr`

`statfs` is the filesystem-wide sibling of `Drive.get_attr`.

- `get_attr` synthesizes stats for one visible path
- `statfs` synthesizes capacity stats for the whole mounted filesystem

Both are reporting paths, but `statfs` is global and quota-driven rather than
resource-driven.

## What `Drive.statfs` Does Not Do

`Drive.statfs` does not:

- query the local filesystem for disk capacity
- inspect any specific mounted path
- count actual Drive files
- account for the local cache size limit in its free-space result

They only convert the current Drive quota snapshot into a synthetic `statvfs`
record.

## Related Docs

- `docs/agent-docs/drive-get-metadata.md`
- `docs/agent-docs/drive-get-attr.md`

## Source Pointers

- `src/driveFilesystemStats.ml`: quota/block policy and `statvfs` construction
- `src/drive.ml`: thin `statfs` wrapper
- `src/driveMetadataRefresh.ml`: `get_metadata` policy
- `src/drive.ml`: thin `get_metadata` wrapper
- `src/cacheData.ml`: `CacheData.Metadata`
- `bin/gdfuseFuse.ml`: `statfs`
