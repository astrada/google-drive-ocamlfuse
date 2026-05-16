# Extract Drive Filesystem Stats

## Goal

Move filesystem-wide `statfs` quota reporting out of `src/drive.ml` and into a
small, pure `DriveFilesystemStats` module.

The extraction target is:

- `f_bsize`
- quota limit selection for normal, zero-limit, and team-drive metadata
- Drive quota byte-to-block conversion
- synthetic `Fuse.Unix_util.statvfs` record construction
- `statfs`

The production behavior should stay unchanged. `src/drive.ml` should keep the
public `statfs : unit -> Fuse.Unix_util.statvfs` wrapper because the FUSE
adapter calls `Drive.statfs` directly.

## Current Problem

`Drive.statfs` is small, but it is user-visible and mixes pure reporting policy
with production runtime access:

- `get_metadata ()` may refresh global metadata
- `Context.get_ctx ()` supplies the current config
- quota and team-drive policy choose an effective capacity limit
- quota bytes are converted into 4 KiB block counts
- fixed placeholder fields are put into a `statvfs` record

The policy is pure once metadata and config are available, so it can be tested
directly without production `Context`, cache state, OAuth, Drive API calls, or
metadata refresh side effects.

## Proposed Shape

Add:

- `src/driveFilesystemStats.ml`
- `src/driveFilesystemStats.mli`
- `test/testDriveFilesystemStats.ml`

Expose:

```ocaml
val f_bsize : int64
val f_namemax : int64
val unlimited_quota_limit : int64

type runtime = {
  metadata : CacheData.Metadata.t;
  config : Config.t;
}

val quota_limit : runtime -> int64
val statfs : runtime -> Fuse.Unix_util.statvfs
```

No functor or ports are needed. The extracted behavior depends only on the
supplied metadata and config.

## Extracted Behavior

`quota_limit runtime` should preserve the current effective-limit policy:

- return `Int64.max_int` when `metadata.storage_quota_limit = 0L`
- return `Int64.max_int` when `config.team_drive_id <> ""`
- otherwise return `metadata.storage_quota_limit`

`statfs runtime` should preserve the current block math:

- `limit = quota_limit runtime`
- `f_blocks = limit / f_bsize`
- `free_bytes = limit - metadata.storage_quota_usage`
- `f_bfree = free_bytes / f_bsize`
- `f_bavail = f_bfree`
- `f_files = f_blocks`
- `f_ffree = f_bfree`

The fixed fields should remain:

- `f_bsize = 4096L`
- `f_namemax = 256L`
- `f_frsize = 0L`
- `f_favail = 0L`
- `f_fsid = 0L`
- `f_flag = 0L`

Do not clamp negative free space during this extraction. If usage is greater
than the selected limit, the current arithmetic can report negative free
blocks, and this pass should keep that behavior.

## Production Wiring

In `src/drive.ml`, remove the local `f_bsize` constant and delegate `statfs`
to `DriveFilesystemStats`:

```ocaml
let drive_filesystem_stats_runtime metadata =
  let context = Context.get_ctx () in
  {
    DriveFilesystemStats.metadata;
    config = context |. Context.config_lens;
  }

let statfs () =
  let metadata = get_metadata () in
  DriveFilesystemStats.statfs (drive_filesystem_stats_runtime metadata)
```

Keep `src/drive.mli` stable for this pass.

## Implementation Steps

1. Create `driveFilesystemStats.mli` with constants, `runtime`, and helper
   signatures.
2. Create `driveFilesystemStats.ml` with the current quota/block behavior.
3. Replace the `Drive.statfs` body with a thin wrapper that supplies metadata
   and config to `DriveFilesystemStats`.
4. Keep `Drive.statfs` public call shape unchanged.
5. Add `test/testDriveFilesystemStats.ml`.
6. Register the suite in `test/testSuite.ml`.
7. Run `tools/format_ocaml` or `ocamlformat` on touched OCaml files.
8. Run `dune build @install` and `dune runtest` sequentially.

## Unit Test Plan

Use synthetic `CacheData.Metadata.t` values and `Config.default` variants.

Cover limit selection:

- normal quota returns `metadata.storage_quota_limit`
- zero quota limit returns `Int64.max_int`
- team-drive config returns `Int64.max_int`

Cover statvfs math:

- normal quota converts limit and free bytes into 4 KiB block counts
- `f_bavail` mirrors `f_bfree`
- `f_files` mirrors `f_blocks`
- `f_ffree` mirrors `f_bfree`
- zero-limit metadata reports blocks from `Int64.max_int`
- team-drive config reports blocks from `Int64.max_int`
- over-limit usage preserves the current negative free-block behavior

Cover fixed fields:

- `f_bsize = 4096L`
- `f_namemax = 256L`
- ignored fields stay `0L`

Existing FUSE boundary tests should continue to call the public `Drive.statfs`
wrapper shape.

## Acceptance Criteria

- `src/drive.ml` no longer contains filesystem stats quota/block policy.
- `DriveFilesystemStats` owns the pure statfs constants and record
  construction.
- Existing `Drive.statfs` public call shape remains available.
- Focused unit tests cover normal quota, zero-limit quota, team-drive quota,
  block conversion, fixed fields, and over-limit arithmetic without production
  context or metadata refresh.
- `dune build @install` and `dune runtest` pass.

## Agent Docs Follow-Up

After implementation, update agent docs to describe the current state directly:

- `docs/agent-docs/architecture.md`
- `docs/agent-docs/repo-map.md`
- `docs/agent-docs/drive-statfs.md`
- `docs/agent-docs/drive-get-metadata.md`

Avoid before/after language. The docs should describe `DriveFilesystemStats`
as the current implementation boundary for converting Drive quota metadata and
config into a synthetic `statvfs` record.
