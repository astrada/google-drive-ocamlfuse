# `DriveMetadataRefresh` / `Drive.get_metadata`

## Purpose

`DriveMetadataRefresh` owns the global metadata freshness and change-feed
reconciliation policy for the filesystem. `Drive.get_metadata` is the
production-facing wrapper that builds a refresh runtime from `Context` and
delegates into that module.

It does not just return account metadata. It also decides whether the cached
resource graph can still be trusted, and if not, reconciles it against the
Drive changes feed.

Many other operations depend on it indirectly:

- `Drive.get_resource` uses it before path lookup
- `Drive.read_dir` depends on the resulting resource freshness state
- `Drive.statfs` reads quota information from it directly

So this function is one of the main places where cache coherence is enforced.

## What Metadata Means Here

The returned value is `CacheData.Metadata.t`, which stores:

- `display_name`
- `storage_quota_limit`
- `storage_quota_usage`
- `start_page_token`
- `cache_size`
- `last_update`
- `clean_shutdown`

Two fields are especially important for runtime behavior:

- `start_page_token`: the checkpoint for incremental Drive change polling
- `last_update`: the timestamp used to decide whether cached resources are still
  fresh enough to reuse

## APIs Used

The production ports behind `get_metadata` combine three different Google Drive
API calls:

- `AboutResource.get` for account display name and quota
- `ChangesResource.getStartPageToken` for a fresh change checkpoint
- `ChangesResource.list` for incremental resource-cache reconciliation

The request field sets are defined at the top of `src/drive.ml`:

- `changes_std_params` for change polling
- a smaller `AboutResource.get` field set for account metadata

## High-Level Flow

At a high level, `DriveMetadataRefresh.get_metadata` does this:

1. take `Context.metadata_lock`
2. load metadata from `Context` if already present, otherwise from the cache DB
3. if DB metadata was loaded, recompute `cache_size` from the filesystem
4. check `Config.metadata_cache_time`
5. if metadata is still valid, return it unchanged
6. otherwise fetch fresh metadata from Drive and reconcile the resource cache
7. persist the updated metadata back to cache and `Context`

The lock matters because metadata refresh mutates both:

- the global metadata snapshot
- the cached resource graph that other operations consult

## Locking And Storage Layers

The refresh module runs under the injected metadata lock. In production that is:

```ocaml
Utils.with_lock context.Context.metadata_lock (fun () -> ...)
```

Inside that critical section it prefers the in-memory `Context.metadata` value.
If the context has no metadata yet, it loads the metadata row from the cache DB:

The DB lookup is provided by the production `select_metadata` port, which
delegates to `Cache.Metadata.select_metadata`.

and stores the result back into `Context`.

### Cache Size Resync On DB Load

When metadata is loaded from the DB, `get_metadata` recalculates `cache_size`
from the real cache directory contents before using it.

That means the metadata row on disk is treated as advisory for cache size, not
authoritative. This avoids drifting forever if previous runs exited badly or if
the on-disk cache was modified independently.

## Validity Check

The freshness test is:

```ocaml
CacheData.Metadata.is_valid metadata_cache_time metadata
```

which means:

```ocaml
Unix.gettimeofday () -. metadata.last_update <= metadata_cache_time
```

If the metadata snapshot is still valid under `Config.metadata_cache_time`,
`get_metadata` returns immediately and does not contact Drive.

If it is missing or stale, it runs the refresh path.

## Refresh Path

The refresh path is `DriveMetadataRefresh`'s internal
`refresh_metadata old_metadata`.

That helper:

1. requests fresh account metadata from Drive
2. carries forward the prior `cache_size`
3. chooses a change checkpoint token
4. sets `last_update = now`
5. sets `clean_shutdown = false`
6. reconciles cached resources through `update_resource_cache`
7. stores the final metadata in the cache DB
8. updates `Context.metadata`

The new metadata row is created from the injected account-metadata and
start-page-token request ports.

## Start Page Token Handling

`request_metadata` decides the starting token like this:

- if the old metadata already has a non-empty `start_page_token`, reuse it
- otherwise call `ChangesResource.getStartPageToken`

So the token logic is:

- first-time metadata creation establishes a baseline token
- later refreshes reuse the previous token and ask Drive for changes since then

## Resource-Cache Reconciliation

The main policy lives in `DriveMetadataRefresh`'s internal
`update_resource_cache new_metadata old_metadata`.

This function does not always fetch the full change list immediately. It first
performs a cheap preflight check with `request_remaining_changes`.

### Preflight Probe

The probe calls `ChangesResource.list` with:

- `pageSize = change_limit` where `change_limit = 50`
- fields restricted to `newStartPageToken`

It interprets the result as:

- `no_changes`: `newStartPageToken` equals the previous token
- `over_limit`: `newStartPageToken` is empty
- otherwise: there are changes and the full change list can be fetched normally

This gives `get_metadata` three distinct branches.

## Branch 1: No Changes

If there are no server-side changes since the stored token, `get_metadata` does
not touch individual resource rows.

Instead it updates every resource timestamp to the new metadata timestamp:

```ocaml
Cache.Resource.update_all_timestamps cache new_metadata.last_update
```

That is important because resource validity is compared against metadata
freshness later. Advancing all resource timestamps keeps unchanged resources
reusable without forcing path-by-path refreshes.

## Branch 2: Too Many Changes

If the preflight probe reports `over_limit`, the function does not attempt
incremental replay.

Instead it:

1. fetches a brand-new start page token
2. invalidates all invalidable cached resources
3. returns metadata with the new token

`invalidate_all` does not touch every row equally. It leaves these states alone:

- `ToUpload`
- `Uploading`
- `NotFound`

Everything else becomes `ToDownload`.

So the over-limit path means:

- keep local-upload state intact
- keep negative-cache tombstones intact
- force later operations to refresh ordinary resources lazily

This is a pragmatic fallback when the incremental diff is too large to replay
cheaply or safely through the current code path.

## Branch 3: Normal Incremental Replay

If changes are available and still within the incremental path, the function:

1. updates all resource timestamps to `new_metadata.last_update`
2. fetches the full changes list with pagination
3. replays additions, updates, trashing, and removals
4. invalidates synthetic views if any change was seen
5. stores the new `newStartPageToken`

### Why Timestamps Are Updated First

The timestamp bump happens before replaying individual changes.

That means:

- unchanged resources remain fresh
- changed resources are then selectively rewritten or invalidated

Without that first step, a successful metadata refresh would make unrelated
resources look stale relative to the new metadata timestamp.

### Adding Newly Discoverable Resources

The `"Adding new resources to cache"` phase only adds a changed file when:

- the change is not `removed`
- the changed file is not already trashed
- no resource row already exists for the same remote id
- at least one cached parent resource is already known and `Synchronized`

That last condition is important. The code only synthesizes a path for a new
remote item when it can anchor that item under a known parent path.

The inserted path uses the same filename-cleaning and duplicate-disambiguation
logic as `read_dir`, through the `DriveResourceMapping` helpers exposed by the
production ports.

### Updating Existing Resources

The `"Updating resource cache"` phase refreshes resources already known by remote
id, but only when the changed file version is newer than the cached version.

After updating those rows with `update_resource_from_file`, it calls:

```ocaml
Cache.Resource.invalidate_resources cache ids
```

That turns the affected resources into `ToDownload` unless they are in one of
the protected states (`ToUpload`, `Uploading`, `NotFound`).

So this phase does two things at once:

- update remote metadata immediately
- mark existing content or derived state as needing refresh

### Trashing Resources

For changes whose file is now trashed, the function marks matching cached rows as
`trashed = true`.

It does not delete them immediately. They stay in cache, but now belong to the
trash namespace.

### Removing Deleted Resources

For changes marked `removed`, the function deletes matching cached resources and
their local cached files via `delete_cached_resources`.

This is stronger than invalidation: the rows disappear entirely.

### Invalidating Synthetic Views

If any changes were processed, `get_metadata` invalidates these synthetic views:

- the trash bin root, if trash is enabled
- `/lost+found`, if enabled
- `/.shared`

That ensures later `read_dir` calls rebuild those directories instead of
reusing a now-stale synthetic listing.

## First-Time Metadata Creation

`old_metadata = None` is a special case.

In the normal incremental branch, after updating timestamps, the code returns
immediately without replaying a change list if there was no prior metadata row.

That means first-time metadata creation establishes the freshness baseline and
token checkpoint, but it does not attempt to reconstruct cache contents from a
historical changes stream.

In practice that is reasonable because startup already begins with:

- `Context.metadata = None`
- an empty or freshly prepared cache on many cold-start paths

## `clean_shutdown` Behavior

Every refreshed metadata row is written with:

```ocaml
clean_shutdown = false
```

That flag is not owned by `get_metadata` alone. Shutdown later flips it through
the cache layer when the process exits cleanly.

So `get_metadata` should be understood as writing the "filesystem is live"
version of metadata, while shutdown is responsible for marking the final clean
state.

## Why `get_resource` Depends On This

`Drive.get_resource` uses `get_metadata().last_update` as the freshness boundary
for individual resources.

That only works because `get_metadata` also reconciles the resource cache:

- unchanged resources get refreshed timestamps
- changed resources get invalidated or rewritten
- deleted resources disappear

So `get_resource` can treat `metadata.last_update` as a meaningful global fence.

## Interaction With `statfs`

`Drive.statfs` reads quota information from `get_metadata`.

That means a `statfs` call can also trigger a metadata refresh and change-feed
reconciliation, even though it looks like a quota-only query from the outside.

See `docs/agent-docs/drive-statfs.md` for the reporting-side logic that turns
those metadata fields into a synthetic `statvfs` record.

## Maintenance Notes

When changing `DriveMetadataRefresh` or its production ports, watch these
invariants:

- metadata freshness and resource freshness are intentionally coupled
- `update_all_timestamps` is required to keep unchanged resources reusable
- over-limit fallback must not destroy `ToUpload` / `Uploading` state
- synthetic views must be invalidated when the underlying change set may affect
  them
- DB-loaded `cache_size` is intentionally recomputed before reuse
- first-time metadata initialization is not the same as replaying an incremental
  diff

## Related Docs

- `docs/agent-docs/drive-get-resource.md`
- `docs/agent-docs/drive-read-dir.md`

## Source Pointers

- `src/driveMetadataRefresh.ml`: metadata freshness and change-feed replay
  policy
- `src/drive.ml`: `DriveMetadataRefreshPorts`
- `src/drive.ml`: thin `get_metadata` wrapper
- `src/driveResourceMapping.ml`: resource construction and filename mapping
- `src/cacheData.ml`: `CacheData.Metadata`
- `src/cacheData.ml`: `CacheData.Metadata.is_valid`
- `src/cache.ml`: metadata/resource cache dispatch
