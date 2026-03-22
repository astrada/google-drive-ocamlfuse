# `Drive.start_uploading_if_dirty`

## Purpose

`Drive.start_uploading_if_dirty` is the tiny state-transition gate in front of
the upload lifecycle.

Its job is not to perform any upload work itself. It only decides whether a
path currently points to a cached resource that is still in `ToUpload` state
and, if so, flips that row to `Uploading`.

So this helper is the local scheduling gate behind `flush`, `fsync`, and
`release`.

## Signature

```ocaml
val start_uploading_if_dirty : string -> bool
```

The return value answers one narrow question:

- `true`: this call observed `ToUpload` and transitioned the cached row to
  `Uploading`
- `false`: no upload should be started from this call

It does not return a resource handle or a request object.

## Entire Implementation

The implementation is:

```ocaml
let start_uploading_if_dirty path =
  let config = Context.get_ctx () |. Context.config_lens in
  let path_in_cache, trashed = get_path_in_cache path config in
  let resource = lookup_resource path_in_cache trashed in
  match resource with
  | None -> false
  | Some r ->
      if r.CacheData.Resource.state == CacheData.Resource.State.ToUpload then (
        let cache = Context.get_cache () in
        update_cached_resource_state cache CacheData.Resource.State.Uploading
          r.CacheData.Resource.id;
        true)
      else false
```

That is the whole control flow.

## High-Level Flow

At a high level, the helper does this:

1. normalize the visible path into `(path_in_cache, trashed)`
2. do a direct cache lookup with `lookup_resource`
3. if no row is found, return `false`
4. if the row exists but is not exactly `ToUpload`, return `false`
5. otherwise update the cached row to `Uploading` and return `true`

There is no Drive API call and no request/session machinery here.

## Path Normalization

The helper begins with:

```ocaml
let path_in_cache, trashed = get_path_in_cache path config
```

So even this tiny gate uses the normal namespace model:

- ordinary paths stay in the main namespace
- trash-view paths are mapped into `(path, trashed = true)`

That matters because the cache lookup is path-and-namespace sensitive.

## Direct Cache Lookup, Not `get_resource`

The lookup step is:

```ocaml
let resource = lookup_resource path_in_cache trashed
```

This is intentionally cheaper than:

```ocaml
get_resource path_in_cache trashed
```

Key consequences:

- no metadata refresh happens here
- no remote lookup happens here
- no stale-row repair happens here
- only the currently cached row is considered

That is why this helper is fast enough to sit in the hot path of repeated
`flush` / `fsync` / `release` callbacks.

## Exact State Match: Only `ToUpload`

The function only starts work when the cached state is exactly:

```ocaml
CacheData.Resource.State.ToUpload
```

Everything else returns `false`, including:

- `Synchronized`
- `Uploading`
- `ToDownload`
- `Downloading`
- `NotFound`

So this helper is not a generic "is this file interesting?" check. It is a
very specific upload-dispatch gate.

## The Only Side Effect: Flip To `Uploading`

When the helper sees `ToUpload`, it immediately does:

```ocaml
update_cached_resource_state cache CacheData.Resource.State.Uploading
  r.CacheData.Resource.id
```

That state flip is the whole point of the helper.

It means later close/sync callbacks will usually observe `Uploading` instead of
`ToUpload` and therefore return `false` instead of scheduling the upload again.

So this function is the main idempotency guard for repeated trigger callbacks.

## Relationship To `upload_if_dirty`

The immediate caller is:

```ocaml
let upload_if_dirty path =
  if start_uploading_if_dirty path then
    do_request (upload_with_retry path) |> ignore
```

So `start_uploading_if_dirty` is phase 1 of a two-phase dispatch path:

1. local cheap state gate
2. real request/session upload dispatch

If this function returns `false`, the later upload path is never entered.

If it returns `true`, the next step is `upload_with_retry path`, which performs
full path resolution and eventually reaches `queue_upload`.

See `docs/agent-docs/drive-upload-if-dirty.md` for the tiny bridge helper that
performs that handoff into `do_request`.

## Why `upload_with_retry` Re-Resolves The Resource

A subtle but important design point is that `start_uploading_if_dirty` does not
return the resource it found.

The later upload step resolves again by path.

That means this helper is responsible only for:

- the cheap cached-state decision
- the `ToUpload -> Uploading` transition

It is not responsible for supplying the final authoritative row that upload
execution should use.

See `docs/agent-docs/drive-upload-path.md` for the later `upload_with_retry`
and `queue_upload` stages.

## Relationship To The Dirtying Operations

This helper assumes some earlier path already set the resource state to
`ToUpload`.

The main producers are:

- `Drive.write`
- `Drive.truncate`
- one `Drive.rename` replacement path

So this function only works because those mutation paths separate:

- "mark dirty"
- from "later start the upload"

It is not meaningful on a path that has never been dirtied locally.

## Maintenance Caveat: Check Then Flip

The helper does a simple two-step local sequence:

1. read cached row
2. write new cached state

It is not written as a compare-and-swap primitive at this layer.

That means its correctness relies on the current cache/update model being good
enough for repeated callback suppression, not on a stronger explicit atomicity
contract documented here.

So if upload-trigger concurrency changes substantially, this small helper is one
of the first places to re-examine.

## What `Drive.start_uploading_if_dirty` Does Not Do

`Drive.start_uploading_if_dirty` does not:

- call `get_resource`
- refresh metadata
- flush memory buffers
- call `queue_upload`
- perform the actual upload

It only decides whether local cached state justifies entering the later upload
dispatch path.

## Related Docs

- `docs/agent-docs/drive-flush-fsync-release.md`
- `docs/agent-docs/drive-upload-path.md`
- `docs/agent-docs/drive-write.md`
- `docs/agent-docs/drive-truncate.md`

## Source Pointers

- `src/drive.ml`: `start_uploading_if_dirty`
- `src/drive.ml`: `lookup_resource`
- `src/drive.ml`: `update_cached_resource_state`
- `src/drive.ml`: `upload_if_dirty`
