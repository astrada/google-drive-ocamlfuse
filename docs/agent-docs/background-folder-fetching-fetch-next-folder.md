# `BackgroundFolderFetching.fetch_next_folder`

## Purpose

`BackgroundFolderFetching.fetch_next_folder` is the runtime selection-and-callback
step in the background folder-prefetch subsystem.

It sits between:

- `folder_fetch`, which provides the polling loop
- the injected `read_dir` callback, which performs the actual folder refresh

Its job is narrower than either of those layers. It:

- asks the cache which folder should be prefetched next
- returns immediately if no folder is eligible
- loads the injected callback from the concurrent runtime state
- calls that callback with the selected folder path

So this helper is best read as "pick one folder and prefetch it now".

## Internal Status And Effective Signature

`fetch_next_folder` is internal to `src/backgroundFolderFetching.ml`. It is not
exposed in `src/backgroundFolderFetching.mli`.

Its effective shape is:

```ocaml
val fetch_next_folder : CacheData.t -> unit
```

The input is the cache handle. The callback and stop-related runtime state come
from `ConcurrentBackgroundFolderFetching`.

## Entire Implementation

```ocaml
let fetch_next_folder cache =
  let resource = Cache.Resource.select_next_folder_to_fetch cache in
  match resource with
  | None -> ()
  | Some r ->
      let d = ConcurrentBackgroundFolderFetching.get () in
      let path = r.CacheData.Resource.path in
      let remote_id = Option.default "" r.CacheData.Resource.remote_id in
      Utils.log_with_header "BEGIN: Prefetching folder %s (id=%s).\n%!" path
        remote_id;
      d.read_dir path;
      Utils.log_with_header "END: Prefetching folder %s (id=%s).\n%!" path
        remote_id
```

That is the whole helper.

## High-Level Flow

At a high level, the helper does this:

1. ask the cache for the next eligible folder resource
2. if none exists, return immediately
3. otherwise load the runtime record from
   `ConcurrentBackgroundFolderFetching`
4. extract the folder path and a logging-friendly remote id
5. log prefetch begin
6. call the injected `read_dir` callback with the folder path
7. log prefetch end

So this helper owns selection and callback handoff for exactly one folder.

## Cache-Side Selection Rule

The actual selection gate is:

```ocaml
Cache.Resource.select_next_folder_to_fetch cache
```

Across both cache backends, the selected resource must be:

- a folder
- in `CacheData.Resource.State.ToDownload`
- not trashed

Both backends also pick the oldest eligible folder by `last_update`:

- the SQLite backend uses `ORDER BY last_update`
- the in-memory backend sorts eligible resources by `last_update`

So this helper should be read as:

- "prefetch the oldest eligible non-trashed folder still marked `ToDownload`"

## Empty-Queue Behavior

If the selector returns `None`, the helper does exactly this:

```ocaml
| None -> ()
```

There is no log line and no retry loop at this layer. That is deliberate
because the polling loop calls this helper repeatedly. No eligible folder is a
normal idle condition.

## Runtime Callback Lookup

When a folder is selected, the helper loads:

```ocaml
let d = ConcurrentBackgroundFolderFetching.get () in
```

and then invokes:

```ocaml
d.read_dir path
```

So `fetch_next_folder` does not know how to refresh a folder itself. It relies
on the callback installed earlier by `start_folder_fetching_thread`.

In production, that callback eventually re-enters `Drive.read_dir`.

## Logging Fields

For logging, the helper derives:

```ocaml
let path = r.CacheData.Resource.path in
let remote_id = Option.default "" r.CacheData.Resource.remote_id in
```

Two details matter:

- the callback itself only receives `path`
- `remote_id` is used only for logging, and missing ids become `""`

So the log is richer than the callback contract.

## Relationship To `Drive.read_dir`

In production, the callback eventually runs:

```ocaml
fun path -> read_dir path |> ignore
```

That means `fetch_next_folder` does not mark folders `Synchronized` itself. The
state transition happens only because `Drive.read_dir` owns the actual folder
refresh logic.

So the split is:

- `fetch_next_folder`: choose one folder and invoke the callback
- `Drive.read_dir`: perform the real snapshot refresh and state transition

## No Local Exception Handling

`fetch_next_folder` has no local `try ... with` around:

```ocaml
d.read_dir path
```

So if the injected callback raises:

- the `"END: Prefetching folder ..."` log line is skipped
- the exception propagates back into `folder_fetch`

And because `folder_fetch` only catches `Exit`, an arbitrary callback failure
can currently terminate the background folder-fetch thread.

That is an important maintenance property of this helper.

## What `BackgroundFolderFetching.fetch_next_folder` Does Not Do

`BackgroundFolderFetching.fetch_next_folder` does not:

- poll or check the stop flag itself
- sleep or pace the loop itself
- mark the folder `Synchronized` itself
- create or manage a worker pool
- retry callback failures

It only selects one folder and invokes the installed callback.

## Related Docs

- `docs/agent-docs/background-folder-fetching-start-thread.md`
- `docs/agent-docs/background-folder-fetching-stop-thread.md`
- `docs/agent-docs/drive-read-dir.md`
- `docs/agent-docs/drive-init-filesystem.md`

## Source Pointers

- `src/backgroundFolderFetching.ml`: `fetch_next_folder`
- `src/backgroundFolderFetching.ml`: `folder_fetch`
- `src/backgroundFolderFetching.ml`: `start_folder_fetching_thread`
- `src/cache.ml`: `Cache.Resource.select_next_folder_to_fetch`
- `src/memoryCache.ml`: `Resource.select_next_folder_to_fetch`
- `src/dbCache.ml`: `Resource.select_next_folder_to_fetch`
- `src/drive.ml`: `read_dir`
