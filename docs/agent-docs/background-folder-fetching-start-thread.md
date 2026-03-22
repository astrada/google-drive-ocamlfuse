# `BackgroundFolderFetching.start_folder_fetching_thread`

## Purpose

`BackgroundFolderFetching.start_folder_fetching_thread` is the startup boundary
for background folder prefetching.

It owns three things:

- building the small runtime record used by the folder-fetch subsystem
- publishing that record through `ConcurrentBackgroundFolderFetching`
- spawning the long-lived prefetch thread and storing its handle in `Context`

It does not choose which folder to prefetch itself. That begins later in the
poll loop started here.

## Public Signature

`start_folder_fetching_thread` is part of
`src/backgroundFolderFetching.mli`:

```ocaml
val start_folder_fetching_thread : CacheData.t -> (string -> unit) -> unit
```

The parameters are:

- `cache`: cache handle used by the prefetch loop
- `read_dir`: callback used to prefetch one folder path

In production, `Drive.init_filesystem` passes:

```ocaml
fun path -> read_dir path |> ignore
```

so the background thread reuses the normal `Drive.read_dir` path.

## Entire Implementation

```ocaml
let start_folder_fetching_thread cache read_dir =
  let data = { stop_folder_fetching = false; read_dir } in
  ConcurrentBackgroundFolderFetching.set data;
  let thread = Thread.create folder_fetch cache in
  Utils.log_with_header
    "Starting background folder fetching thread (TID=%d)\n%!" (Thread.id thread);
  Context.update_ctx (Context.folder_fetching_thread ^= Some thread)
```

That is the whole helper.

## High-Level Flow

At a high level, startup does this:

1. build a runtime record with `stop_folder_fetching = false`
2. store the supplied `read_dir` callback in that record
3. publish the record through `ConcurrentBackgroundFolderFetching.set`
4. spawn the poll thread with `Thread.create folder_fetch cache`
5. store the resulting thread handle in `Context.folder_fetching_thread`

So this helper installs runtime state and starts the background prefetch thread.

## Installed Runtime State

The installed record is:

```ocaml
{ stop_folder_fetching = false; read_dir }
```

That record is the whole shared state for this subsystem:

- `stop_folder_fetching` is the stop flag later flipped during shutdown
- `read_dir` is the callback invoked for each selected folder path

Unlike the async upload subsystem, there is no worker pool here. The
folder-fetch thread runs the callback itself.

## Why The Callback Is Injected

The helper does not hard-code `Drive.read_dir`.

Instead it stores a plain `(string -> unit)` callback in the concurrent global.
That keeps the ownership boundary clean:

- `BackgroundFolderFetching` owns scheduling and stop-flag polling
- `Drive` owns what it means to refresh one folder path

In production, `Drive.init_filesystem` connects the two by passing its own
`read_dir` wrapper.

## Why `set` Happens Before `Thread.create`

The ordering is deliberate:

```ocaml
ConcurrentBackgroundFolderFetching.set data;
let thread = Thread.create folder_fetch cache in
```

`folder_fetch` and `fetch_next_folder` both read from
`ConcurrentBackgroundFolderFetching` immediately. Installing the runtime record
first avoids a race against uninitialized shared state.

## Thread Entry Point

The created thread runs:

```ocaml
folder_fetch cache
```

This helper therefore begins a long-lived polling thread rather than a one-shot
prefetch.

See `docs/agent-docs/background-folder-fetching-folder-fetch.md` for the exact
loop ordering, cadence, and stop behavior.
See `docs/agent-docs/background-folder-fetching-fetch-next-folder.md` for the
one-folder selection and callback step that the loop runs each iteration.

## Relationship To `Drive.read_dir`

The main production callback eventually runs:

```ocaml
fun path -> read_dir path |> ignore
```

That reuse is important. Background folder fetching is not a separate metadata
refresh implementation; it drives the same `Drive.read_dir` path that a
foreground `readdir` request would use.

See `docs/agent-docs/drive-read-dir.md` for the folder-refresh behavior that
the background thread reuses.

## Context Registration

After starting the thread, the helper stores it in:

```ocaml
Context.folder_fetching_thread
```

That is the shutdown-facing side effect of startup. Higher-level shutdown code
uses this stored handle to request stop and then `Thread.join` the thread.

## Single-Start Assumption

There is no explicit guard against calling this helper twice.

A second call would:

- replace the `ConcurrentBackgroundFolderFetching` runtime state
- create a new background thread
- overwrite `Context.folder_fetching_thread`

So the current design assumes one folder-fetch startup per mounted runtime.

## Relationship To Shutdown

The matching stop path later does:

```ocaml
BackgroundFolderFetching.stop_folder_fetching_thread ();
Thread.join folder_fetching_thread
```

So the lifecycle split is:

- `start_folder_fetching_thread`: install runtime state and start the thread
- `stop_folder_fetching_thread` plus `Thread.join`: request exit and wait for
  the thread to finish

See `src/gdfuseFlow.ml` for the higher-level shutdown call site.
See `docs/agent-docs/background-folder-fetching-folder-fetch.md` for the loop
that actually observes the stop request and exits.
See `docs/agent-docs/background-folder-fetching-stop-thread.md` for the
stop-request half of that lifecycle.

## What `BackgroundFolderFetching.start_folder_fetching_thread` Does Not Do

`BackgroundFolderFetching.start_folder_fetching_thread` does not:

- select folders itself
- run `read_dir` itself
- create a worker pool
- request shutdown
- join the background thread

It only installs the runtime state and starts the polling thread.

## Related Docs

- `docs/agent-docs/background-folder-fetching-folder-fetch.md`
- `docs/agent-docs/background-folder-fetching-fetch-next-folder.md`
- `docs/agent-docs/background-folder-fetching-stop-thread.md`
- `docs/agent-docs/drive-init-filesystem.md`
- `docs/agent-docs/drive-read-dir.md`
- `docs/agent-docs/architecture.md`

## Source Pointers

- `src/backgroundFolderFetching.ml`: `start_folder_fetching_thread`
- `src/backgroundFolderFetching.ml`: `folder_fetch`
- `src/backgroundFolderFetching.ml`: `fetch_next_folder`
- `src/backgroundFolderFetching.ml`: `stop_folder_fetching_thread`
- `src/drive.ml`: `init_filesystem`
- `src/gdfuseFlow.ml`: `stop_folder_fetching_thread`
