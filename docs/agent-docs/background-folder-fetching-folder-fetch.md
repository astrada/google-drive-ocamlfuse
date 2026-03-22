# `BackgroundFolderFetching.folder_fetch`

## Purpose

`BackgroundFolderFetching.folder_fetch` is the long-lived polling loop for
background folder prefetching.

It sits between:

- `start_folder_fetching_thread`, which installs runtime state and spawns the
  thread
- `fetch_next_folder`, which selects and refreshes one eligible folder per
  iteration

Its job is deliberately small:

- read the stop flag from `ConcurrentBackgroundFolderFetching`
- sleep for a fixed `0.5` seconds between iterations
- call `fetch_next_folder cache`
- convert the normal stop path into a quiet `unit` return

It does not choose folders itself, and it does not recover from arbitrary
prefetch failures.

## Internal Status And Effective Signature

`folder_fetch` is internal to `src/backgroundFolderFetching.ml`. It is not
exposed in `src/backgroundFolderFetching.mli`.

Its effective shape is:

```ocaml
val folder_fetch : CacheData.t -> unit
```

The only explicit argument is the cache handle. The stop flag and injected
`read_dir` callback are loaded from the shared
`ConcurrentBackgroundFolderFetching` runtime state.

## Entire Implementation

```ocaml
let folder_fetch cache =
  let check () =
    let d = ConcurrentBackgroundFolderFetching.get () in
    if d.stop_folder_fetching then raise Exit
  in
  try
    while true do
      check ();
      Thread.delay 0.5;
      fetch_next_folder cache
    done
  with Exit -> ()
```

That is the whole loop.

## High-Level Flow

At a high level, the helper does this:

1. define a local `check ()` helper that reads the shared stop flag
2. if the flag is `true`, raise `Exit`
3. otherwise enter an infinite loop
4. at the top of each iteration, run `check ()`
5. sleep for `0.5` seconds
6. call `fetch_next_folder cache`
7. repeat until an `Exit` escapes the loop body
8. catch that `Exit` and return `()`

So `folder_fetch` owns cadence and exit behavior, not folder selection.

## Polling Cadence

This loop is time-based, not event-driven.

- every iteration waits `0.5` seconds
- every iteration attempts at most one `fetch_next_folder cache`
- if no folder is eligible, `fetch_next_folder` returns `()` and the next
  iteration simply waits again

That means background prefetching is deliberately low-frequency and cooperative.

## Stop Check Ordering

The precise order inside the loop is:

```ocaml
check ();
Thread.delay 0.5;
fetch_next_folder cache
```

That ordering matters.

- if the stop flag is already `true` when an iteration begins, the thread exits
  before sleeping or fetching again
- if the stop flag flips while the thread is inside `Thread.delay 0.5`, this
  iteration still proceeds to `fetch_next_folder cache`
- if the stop flag flips while `fetch_next_folder` or its injected `read_dir`
  callback is already running, the current work is not interrupted

So stop is cooperative rather than immediate, and a stop request can still
allow one final prefetch attempt before the loop exits.

## Relationship To `stop_folder_fetching_thread`

`stop_folder_fetching_thread` only flips the shared flag:

```ocaml
stop_folder_fetching = true
```

`folder_fetch` is the layer that turns that flag into actual thread exit.

Because the flag is observed only at the top of each iteration, practical stop
latency can include:

- up to the current `Thread.delay 0.5`
- one final `fetch_next_folder cache` call
- any injected `read_dir` callback that final fetch starts

The later `Thread.join` in `GdfuseFlow.shutdown` waits for this loop to finish.

## Relationship To `fetch_next_folder`

`folder_fetch` does not inspect cache contents directly. It delegates the
one-folder step to:

```ocaml
fetch_next_folder cache
```

So the ownership split is:

- `folder_fetch`: polling cadence, stop checks, thread lifetime
- `fetch_next_folder`: cache selection and callback handoff for one folder

This is why the helper remains small even though it owns the long-lived thread.

## Failure Boundary

The only handled exception is:

```ocaml
with Exit -> ()
```

That has two important consequences:

- the intended normal exit path is the local stop check raising `Exit`
- any non-`Exit` exception from `fetch_next_folder` is not caught here and can
  terminate the background thread

In practice, this means callback failures from the injected `read_dir` path are
fatal to the thread unless they raise `Exit`.

## What `BackgroundFolderFetching.folder_fetch` Does Not Do

`BackgroundFolderFetching.folder_fetch` does not:

- start the thread or store its handle in `Context`
- flip the stop flag itself
- choose which folder to prefetch
- retry failed prefetches
- create or manage a worker pool

It only runs the polling loop around the one-folder prefetch step.

## Related Docs

- `docs/agent-docs/background-folder-fetching-start-thread.md`
- `docs/agent-docs/background-folder-fetching-fetch-next-folder.md`
- `docs/agent-docs/background-folder-fetching-stop-thread.md`
- `docs/agent-docs/drive-read-dir.md`
- `docs/agent-docs/drive-init-filesystem.md`
- `docs/agent-docs/architecture.md`

## Source Pointers

- `src/backgroundFolderFetching.ml`: `folder_fetch`
- `src/backgroundFolderFetching.ml`: `fetch_next_folder`
- `src/backgroundFolderFetching.ml`: `start_folder_fetching_thread`
- `src/backgroundFolderFetching.ml`: `stop_folder_fetching_thread`
- `src/gdfuseFlow.ml`: `stop_folder_fetching_thread`
