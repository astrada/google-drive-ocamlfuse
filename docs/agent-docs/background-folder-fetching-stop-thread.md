# `BackgroundFolderFetching.stop_folder_fetching_thread`

## Purpose

`BackgroundFolderFetching.stop_folder_fetching_thread` is the stop-request
helper for background folder prefetching.

Its job is deliberately small:

- set the subsystem's stop flag to `true`

That is all.

It does not join the thread, drain work by itself, or modify the stored thread
handle in `Context`. It only asks the already-running poll loop to exit on its
next stop check.

## Public Signature

`stop_folder_fetching_thread` is part of
`src/backgroundFolderFetching.mli`:

```ocaml
val stop_folder_fetching_thread : unit -> unit
```

It takes no arguments and returns `unit`. The helper relies entirely on the
previously installed `ConcurrentBackgroundFolderFetching` runtime state.

## Entire Implementation

```ocaml
let stop_folder_fetching_thread () =
  ConcurrentBackgroundFolderFetching.update (fun b ->
      b |> stop_folder_fetching ^= true)
```

That is the whole helper.

## High-Level Flow

At a high level, the helper does this:

1. load the current background-folder-fetch runtime record
2. set `stop_folder_fetching = true`
3. write the updated record back
4. return immediately

So this helper is synchronous as a state update, but asynchronous as a thread
shutdown effect.

## The Only Side Effect: Flip The Stop Flag

The helper changes exactly one field:

```ocaml
stop_folder_fetching
```

The runtime record was created earlier by:

```ocaml
start_folder_fetching_thread cache read_dir
```

So the stop request is simply: mutate the existing runtime record so the poll
loop sees `stop_folder_fetching = true`.

## Precondition: Startup Already Happened

Because the helper uses:

```ocaml
ConcurrentBackgroundFolderFetching.update ...
```

it assumes the runtime state already exists.

That means `start_folder_fetching_thread` must have run earlier in the same
process. This helper is not a safe "maybe-stop-if-running" probe.

## What Happens Next

The flag flipped here is consumed by the polling loop:

```ocaml
let check () =
  let d = ConcurrentBackgroundFolderFetching.get () in
  if d.stop_folder_fetching then raise Exit
```

So the running thread does not stop immediately. It exits on the next loop
iteration after that check observes the updated flag.

Because the loop sleeps for `0.5` seconds between checks, stop latency is
bounded by that polling cadence plus any `read_dir` call already in progress.

## Not An Immediate Stop

This helper does not interrupt the background thread in the middle of:

- `Thread.delay 0.5`
- `fetch_next_folder cache`
- the injected `read_dir` callback

So after this function returns:

- the background thread may still be sleeping
- one prefetch callback may still be running
- `Thread.join folder_fetching_thread` may still need to wait

That is expected. Immediate termination is not the contract.

## Relationship To `GdfuseFlow.shutdown`

The production shutdown caller is:

```ocaml
BackgroundFolderFetching.stop_folder_fetching_thread ();
Thread.join folder_fetching_thread
```

So the shutdown sequence is intentionally split:

1. request stop here
2. let the polling loop observe the flag and exit
3. wait for thread completion in `GdfuseFlow.shutdown`

This is why the helper must stay small. The wait belongs to the owner of the
thread handle.

## What It Does Not Do To `Context`

The helper does not modify:

- `Context.folder_fetching_thread`

That thread handle remains stored in `Context` until higher-level shutdown code
joins the thread.

## Weak Idempotency After Startup

After startup, repeated calls are effectively harmless at this layer because
the helper just writes:

- `stop_folder_fetching = true`

again.

But it still depends on startup having happened first, and it does not verify
whether the thread has already exited.

## What `BackgroundFolderFetching.stop_folder_fetching_thread` Does Not Do

`BackgroundFolderFetching.stop_folder_fetching_thread` does not:

- join the background thread
- clear `ConcurrentBackgroundFolderFetching`
- clear `Context.folder_fetching_thread`
- cancel an in-flight `read_dir` call
- select or process any folder itself

It only sets the stop flag that the polling thread will observe later.

## Related Docs

- `docs/agent-docs/background-folder-fetching-start-thread.md`
- `docs/agent-docs/drive-init-filesystem.md`
- `docs/agent-docs/drive-read-dir.md`
- `docs/agent-docs/architecture.md`

## Source Pointers

- `src/backgroundFolderFetching.ml`: `stop_folder_fetching_thread`
- `src/backgroundFolderFetching.ml`: `folder_fetch`
- `src/backgroundFolderFetching.ml`: `start_folder_fetching_thread`
- `src/gdfuseFlow.ml`: `stop_folder_fetching_thread`
