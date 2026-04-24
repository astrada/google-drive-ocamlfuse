# Extract Drive Fopen

## Goal

Move the file-open access-validation policy out of `src/drive.ml` into a small
testable module that follows the current functorized runtime pattern used by
`DriveViews`, `DriveDirectoryReads`, `DriveReads`, `DriveFileMutations`,
`DriveMetadataMutations`, `DriveUploadDispatch`, `DriveMutations`, and
`DriveXattrs`.

The extraction target is:

- `Drive.fopen`
- the per-resource read-only predicate used by `fopen`

The production behavior should stay unchanged. `src/drive.ml` should retain a
thin wrapper that builds the runtime, calls the extracted operation, runs the
session through `do_request`, and returns `None`.

## Current Problem

`Drive.fopen` still contains access-validation policy directly in
`src/drive.ml`. The function is small, but it is the front door for write
permission enforcement before later `write` and `truncate` calls run.

The important behavior is currently hard to unit test without `Context` and the
production resource lookup path:

- visible path normalization
- trash-path handling
- read-only request detection from Unix open flags
- filesystem-wide read-only rejection
- resource lookup for existence validation
- per-resource write rejection
- always returning `None` after successful validation

The per-resource read-only predicate is also policy-heavy:

- `can_edit = Some false` denies write-capable opens
- Google documents are denied unless editable documents are enabled and the
  selected format is not `desktop`
- large files are denied when `large_file_read_only = true`

## Proposed Shape

Add:

- `src/driveOpens.ml`
- `src/driveOpens.mli`
- `test/testDriveOpens.ml`

Expose a functor:

```ocaml
type runtime = { config : Config.t }

module type PORTS = sig
  val get_path_in_cache : string -> Config.t -> string * bool
  val get_resource : string -> bool -> CacheData.Resource.t GapiMonad.SessionM.m
end

module Make (P : PORTS) : sig
  val fopen :
    runtime -> string -> Unix.open_flag list -> unit GapiMonad.SessionM.m
end
```

Also expose the pure access predicate:

```ocaml
val is_file_read_only : Config.t -> CacheData.Resource.t -> bool
```

`DriveOpens` should own the open-time access policy. Production ports should
own only path normalization and resource resolution.

## Production Wiring

In `src/drive.ml`, keep the existing helper names where other production ports
depend on them, but delegate the per-resource predicate to `DriveOpens`:

```ocaml
let is_filesystem_read_only () =
  Context.get_ctx () |. Context.config_lens |. Config.read_only

let is_file_read_only resource =
  let config = Context.get_ctx () |. Context.config_lens in
  DriveOpens.is_file_read_only config resource
```

Add a ports module near the existing read/view wiring:

```ocaml
module DriveOpenPorts = struct
  let get_path_in_cache = get_path_in_cache
  let get_resource = get_resource
end

module OpenOps = DriveOpens.Make (DriveOpenPorts)

let drive_open_runtime () =
  let context = Context.get_ctx () in
  { DriveOpens.config = context |. Context.config_lens }
```

Replace `Drive.fopen` with:

```ocaml
let fopen path flags =
  do_request (OpenOps.fopen (drive_open_runtime ()) path flags) |> ignore;
  None
```

The extracted `fopen` should preserve the current synchronous global read-only
rejection behavior by checking `runtime.config.Config.read_only` while building
the request. That keeps write-capable opens on read-only mounts from invoking
`do_request` or `get_resource`.

## Implementation Steps

1. Create `driveOpens.mli` with `runtime`, `is_file_read_only`, `PORTS`, and
   `fopen`.
2. Create `driveOpens.ml`.
3. Move the per-resource read-only predicate into `DriveOpens`.
4. Keep the current read-only request test exactly as `List.mem Unix.O_RDONLY
   flags`.
5. In `DriveOpens.fopen`, normalize the visible path before building the
   validation request.
6. If the request is write-capable and `runtime.config.Config.read_only = true`,
   raise `Permission_denied` before returning a session request.
7. Otherwise, resolve the resource with `P.get_resource path_in_cache trashed`.
8. If the request is write-capable and `is_file_read_only runtime.config
   resource = true`, raise `Permission_denied` through the session monad.
9. Return `SessionM.return ()` on success.
10. Wire `DriveOpens.Make` into `src/drive.ml`.
11. Replace the old `Drive.fopen` body with the thin wrapper.
12. Keep `Drive.opendir` unchanged; it already delegates to `DriveViews`.
13. Add `test/testDriveOpens.ml`.
14. Register the suite in `test/testSuite.ml`.
15. Run `ocamlformat` on touched OCaml files.
16. Run `dune build @install` and `dune runtest`.

## Unit Test Plan

Use fake ports, following the style in `test/testDriveReads.ml` and
`test/testDriveFileMutations.ml`.

Cover path setup:

- visible paths are normalized with the runtime config
- trash paths pass the normalized cache path and `trashed = true` to
  `get_resource`

Cover request-mode behavior:

- `Unix.O_RDONLY` opens are treated as read-only
- `Unix.O_WRONLY` and `Unix.O_RDWR` opens are treated as write-capable
- preserve the current `List.mem Unix.O_RDONLY flags` behavior if a strange
  mixed flag list includes `Unix.O_RDONLY`

Cover filesystem-wide read-only behavior:

- write-capable opens with `config.read_only = true` raise
  `Permission_denied`
- that rejection happens before `get_resource`
- read-only opens with `config.read_only = true` still resolve the resource

Cover per-resource write behavior:

- write-capable opens for `can_edit = Some false` raise `Permission_denied`
- write-capable opens for ordinary editable files succeed
- read-only opens for read-only resources succeed after existence validation
- Google documents reject write-capable opens when `editable_docs = false`
- Google documents reject write-capable opens when the selected format is
  `desktop`
- Google documents allow write-capable opens when `editable_docs = true` and
  the selected format is editable
- large files reject write-capable opens when `large_file_read_only = true`

Cover resource lookup behavior:

- successful opens perform exactly one resource lookup
- missing resources propagate the exception from `get_resource`
- successful `fopen` returns unit from the core and `None` from the production
  wrapper

## Acceptance Criteria

- `src/drive.ml` no longer contains the `fopen` access-validation branch logic.
- The public `Drive.fopen` signature and return value remain unchanged.
- The current read-only request detection semantics remain unchanged.
- Filesystem-wide read-only rejection still happens before resource lookup.
- Per-resource editability policy is unit tested without real `Context`, Drive
  API requests, or cache access.
- `dune build @install` and `dune runtest` pass.

## Agent Docs Follow-Up

After implementation, update the agent docs to describe the current state
directly:

- `docs/agent-docs/drive-fopen.md`
- `docs/agent-docs/architecture.md`
- `docs/agent-docs/repo-map.md`
- `docs/agent-docs/README.md`

Avoid before/after language. The docs should describe `DriveOpens` as the
current implementation boundary for file-open access validation.
