# Extract Drive Path Namespace

## Goal

Move Drive path namespace decisions out of `src/drive.ml` and into a small,
pure `DrivePathNamespace` module.

The extraction target is:

- path namespace constants for root, trash, lost+found, and shared-with-me
- `is_in_trash_directory`
- `is_lost_and_found_root`
- `is_lost_and_found`
- `is_shared_with_me_root`
- `is_shared_with_me`
- `get_path_in_cache`

The production behavior should stay unchanged. `src/drive.ml` should keep thin
wrappers for the existing helper names, and `DriveRootResolution` should keep
compatibility aliases for the root predicates and namespace constants it
already exposes.

## Current Problem

Path namespace behavior is still defined in `Drive`, while many extracted
modules consume it through production ports:

- `DriveDirectoryReads`
- `DriveFileMutations`
- `DriveMutations`
- `DriveOpens`
- `DriveReads`
- `DriveRemoteUpdates`
- `DriveUploadDispatch`
- `DriveViews`
- `DriveXattrs`

The helper block controls how FUSE-visible paths map to cache paths and whether
lookups operate in the normal or trash view. It also defines the prefix checks
used to guard lost+found and shared-with-me behavior. Those branches are pure,
configuration-sensitive, and currently tested mostly through broader Drive
entrypoints.

This extraction should give trash, lost+found, and shared-with-me path behavior
direct unit coverage without involving `Context`, cache state, OAuth, Drive API
requests, or FUSE adapters.

## Proposed Shape

Add:

- `src/drivePathNamespace.ml`
- `src/drivePathNamespace.mli`
- `test/testDrivePathNamespace.ml`

Expose:

```ocaml
val root_directory : string
val trash_directory : string
val trash_directory_name_length : int
val trash_directory_base_path : string
val lost_and_found_directory : string
val shared_with_me_directory : string

val is_in_trash_directory : string -> Config.t -> bool
val is_lost_and_found_root : string -> bool -> Config.t -> bool
val is_lost_and_found : string -> bool -> Config.t -> bool
val is_shared_with_me_root : string -> bool -> Config.t -> bool
val is_shared_with_me : string -> bool -> Config.t -> bool
val get_path_in_cache : string -> Config.t -> string * bool
```

No functor or ports are needed. The extracted behavior is pure and depends only
on the supplied `Config.t` and path string.

`DrivePathNamespace` should own the namespace constants:

- `root_directory = "/"`
- `trash_directory = "/.Trash"`
- `trash_directory_name_length = String.length trash_directory`
- `trash_directory_base_path = "/.Trash/"`
- `lost_and_found_directory = "/lost+found"`
- `shared_with_me_directory = "/.shared"`

`DriveRootResolution` should keep the same public constants by aliasing
`DrivePathNamespace`. It should continue to own root id and device-scope
constants:

- `default_root_folder_id`
- `device_scope`
- `device_root_folder`

## Extracted Behavior

### Trash Namespace

`is_in_trash_directory path config` should preserve the current behavior:

- `path = trash_directory` returns `false`
- `config.disable_trash = true` returns `false`
- otherwise it returns whether `path` starts with
  `trash_directory_base_path`

`get_path_in_cache path config` should preserve the current mapping:

- `"/"` maps to `("/", false)`
- `"/.Trash"` maps to `("/", true)` when trash is enabled
- `"/.Trash/<path>"` maps to `("/<path>", true)` when trash is enabled
- when trash is disabled, trash paths remain unchanged with `trashed = false`
- ordinary paths remain unchanged with `trashed = false`

Do not broaden or tighten the trash prefix during this extraction.
`"/.Trashcan"` and similar paths should continue to be ordinary paths because
the trash directory prefix includes the trailing slash.

### Lost+Found Namespace

`is_lost_and_found_root path trashed config` should preserve the exact-root
check:

- false when `trashed = true`
- false when `config.lost_and_found = false`
- true only when `path = lost_and_found_directory`

`is_lost_and_found path trashed config` should preserve the prefix check:

- false when `trashed = true`
- false when `config.lost_and_found = false`
- otherwise true when `path` starts with `lost_and_found_directory`

Do not add path-boundary normalization in this pass. The goal is to move the
current policy behind a smaller test boundary.

### Shared-With-Me Namespace

`is_shared_with_me_root path trashed config` should preserve the exact-root
check:

- false when `trashed = true`
- true only when `path = shared_with_me_directory`
- the `Config.t` argument remains unused

`is_shared_with_me path trashed config` should preserve the prefix check:

- false when `trashed = true`
- otherwise true when `path` starts with `shared_with_me_directory`
- the `Config.t` argument remains unused

Do not add config gating for shared-with-me behavior during this extraction.

## Production Wiring

In `src/driveRootResolution.ml`, replace the namespace constant and predicate
bodies with aliases:

```ocaml
let root_directory = DrivePathNamespace.root_directory
let trash_directory = DrivePathNamespace.trash_directory
let trash_directory_name_length =
  DrivePathNamespace.trash_directory_name_length
let trash_directory_base_path = DrivePathNamespace.trash_directory_base_path
let lost_and_found_directory = DrivePathNamespace.lost_and_found_directory
let shared_with_me_directory = DrivePathNamespace.shared_with_me_directory

let is_lost_and_found_root = DrivePathNamespace.is_lost_and_found_root
let is_shared_with_me_root = DrivePathNamespace.is_shared_with_me_root
```

Keep `src/driveRootResolution.mli` compatible so existing tests and production
callers do not need to switch modules during this pass.

In `src/drive.ml`, alias namespace constants and helpers to
`DrivePathNamespace`:

```ocaml
let root_directory = DrivePathNamespace.root_directory
let trash_directory = DrivePathNamespace.trash_directory
let trash_directory_name_length =
  DrivePathNamespace.trash_directory_name_length
let trash_directory_base_path = DrivePathNamespace.trash_directory_base_path
let lost_and_found_directory = DrivePathNamespace.lost_and_found_directory
let shared_with_me_directory = DrivePathNamespace.shared_with_me_directory

let is_in_trash_directory = DrivePathNamespace.is_in_trash_directory
let is_lost_and_found_root = DrivePathNamespace.is_lost_and_found_root
let is_lost_and_found = DrivePathNamespace.is_lost_and_found
let is_shared_with_me_root = DrivePathNamespace.is_shared_with_me_root
let is_shared_with_me = DrivePathNamespace.is_shared_with_me
let get_path_in_cache = DrivePathNamespace.get_path_in_cache
```

Leave `default_root_folder_id`, `device_scope`, and `device_root_folder`
aliased to `DriveRootResolution`.

Keep `src/drive.mli` stable for this pass. It already exposes
`is_lost_and_found` and `get_path_in_cache`; the newly extracted helpers can be
used internally through the `Drive` wrappers and directly by unit tests through
`DrivePathNamespace`.

## Implementation Steps

1. Create `drivePathNamespace.mli` with namespace constants and pure helper
   signatures.
2. Create `drivePathNamespace.ml` with the current path behavior from
   `src/drive.ml`.
3. Move root, trash, lost+found, and shared-with-me namespace constants into
   `DrivePathNamespace`.
4. Make `DriveRootResolution` re-export those namespace constants and root
   predicates from `DrivePathNamespace`.
5. Replace the path helper bodies in `Drive` with aliases to
   `DrivePathNamespace`.
6. Keep existing production port signatures unchanged.
7. Add `test/testDrivePathNamespace.ml`.
8. Register the suite in `test/testSuite.ml`.
9. Run `tools/format_ocaml` or `ocamlformat` on touched OCaml files.
10. Run `dune build @install` and `dune runtest` sequentially.

## Unit Test Plan

Use table-driven tests over `Config.default` variants.

Cover constants:

- root, trash, lost+found, and shared-with-me constants match the current
  visible paths
- trash base path and name length are derived from `trash_directory`

Cover trash mapping:

- root maps to `("/", false)`
- ordinary paths remain unchanged
- trash root maps to the root cache path with `trashed = true`
- nested trash paths strip only the trash namespace prefix
- trash root and nested trash paths stay ordinary paths when
  `disable_trash = true`
- trash-like prefixes that do not match `"/.Trash/"` stay ordinary paths

Cover lost+found predicates:

- root predicate requires `lost_and_found = true`
- root predicate rejects trashed views
- root predicate is exact-path only
- prefix predicate accepts nested lost+found paths when enabled
- prefix predicate rejects trashed views and disabled config

Cover shared-with-me predicates:

- root predicate is exact-path only
- root predicate rejects trashed views
- prefix predicate accepts nested shared-with-me paths
- prefix predicate rejects trashed views
- config does not change shared-with-me predicate behavior

Existing tests for root resolution, directory reads, views, mutations, reads,
opens, remote updates, upload dispatch, file mutations, and xattrs should
continue to pass through the `Drive` wrappers.

## Acceptance Criteria

- `src/drive.ml` no longer contains the path namespace helper bodies listed in
  this plan.
- `DrivePathNamespace` owns pure namespace constants and path predicates.
- `DriveRootResolution` keeps compatibility aliases for namespace constants and
  root predicates.
- Existing `Drive` helper names and public `Drive.mli` call shapes remain
  available.
- Focused unit tests cover trash mapping, lost+found predicates, and
  shared-with-me predicates without production context or cache setup.
- `dune build @install` and `dune runtest` pass.

## Agent Docs Follow-Up

After implementation, update agent docs to describe the current state directly:

- `docs/agent-docs/architecture.md`
- `docs/agent-docs/repo-map.md`
- `docs/agent-docs/workflows.md`
- `docs/agent-docs/drive-read-dir.md`
- `docs/agent-docs/drive-get-resource.md`
- `docs/agent-docs/drive-rename.md`
- `docs/agent-docs/drive-create-remote-resource.md`

Avoid before/after language. The docs should describe `DrivePathNamespace` as
the current implementation boundary for mapping FUSE-visible paths into cache
paths and for recognizing trash, lost+found, and shared-with-me namespaces.
