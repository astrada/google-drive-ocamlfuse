# Extract Drive Resource By Id

## Goal

Move remote-id resource lookup out of `src/drive.ml` into a focused,
functorized `DriveResourceById` module.

The extraction target is:

- `Drive.get_resource_with_id`
- `Drive.get_resource_with_id_from_server`
- remote parent traversal used to reconstruct a cache path
- shared-with-me path reconstruction for remote resources without parents
- the root-folder remote-id special case

The production behavior should stay unchanged. `src/drive.ml` should keep the
Google Drive API request adapter, root-folder lookup adapter, cache lookup
adapter, resource construction wrappers, and production logging/retry helpers.

## Current Problem

Remote-id lookup is currently embedded in `src/drive.ml` next to the ordinary
path resolver, but it serves a narrower read-side workflow.

The important caller is `DriveViews.fetch_link_target`: when a Drive shortcut
has no cached `link_target`, `DriveViews` resolves `target_id` through
`get_resource_with_id`, builds a mountpoint-prefixed target path, and writes the
computed target back to the source row.

The current helper mixes several behaviors that are hard to unit test directly:

- cache-first lookup by `remote_id`
- root-folder id lookup and root-resource short-circuiting
- Drive `FilesResource.get` calls for the target and its parents
- parent-chain traversal until the configured root folder is reached
- shared-with-me path prefixing when a shared target has no parents
- filename cleanup for reconstructed path components
- Drive `File` to `CacheData.Resource.t` mapping for uncached targets

This makes shortcut/readlink behavior sensitive to a helper that cannot be
tested without production `Context`, cache, OAuth request, and Drive API
wiring.

## Proposed Shape

Add:

- `src/driveResourceById.ml`
- `src/driveResourceById.mli`
- `test/testDriveResourceById.ml`

Expose:

```ocaml
module File = GapiDriveV3Model.File

type runtime = { cache : CacheData.t }

module type PORTS = sig
  val root_directory : string
  val shared_with_me_directory : string
  val get_root_folder_id : unit -> string
  val get_well_known_resource : string -> bool -> CacheData.Resource.t

  val select_first_resource_with_remote_id :
    CacheData.t -> string -> CacheData.Resource.t option

  val clean_filename : string -> string
  val create_resource : string -> CacheData.Resource.t

  val update_resource_from_file :
    CacheData.Resource.t -> File.t -> CacheData.Resource.t

  val get_file_by_remote_id : string -> File.t GapiMonad.SessionM.m
end

module Make (P : PORTS) : sig
  val get_resource_with_id_from_server :
    string -> CacheData.Resource.t GapiMonad.SessionM.m

  val get_resource_with_id :
    runtime -> string -> CacheData.Resource.t GapiMonad.SessionM.m
end
```

The extracted module should own the branch policy and path reconstruction.
Production ports should own real cache access, Drive API calls, retry behavior,
root-resource creation, and Drive-file mapping helpers.

## Extracted Behavior

`get_resource_with_id runtime remote_id` should:

- query `P.select_first_resource_with_remote_id runtime.cache remote_id`
- return the cached resource on a hit
- call `get_resource_with_id_from_server remote_id` on a miss

`get_resource_with_id_from_server remote_id` should:

- read `root_folder_id = P.get_root_folder_id ()` before applying the root
  special case, preserving the current ordering
- when `remote_id = root_folder_id`, return
  `P.get_well_known_resource P.root_directory false`
- otherwise fetch the target file by remote id
- reconstruct the full cache path from the fetched file and its parents
- create a new resource for that reconstructed path
- map the fetched Drive file into the resource with
  `P.update_resource_from_file`
- return the mapped resource without inserting it into the cache

Parent traversal should preserve the current rules:

- start with the cleaned target filename as the final path component
- if the current file has no parents and the original target file is shared,
  prefix the path with `P.shared_with_me_directory`
- if the current file has exactly `[ root_folder_id ]` as parents, prefix the
  path with `""` so `String.concat Filename.dir_sep` produces an absolute
  cache path
- otherwise fetch `List.hd file.parents`, clean the parent name, prepend it to
  the path parts, and continue with the original target file's `shared` flag

Do not add new fallback behavior for no-parent, non-shared files or alternate
multi-parent selection in this extraction. The goal is to move the existing
behavior behind a smaller test boundary, not to change remote-id semantics.

## Production Wiring

In `src/drive.ml`, add a ports module near the resource lookup helpers:

```ocaml
module DriveResourceByIdPorts = struct
  let root_directory = root_directory
  let shared_with_me_directory = shared_with_me_directory
  let get_root_folder_id = get_root_folder_id_from_context
  let get_well_known_resource = get_well_known_resource

  let select_first_resource_with_remote_id cache remote_id =
    Cache.Resource.select_first_resource_with_remote_id cache remote_id

  let clean_filename = clean_filename
  let create_resource = create_resource

  let update_resource_from_file resource file =
    update_resource_from_file resource file

  let get_file_by_remote_id remote_id =
    with_retry_default
      (FilesResource.get ~supportsAllDrives:true ~std_params:file_std_params
         ~fileId:remote_id)
end
```

Instantiate:

```ocaml
module ResourceByIdOps = DriveResourceById.Make (DriveResourceByIdPorts)
```

Replace the current helper bodies with thin wrappers:

```ocaml
let get_resource_with_id_from_server remote_id =
  ResourceByIdOps.get_resource_with_id_from_server remote_id

let get_resource_with_id remote_id cache =
  ResourceByIdOps.get_resource_with_id { DriveResourceById.cache = cache }
    remote_id
```

Keep `DriveViews.PORTS.get_resource_with_id` unchanged for this pass. It should
continue to receive the `Drive.get_resource_with_id` wrapper.

Logging can stay in `DriveResourceById` for the moved control flow, following
the existing `DriveResourceResolver` precedent, or stay in the production API
port for the `FilesResource.get` call. Either choice should preserve the useful
remote-id and reconstructed-path log lines.

## Implementation Steps

1. Create `driveResourceById.mli` with the `runtime`, `PORTS`, and functor
   signature.
2. Create `driveResourceById.ml`.
3. Move the cache-first `get_resource_with_id` logic into the new module.
4. Move the root-id short circuit and server fallback path into the new module.
5. Move the recursive parent traversal into the new module with explicit
   dependencies on `P.get_file_by_remote_id`, `P.clean_filename`, and
   `P.shared_with_me_directory`.
6. Keep resource construction and Drive-file mapping behind ports.
7. Wire `DriveResourceById.Make` into `src/drive.ml`.
8. Replace the old `Drive.get_resource_with_id` and
   `Drive.get_resource_with_id_from_server` bodies with wrappers.
9. Keep `DriveViews` signatures unchanged unless compilation exposes a better
   local simplification.
10. Add `test/testDriveResourceById.ml`.
11. Register the suite in `test/testSuite.ml`.
12. Run `ocamlformat` on touched OCaml files.
13. Run `dune build @install` and `dune runtest` sequentially.

## Unit Test Plan

Use fake ports and a synthetic `GapiMonad.SessionM` runner, following the style
in `test/testDriveResourceResolver.ml` and `test/testDriveViews.ml`.

Cover cache behavior:

- cached remote id returns the cached resource
- cached remote id does not call root lookup or server fetch
- cache miss delegates to the server reconstruction path

Cover root behavior:

- a cache miss for the current root folder id returns the well-known root
  resource with `trashed = false`
- root special case does not fetch the remote file

Cover path reconstruction:

- an uncached file directly under root maps to `/name`
- filenames are cleaned for the target and every fetched parent
- nested parents are fetched from child to root and produce the expected
  absolute cache path
- a shared target with no parents maps under `/.shared`
- a shared target with ordinary parents follows the parent chain rather than
  forcing the shared-with-me prefix

Cover resource mapping:

- server fallback creates a resource from the reconstructed path
- server fallback calls `update_resource_from_file` with the target file
- server fallback returns the mapped resource without inserting it into cache

Cover failures and ordering:

- root folder id is read before the root special case is applied
- exceptions from target or parent fetches propagate
- the current no-parent, non-shared failure behavior is preserved

Existing `DriveViews` tests should continue to prove that shortcut target
reconstruction calls the same production wrapper.

## Acceptance Criteria

- `src/drive.ml` no longer contains the remote-id path reconstruction logic.
- `Drive.get_resource_with_id` keeps the same internal call shape for
  `DriveViews`.
- Shortcut target reconstruction through `DriveViews.fetch_link_target` behaves
  unchanged.
- The new unit tests cover cache hits, root special casing, nested parent
  traversal, shared-with-me reconstruction, and fetch failures without real
  `Context`, cache files, OAuth, or Drive API requests.
- `dune build @install` and `dune runtest` pass.

## Agent Docs Follow-Up

After implementation, update the agent docs to describe the current state
directly:

- `docs/agent-docs/drive-read-link.md`
- `docs/agent-docs/drive-get-resource.md`
- `docs/agent-docs/architecture.md`
- `docs/agent-docs/repo-map.md`
- `docs/agent-docs/README.md`

Avoid before/after language. The docs should describe `DriveResourceById` as
the current implementation boundary for remote-id lookup and shortcut target
path reconstruction.
