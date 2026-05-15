# Extract Drive Root Resolution

## Goal

Move root-folder id resolution and synthetic well-known resource handling out
of `src/drive.ml` into a focused, functorized `DriveRootResolution` module.

The extraction target is:

- root and well-known path constants currently owned by `Drive`
- `is_lost_and_found_root`
- `is_shared_with_me_root`
- `create_root_resource`
- `create_well_known_resource`
- `get_root_folder_id_from_server`
- `get_root_folder_id`
- `get_root_folder_id_from_context`
- `get_well_known_resource`

The production behavior should stay unchanged. `src/drive.ml` should keep thin
wrappers for existing helper names because `DriveResourceResolver`,
`DriveResourceById`, `DriveViews`, `DriveDirectoryReads`, and `DriveMutations`
already consume root behavior through `Drive` ports.

## Current Problem

Root resolution is still tightly coupled to `Drive` production state and Drive
API adapters. The current block mixes several behaviors:

- device-scope root-folder discovery and creation
- default `"root"` lookup for normal Drive scope
- team-drive root id selection
- configured absolute root-folder traversal by parent/name lookup
- configured relative root-folder id passthrough
- `Context.root_folder_id` memoization
- synthetic resource construction for `/`, trash root, `/lost+found`, and
  `/.shared`
- first-access insertion of synthetic rows into the cache
- path validation for well-known resource requests

Those branches are config-sensitive and currently hard to test without global
`Context`, cache database access, OAuth request execution, and real Drive API
request shapes.

This extraction should isolate the policy while keeping Drive API calls,
context storage, cache insertion, and retry execution behind explicit ports.

## Proposed Shape

Add:

- `src/driveRootResolution.ml`
- `src/driveRootResolution.mli`
- `test/testDriveRootResolution.ml`

Expose:

```ocaml
module File = GapiDriveV3Model.File

val root_directory : string
val default_root_folder_id : string
val trash_directory : string
val trash_directory_name_length : int
val trash_directory_base_path : string
val lost_and_found_directory : string
val shared_with_me_directory : string
val device_scope : string
val device_root_folder : string

val is_lost_and_found_root : string -> bool -> Config.t -> bool
val is_shared_with_me_root : string -> bool -> Config.t -> bool

type runtime = {
  cache : CacheData.t;
  config : Config.t;
  root_folder_id : string option;
}

module type PORTS = sig
  val folder_mime_type : string
  val create_resource : string -> CacheData.Resource.t

  val find_file_in_folder :
    parent_folder_id:string ->
    name:string ->
    trashed:bool ->
    File.t option GapiMonad.SessionM.m

  val get_file_by_remote_id : string -> File.t GapiMonad.SessionM.m

  val create_folder : name:string -> File.t GapiMonad.SessionM.m

  val run_request : 'a GapiMonad.SessionM.m -> 'a
  val set_context_root_folder_id : string -> unit

  val lookup_resource :
    CacheData.t -> string -> bool -> CacheData.Resource.t option

  val insert_resource :
    CacheData.t ->
    label:string ->
    CacheData.Resource.t ->
    CacheData.Resource.t
end

module Make (P : PORTS) : sig
  val create_root_resource : string -> bool -> CacheData.Resource.t
  val create_well_known_resource : string -> CacheData.Resource.t
  val get_root_folder_id_from_server : Config.t -> string GapiMonad.SessionM.m
  val get_root_folder_id : Config.t -> string GapiMonad.SessionM.m
  val get_root_folder_id_from_context : runtime -> string
  val get_well_known_resource :
    runtime -> string -> bool -> CacheData.Resource.t
end
```

The final implementation can adjust names slightly, but it should preserve the
same responsibilities:

- `DriveRootResolution` owns the branch policy and synthetic resource shape.
- Production ports own Drive API request construction, retry wrappers, global
  context updates, cache lookup/insert logging, and resource creation clock.
- `Drive` keeps compatibility wrappers with the current helper names.

## Extracted Behavior

### Pure Predicates And Constants

Move the constants and predicates together so callers use one source of truth:

- `root_directory = "/"`
- `default_root_folder_id = "root"`
- `trash_directory = "/.Trash"`
- `trash_directory_name_length = String.length trash_directory`
- `trash_directory_base_path = "/.Trash/"`
- `lost_and_found_directory = "/lost+found"`
- `shared_with_me_directory = "/.shared"`
- `device_scope = "https://www.googleapis.com/auth/drive.file"`
- `device_root_folder = "gdfuse"`

Preserve the current predicate behavior:

- `is_lost_and_found_root path trashed config` is false when `trashed = true`
  or `config.lost_and_found = false`; otherwise it matches `/lost+found`
- `is_shared_with_me_root path trashed config` is false when `trashed = true`;
  otherwise it matches `/.shared`

Leave broader `is_lost_and_found`, `is_shared_with_me`, and
`get_path_in_cache` in `Drive` for this pass unless compilation shows a very
small constant-alias adjustment is useful. Those helpers are path-normalization
and namespace checks rather than root-resource creation policy.

### Synthetic Resource Construction

`create_root_resource root_folder_id trashed` should preserve the current root
row shape:

- starts from `P.create_resource root_directory`
- `remote_id = Some root_folder_id`
- `mime_type = Some P.folder_mime_type`
- `size = Some 0L`
- `parent_path = ""`
- `trashed = Some trashed`

`create_well_known_resource path` should preserve the current synthetic row
shape:

- starts from `P.create_resource path`
- `remote_id = Some ""`
- `mime_type = Some P.folder_mime_type`
- `size = Some 0L`
- `parent_path = ""`
- `trashed = Some false`

Do not change the synthetic remote id for lost+found or shared-with-me during
this extraction.

### Server Root Resolution

`get_root_folder_id_from_server config` should preserve the current flow.

For device scope:

- search for `device_root_folder` under `default_root_folder_id`
- if found, return that file id
- if missing, create a folder named `device_root_folder`
- return the created folder id

For non-device scope:

- fetch `default_root_folder_id` by remote id
- return the fetched file id

The Drive API request shape should stay in production ports:

- `P.find_file_in_folder` should wrap the current `get_file_from_server`
  helper, including retry, list request shape, and logging
- `P.get_file_by_remote_id` should wrap the current `FilesResource.get`
  request for `"root"`
- `P.create_folder` should wrap the current `FilesResource.create` request
  with `enforceSingleParent` and `supportsAllDrives`

### Configured Root Folder Resolution

`get_root_folder_id config` should preserve the current config rules:

- `default_root_id` is `default_root_folder_id` when `config.team_drive_id = ""`
- otherwise `default_root_id` is `config.team_drive_id`
- `config.root_folder = ""` resolves to `default_root_id`
- absolute `config.root_folder` values are traversed segment by segment from
  `default_root_id`
- relative `config.root_folder` values are treated as an already-known remote id
- if the resolved id is `default_root_folder_id`, call
  `get_root_folder_id_from_server config`
- otherwise return the resolved id

Absolute path traversal should preserve the current behavior:

- strip the leading `/`
- split each step on `Filename.dir_sep`
- query each segment by parent id and name with `trashed = false`
- raise `Failure "Invalid root folder in configuration"` when any segment is
  missing

### Context Memoization

`get_root_folder_id_from_context runtime` should preserve the synchronous
adapter behavior:

- if `runtime.root_folder_id = Some id`, return it without running a request
- if `None`, run `get_root_folder_id runtime.config` through `P.run_request`
- store the result with `P.set_context_root_folder_id`
- return the resolved id

The production `Drive` wrapper should build the runtime from `Context.get_ctx`
and should keep the current no-argument helper shape:

```ocaml
let get_root_folder_id_from_context () =
  RootResolutionOps.get_root_folder_id_from_context
    (drive_root_resolution_runtime ())
```

### Well-Known Resource Lookup

`get_well_known_resource runtime path trashed` should preserve the current
ordering and side effects:

- resolve `root_folder_id` through `get_root_folder_id_from_context runtime`
  before checking the cache
- look up `(path, trashed)` through `P.lookup_resource runtime.cache`
- return the cached row on a hit
- on a miss, construct the appropriate synthetic row:
  - `path = root_directory`: root row with the resolved root folder id and the
    supplied `trashed` flag
  - lost+found root: synthetic lost+found row
  - shared-with-me root: synthetic shared-with-me row
- reject any other path with the same `invalid_arg` message shape
- insert the synthetic row through `P.insert_resource runtime.cache ~label`
- return the inserted row

The production insert port should preserve the current "Saving root/lost+found/
shared with me resource to db" logging.

## Production Wiring

In `src/drive.ml`, alias constants to `DriveRootResolution`:

```ocaml
let root_directory = DriveRootResolution.root_directory
let default_root_folder_id = DriveRootResolution.default_root_folder_id
let trash_directory = DriveRootResolution.trash_directory
let trash_directory_name_length =
  DriveRootResolution.trash_directory_name_length
let trash_directory_base_path = DriveRootResolution.trash_directory_base_path
let lost_and_found_directory = DriveRootResolution.lost_and_found_directory
let shared_with_me_directory = DriveRootResolution.shared_with_me_directory
let device_scope = DriveRootResolution.device_scope
let device_root_folder = DriveRootResolution.device_root_folder
```

Delegate the root predicates:

```ocaml
let is_lost_and_found_root = DriveRootResolution.is_lost_and_found_root
let is_shared_with_me_root = DriveRootResolution.is_shared_with_me_root
```

Add root-resolution ports near the current root helper block:

```ocaml
module DriveRootResolutionPorts = struct
  let folder_mime_type = folder_mime_type
  let create_resource = create_resource

  let find_file_in_folder ~parent_folder_id ~name ~trashed =
    get_file_from_server parent_folder_id name trashed

  let get_file_by_remote_id remote_id =
    with_retry_default
      (FilesResource.get ~supportsAllDrives:true ~std_params:file_std_params
         ~fileId:remote_id)

  let create_folder ~name =
    let file = { File.empty with File.name = name; mimeType = folder_mime_type } in
    with_retry_default
      (FilesResource.create ~enforceSingleParent:true ~supportsAllDrives:true
         ~std_params:file_std_params file)

  let run_request request = do_request request |> fst

  let set_context_root_folder_id root_folder_id =
    Context.update_ctx (Context.root_folder_id ^= Some root_folder_id)

  let lookup_resource cache path trashed =
    ignore cache;
    lookup_resource path trashed

  let insert_resource cache ~label resource =
    Utils.log_with_header "BEGIN: Saving %s resource to db\n%!" label;
    let inserted = Cache.Resource.insert_resource cache resource in
    Utils.log_with_header "END: Saving %s resource to db (id=%Ld)\n%!" label
      inserted.CacheData.Resource.id;
    inserted
end
```

The label should be selected by `DriveRootResolution` alongside the synthetic
resource so production logging remains descriptive without duplicating the
branch policy in `Drive`.

Instantiate:

```ocaml
module RootResolutionOps =
  DriveRootResolution.Make (DriveRootResolutionPorts)
```

Add:

```ocaml
let drive_root_resolution_runtime () =
  let context = Context.get_ctx () in
  {
    DriveRootResolution.cache = context.Context.cache;
    config = context |. Context.config_lens;
    root_folder_id = context.Context.root_folder_id;
  }
```

Keep existing helper names as wrappers:

```ocaml
let create_root_resource root_folder_id trashed =
  RootResolutionOps.create_root_resource root_folder_id trashed

let create_well_known_resource path =
  RootResolutionOps.create_well_known_resource path

let get_root_folder_id_from_server config =
  RootResolutionOps.get_root_folder_id_from_server config

let get_root_folder_id config =
  RootResolutionOps.get_root_folder_id config

let get_root_folder_id_from_context () =
  RootResolutionOps.get_root_folder_id_from_context
    (drive_root_resolution_runtime ())

let get_well_known_resource path trashed =
  RootResolutionOps.get_well_known_resource
    (drive_root_resolution_runtime ()) path trashed
```

## Implementation Steps

1. Create `driveRootResolution.mli` with constants, pure predicates, `runtime`,
   `PORTS`, and functor signatures.
2. Create `driveRootResolution.ml`.
3. Move root and well-known constants into the new module and make `Drive`
   alias them.
4. Move `is_lost_and_found_root` and `is_shared_with_me_root`.
5. Move synthetic resource construction into the new module behind
   `folder_mime_type` and `create_resource` ports.
6. Move `get_root_folder_id_from_server` into the new module behind
   file lookup, root get, and folder-create ports.
7. Move configured root-folder resolution into the new module.
8. Move context memoization into the new module behind `run_request` and
   `set_context_root_folder_id` ports.
9. Move well-known resource cache lookup/insert policy into the new module.
10. Wire the module into `src/drive.ml` with compatibility wrappers.
11. Keep `DriveResourceResolverPorts` and `DriveResourceByIdPorts` call shapes
    unchanged.
12. Add `test/testDriveRootResolution.ml`.
13. Register the suite in `test/testSuite.ml`.
14. Run `ocamlformat` only on touched OCaml files.
15. Run `dune build @install` and `dune runtest` sequentially.

## Unit Test Plan

Use fake ports with trace lists and a synthetic `GapiMonad.SessionM` runner.

Cover pure predicates:

- lost+found root enabled and not trashed
- lost+found disabled
- lost+found in trash view
- shared-with-me root enabled by path and not trashed
- shared-with-me in trash view

Cover synthetic resources:

- root resource fields for normal root and trash root
- lost+found synthetic row fields
- shared-with-me synthetic row fields

Cover server root resolution:

- non-device scope fetches `"root"` by remote id
- device scope reuses an existing `gdfuse` folder
- device scope creates `gdfuse` when it is missing
- folder creation uses the configured folder name and folder MIME type
- Drive request exceptions propagate

Cover configured root resolution:

- empty root folder with no team drive resolves through default root server
  lookup
- empty root folder with team drive returns the team drive id without server
  lookup
- relative root folder returns the configured id directly
- absolute root folder traverses each path segment from the default root id
- absolute root folder under a team drive starts from the team drive id
- missing path segment raises `Failure "Invalid root folder in configuration"`

Cover context memoization:

- cached context root id returns without running a request
- missing context root id runs the request once and stores the result

Cover well-known resources:

- root lookup resolves the root folder id before cache lookup
- cache hit returns the cached synthetic resource without inserting
- root cache miss inserts a root row with the current `trashed` flag
- lost+found cache miss inserts the synthetic lost+found row
- shared-with-me cache miss inserts the synthetic shared-with-me row
- invalid well-known path raises `Invalid_argument`

Existing tests for `DriveResourceResolver`, `DriveResourceById`,
`DriveViews`, `DriveDirectoryReads`, and `DriveMutations` should continue to
pass through the `Drive` wrappers.

## Acceptance Criteria

- `src/drive.ml` no longer contains the root-folder resolution and
  well-known-resource policy bodies listed in this plan.
- Existing `Drive` helper names and call shapes remain available.
- `DriveResourceResolver` and `DriveResourceById` continue to receive root
  behavior through their existing ports.
- Focused unit tests cover device scope, team drive, configured root paths,
  context memoization, synthetic rows, and well-known cache insertion without
  real `Context`, cache files, OAuth, or Drive API requests.
- `dune build @install` and `dune runtest` pass.

## Agent Docs Follow-Up

After implementation, update agent docs to describe the current state directly:

- `docs/agent-docs/architecture.md`
- `docs/agent-docs/repo-map.md`
- `docs/agent-docs/drive-get-resource.md`
- `docs/agent-docs/drive-read-dir.md`
- `docs/agent-docs/drive-init-filesystem.md`

Avoid before/after language. The docs should describe `DriveRootResolution` as
the current implementation boundary for configured root-folder id resolution and
synthetic well-known resource rows.
