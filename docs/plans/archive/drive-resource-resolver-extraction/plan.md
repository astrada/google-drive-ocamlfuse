# Extract `get_resource` / `get_folder_id` Resolution

## Goal

Move the path-to-resource resolution policy out of `src/drive.ml` into a
functorized module that can be unit tested without a live `Context`, cache
database, Google Drive request, or retry loop.

The extraction target is:

- `Drive.get_resource`
- `Drive.get_folder_id`
- `Drive.check_resource_in_cache`
- the private `get_resource_from_server` policy currently used only by
  `get_resource`

The production behavior must stay unchanged. `src/drive.ml` should remain the
adapter for Context access, root-folder lookup, well-known resource creation,
cache persistence helpers, Drive API calls, logging, and retry wrappers.

## Current Problem

The current resolver block combines several behaviors that are difficult to
exercise directly:

- metadata refresh before ordinary path resolution
- well-known root handling for `/`, lost+found, and shared-with-me
- valid/stale cache classification
- a stricter synchronized-folder rule for parent cache reuse
- recursive parent folder id lookup
- exact-name server lookup by parent folder id
- negative cache insertion with `NotFound`
- stale resource refresh by `remote_id`
- stale resource deletion and retry when no remote id is available
- final `File_not_found` translation for `NotFound` resources

This makes regressions likely when changing cache invalidation, directory read,
or metadata refresh behavior, because the useful branches currently require
production wiring to reach.

## Proposed Module Shape

Add:

- `src/driveResourceResolver.ml`
- `src/driveResourceResolver.mli`
- `test/testDriveResourceResolver.ml`

Expose:

```ocaml
module File = GapiDriveV3Model.File

exception File_not_found

type runtime = {
  cache : CacheData.t;
  config : Config.t;
}

module type PORTS = sig
  val root_directory : string
  val lost_and_found_directory : string
  val shared_with_me_directory : string

  val get_metadata : unit -> CacheData.Metadata.t
  val current_metadata_last_update : unit -> float
  val get_root_folder_id : unit -> string
  val get_well_known_resource : string -> bool -> CacheData.Resource.t

  val is_lost_and_found_root : string -> bool -> Config.t -> bool
  val is_shared_with_me_root : string -> bool -> Config.t -> bool

  val lookup_resource :
    CacheData.t -> string -> bool -> CacheData.Resource.t option

  val create_resource : string -> CacheData.Resource.t
  val insert_resource :
    CacheData.t -> CacheData.Resource.t -> CacheData.Resource.t

  val insert_resource_from_file :
    CacheData.t -> CacheData.Resource.t -> File.t -> CacheData.Resource.t

  val update_resource_from_file :
    CacheData.Resource.t -> File.t -> CacheData.Resource.t

  val update_cached_resource : CacheData.t -> CacheData.Resource.t -> unit
  val delete_cached_resource : CacheData.Resource.t -> unit

  val select_first_resource_with_remote_id :
    CacheData.t -> string -> CacheData.Resource.t option

  val find_file_in_folder :
    parent_folder_id:string ->
    name:string ->
    trashed:bool ->
    File.t option GapiMonad.SessionM.m

  val get_file_by_remote_id : string -> File.t GapiMonad.SessionM.m
  val with_default_retry : 'a GapiMonad.SessionM.m -> 'a GapiMonad.SessionM.m
end

module Make (P : PORTS) : sig
  val check_resource_in_cache : runtime -> string -> bool -> bool
  val get_folder_id : runtime -> string -> bool -> string GapiMonad.SessionM.m
  val get_resource :
    runtime -> string -> bool -> CacheData.Resource.t GapiMonad.SessionM.m
end
```

The public signatures above should use `SessionM.m` for `get_resource` and
`get_folder_id`, matching the current `Drive` behavior.

In the implementation, define the exception consistently with the other
extracted Drive modules:

```ocaml
exception File_not_found = DriveMutations.File_not_found
```

## Resolver Behavior

`DriveResourceResolver` should own the branch policy and keep production effects
behind ports.

- `check_resource_in_cache runtime path trashed`
  - Reads `P.current_metadata_last_update ()`.
  - Uses `P.lookup_resource runtime.cache path trashed`.
  - Returns `false` when no row exists or when
    `CacheData.Resource.is_valid resource metadata_last_update` is false.
  - Returns `true` for valid non-folder resources.
  - Returns `true` for valid folders only when
    `resource.state = CacheData.Resource.State.Synchronized`.

- `get_folder_id runtime path trashed`
  - For `P.root_directory`, returns `P.get_root_folder_id ()`.
  - For every other path, calls `get_resource runtime path trashed` and returns
    `resource.remote_id` using the same option-get behavior as today.
  - Do not add a new fallback for missing `remote_id`; preserve the existing
    failure behavior.

- `get_resource runtime path trashed`
  - Reads `runtime.config`.
  - Calls `P.get_metadata ()` before any path-specific handling and uses
    `CacheData.Metadata.last_update` from the returned metadata to classify the
    primary cache lookup.
  - For `P.root_directory`, returns
    `P.get_well_known_resource P.root_directory trashed`.
  - For `P.is_lost_and_found_root path trashed runtime.config`, returns
    `P.get_well_known_resource P.lost_and_found_directory trashed`.
  - For `P.is_shared_with_me_root path trashed runtime.config`, returns
    `P.get_well_known_resource P.shared_with_me_directory trashed`.
  - For ordinary paths, performs the current lookup/refresh flow against
    `runtime.cache`.

- Ordinary cache lookup flow:
  - `None` calls the new-resource path.
  - `Some resource` with `CacheData.Resource.is_valid resource
    metadata_last_update` returns the cached resource.
  - `Some stale_resource` calls
    `P.with_default_retry (refresh_resource runtime stale_resource)`.
  - After either lookup, miss, or refresh, translate
    `CacheData.Resource.State.NotFound` to `Utils.raise_m File_not_found`.

- New-resource path:
  - Computes `parent_path = Filename.dirname path`.
  - Calls `check_resource_in_cache runtime parent_path trashed`.
  - If the parent is already a valid synchronized folder, raises
    `File_not_found` without querying Drive.
  - Otherwise creates `new_resource = P.create_resource path`.
  - Uses `Filename.basename path` as the server lookup name.
  - Resolves the parent folder id with
    `get_folder_id runtime new_resource.parent_path trashed`.
  - Calls `P.find_file_in_folder ~parent_folder_id ~name ~trashed`.
  - On no file, sets `trashed = Some trashed` and
    `state = CacheData.Resource.State.NotFound`, then inserts with
    `P.insert_resource`.
  - On a file, inserts with `P.insert_resource_from_file`.

- Stale refresh path:
  - If `resource.remote_id = None`, deletes the cached resource with
    `P.delete_cached_resource resource`, then follows the new-resource path.
  - If `resource.remote_id = Some remote_id`, calls
    `P.get_file_by_remote_id remote_id`.
  - Reloads the current cache row with
    `P.select_first_resource_with_remote_id runtime.cache remote_id`.
  - Uses the reloaded row when available, otherwise the original stale row.
  - Rebuilds it with `P.update_resource_from_file`.
  - Persists it with `P.update_cached_resource runtime.cache`.
  - Returns the updated resource.

## Production Wiring

In `src/drive.ml`, add the resolver ports close to the existing resource
helpers:

```ocaml
module DriveResourceResolverPorts = struct
  let root_directory = root_directory
  let lost_and_found_directory = lost_and_found_directory
  let shared_with_me_directory = shared_with_me_directory

  let get_metadata = get_metadata

  let current_metadata_last_update () =
    Context.get_ctx () |. Context.metadata_last_update_lens

  let get_root_folder_id = get_root_folder_id_from_context
  let get_well_known_resource = get_well_known_resource
  let is_lost_and_found_root = is_lost_and_found_root
  let is_shared_with_me_root = is_shared_with_me_root

  let lookup_resource _cache path trashed = lookup_resource path trashed
  let create_resource = create_resource
  let insert_resource = Cache.Resource.insert_resource
  let insert_resource_from_file = insert_resource_into_cache
  let update_resource_from_file resource file = update_resource_from_file resource file
  let update_cached_resource = update_cached_resource
  let delete_cached_resource = delete_cached_resource
  let select_first_resource_with_remote_id =
    Cache.Resource.select_first_resource_with_remote_id

  let find_file_in_folder ~parent_folder_id ~name ~trashed =
    get_file_from_server parent_folder_id name trashed

  let get_file_by_remote_id remote_id =
    Utils.log_with_header
      "BEGIN: Getting file from server (remote id=%s)\n%!" remote_id;
    with_retry_default
      (FilesResource.get ~supportsAllDrives:true ~std_params:file_std_params
         ~fileId:remote_id)
    >>= fun file ->
    Utils.log_with_header
      "END: Getting file from server (remote id=%s)\n%!" remote_id;
    SessionM.return file

  let with_default_retry = with_retry_default
end

module ResourceResolverOps =
  DriveResourceResolver.Make (DriveResourceResolverPorts)
```

The production `get_file_by_remote_id` port should keep the current inner
`with_retry_default` around `FilesResource.get`. The resolver should also keep
the current outer `with_retry_default` by wrapping the whole stale-refresh
operation through `P.with_default_retry`.

Add a local runtime constructor:

```ocaml
let drive_resource_resolver_runtime ?cache () =
  let context = Context.get_ctx () in
  {
    DriveResourceResolver.cache =
      Option.default context.Context.cache cache;
    config = context |. Context.config_lens;
  }
```

Then replace the existing public functions with thin wrappers:

```ocaml
let check_resource_in_cache cache path trashed =
  ResourceResolverOps.check_resource_in_cache
    (drive_resource_resolver_runtime ~cache ()) path trashed

let rec get_folder_id path trashed =
  ResourceResolverOps.get_folder_id
    (drive_resource_resolver_runtime ()) path trashed

and get_resource path trashed =
  ResourceResolverOps.get_resource
    (drive_resource_resolver_runtime ()) path trashed
```

If the mutual recursion wrapper is no longer needed after extraction, use
ordinary `let` bindings instead. Keep the exported names and call signatures
unchanged.

## Scope Boundaries

- Keep `get_file_from_server` in `src/drive.ml`; it remains the exact Drive
  query adapter for parent/name lookup.
- Keep `get_well_known_resource`, `create_root_resource`, and
  `create_well_known_resource` in `src/drive.ml`; this pass should not move
  synthetic resource construction.
- Keep `get_resource_with_id` and `get_resource_with_id_from_server` in
  `src/drive.ml`.
- Do not change the query built by `get_file_from_server`.
- Do not change cache schema, resource fields, state names, logging text unless
  mechanically required by the extraction, or exception behavior.
- Do not change `DriveDirectoryReads`, `DriveViews`, or other callers beyond
  preserving their existing port wiring to the public `Drive` functions.

## Unit Tests

Add `test/testDriveResourceResolver.ml` and register `TestDriveResourceResolver.suite`
in `test/testSuite.ml`.

Use fake ports with mutable trace lists and in-memory lookup tables, following
the style used by the existing extracted Drive modules. The tests should run
resolver functions through a fake `SessionM` session rather than using real
Context or Drive API calls.

Cover:

- `check_resource_in_cache` returns `false` for missing rows.
- `check_resource_in_cache` returns `false` for stale rows.
- `check_resource_in_cache` returns `true` for valid non-folder rows.
- `check_resource_in_cache` returns `false` for valid folders that are not
  `Synchronized`.
- `check_resource_in_cache` returns `true` for valid synchronized folders.
- `get_folder_id` for root returns `P.get_root_folder_id ()` and does not call
  `get_resource`.
- `get_folder_id` for non-root returns the resolved resource remote id.
- `get_resource` calls `P.get_metadata ()` before returning root,
  lost+found, or shared-with-me well-known resources.
- Root, lost+found, and shared-with-me paths call `P.get_well_known_resource`
  with the canonical path constants.
- A valid cached ordinary resource returns without server lookup or refresh.
- A cache miss whose parent is a valid synchronized folder raises
  `File_not_found` and does not call `find_file_in_folder`.
- A cache miss whose parent is not reusable recursively resolves the parent,
  then calls `find_file_in_folder` with the resolved parent id, basename, and
  trashed flag.
- Server hit inserts through `insert_resource_from_file` and returns the
  inserted resource.
- Server miss inserts a `NotFound` resource with `trashed = Some trashed`; the
  final public result raises `File_not_found`.
- Existing cached `NotFound` rows raise `File_not_found`.
- Stale resource with a remote id wraps refresh in `with_default_retry`, fetches
  by remote id, reloads the current cached row by remote id, updates from the
  fetched file, persists it, and returns the updated row.
- Stale resource with a remote id uses the original stale row if reload by
  remote id returns `None`.
- Stale resource without a remote id deletes the stale row and then follows the
  new-resource path.
- Exceptions raised by `get_metadata`, `find_file_in_folder`, or
  `get_file_by_remote_id` propagate through the returned `SessionM.m`.

Useful direct helper assertions:

- Event order: metadata refresh precedes well-known root return.
- Event order: parent folder id resolution precedes server lookup.
- Event order: stale remote-id refresh fetches, reloads, updates from file, then
  writes the cache.

## Documentation Updates

After implementation, update agent docs so they describe the current ownership
without historical notes:

- `docs/agent-docs/drive-get-resource.md` should name
  `DriveResourceResolver` as the owner of path resolution policy and
  `src/drive.ml` as the production adapter.
- Any repo map or architecture page that points directly at
  `src/drive.ml` for `get_resource`, `get_folder_id`, or
  `check_resource_in_cache` should point at the resolver module instead.
- Caller-focused docs such as directory read docs should continue to describe
  behavior, not the extraction history.

## Validation

Run:

```sh
dune build @install
dune runtest
```

Also inspect `git diff --check` before finalizing.

## Assumptions

- The new module name is `DriveResourceResolver`.
- `DriveResourceResolver` should depend only on existing data modules,
  `Config`, `DriveMutations` for the shared `File_not_found` exception,
  `GapiDriveV3Model.File`, `GapiLens` if lenses are used, `GapiMonad`, and
  `Utils`; it should not read `Context` directly.
- Existing double retry behavior during stale remote-id refresh is intentional
  and should be preserved.
- The public `Drive` function signatures stay unchanged.
- No user-visible behavior, cache schema, or Drive query semantics should
  change.
