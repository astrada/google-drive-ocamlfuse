# Drive Runtime And Shared Concepts Refactor

## Goal

Reduce repeated concepts across the extracted Drive modules without collapsing
the current testable boundaries back into one large abstraction.

The main target is the repeated runtime shape used by many modules extracted
from `Drive`: most modules carry some form of `cache`, `config`, or both. The
secondary target is repeated production wiring in `src/drive.ml`, where many
small wrappers rebuild equivalent runtime records from `Context.get_ctx ()`.

The refactor should make future Drive extractions easier to test, keep the
existing unit-test style, and avoid changing public FUSE behavior.

## Current Problem

The extracted modules are intentionally small and testable, but several of them
now repeat similar local concepts:

- Many modules define their own `runtime = { cache : CacheData.t; config :
  Config.t }`.
- Some modules define smaller variants such as `{ cache : CacheData.t }` or
  `{ config : Config.t }`.
- A few modules define extended runtime records with the same base fields plus
  extra state, such as metadata, mountpoint paths, memory buffers, or root-folder
  ids.
- `src/drive.ml` repeatedly reads `Context.get_ctx ()`, extracts cache/config,
  and builds module-specific runtime records.
- Several `PORTS` signatures repeat resource lookup, path normalization, remote
  update, resource mapping, and cache persistence concepts.

This duplication is not currently a correctness bug, but it increases the cost
of future refactors and makes tests maintain parallel fake runtime and fake port
shapes.

## Design Principles

- Keep extracted modules focused. Do not introduce a whole-module
  `Drive.Make(DEPS)`.
- Prefer small shared concepts over broad dependency bundles.
- Factor runtime data before factoring behavior.
- Keep `PORTS` signatures semantic and narrow; avoid one giant shared ports
  signature.
- Preserve existing public functions in `Drive`.
- Preserve the current unit-test strategy: fake ports, explicit runtime records,
  and no real `Context`, Drive API, background threads, or filesystem mutations
  in unit tests.
- Avoid changing behavior while changing shapes. Each phase should be compile-
  and test-verifiable on its own.

## Proposed Shared Runtime Module

Add a focused module, most likely named `DriveRuntime`, with shared runtime data
types used by extracted modules.

Suggested initial shape:

```ocaml
type base = {
  cache : CacheData.t;
  config : Config.t;
}

type cache_only = { cache : CacheData.t }
type config_only = { config : Config.t }
```

Do not immediately force every specialized runtime into this module. Start with
the exact common cases and convert extended runtimes only when the resulting
shape stays clear.

Likely direct aliases:

- `DriveReads.runtime`
- `DriveDirectoryReads.runtime`
- `DriveResourceResolver.runtime`
- `DriveDownloads.runtime`
- `DriveUploads.runtime`
- `DriveRemoteUpdates.runtime`
- `DriveFileMutations.runtime`
- `DriveMetadataMutations.runtime`
- `DriveXattrs.runtime`
- `DriveRuntimeServices.runtime`

Likely direct smaller aliases:

- `DriveResourceById.runtime` as `DriveRuntime.cache_only`
- `DriveUploadWorkerBridge.runtime` as `DriveRuntime.cache_only`
- `DriveOpens.runtime` as `DriveRuntime.config_only`

Likely deferred or extended cases:

- `DriveRootResolution.runtime`, because it adds `root_folder_id`.
- `DriveViews.runtime`, because it adds mountpoint path and stats.
- `DriveStreaming.runtime`, because it adds memory-buffer and eviction-thread
  state.
- `DriveCacheMaintenance.runtime`, because it adds optional metadata.
- `DriveFilesystemStats.runtime`, because it uses metadata plus config.
- `DriveMutations.runtime`, because it adds mountpoint path and skip-trash
  behavior.

Review result: keep these runtimes module-specific. Each extra field is tied to
the owning module's policy, and introducing shared extended records would mostly
create single-use aliases or hide important test fixture state.

## Production Runtime Builders

After introducing shared runtime types, centralize production runtime
construction in `src/drive.ml`.

Suggested helper shape:

```ocaml
let drive_runtime_base () =
  let context = Context.get_ctx () in
  {
    DriveRuntime.cache = context.Context.cache;
    config = context |. Context.config_lens;
  }

let drive_runtime_cache_only () =
  let context = Context.get_ctx () in
  { DriveRuntime.cache = context.Context.cache }

let drive_runtime_config_only () =
  let context = Context.get_ctx () in
  { DriveRuntime.config = context |. Context.config_lens }
```

If OCaml record-label namespacing makes module-specific aliases awkward, keep
these helpers in `drive.ml` and return module-specific records temporarily. The
important first step is reducing repeated `Context.get_ctx ()` extraction and
making the shared concept explicit.

## Secondary Factoring Candidates

### Resource Lookup Ports

Several modules repeat path and resource lookup dependencies:

- `get_path_in_cache`
- `get_resource`
- `get_folder_id`
- `check_resource_in_cache`
- lost-and-found and shared-with-me root checks

Consider small reusable module-type fragments after runtime factoring:

```ocaml
module type PATH_LOOKUP = sig
  val get_path_in_cache : string -> Config.t -> string * bool
end

module type RESOURCE_LOOKUP = sig
  val get_resource :
    string -> bool -> CacheData.Resource.t GapiMonad.SessionM.m
end
```

Use these only where they reduce real duplication. Do not make every extracted
module depend on a large shared lookup service.

### Resource Store Facade

`src/drive.ml` repeatedly wires cache and mapping operations:

- `create_resource`
- `update_resource_from_file`
- `insert_resource_into_cache`
- `update_cached_resource`
- `update_cached_resource_state`
- `select_first_resource_with_remote_id`

A future `DriveResourceStore` facade could group production cache persistence
and mapping helpers while preserving fakeable semantic ports in tests.

This should come after runtime factoring because it is more behavioral and has
larger blast radius.

### Remote Update Service

`DriveRemoteUpdates` is already close to the right shared abstraction for
metadata-side updates. Metadata mutations, xattrs, uploads, and mutation paths
all carry similar remote update concepts:

- build resource-key headers
- patch or upload a Drive file
- update local cache after a successful remote change

After runtime cleanup, review whether metadata mutations and xattrs can depend
on the existing remote-update module more directly instead of each carrying
parallel remote-update ports.

### Local Content Service

Reads, downloads, file mutations, uploads, and cache maintenance all interact
with local cached content:

- content path resolution
- local materialization
- memory-buffer flushing
- local read/write/truncate/stat
- cache shrink and deletion

This is useful but higher risk. Defer it until runtime and remote-update
factoring are stable.

## Implementation Phases

### 1. Introduce `DriveRuntime`

Add `src/driveRuntime.ml` and `src/driveRuntime.mli` with shared runtime data
types.

Acceptance criteria:

- The module contains data types only, or only trivial constructors.
- No production `Context` access is introduced in extracted policy modules.
- `dune build @install` and `dune runtest` pass.

### 2. Convert Exact `{ cache; config }` Modules

Change modules with exactly `{ cache; config }` runtimes to alias the shared
runtime type.

Suggested first batch:

- `DriveRuntimeServices`
- `DriveReads`
- `DriveDirectoryReads`
- `DriveRemoteUpdates`
- `DriveFileMutations`
- `DriveMetadataMutations`
- `DriveXattrs`

Suggested second batch:

- `DriveResourceResolver`
- `DriveDownloads`
- `DriveUploads`

Acceptance criteria:

- Existing tests continue to construct runtimes with minimal churn.
- No behavior changes are made inside module bodies.
- `dune runtest` passes after each batch.

### 3. Convert Simple Smaller Runtimes

Convert cache-only and config-only runtimes when the type alias remains clear.

Candidates:

- `DriveResourceById`
- `DriveUploadWorkerBridge`
- `DriveOpens`

Acceptance criteria:

- Unit tests stay explicit about the runtime data being supplied.
- `drive.ml` wrappers become smaller or clearer.

### 4. Centralize Production Runtime Builders

Add helper functions in `src/drive.ml` for common runtime construction from
`Context`.

Acceptance criteria:

- Repeated `Context.get_ctx ()` plus cache/config extraction is reduced.
- Public `Drive` functions keep their current signatures.
- Existing unit and integration tests continue to pass.

### 5. Evaluate Extended Runtime Shapes

Review specialized runtime records and decide whether they should extend the
shared base concept or stay module-specific.

Candidates:

- `DriveViews`
- `DriveRootResolution`
- `DriveCacheMaintenance`
- `DriveFilesystemStats`
- `DriveStreaming`
- `DriveMutations`

Acceptance criteria:

- No extended runtime is changed merely for uniformity.
- Changes are made only where the resulting runtime is clearer than the current
  module-specific shape.

Review result:

- `DriveViews`: keep module-specific; `mountpoint_path` and
  `mountpoint_stats` are view/stat-specific state.
- `DriveRootResolution`: keep module-specific; `root_folder_id` is the
  root-resolution memoization boundary.
- `DriveCacheMaintenance`: keep module-specific; optional metadata is tied to
  cache-size accounting and cleanup paths.
- `DriveFilesystemStats`: keep module-specific; quota metadata plus config is a
  pure statfs input, not a reusable Drive runtime.
- `DriveStreaming`: keep module-specific; memory buffers and eviction-thread
  state belong to the streaming policy.
- `DriveMutations`: keep module-specific; `mountpoint_path` and `skip_trash`
  are mutation-only operational state.

### 6. Factor Small Port Fragments

Introduce reusable module-type fragments only for repeated concepts that remain
after runtime cleanup.

First pass:

- Add a type-only `DrivePortFragments` module.
- Define small fragments for path lookup, resource lookup, and single-resource
  resource-key header construction.
- Include those fragments in extracted modules that already declare the same
  ports.

Initial fragments:

```ocaml
module type PATH_LOOKUP = sig
  val get_path_in_cache : string -> Config.t -> string * bool
end

module type RESOURCE_LOOKUP = sig
  val get_resource :
    string -> bool -> CacheData.Resource.t GapiMonad.SessionM.m
end

module type RESOURCE_KEYS = sig
  val build_resource_keys_header_from_resource :
    CacheData.Resource.t -> GapiCore.Header.t list
end
```

Initial targets:

- lookup fragments: `DriveReads`, `DriveOpens`, `DriveRemoteUpdates`,
  `DriveFileMutations`, `DriveXattrs`, `DriveViews`, `DriveDirectoryReads`,
  `DriveUploadDispatch`, and `DriveMutations`
- resource-key fragment: `DriveXattrs`, `DriveUploads`, `DriveStreaming`,
  `DriveMetadataMutations`, and `DriveMutations`

Deferred:

- resource store/cache update fragments
- remote-update service or remote-update port fragments

Acceptance criteria:

- `DrivePortFragments` contains module types only.
- Port fragments reduce fake-test duplication.
- Individual extracted modules still expose narrow semantic `PORTS`.
- No behavior moves between modules.
- No module starts depending on an oversized shared dependency signature.

### 7. Update Documentation

Update agent docs to describe the current shared runtime concept directly.

Likely docs:

- `docs/agent-docs/repo-map.md`
- `docs/agent-docs/drive-read.md`
- `docs/agent-docs/drive-read-dir.md`
- `docs/agent-docs/drive-get-resource.md`
- `docs/agent-docs/drive-upload.md`
- `docs/agent-docs/drive-xattr.md`

Acceptance criteria:

- Documentation describes the current architecture, not the refactoring history.
- The docs make clear that `Drive` remains the production adapter and extracted
  modules remain focused policy modules.

## Test Strategy

Run after every implementation phase:

```sh
dune build @install
dune runtest
```

Focused unit tests should be updated only where type aliases or constructor
names change. The refactor should not require new behavior tests unless a
factoring phase moves behavior into a new module such as `DriveResourceStore`.

For larger phases, also run:

```sh
make e2e-preflight
```

Live e2e tests are not required for pure runtime-type factoring unless the
production `Drive` wrappers or mount-time behavior changes.

## Non-Goals

- Do not introduce a whole-Drive functor.
- Do not combine all extracted module ports into one shared signature.
- Do not move behavior and change runtime types in the same broad patch.
- Do not use this pass to alter Google Drive request behavior.
- Do not change public FUSE callback signatures.
- Do not remove the extracted modules' focused unit tests.

## Completion Criteria

- A shared runtime module exists for common Drive runtime data.
- Modules with exact common runtime shapes use the shared runtime type.
- `src/drive.ml` has less repeated production runtime construction.
- Existing extracted modules remain independently unit-testable.
- Documentation reflects the shared runtime concept.
- `dune build @install` and `dune runtest` pass.
