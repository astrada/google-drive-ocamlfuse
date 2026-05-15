# DriveResourceMapping Extraction Plan

## Goal

Extract Drive file-to-cache-resource mapping and filename/path calculation from
`src/drive.ml` into a focused `DriveResourceMapping` module.

The extraction should make the mapping rules testable without `Context`, cache
database access, Drive API requests, OAuth, FUSE callbacks, or background
threads. `Drive` should keep the current public surface and delegate to the new
module through thin wrappers.

## Current Scope

The candidate block in `src/drive.ml` currently includes:

- Drive resource type constants:
  - `folder_mime_type`
  - `shortcut_mime_type`
- filename helpers:
  - `clean_filename`
  - `get_file_extension`
  - `get_filename`
  - `get_file_extension_from_format`
  - `get_file_extension_from_mime_type`
  - `get_remote_id_fingerprint`
  - `disambiguate_filename`
- cache-table and path helpers:
  - `build_resource_tables`
  - `get_unique_filename`
  - `get_unique_filename_from_resource`
  - `get_unique_filename_from_file`
  - `recompute_path`
- resource mapping helpers:
  - `create_resource`
  - `clean_document_extension`
  - `update_resource_from_file`

`insert_resource_into_cache` should stay in `src/drive.ml` because it performs
cache writes and logging around database insertion. It should delegate the
resource transformation to the extracted mapping helper.

## Target Module

Add:

- `src/driveResourceMapping.ml`
- `src/driveResourceMapping.mli`
- `test/testDriveResourceMapping.ml`

Register the new test module in `test/testSuite.ml`.

The new module should avoid global context reads. Prefer explicit inputs:

```ocaml
module File = GapiDriveV3Model.File

val folder_mime_type : string
val shortcut_mime_type : string

val clean_filename : string -> string
val get_file_extension : string -> string

val get_filename :
  Config.t -> string -> bool -> (Config.t -> string) -> string

val get_file_extension_from_format :
  CacheData.Resource.t -> Config.t -> string

val get_file_extension_from_mime_type : string -> Config.t -> string

val clean_document_extension :
  Config.t -> string -> CacheData.Resource.t -> string

val build_resource_tables :
  Config.t ->
  CacheData.Resource.t list ->
  (string, int) Hashtbl.t * (string, CacheData.Resource.t) Hashtbl.t

val create_resource :
  now:(unit -> float) -> string -> CacheData.Resource.t

val get_unique_filename_from_resource :
  Config.t ->
  CacheData.Resource.t ->
  string ->
  (string, int) Hashtbl.t ->
  string

val get_unique_filename_from_file :
  Config.t ->
  File.t ->
  (string, int) Hashtbl.t ->
  string

val recompute_path :
  Config.t ->
  CacheData.Resource.t ->
  string ->
  (string, int) Hashtbl.t ->
  string

val update_resource_from_file :
  now:(unit -> float) ->
  recompute_path:(CacheData.Resource.t -> string -> string) ->
  ?state:CacheData.Resource.State.t ->
  ?link_target:string ->
  CacheData.Resource.t ->
  File.t ->
  CacheData.Resource.t
```

Keep `Drive.create_root_resource` and `Drive.create_well_known_resource` in
`src/drive.ml` for this pass. Those helpers belong more naturally with the
future root/well-known resource resolution refactoring, though they can call the
new `DriveResourceMapping.create_resource`.

Keep `json_length`, request constants, resource-key header construction, and
cache insertion in `src/drive.ml` or their current modules. They are used by
xattrs, remote requests, and database code rather than by the mapping module's
core responsibility.

## Drive Wrappers

`src/drive.ml` should preserve existing internal and public call shapes where
other modules already depend on them.

Expected wrapper shape:

```ocaml
let folder_mime_type = DriveResourceMapping.folder_mime_type
let shortcut_mime_type = DriveResourceMapping.shortcut_mime_type

let create_resource path =
  DriveResourceMapping.create_resource ~now:Unix.gettimeofday path
```

`build_resource_tables parent_path trashed` should remain a `Drive` helper that
reads `Context`, selects resources with
`Cache.Resource.select_resources_with_parent_path`, then passes the selected
resource list and config into `DriveResourceMapping.build_resource_tables`.

`get_unique_filename_from_file` and `get_unique_filename_from_resource` should
remain available as `Drive` helpers by reading the current config and calling
the new module.

`recompute_path resource name` should remain a `Drive` helper by building the
current filename table and then calling `DriveResourceMapping.recompute_path`.

`update_resource_from_file` should delegate to
`DriveResourceMapping.update_resource_from_file` with:

- `~now:Unix.gettimeofday`
- `~recompute_path`
- the current optional `state` and `link_target`

Using a recompute callback preserves the current behavior where path
recalculation only needs `Context` when a cached resource already has a name and
the remote file name changed.

## Behavior To Preserve

The extraction should preserve these existing rules:

- Drive names are cleaned by replacing `/` and NUL with `_`.
- Google document export extensions are appended only when:
  - the resource/file is a document,
  - `Config.docs_file_extension` is enabled,
  - the configured export format is not empty,
  - the cleaned name does not already end with that extension.
- The configured `desktop` export format maps to `html` when
  `Config.desktop_entry_as_html` is enabled.
- `clean_document_extension` removes only the configured document extension and
  only for document resources.
- duplicate filenames are disambiguated with the existing remote-id fingerprint
  logic and the same "too many filename conflicts" limit.
- duplicate disambiguation preserves extensions, including
  `fullFileExtension` when Drive provides it.
- `build_resource_tables` still records both the cached basename and the clean
  Drive-derived name when they differ.
- `build_resource_tables` still builds a remote-id table keyed by
  `resource.remote_id`.
- `create_resource` still sets:
  - `id = 0L`
  - `state = ToDownload`
  - `parent_path = Filename.dirname path`
  - `path = path`
  - empty xattrs
  - `last_update` from the supplied clock
- `update_resource_from_file` still:
  - recomputes the path only when `resource.name` exists and differs from
    `file.name`,
  - derives `parent_path` from the final path,
  - preserves size for `Uploading` and `ToUpload`,
  - stores `Some file.size` for other states,
  - maps empty `resourceKey` to `None`,
  - maps shortcut target fields only for shortcut MIME type,
  - uses the optional `link_target` only for shortcuts,
  - otherwise reads symlink target, mode bits, uid, gid, and xattrs from Drive
    app properties,
  - serializes export links the same way,
  - refreshes `last_update` from the supplied clock.

## Implementation Steps

1. Create `DriveResourceMapping` with constants and pure filename helpers.
   Adjust function signatures so config and clocks are explicit rather than
   read from `Context` or `Unix.gettimeofday` directly.

2. Move table and unique-name logic into the new module. Make
   `build_resource_tables` accept a `Config.t` and a resource list, leaving the
   cache selection in `Drive`.

3. Move `create_resource`, `clean_document_extension`, `recompute_path`, and
   `update_resource_from_file` into the new module with explicit dependencies.

4. Replace the moved implementations in `src/drive.ml` with wrappers that keep
   existing call sites working.

5. Keep existing extracted modules unchanged unless compilation requires a
   signature adjustment. Their ports should continue to receive mapping helpers
   from `Drive` for now.

6. Add focused unit tests in `test/testDriveResourceMapping.ml`.

7. Run formatting, then run `dune build @install` and `dune runtest`
   sequentially.

## Test Plan

Add unit coverage for:

- `create_resource` with a fixed clock.
- filename cleanup for `/` and NUL.
- document extension addition when enabled, unchanged names when disabled, and
  `desktop` to `html` mapping.
- `clean_document_extension` for document and non-document resources.
- duplicate filename disambiguation:
  - no conflict keeps the clean filename,
  - conflict inserts a fingerprint before the extension,
  - Drive `fullFileExtension` is honored,
  - extensionless names remain valid.
- `build_resource_tables`:
  - records cached basenames,
  - records clean Drive-derived names when different,
  - stores resources by remote id.
- `recompute_path` using a supplied filename table.
- `update_resource_from_file`:
  - standard file metadata mapping,
  - fixed `last_update`,
  - size preservation for upload states,
  - resource key empty/non-empty behavior,
  - shortcut target and resource-key mapping,
  - symlink target mapping from app properties for non-shortcuts,
  - path recomputation callback called only on name changes.

Existing tests that exercise `Drive.create_resource`,
`Drive.update_resource_from_file`, directory reads, metadata refresh, resource
resolution, mutations, upload, remote updates, and xattrs should continue to
pass through the `Drive` wrappers.

## Documentation

This is an internal testability extraction. No user-facing wiki updates are
expected.

If implementation changes where maintainers should look for filename or
Drive-file-to-resource mapping behavior, update the relevant agent docs to name
`DriveResourceMapping` as the current implementation location. Keep those docs
descriptive of the current code rather than describing the refactoring.

## Validation

Run these commands sequentially:

```sh
tools/format_ocaml
dune build @install
dune runtest
```
