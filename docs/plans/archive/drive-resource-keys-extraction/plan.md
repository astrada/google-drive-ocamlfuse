# Extract Drive Resource Keys

## Goal

Move Drive resource-key header construction out of `src/drive.ml` and into a
small, pure `DriveResourceKeys` module.

The extraction target is:

- `build_resource_keys_header`
- `build_resource_keys_header_from_resource`
- `build_resource_keys_header_from_resources`

The production behavior should stay unchanged. `src/drive.ml` should keep thin
wrappers for the existing helper names because metadata mutations, uploads,
xattrs, media downloads, and mutation flows already consume these helpers
through `Drive` ports.

## Current Problem

Resource-key header construction is pure data shaping, but it currently lives
inside `Drive` next to request retry and filesystem behavior. The helper builds
the Google Drive `X-Goog-Drive-Resource-Keys` header used when operating on
Drive resources that require a resource key.

The current behavior is consumed by:

- `download_media`
- `DriveMetadataMutations`
- `DriveUploads`
- `DriveMutations`
- `DriveXattrs`

Because the logic is pure and order-sensitive, it can be tested directly
without production `Context`, OAuth, Drive API calls, cache state, or mutation
ports.

## Proposed Shape

Add:

- `src/driveResourceKeys.ml`
- `src/driveResourceKeys.mli`
- `test/testDriveResourceKeys.ml`

Expose:

```ocaml
val resource_keys_header_name : string

val build_resource_keys_header :
  (string option * string option) list -> GapiCore.Header.t list

val build_resource_keys_header_from_resource :
  CacheData.Resource.t -> GapiCore.Header.t list

val build_resource_keys_header_from_resources :
  CacheData.Resource.t list -> GapiCore.Header.t list
```

No functor or ports are needed. The extracted behavior depends only on resource
records and already-computed remote id/resource-key pairs.

## Extracted Behavior

`build_resource_keys_header ids_and_resource_keys` should preserve the current
rules:

- ignore entries whose resource key is `None`
- ignore entries whose resource key is `Some ""`
- ignore entries whose remote id is `None`
- for valid entries, emit `"<remote_id>/<resource_key>"`
- preserve input order for valid entries
- join multiple valid entries with commas
- return `[]` when there are no valid pairs
- otherwise return exactly one `GapiCore.Header.KeyValueHeader` with name
  `"X-Goog-Drive-Resource-Keys"`

The extraction should not add trimming, deduplication, escaping, sorting, or
validation for slash/comma characters. The goal is to keep the current header
construction policy behind a focused module.

`build_resource_keys_header_from_resource resource` should build a one-element
pair list from:

- `resource.CacheData.Resource.remote_id`
- `resource.CacheData.Resource.resource_key`

`build_resource_keys_header_from_resources resources` should map resources in
input order to those same pairs before calling `build_resource_keys_header`.

## Production Wiring

In `src/drive.ml`, replace the helper bodies with aliases:

```ocaml
let build_resource_keys_header =
  DriveResourceKeys.build_resource_keys_header

let build_resource_keys_header_from_resource =
  DriveResourceKeys.build_resource_keys_header_from_resource

let build_resource_keys_header_from_resources =
  DriveResourceKeys.build_resource_keys_header_from_resources
```

Keep `src/drive.mli` stable for this pass. It already exposes
`build_resource_keys_header_from_resource`; the raw and list helpers can stay
internal to `Drive` while tests exercise them directly through
`DriveResourceKeys`.

## Implementation Steps

1. Create `driveResourceKeys.mli` with the header name and helper signatures.
2. Create `driveResourceKeys.ml` with the current header construction behavior.
3. Replace the resource-key helper bodies in `Drive` with aliases to
   `DriveResourceKeys`.
4. Keep existing production port signatures unchanged.
5. Add `test/testDriveResourceKeys.ml`.
6. Register the suite in `test/testSuite.ml`.
7. Run `tools/format_ocaml` or `ocamlformat` on touched OCaml files.
8. Run `dune build @install` and `dune runtest` sequentially.

## Unit Test Plan

Use synthetic `CacheData.Resource.t` values and direct
`GapiCore.Header.KeyValueHeader` pattern matching.

Cover raw pair behavior:

- empty input returns no headers
- resource keys that are `None` are ignored
- empty resource keys are ignored
- missing remote ids are ignored
- one valid pair emits one header
- multiple valid pairs are comma-joined in input order
- invalid pairs are skipped without reordering valid pairs

Cover resource wrappers:

- a resource with both remote id and resource key emits a header
- a resource without a resource key emits no header
- a resource without a remote id emits no header
- a resource list emits valid entries in list order while skipping invalid
  resources

Existing tests for downloads, metadata mutations, uploads, mutations, and
xattrs should continue to pass through the `Drive` wrappers.

## Acceptance Criteria

- `src/drive.ml` no longer contains resource-key header construction policy.
- `DriveResourceKeys` owns the header name and pure header-building helpers.
- Existing `Drive` helper names and public `Drive.mli` call shapes remain
  available.
- Focused unit tests cover filtering and ordering without production context,
  cache files, OAuth, or Drive API requests.
- `dune build @install` and `dune runtest` pass.

## Agent Docs Follow-Up

After implementation, update agent docs to describe the current state directly:

- `docs/agent-docs/architecture.md`
- `docs/agent-docs/repo-map.md`
- `docs/agent-docs/drive-upload.md`
- `docs/agent-docs/drive-create-remote-resource.md`
- `docs/agent-docs/drive-rename.md`
- `docs/agent-docs/drive-xattr.md`

Avoid before/after language. The docs should describe `DriveResourceKeys` as
the current implementation boundary for constructing Google Drive resource-key
headers from cached resources.
