# OToml Config Implementation Checklist

## Purpose

This checklist turns the TOML migration plan into concrete implementation work.
It is ordered to minimize risk and keep the repository buildable after each
stage.

The goal is to migrate config handling to `otoml` without breaking:

- existing `key=value` config files
- current runtime semantics
- startup stability

## Scope

Primary files to change:

- `google-drive-ocamlfuse.opam`
- `src/dune`
- `src/config.ml`
- `src/context.ml`
- `bin/gdfuse.ml`
- `test/testSuite.ml`
- `test/dune`

Primary files to add:

- `src/configStore.ml`
- `src/configStore.mli`
- `test/testConfigStore.ml`

Files to keep unchanged if possible in phase 1:

- `src/keyValueStore.ml`
- `src/state.ml`

`KeyValueStore` should continue to back the state file, even after config moves
away from it.

## Fixed Decisions

These decisions are now part of the implementation contract:

- keep the filename `config`
- keep the previous config version as `config.bak`, replacing any older backup
- persist CLI overrides only for `-id` and `-secret`
- reject duplicate keys
- emit a minimal TOML file

## Milestone 1: Add Dependency Plumbing

### Checklist

- Update `google-drive-ocamlfuse.opam`:
  - add `otoml`
- Update `src/dune`:
  - add `otoml` to library dependencies
- Confirm tests still compile after the dependency change

### Expected Result

The codebase can use `Otoml` APIs without any config behavior changes yet.

## Milestone 2: Introduce Dedicated Config Store

### Checklist

- Add `src/configStore.mli`
- Add `src/configStore.ml`
- Define a config store record, likely:
  - `type t = { path : string; data : Config.t }`
- Expose functions for:
  - `load : string -> t`
  - `save : t -> unit`
  - `create_default : debug:bool -> path:string -> t`
  - `load_or_create : debug:bool -> path:string -> t`

### Implementation Notes

- Do not reuse `KeyValueStore.MakeFileStore(Config)` here
- Keep the API similar enough that `bin/gdfuse.ml` can be migrated cleanly
- Decide whether to define lenses for `path` and `data`; keeping parity with
  the old `ConfigFileStore` shape will reduce churn

### Expected Result

Config persistence becomes isolated from the generic key/value store.

## Milestone 3: Add Legacy Reader Without Changing Startup Policy Yet

### Checklist

- In `src/configStore.ml`, implement a legacy parser for current files
- Preserve current semantics for:
  - missing keys use defaults
  - empty string values are allowed
- Enforce the new migration rule:
  - duplicate keys are rejected
- Add precise error reporting:
  - include path
  - include whether TOML parse failed or legacy parse failed

### Suggested Internal API

- `load_legacy : string -> Config.t`
- `load_toml : string -> Config.t`
- `detect_and_load : string -> [ \`Toml | \`Legacy ] * Config.t`

### Important Compatibility Rule

Keep legacy parsing intentionally compatible with the current code, even if it
is imperfect, except for duplicate-key handling which is now intentionally
stricter: duplicates must be rejected.

### Expected Result

The repo can parse both formats, but startup behavior may still be unchanged.

## Milestone 4: Implement TOML Encode/Decode

### Checklist

- Implement the recommended grouped TOML structure:
  - `[auth]`
  - `[mount]`
  - `[docs]`
  - `[cache]`
  - `[io]`
  - `[network]`
  - `[async]`
  - `[logging]`
- Implement TOML decode into `Config.t`
- Implement TOML encode from `Config.t`
- Add support for human-readable comments in newly generated files

### Recommendation

Use the recommended grouped tables for readability, but keep `Config.t` flat.

### Design Rule

Do not make `Config.t` nested during this refactor.

### Specific Decisions To Make

- represent `umask` as:
  - string like `"0o002"`; recommended for preserving intent
  - or integer; less readable
- generate a minimal TOML file, not an exhaustive dump

### Table Mapping To Implement

- `[auth]`
  - `client_id`
  - `client_secret`
  - `verification_code`
  - `scope`
  - `redirect_uri`
  - `oauth2_loopback`
  - `oauth2_loopback_port`
  - `service_account_credentials_path`
  - `service_account_user_to_impersonate`
- `[mount]`
  - `metadata_cache_time`
  - `read_only`
  - `umask`
  - `root_folder`
  - `team_drive_id`
  - `lost_and_found`
  - `disable_trash`
  - `keep_duplicates`
  - `mv_keep_target`
- `[docs]`
  - `download_docs`
  - `docs_file_extension`
  - document, drawing, form, presentation, spreadsheet, map, fusion-table, and
    apps-script formats/icons
  - `desktop_entry_exec`
  - `desktop_entry_as_html`
- `[cache]`
  - `max_cache_size_mb`
  - `metadata_memory_cache`
  - `metadata_memory_cache_saving_interval`
  - `sqlite3_busy_timeout`
  - `data_directory`
  - `cache_directory`
  - `log_directory`
- `[io]`
  - `stream_large_files`
  - `large_file_threshold_mb`
  - `large_file_read_only`
  - `memory_buffer_size`
  - `max_memory_cache_size`
  - `read_ahead_buffers`
  - `write_buffers`
  - `max_upload_chunk_size`
  - `autodetect_mime`
  - `acknowledge_abuse`
- `[network]`
  - `connect_timeout_ms`
  - `max_download_speed`
  - `max_upload_speed`
  - `low_speed_limit`
  - `low_speed_time`
  - `max_retries`
  - `curl_debug_off`
- `[async]`
  - `async_upload_queue`
  - `async_upload_threads`
  - `async_upload_queue_max_length`
  - `background_folder_fetching`
- `[logging]`
  - `log_to`
  - `debug_buffers`

### Expected Result

The project can serialize a valid, readable TOML config file.

## Milestone 5: Migrate `context.ml` Off `ConfigFileStore`

### Checklist

- Update `src/context.ml`
- Replace:
  - `module ConfigFileStore = KeyValueStore.MakeFileStore (Config)`
- With:
  - `module ConfigFileStore = ConfigStore`
  - or direct use of `ConfigStore.t`

### Review Points

- `config_store` field in `Context.t`
- `config_lens`
- `save_config_store`

### Constraint

Do not disturb state-store handling. `StateFileStore` should remain backed by
`KeyValueStore`.

### Expected Result

The runtime context uses the new config persistence module without impacting
state handling.

## Milestone 6: Switch `bin/gdfuse.ml` To `ConfigStore`

### Checklist

- Replace old config creation/loading helpers:
  - `create_default_config_store`
  - `get_config_store`
- Delegate to `ConfigStore`
- Keep path resolution in `AppDir.get_config_path` unchanged

### Behavior To Preserve Initially

- if the config file is missing, create a default config
- if the file exists, load it

### New Behavior To Add

- if the file is legacy, migrate it on startup
- if migration succeeds, save TOML and let the normal save path rotate the old
  file to `config.bak`

### Expected Result

Startup loads config through the new subsystem.

## Milestone 7: Stop Unconditional Rewrites

### Checklist

- Refactor `setup_application` in `bin/gdfuse.ml`
- Separate:
  - persisted config
  - effective runtime config
- Remove the unconditional:
  - `Context.save_config_store config_store`
  after CLI/runtime overlay

### New Save Triggers

- first config creation
- legacy migration
- explicit future schema upgrade

### Decisions To Encode

- `-id` and `-secret` must still persist
- other CLI flags like `-scope`, `-redirect_uri`, `-port`, and `-docsmode`
  should become runtime overrides unless an explicit policy later says
  otherwise

### Expected Result

Normal startup no longer rewrites config files, which allows comments to
survive.

## Milestone 8: Add Config-Versioned Migration Hooks

### Checklist

- Add a top-level TOML key such as:
  - `config_version = 1`
- Implement migration logic:
  - legacy -> version 1 TOML
  - version N -> version N+1 hooks for future use

### Suggested API

- `upgrade_config : Config.t -> loaded_version:int option -> upgrade_result`

Where `upgrade_result` can indicate:

- no rewrite needed
- rewrite needed
- fatal migration error

### Expected Result

New options can be introduced via targeted migration logic instead of rewriting
the whole file on every startup.

## Milestone 9: Add Dedicated Tests

### Checklist

- Add `test/testConfigStore.ml`
- Register it in `test/testSuite.ml`
- Keep tests file-system based where relevant; use temp directories/files

### Test Groups

#### Legacy Parse Tests

- load minimal legacy config
- load legacy config with missing keys
- reject legacy config with duplicate keys
- reject legacy config with unknown keys
- load legacy config with empty values
- reject malformed legacy config

#### TOML Parse Tests

- load minimal TOML config
- load TOML with grouped tables
- load TOML containing comments
- reject TOML with unknown keys
- reject TOML with wrong types
- reject TOML with invalid field values

#### Migration Tests

- migrate legacy config to TOML on load
- ensure migrated semantic values match legacy semantic values
- ensure the previous file is written to `config.bak`
- ensure second startup does not remigrate TOML

#### Save Policy Tests

- creating a missing config writes TOML
- loading an existing TOML without migration does not rewrite it
- comments survive a normal load path because no save occurs
- saving over an existing config rotates the old file to `config.bak`
- `-id` and `-secret` persistence still works
- non-persistent CLI runtime overrides do not trigger a rewrite

#### Validation Tests

- invalid `memory_buffer_size`
- invalid `max_memory_cache_size`
- invalid `max_upload_chunk_size`
- invalid `umask` representation

### Test Helpers To Add

Inside `test/testConfigStore.ml`, add small helpers for:

- writing temp files
- reading file contents back
- asserting file equality / inequality
- asserting parse failures with useful messages

## Milestone 10: Update Docs And Samples

### Checklist

- Update `docs/wiki/Configuration.md`
- Update `docs/agent-docs/config-file-handling.md`
- Document:
  - TOML syntax
  - comments support
  - one-time legacy migration
  - whether `.bak` files are created
  - whether CLI overrides persist

### Optional

- Add a small commented TOML example to `docs/`, but keep generated config
  files minimal

## Suggested Commit Sequence

If this work is split into multiple commits, this is the cleanest order:

1. add `otoml` dependency
2. add `ConfigStore` and TOML encode/decode
3. wire `Context` and startup to `ConfigStore`
4. add legacy migration
5. stop unconditional config rewrites
6. add config migration tests
7. update docs

## Acceptance Criteria

The work is done when:

- the app can start with an old `key=value` config
- first startup migrates that file to TOML
- duplicate keys are rejected
- the old file is rotated to `config.bak` on overwrite
- existing semantics are preserved after migration
- TOML config supports comments
- normal startup does not remove comments
- `-id` and `-secret` persist
- generated TOML stays minimal
- targeted tests cover parse, migration, and save behavior

## Notes For Implementation

- Keep the initial migration conservative; avoid changing config semantics
- Prefer atomic file replacement during migration
- Preserve legacy-path behavior; do not bundle path changes with format changes
- Avoid touching unrelated subsystems such as OAuth or cache logic in the same
  patch series
