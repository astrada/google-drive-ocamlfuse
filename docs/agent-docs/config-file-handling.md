# Configuration File Handling

## Current Design

Config handling is TOML-backed and split across:

- `src/configStore.ml`
- `src/configRuntime.ml`
- `src/config.ml`
- `src/appDir.ml`
- `src/gdfuseCommon.ml`
- `src/gdfuseFlow.ml`

`bin/gdfuse.ml` is only a thin executable entrypoint.

The state file uses the key/value store through
`Context.StateFileStore = KeyValueStore.MakeFileStore(State)` in
`src/context.ml`. Config uses the `ConfigStore` path.

## Persistent Format

The default config filename remains `config`.

`AppDir.get_config_path` may still point startup at a custom path when `-config`
is used.

The persisted format is TOML, with:

- grouped tables
- comment support
- minimal emission
- a top-level `config_version`

Current grouped layout:

- `[auth]`
- `[mount]`
- `[docs]`
- `[cache]`
- `[io]`
- `[network]`
- `[async]`
- `[logging]`

The `[docs]` table carries both export-format settings and editability policy,
including `editable_docs` for allowing writes to exported Google-native files
when their configured format is not `desktop`.

A full commented reference file lives at
[`docs/wiki/full-config.example.toml`](../wiki/full-config.example.toml).

Example:

```toml
config_version = 1

[auth]
client_id = "..."
client_secret = "..."

[mount]
read_only = false
umask = "0o022"
```

The serializer only writes keys whose values differ from `Config.default`, so
new files are intentionally minimal.

## Load Pipeline

At startup, `GdfuseFlow.setup_application` orchestrates config handling:

1. `resolve_paths_and_logging` resolves the config path with
   `AppDir.get_config_path`.
2. `GdfuseCommon.get_config_store` wraps `ConfigStore.load_or_create`, which
   classifies the file as:
   - missing
   - legacy `key=value`
   - TOML
3. `ConfigStore` parses legacy or TOML input into `Config.t`, and reports a
   `load_state` of `Created`, `Migrated`, `Upgraded`, or `Loaded`.
4. `ConfigRuntime.resolve` computes:
   - `runtime_config`
   - `persisted_config`
   - `should_persist`
   - `clear_cache`
5. `GdfuseFlow.resolve_runtime_config` decides whether to persist the config
   store:
   - always for `Created`, `Migrated`, or `Upgraded`
   - also when `ConfigRuntime.should_persist` is true
6. `GdfuseCommon.get_state_store` then loads or creates the separate key/value
   `state` file used for OAuth tokens and the saved application version.

## Legacy Migration

Legacy `key=value` files are still supported on startup, but only as an input
format for migration.

Current migration behavior:

- duplicate keys are rejected
- unknown keys are rejected
- missing keys fall back to defaults
- successful migration reports `load_state = Migrated`, and startup rewrites the
  file as TOML during the normal config save step
- the previous file is rotated to `config.bak`

The old first-wins duplicate behavior is gone.

## TOML Parsing Rules

TOML files support:

- comments
- grouped tables
- strings, integers, and booleans used by the current schema

Current strictness:

- unknown keys are parse errors
- duplicate keys are parse errors
- unsupported future `config_version` values are parse errors
- unversioned TOML is treated as version `0` and upgraded to `1`

`ConfigStore` flattens grouped TOML entries back into the flat runtime
`Config.t`.

## Save Behavior

`ConfigStore.save` is the only persistence path.

When saving:

1. write the new file to `config.tmp`
2. if `config` already exists, move it to `config.bak`
3. rename `config.tmp` to `config`

Any older `.bak` is replaced.

This means backup rotation is not migration-specific. It happens on any
overwrite.

## Persistence Policy

Config persistence policy is split between `ConfigRuntime.resolve` and
`GdfuseFlow.resolve_runtime_config`.

Important current rules:

- `-id` and `-secret` persist
- other CLI overrides are runtime-only
- `Created`, `Migrated`, and `Upgraded` config loads force a save even if the
  persisted values did not otherwise change
- docs-mode changes affect runtime config and may request cache clear
- normal startup does not rewrite the config file unless:
  - it was created
  - it was migrated
  - it was upgraded
  - persisted values actually changed

That is what preserves comments in steady-state operation.

## Validation

Validation is centralized in `Config.validate`.

It currently enforces:

- `max_upload_chunk_size > 0`
- `memory_buffer_size >= 131072`
- `max_memory_cache_size >= memory_buffer_size`

Validation errors are surfaced through `ConfigStore.Parse_error` during config
load.

## Current Versioning Model

The persisted format currently uses:

- `config_version = 1`

Explicit upgrade hooks exist in `ConfigStore.upgrade_config`.

Current behavior:

- version `0` means unversioned TOML and is upgraded to `1`
- version `1` is current
- higher versions are rejected

## What To Read Before Editing

- `src/configStore.ml`
- `src/configRuntime.ml`
- `src/config.ml`
- `src/gdfuseCommon.ml`
- `src/gdfuseFlow.ml`
- `src/context.ml`
- `test/testConfigStore.ml`
- `test/testConfigRuntime.ml`
- `test/testGdfuseFlow.ml`

## Common Change Risks

- adding a new config key but forgetting to register it in TOML grouping
- adding a new key but forgetting `known_keys` validation coverage
- assuming `ConfigStore.load_or_create` writes the config file immediately when
  it only reports `Created`/`Migrated`/`Upgraded`
- changing CLI persistence policy in `GdfuseFlow.resolve_runtime_config`
  instead of `ConfigRuntime.resolve`, or vice versa
- changing save behavior and breaking comment preservation
- changing versioned upgrade behavior without updating tests
