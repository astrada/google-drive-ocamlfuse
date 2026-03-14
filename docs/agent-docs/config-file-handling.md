# Configuration File Handling

## Current Design

Config handling no longer uses `KeyValueStore.MakeFileStore(Config)`.

The current implementation is split across:

- `src/configStore.ml`
- `src/configRuntime.ml`
- `src/config.ml`
- `src/appDir.ml`
- `bin/gdfuse.ml`

The state file still uses the old key/value store. Only config moved to the new
path.

## Persistent Format

The config filename remains `config`.

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

At startup:

1. `AppDir.get_config_path` resolves the path.
2. `ConfigStore.load_or_create` decides whether the file is:
   - missing
   - legacy `key=value`
   - TOML
3. Legacy files are parsed, migrated to TOML, and rewritten.
4. TOML files are parsed and optionally upgraded through explicit
   `config_version` hooks.
5. The resulting `Config.t` is passed into `ConfigRuntime.resolve`, which
   computes:
   - `runtime_config`
   - `persisted_config`
   - `should_persist`
   - `clear_cache`

## Legacy Migration

Legacy `key=value` files are still supported on startup, but only as an input
format for migration.

Current migration behavior:

- duplicate keys are rejected
- unknown keys are rejected
- missing keys fall back to defaults
- successful migration rewrites the file as TOML
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

Config persistence policy is now encoded in `ConfigRuntime.resolve`.

Important current rules:

- `-id` and `-secret` persist
- other CLI overrides are runtime-only
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
- `bin/gdfuse.ml`
- `test/testConfigStore.ml`
- `test/testConfigRuntime.ml`

## Common Change Risks

- adding a new config key but forgetting to register it in TOML grouping
- adding a new key but forgetting `known_keys` validation coverage
- changing CLI persistence policy in `gdfuse.ml` instead of `ConfigRuntime`
- changing save behavior and breaking comment preservation
- changing versioned upgrade behavior without updating tests
