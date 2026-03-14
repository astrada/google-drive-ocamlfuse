# OToml Configuration Migration Plan

## Goal

Replace the current ad hoc `key=value` configuration format with a TOML-based
format using `otoml`, while keeping startup safe for existing users and adding
dedicated automated tests for config behaviors.

This plan is based on the current implementation in:

- `bin/gdfuse.ml`
- `src/appDir.ml`
- `src/context.ml`
- `src/keyValueStore.ml`
- `src/config.ml`

## Requirements

1. The old format should be migrated on startup.
2. Full config regeneration at startup is not required; a more solid migration
   strategy is preferred.
3. The new format should allow comments.
4. A dedicated test suite should cover the relevant behavior.

## Settled Decisions

The following design choices are now fixed:

1. Keep the filename `config`.
2. Back up legacy config automatically during migration.
3. Persist CLI overrides for `-id` and `-secret`.
4. Reject duplicate keys.
5. Emit a minimal TOML file rather than a large commented full dump.

## High-Level Direction

Use `otoml` as the parser/serializer for the new config format and stop
modeling config persistence as a generic `Hashtbl<string, string>` store.

The important design change is:

- parsing becomes syntax-aware and typed
- migration becomes explicit
- save behavior becomes policy-driven instead of “always rewrite sorted keys”

## Proposed End State

### Format

Use TOML for config files.

Suggested default path stays the same:

- `~/.gdfuse/<label>/config`
- or XDG config path when applicable

This avoids mixing “format migration” with “path migration”.

The filename remains `config`. There is no path-level rename to `config.toml`.

### Loading Rules

At startup:

1. detect whether the file is legacy `key=value` or TOML
2. parse accordingly
3. migrate legacy config into `Config.t`
4. write TOML back to disk once migration succeeds
5. continue using the typed `Config.t` in memory

For TOML files:

- parse directly with `otoml`
- decode into `Config.t`
- do not rewrite the file automatically unless an explicit migration or repair
  step is required

### Saving Rules

Do not perform unconditional full regeneration on every startup.

Instead:

- write the file when it is first created
- write the file after successful legacy-to-TOML migration
- write the file when a deliberate config-upgrade step needs to materialize new
  keys or rewrite deprecated keys
- avoid writing the file for ephemeral runtime-only overrides unless that
  persistence is intentional

This is the key policy change that preserves comments in normal operation.

Settled override policy:

- `-id` and `-secret` remain persisted
- other CLI overrides should be treated as runtime-only unless a later decision
  explicitly says otherwise

## Proposed Implementation Phases

## Phase 1: Introduce TOML Dependency

### Changes

- add `otoml` to `google-drive-ocamlfuse.opam`
- add `otoml` to the library dependencies in `src/dune`

### Outcome

The project can parse and emit TOML without changing behavior yet.

## Phase 2: Split Config Persistence From Generic Key/Value Store

### Changes

Introduce a dedicated config persistence module, for example:

- `src/configStore.ml`
- `src/configStore.mli`

Responsibilities:

- locate and load config files
- detect file format
- parse legacy format
- parse TOML format
- decode into `Config.t`
- migrate legacy files to TOML
- save TOML files

### Why

`KeyValueStore.MakeFileStore(Config)` is the wrong abstraction for a
comment-friendly config format because it only supports lossy
`Hashtbl<string, string>` round-tripping.

The generic file store can remain for `State`, but config should stop using it.

### Outcome

Config becomes a first-class subsystem with explicit format handling.

## Phase 3: Add TOML Schema Mapping

### Changes

Implement a TOML decoder/encoder for `Config.t`.

Recommended shape:

- keep `Config.t` as the runtime schema
- add helpers in `Config` or `ConfigStore` for:
  - `of_toml`
  - `to_toml`
  - `validate`

Suggested TOML layout:

```toml
metadata_cache_time = 60
read_only = false
umask = "0o002"
sqlite3_busy_timeout = 5000

[docs]
download_docs = true
document_format = "desktop"
drawing_format = "desktop"
presentation_format = "desktop"
spreadsheet_format = "desktop"

[auth]
client_id = ""
client_secret = ""
verification_code = ""
scope = ""
redirect_uri = ""
oauth2_loopback = true
oauth2_loopback_port = 8080

[cache]
max_cache_size_mb = 512
metadata_memory_cache = true
metadata_memory_cache_saving_interval = 30
write_buffers = false
```

The exact grouping is flexible, but the emitted file should stay minimal. The
serializer should avoid dumping every possible option by default.

### Compatibility Note

The runtime `Config.t` can stay flat even if the TOML file is grouped into
tables. The grouping is a persistence concern, not a runtime-schema
requirement.

## Phase 4: Implement Legacy Format Reader And Startup Migration

### Changes

Add a legacy parser that reproduces current behavior closely enough to read
existing files.

Recommended policy:

- detect legacy format by attempting TOML parse first, then legacy parse
- if TOML parse succeeds, use TOML
- if TOML parse fails but legacy parse succeeds, load legacy config, write TOML
  atomically, and continue
- if both fail, report a config parse error with file path and reason

### Migration Write Policy

For legacy files:

- preserve the original file by renaming it to something like
  `config.legacy.bak` before writing TOML, or
- write TOML to `config`, and keep `config.bak`

This is now a fixed requirement: keep a backup of the legacy file
automatically.

### Compatibility Decisions To Make

- duplicate legacy keys: reject them during migration
- invalid legacy values: fail with a precise message rather than silently
  defaulting
- deprecated keys: optionally warn and rewrite to the canonical TOML shape

## Phase 5: Stop Rewriting Config On Normal Startup

### Changes

Refactor `setup_application` in `bin/gdfuse.ml` so config persistence is
intentional, not automatic.

Current behavior:

- load config
- overlay selected CLI/runtime values
- save config immediately

Proposed behavior:

- load config
- compute effective runtime config from:
  - persisted config
  - CLI overrides
  - mode-derived overrides like `-docsmode`
- only persist if:
  - config was newly created
  - legacy migration happened
  - a config-upgrade routine explicitly decided to rewrite it

### Why

If startup keeps rewriting the file, comments will still be destroyed even with
TOML.

### Special Case To Review

Persistence policy for CLI values:

- `client_id` and `client_secret` remain persisted
- `service_account_credentials_path`, `service_account_user_to_impersonate`,
  `scope`, `redirect_uri`, `oauth2_loopback_port`, and `-docsmode` derived
  settings should be treated as runtime overrides unless a later migration
  policy explicitly persists them

## Phase 6: Add Explicit Config Migration Versioning

### Changes

Add a lightweight config-format version field to TOML, for example:

```toml
config_version = 1
```

Use it only for persistence migration, not for app runtime behavior.

### Why

This allows future migrations without reintroducing “rewrite everything every
startup”.

### Migration Strategy

On load:

- if version is missing and file is legacy, run legacy migration
- if version is present but older, run a targeted upgrade function
- if version is current, load without rewrite

## Phase 7: Add Dedicated Config Tests

### New Test Module

Add a dedicated test suite, for example:

- `test/testConfigStore.ml`

Hook it into:

- `test/testSuite.ml`
- `test/dune`

### Behaviors To Test

#### Legacy Loading

- parse minimal legacy file
- parse full legacy file
- missing keys fall back to defaults
- duplicate keys are rejected with a precise error
- malformed lines fail with a useful error

#### TOML Loading

- parse minimal TOML file
- parse grouped TOML tables
- comments are tolerated
- whitespace/layout variations do not change semantics
- invalid types fail with a useful error

#### Migration

- legacy file is migrated to TOML on startup load
- migrated config preserves semantic values
- backup file is created automatically
- startup does not remigrate already-migrated TOML

#### Save Policy

- new config creation writes TOML
- normal startup with existing TOML does not rewrite the file
- comment-preserving behavior is indirectly verified by checking unchanged file
  content when no migration/save is required

#### Validation

- semantic validations still fire for invalid combinations
- defaults match current intended behavior

## Phase 8: Update Documentation

### Changes

Update:

- `docs/wiki/Configuration.md`
- `docs/agent-docs/config-file-handling.md`

Document:

- TOML syntax
- comment support
- migration behavior
- whether CLI overrides persist or are runtime-only
- backup-file behavior during migration

## Detailed Design Recommendations

## Recommendation 1: Keep `Config.t` Flat

Do not refactor the runtime config record into nested records at the same time.

Reason:

- it increases the change surface
- the migration already touches parsing, persistence, startup, and tests

Keep the runtime representation stable; improve only the persistence format.

## Recommendation 2: Make Save Atomic

When writing migrated TOML:

1. write to a temp file in the same directory
2. `fsync` if desired
3. rename into place
4. keep a `.bak` copy for migrated legacy files

This is safer than direct overwrite.

## Recommendation 3: Centralize Validation

Keep one typed decode step and one semantic validation step.

A good split is:

- parser/decoder: types and presence
- validator: cross-field rules

That is clearer than spreading validation across startup code.

## Recommendation 4: Emit Minimal TOML

For new config creation, emit a minimal TOML file containing only the keys that
need to be materialized explicitly.

Comments should still be allowed in the format, but the generated file should
not be a long exhaustive template.

## Recommendation 5: Minimize User-Visible Behavior Changes In Phase 1

The safest first release is:

- legacy config still works
- it migrates once to TOML
- existing values preserve behavior
- startup stops rewriting on every run

Avoid changing semantics of config keys in the same change.

## Remaining Policy Surface

The main decisions are now settled. The remaining implementation choices are
smaller, such as:

1. exact TOML table grouping
2. exact backup filename convention
3. whether TOML unknown keys are warnings or hard errors

## Recommended Execution Order

1. Add `otoml` dependency.
2. Add `ConfigStore` with TOML load/save only.
3. Add tests for TOML parsing and save policy.
4. Add legacy parser and startup migration.
5. Refactor startup to stop unconditional rewrites.
6. Add config versioning and targeted upgrade hooks.
7. Update docs.

## Success Criteria

The migration is successful when all of these are true:

- existing legacy configs still start the application
- first startup migrates them to TOML
- duplicate keys are rejected with a precise error
- a legacy backup file is written automatically
- TOML config supports comments
- normal startup does not destroy comments
- `-id` and `-secret` still persist
- new options can be introduced through explicit migration logic, not full
  regeneration
- dedicated tests cover parsing, migration, save policy, and validation
