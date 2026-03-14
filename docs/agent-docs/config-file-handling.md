# Configuration File Handling

## Why This Doc Exists

This file is a deep dive into the current configuration-file subsystem, aimed
at refactoring work. It focuses on:

- how the config file is located
- how it is parsed and serialized
- what behavior is implicit in the current implementation
- which parts are brittle
- what compatibility constraints matter if the format becomes more readable

The implementation is split across:

- `bin/gdfuse.ml`
- `src/appDir.ml`
- `src/context.ml`
- `src/keyValueStore.ml`
- `src/config.ml`

## Current Storage Model

Configuration uses a generic file-store abstraction:

- `Context.ConfigFileStore = KeyValueStore.MakeFileStore(Config)`

This means the config subsystem is structurally:

1. locate config path
2. load file into a `(string, string) Hashtbl.t`
3. convert table to `Config.t`
4. mutate `Config.t` in memory during startup
5. serialize `Config.t` back to a `(string, string) Hashtbl.t`
6. rewrite the config file

The state file uses the same mechanism, but this doc is only about config.

## Config File Location

Path resolution is handled by `AppDir.get_config_path`.

Priority is:

1. explicit `-config <path>`
2. XDG config path if `-xdgbd` is set
3. existing XDG config path if it already exists
4. legacy `~/.gdfuse/<label>/config`

Important detail:

- the decision is made before reading config contents
- the `xdg_base_directory` field is not read from the config file to decide
  where the config file itself lives

That matches the user docs, but the real rule is in `src/appDir.ml`, not in the
configuration parser.

## Config File Creation

If the config file does not exist, `bin/gdfuse.ml` creates it via
`create_default_config_store`.

The initial contents come from:

- `Config.default`
- or `Config.default_debug` if `-debug` was passed

This creation path immediately saves a fully materialized config file.

Implications:

- the config file is always generated from code defaults
- there is no preserved template file
- comments and user formatting are impossible in the current design

## Parse Format

The file parser is implemented in `KeyValueStore.MakeFileStore.load`.

It expects each line to match:

```text
key=value
```

with a `Scanf.bscanf` pattern:

```ocaml
"%s@=%s@\n"
```

The parser then trims both key and value before adding them to a hash table.

### What The Parser Accepts

- one key/value pair per line
- leading/trailing whitespace around key or value, if the scanner still
  matches the line
- empty values such as `client_id=`

### What The Parser Does Not Accept Reliably

- comments
- blank lines
- section headers
- inline comments
- quoting rules
- multi-line values
- escaped `=` semantics

The wiki says the parser is simple; the code is stricter than many users would
expect.

## Duplicate Keys

During load, entries are inserted with `Hashtbl.add`, not `Hashtbl.replace`.

Later, values are read with `Hashtbl.find` through `Utils.get_from_string_table`.

Practical consequence:

- if the file contains duplicate keys, the first inserted value wins
- later duplicates are silently ignored by normal lookups

That is easy to miss, because many config systems use “last key wins”.

A refactor should decide whether to preserve this behavior or intentionally
change it.

## Missing Keys

`Config.of_table` uses:

- `Utils.get_from_string_table table key conv default`

If the key is missing, the field falls back to the corresponding field in
`Config.default`.

This gives the current format an implicit schema evolution property:

- newly added fields do not require an existing config file to be updated
- old config files keep working as long as old keys are still recognized

## Type Conversion And Failure Modes

`Config.of_table` is a long direct mapping from string keys to typed fields.

Conversions are performed with functions like:

- `bool_of_string`
- `int_of_string`
- `Int64.of_string`
- `Std.identity`

There is no error accumulation and no location-aware error reporting.

Current behavior on bad input:

- parsing or conversion raises an exception
- that exception propagates up to `bin/gdfuse.ml`
- the user eventually gets a generic `Error: ...` message

Examples of brittle cases:

- `umask` is written as `0o002`, but parsed with `int_of_string`
- invalid booleans fail hard
- malformed lines fail hard in `Scanf`
- there is no file/line/column context in the error

The `umask` case is especially important in a refactor because the current
writer emits OCaml-style octal syntax.

## Serialization Format

`Config.to_table` writes every field from `Config.t` into a hash table.

`KeyValueStore.MakeFileStore.save` then:

1. collects the table into a list
2. sorts entries by key
3. writes every line as `key=value`

Important consequences:

- keys are always rewritten in sorted order
- fields with empty-string values are still emitted
- any original file order is lost
- any original spacing is lost
- comments cannot be preserved
- unknown keys are dropped

Because the in-memory representation is only `Config.t`, there is no AST or
lossless parse tree to round-trip formatting.

## Startup Rewrites The Config File

The config file is not only written when it is first created.

`setup_application` in `bin/gdfuse.ml`:

1. loads config from disk
2. overlays selected CLI/runtime values
3. builds a new `Config.t`
4. saves the updated config file back to disk

Fields that may be rewritten from CLI/runtime inputs include:

- `client_id`
- `client_secret`
- `service_account_credentials_path`
- `service_account_user_to_impersonate`
- `log_to`
- `scope`
- `redirect_uri`
- `oauth2_loopback_port`
- document export settings through `-docsmode`

This matters for a readability refactor because even if comments become
parsable, they will still be destroyed unless save behavior also changes.

## Runtime Validation Outside The Parser

Some config correctness checks happen after parsing, in `setup_application`,
not in `Config.of_table`.

Examples:

- `max_upload_chunk_size > 0`
- `memory_buffer_size >= 131072`
- `max_memory_cache_size >= memory_buffer_size`

So the current config pipeline has two validation layers:

1. parse-time type conversion
2. startup-time semantic validation

A refactor should decide whether to keep this split or centralize validation.

## Interaction With Defaults

There are two default records:

- `Config.default`
- `Config.default_debug`

`default_debug` changes at least one operational value:

- `large_file_threshold_mb = 1` instead of `16`

This only matters when the config file is created from scratch while `-debug`
is on. After that, the saved file becomes the source of truth.

That is subtle and worth preserving or removing intentionally.

## Interaction With Documentation

There is already some drift between documentation and code.

Examples:

- the wiki says `oauth2_loopback=false` as the sample default for a later
  section, while `Config.default` currently sets `oauth2_loopback = true`
- the wiki describes a “simple parser”, but the actual scanner constraints are
  stricter than most readers will infer

A config-format refactor should treat code as authoritative and update docs in
the same change.

## Unknown Keys

Unknown keys currently have this behavior:

- they are accepted into the raw table
- `Config.of_table` ignores them because it only reads known keys
- after the next save, they disappear

That means the current system does not preserve forward-compatible extensions
or user annotations encoded as fake keys.

## Separation Of Concerns Today

The current implementation mixes three concerns:

### 1. Generic file storage

`src/keyValueStore.ml`

- line-oriented file parsing
- bare key/value persistence

### 2. Config schema

`src/config.ml`

- `Config.t`
- defaults
- field-by-field parse and render logic
- GAPI config creation

### 3. Startup policy

`bin/gdfuse.ml`

- deciding when config is created
- deciding when config is rewritten
- deciding which CLI values persist into config

Any meaningful refactor likely needs to touch all three.

## Refactor Pressure Points

If the goal is “more readable config, comments, etc.”, the main blockers are:

### Lossy parse/save model

The current model converts file contents directly into `Config.t`.

That makes it impossible to preserve:

- comments
- blank lines
- section ordering
- user ordering
- unknown keys

### Parser rigidity

The current `Scanf` parser is a bad fit for a human-friendly config format.

It gives:

- weak diagnostics
- awkward grammar evolution
- poor tolerance for cosmetic formatting

### Implicit persistence policy

Even a better parser will not preserve comments if `setup_application`
continues to rewrite the full file on every startup-affecting change.

## Compatibility Constraints For A Refactor

A safe refactor should explicitly choose behavior for:

- existing `key=value` files
- duplicate keys
- blank lines
- comment syntax
- whitespace around separators
- unknown keys
- invalid keys
- save ordering
- whether save is lossless or canonicalizing
- whether CLI overrides persist to disk automatically

At minimum, old config files should remain readable.

## Recommended Refactor Shape

For this repository, the cleanest direction is likely:

1. replace `Scanf` parsing with a dedicated config parser
2. parse into an intermediate representation, not directly into `Config.t`
3. keep enough source structure to preserve comments and ordering
4. validate and convert that representation into `Config.t`
5. separate “load config” from “rewrite config”

That allows readable files without forcing full-file normalization on every
startup.

## Suggested Architecture

One practical shape would be:

- `ConfigSyntax`
  - line/section/token model
  - parser with line-aware errors
  - serializer / pretty-printer
- `ConfigDecode`
  - converts syntax entries into `Config.t`
  - performs typed field decoding
  - reports unknown/invalid keys explicitly
- `ConfigEncode`
  - updates or emits config entries from `Config.t`
- existing `Config`
  - remains the typed runtime schema

This would let the code preserve comments while still producing a typed record
for the rest of the application.

## Specific Questions To Decide Before Implementation

1. Should saving preserve comments and unknown keys, or normalize the file?
2. Should duplicate keys become an error, first-wins, or last-wins?
3. Should CLI-provided credentials still be persisted automatically?
4. Should there be separate commands or flags for “write defaults” vs “use
   runtime override only”?
5. Should parse warnings be tolerated for unknown keys, or fail fast?
6. Should config formatting become grouped/sectioned instead of sorted
   alphabetically?

Without answering these, a refactor will drift into accidental behavior
changes.

## High-Risk Areas During Refactor

- preserving compatibility with existing config files
- not breaking startup for users with hand-edited files
- handling `umask` and numeric fields consistently
- not rewriting comments away during setup
- keeping `Config.default` / `default_debug` semantics clear
- keeping docs in sync with code defaults

## Source Reading Checklist

Read these files before changing the config subsystem:

- `src/keyValueStore.ml`
- `src/config.ml`
- `src/appDir.ml`
- `src/context.ml`
- `bin/gdfuse.ml`
- `docs/wiki/Configuration.md`

If the refactor changes save semantics, also review any code path that calls:

- `Context.save_config_store`

Currently that is mostly driven by setup, so startup behavior is the real
policy surface.
