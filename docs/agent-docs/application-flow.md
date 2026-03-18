# Application Flow (`bin/gdfuse*.ml`)

## Purpose

The `gdfuse` executable is split across a small set of files under `bin/`:

- `bin/gdfuse.ml`: top-level entrypoint and mode dispatch
- `bin/gdfuseCli.ml`: CLI parsing and `application_params` construction
- `bin/gdfuseSetup.ml`: config/state/bootstrap, auth, and `Context` setup
- `bin/gdfuseFuse.ml`: FUSE callback wrappers and exception mapping
- `bin/gdfuseShutdown.ml`: `at_exit` cleanup
- `bin/gdfuseCommon.ml`: shared constants, request-id generation, config/state
  helpers

The executable does not implement filesystem semantics itself.
Mounted operations are delegated to `Drive`.

## Flow At A Glance

1. Parse CLI options and mount options in `GdfuseCli.parse`.
2. If `-version` is set, print version information and exit.
3. Build an `application_params` record.
4. If no mountpoint was passed, run `GdfuseSetup.setup_application` with `"."`
   only and
   return.
5. Otherwise run `GdfuseSetup.setup_application`.
6. Register `GdfuseShutdown.shutdown` via `at_exit`.
7. Enter `GdfuseFuse.start_filesystem`.
8. Let FUSE callbacks forward requests to `Drive`.

The "no mountpoint" branch is important: it performs setup and authorization
without starting the FUSE loop. That is effectively a bootstrap/auth-only mode.

## Module Responsibilities

### `bin/gdfuse.ml`

The entrypoint is intentionally thin:

- `quit_with_error`
- `run_bootstrap_only`
- `run_mount_mode`
- `let () = ...` dispatch

See [bin/gdfuse.ml](/home/alex/src/google-drive-ocamlfuse/bin/gdfuse.ml#L1).

### `bin/gdfuseCli.ml`

`GdfuseCli.parse` owns all command-line key parsing behavior:

- `fuse_args` starts with `-f` and `-obig_writes`
- foreground mode is forced unconditionally
- this is intentional: on OCaml 5+, calling `fork()` after `caml_main()` is not
  safe, so the old daemonizing path should not be relied on
- there is no separate `-f` CLI option anymore
- `-debug` implies `Utils.verbose := true`
- `-s` forces single-threaded FUSE and clears app-level `multi_threading`
- `-m` only affects app-level `multi_threading`
- `-o` is split by commas
- `gdfroot=...` is handled specially and stored in `base_dir`
- all remaining mount options are forwarded into the FUSE argv

See [bin/gdfuseCli.ml](/home/alex/src/google-drive-ocamlfuse/bin/gdfuseCli.ml#L15).

## `GdfuseSetup.setup_application`

`setup_application` lives in
[bin/gdfuseSetup.ml](/home/alex/src/google-drive-ocamlfuse/bin/gdfuseSetup.ml#L266).
It performs all runtime bootstrap before FUSE starts.

### 1. Validate mountpoint

The function immediately checks that `params.mountpoint` exists and is a
directory. Failure raises `Failure`, which is converted by the outer `try` into
`Error: ...` on stderr.

### 2. Prepare the authorization helper

`get_auth_tokens_from_server` is a top-level helper in `GdfuseSetup` and is
used only for legacy GAE-proxy auth mode.

Its sequence is:

1. Read the current global context with `Context.get_ctx ()`.
2. Reuse the stored request id if present; otherwise generate a new one with
   `generate_request_id`.
3. Persist that request id back into the state file.
4. Build the browser authorization URL with `get_authorization_url`.
5. Launch the browser.
6. Poll `GaeProxy` for the resulting auth tokens.

If `GaeProxy.start_server_polling ()` fails with `GaeProxy.ServerError`, the
request id is cleared from state before the process exits.

See:

- [bin/gdfuseSetup.ml](/home/alex/src/google-drive-ocamlfuse/bin/gdfuseSetup.ml#L8)

### 3. Resolve config location and load persisted config

Bootstrap then moves through the filesystem/config layer:

1. `AppDir.get_config_path` resolves the actual config path and whether XDG
   mode is active.
2. `get_config_store` calls `ConfigStore.load_or_create`.
3. `AppDir.create` derives the application directory layout from the loaded
   config plus CLI inputs.
4. `AppDir.create_directories` ensures those directories exist.
5. Logging is redirected through `Utils.open_log_out_ch`.

At this point logging is fully switched to the configured destination.

See:

- [bin/gdfuseSetup.ml](/home/alex/src/google-drive-ocamlfuse/bin/gdfuseSetup.ml#L30)

### 4. Resolve runtime config and optionally persist upgrades

`ConfigRuntime.resolve` merges:

- persisted config
- config migration/load state
- CLI overrides
- mode flags such as `device` and `multi_threading`

It returns:

- `runtime_config`: the config used for this process
- `persisted_config`: the config that should be saved back to disk
- `should_persist`: whether persistence is required
- `clear_cache`: whether runtime changes require cache invalidation

The code persists `persisted_config` when:

- the config was just created, migrated, or upgraded, or
- `ConfigRuntime.resolve` requested persistence

Then it stores the runtime-only config into the in-memory `Context` copy.

See:

- [bin/gdfuseSetup.ml](/home/alex/src/google-drive-ocamlfuse/bin/gdfuseSetup.ml#L59)

### 5. Build GAPI auth config

`Config.create_gapi_config` builds the base Google API config.

There is one important patch-up step: in GAE-proxy mode, the code rewrites the
OAuth config so `refresh_access_token` points at
`gae_proxy_refresh_access_token`.

That means the rest of the code can keep using the normal GAPI auth pipeline
while the actual refresh behavior is replaced for this mode.

See:

- [bin/gdfuseSetup.ml](/home/alex/src/google-drive-ocamlfuse/bin/gdfuseSetup.ml#L103)
- [bin/gdfuseSetup.ml](/home/alex/src/google-drive-ocamlfuse/bin/gdfuseSetup.ml#L107)

### 6. Load state and initialize cache

State/cache bootstrap happens next:

1. `get_state_store` loads the state file or creates a new one.
2. `Cache.create_cache` creates the cache handle.
3. `clear_cache` is computed from:
   - `ConfigRuntime.clear_cache`, usually due to docs-mode changes
   - the explicit CLI `-cc` flag
4. If cache clearing is needed, `Cache.clean_up_cache` runs immediately.
5. If the saved app version differs from `Config.version`, the cache is also
   cleared and the saved version is updated in state.
6. `Cache.setup_db` initializes the cache database.
7. `GapiCurl.global_init` initializes libcurl state.
8. `Buffering.MemoryBuffers.create` initializes the shared memory-buffer pool.

See:

- [bin/gdfuseSetup.ml](/home/alex/src/google-drive-ocamlfuse/bin/gdfuseSetup.ml#L165)

### 7. Install global `Context`

The code constructs a `Context.t` record with:

- app-dir paths
- runtime config store
- state store
- cache handle
- gapi/curl state
- mountpoint metadata
- thread slots initialized to `None`
- memory buffers
- per-file lock table
- `skip_trash`

Then it installs that record globally with `Context.set_ctx`.

This is the point after which the rest of the process can assume a valid global
context exists.

See:

- [bin/gdfuseSetup.ml](/home/alex/src/google-drive-ocamlfuse/bin/gdfuseSetup.ml#L186)
- [bin/gdfuseSetup.ml](/home/alex/src/google-drive-ocamlfuse/bin/gdfuseSetup.ml#L280)

### 8. Recover from dirty shutdown

Immediately after installing the context, the code checks
`DbCache.check_clean_shutdown`.

- If the previous run shut down cleanly, it resets the clean-shutdown flag for
  this new session with `DbCache.reset_clean_shutdown`.
- If not, it logs the dirty shutdown and rebuilds the cache database unless the
  cache was already being cleared.

This keeps stale metadata from surviving a crash or forced unmount.

See:

- [bin/gdfuseSetup.ml](/home/alex/src/google-drive-ocamlfuse/bin/gdfuseSetup.ml#L210)

### 9. Acquire credentials

Credential bootstrap is split into three branches:

1. Service account mode:
   - no interactive auth
   - optionally log the impersonated user
2. User OAuth with an existing refresh token:
   - log that the refresh token is already present
3. User OAuth without a refresh token:
   - GAE-proxy mode: `get_auth_tokens_from_server ()`
   - explicit client id/secret: `Oauth2.get_access_token headless device ...`
   - missing client id/secret: fail with a mode-specific message

After this branch, the code always calls `refresh_access_token ()` once as a
validation step:

- GAE-proxy mode: `gae_proxy_refresh_access_token`
- regular OAuth mode: `Oauth2.refresh_access_token`
- service account mode: no-op

If refresh fails, startup aborts immediately.

See:

- [bin/gdfuseSetup.ml](/home/alex/src/google-drive-ocamlfuse/bin/gdfuseSetup.ml#L222)
- [bin/gdfuseSetup.ml](/home/alex/src/google-drive-ocamlfuse/bin/gdfuseSetup.ml#L247)

## FUSE Boundary

The FUSE adapter layer lives in
[bin/gdfuseFuse.ml](/home/alex/src/google-drive-ocamlfuse/bin/gdfuseFuse.ml#L1).

### Exception mapping

`handle_exception` converts repository-specific exceptions into Unix/FUSE
errors. Important mappings:

- `Drive.File_not_found` -> `ENOENT`
- `Drive.Permission_denied` -> `EACCES`
- `Drive.Directory_not_empty` -> `ENOTEMPTY`
- `Drive.IO_error` -> `EIO`
- `Drive.Existing_attribute` -> `EEXIST`
- `Drive.Invalid_operation` -> `EINVAL`

Unrecognized exceptions are logged and converted to `EIO`.

### Callback wrappers

Each callback has the same shape:

1. log the request
2. call the corresponding `Drive` function
3. translate any exception through `handle_exception`

Examples:

- `getattr` -> `Drive.get_attr`
- `readdir` -> `Drive.read_dir`, then prepend `"."` and `".."` locally
- `fopen` -> `Drive.fopen`
- `read` -> `Drive.read`
- `write` -> `Drive.write`
- `flush` -> `Drive.flush`
- `fsync` -> `Drive.fsync`

Two callbacks are effectively no-ops in this file and only log:

- `releasedir`
- `fsyncdir`

### Hand-off to `Fuse.main`

`start_filesystem` in `GdfuseFuse` builds the argv passed to FUSE as:

- executable name
- accumulated `fuse_args`
- mountpoint

Then it installs the callback table and enters `Fuse.main`.

See:

- [bin/gdfuseFuse.ml](/home/alex/src/google-drive-ocamlfuse/bin/gdfuseFuse.ml#L172)

## Shutdown Flow

The cleanup handler is registered only in the mounted-filesystem path in
`bin/gdfuse.ml`, but the actual logic lives in `GdfuseShutdown.shutdown`.

On process exit it:

1. reads the current global context
2. stops and joins `buffer_eviction_thread`, if any
3. stops and joins `flush_db_thread`, if any
4. stops and joins `async_upload_thread`, if any
5. stops and joins `folder_fetching_thread`, if any
6. flushes cache state with `Cache.flush`
7. stores the clean-shutdown flag with `DbCache.set_clean_shutdown`
8. runs `GapiCurl.global_cleanup`
9. clears the global context with `Context.clear_ctx`

See:

- [bin/gdfuse.ml](/home/alex/src/google-drive-ocamlfuse/bin/gdfuse.ml#L8)
- [bin/gdfuseShutdown.ml](/home/alex/src/google-drive-ocamlfuse/bin/gdfuseShutdown.ml#L62)

## Practical Reading Guide

When changing startup behavior, read these sections in order:

1. `let () =` main block
2. `GdfuseCli.parse`
3. `GdfuseSetup.setup_application`
4. `GdfuseFuse.start_filesystem`
5. `Drive.init_filesystem` in `src/drive.ml`

When debugging auth issues, focus on:

- `get_authorization_url`
- `get_auth_tokens_from_server`
- the credential branch inside `GdfuseSetup.setup_application`
- `src/oauth2.ml`
- `src/gaeProxy.ml`

When debugging mount/runtime issues, focus on:

- context creation in `setup_application`
- shutdown behavior in `GdfuseShutdown`
- dirty-shutdown recovery
- `handle_exception`
- the specific `Drive.*` callback being exercised
