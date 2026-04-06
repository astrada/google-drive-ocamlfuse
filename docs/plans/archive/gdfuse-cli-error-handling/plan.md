# Plan: Restore CLI Help/Error Handling And Expand CLI Tests

## Summary

The `gdfuse` readability/testability work moved CLI parsing into
`GdfuseCli.parse_argv`, which uses `Arg.parse_argv` instead of the old
executable-local `Arg.parse`.

That changed one important behavior contract:

- `Arg.parse` handled `-help`, `--help`, and invalid options internally by
  printing a message and terminating cleanly
- `Arg.parse_argv` raises `Arg.Help` and `Arg.Bad`

`bin/gdfuse.ml` currently calls `GdfuseCli.parse ()` before entering its
protected execution branch, so those parser exceptions escape as uncaught
exceptions instead of producing normal CLI output.

The fix should restore clean CLI behavior for version, help, and parse
failures without giving up the testability benefit of `parse_argv`. It should
also expand test coverage so parser outcomes and top-level CLI dispatch are
both exercised.

## Goals

- restore clean handling for `-help` and `--help`
- make `-version` part of the same explicit top-level CLI outcome model
- restore clean handling for unexpected options and other parse-time CLI errors
- keep successful parsing behavior unchanged for existing flags and mount-mode
  dispatch
- add tests for parser error/help paths, not only successful parses
- add tests for the top-level CLI behavior that decides whether to print,
  exit, or invoke `GdfuseFlow`

## Non-Goals

- changing CLI flag names, defaults, or mount-option semantics
- changing startup/auth/cache/FUSE behavior in `GdfuseFlow`
- broad refactoring of application flow beyond what is needed to make the CLI
  behavior testable and stable

## Root Cause To Fix

The current regression is specifically this boundary mismatch:

- `GdfuseCli.parse_argv` is a testable wrapper around `Arg.parse_argv`
- `Arg.parse_argv` raises `Arg.Help` / `Arg.Bad`
- `bin/gdfuse.ml` only catches exceptions after parsing has already completed

The implementation should make top-level CLI outcomes explicit at the library
level instead of relying on uncaught exceptions or ad hoc booleans to
communicate version/help/error behavior.

## Scope

In scope:

- `src/gdfuseCli.ml`
- `src/gdfuseCli.mli`
- a small library-visible top-level CLI runner if needed for testability
- `bin/gdfuse.ml`
- `test/testGdfuseCli.ml`
- a new test module for top-level CLI behavior, if the implementation adds a
  testable runner
- `test/testSuite.ml`
- `docs/agent-docs/application-flow.md` and `docs/agent-docs/repo-map.md` if
  the executable wiring changes materially

Out of scope:

- `GdfuseFlow` startup/shutdown logic
- FUSE callback behavior
- config/runtime merge semantics
- auth flows and cache lifecycle behavior

## Proposed Implementation

### 1. Add An Explicit CLI Outcome Type

Add an explicit outcome type in `GdfuseCli` so CLI parsing can describe
user-visible top-level outcomes without throwing raw `Arg` exceptions across
the executable boundary or carrying version handling in a separate boolean.

The outcome should distinguish at least:

- successful parse
- version display
- help text to print
- CLI error text to print

Representative shape:

```ocaml
type parse_result =
  | Parsed of parsed
  | Show_version
  | Help of string
  | Error of string
```

or equivalently a renamed type such as `cli_outcome` if implementation finds
that name clearer.

The safe parser should:

- wrap `Arg.parse_argv`
- catch `Arg.Help`
- catch `Arg.Bad`
- translate `-version` into `Show_version`
- route parser-local failures such as invalid `gdfroot=...` mount-option
  syntax into the same error channel instead of leaving them as uncaught
  `Failure`

The successful parse result should stop carrying `show_version`.

Representative shape:

```ocaml
type parsed = {
  mount_requested : bool;
  params : GdfuseCommon.application_params;
  fuse_args : string list;
}
```

That keeps `Parsed of parsed` aligned with one meaning: "ready to run the
flow".

### 2. Add A Testable CLI Runner Above Parsing

Introduce a small library-visible runner, preferably in `src/`, so the
top-level `gdfuse` behavior can be unit tested without executing the real
binary.

This helper should own:

- calling the explicit CLI parser
- printing help text
- printing version text
- printing clean CLI error text
- dispatching to bootstrap-only or mount mode on success
- choosing exit codes for help and parse failures

Possible location:

- `src/gdfuseApp.ml`
- `src/gdfuseApp.mli`

Keep the dependency boundary small and semantic. It only needs enough
injection to test user-visible CLI behavior:

- stdout printing
- stderr printing
- exit
- `GdfuseFlow` bootstrap/mount calls

This keeps `bin/gdfuse.ml` thin while making the previously untestable
top-level branch behavior observable from `test/`.

### 3. Keep `bin/gdfuse.ml` Thin

After the explicit CLI outcome and CLI runner exist, `bin/gdfuse.ml` should
only:

1. instantiate the real flow module
2. call the library-visible CLI runner
3. keep the existing production error formatting for runtime failures

The executable should stop owning parser-exception behavior directly.

### 4. Preserve Existing Successful CLI Semantics

The implementation should keep these behaviors intact:

- foreground mode is still forced
- `-debug` still implies verbose logging
- `-s` and `-m` keep their current threading behavior
- `-o gdfroot=...` still updates `base_dir`
- non-`gdfroot` mount options are still forwarded into FUSE argv
- no mountpoint still means bootstrap-only mode
- a mountpoint still means full mount mode
- `-version` still prints version information and skips flow execution

## Test Plan

### Extend `test/testGdfuseCli.ml`

Add parser-focused tests for:

- `-help` returns a help outcome instead of raising
- `--help` returns the same help outcome class
- `-version` returns `Show_version` instead of producing a `Parsed` record
- an unexpected option returns an error outcome instead of raising
- invalid `-o gdfroot` syntax returns a clean error outcome
- `-d` still forwards FUSE debug mode
- `-s` still disables app-level multithreading and forwards `-s`
- `-m` still keeps the app-level multithreading flag without changing forced
  foreground argv
- no mountpoint vs mountpoint is still classified correctly

These tests should continue to reset any process-global CLI refs such as
`Utils.verbose`.

### Add A Top-Level CLI Behavior Test Module

Add a new test module, preferably `test/testGdfuseApp.ml`, that exercises the
library-visible CLI runner with fake printing/exit/flow dependencies.

Scenarios to cover:

- `-help` prints help text, exits successfully, and does not call flow
- `--help` behaves the same way
- an unexpected option prints a clean CLI error and does not call flow
- `-version` prints version information and does not call flow
- a valid mount invocation calls mount mode with the parsed params/fuse args
- a valid no-mount invocation calls bootstrap-only mode
- a runtime `Failure` from flow still becomes the current clean `Error: ...`
  stderr path

Use the same style as `test/testGdfuseFlow.ml`: fake deps with an event trace
plus a dedicated exception for intercepted `exit`.

### Register The New Tests

- add the new module to `test/testSuite.ml`
- keep the tests serial, since CLI parsing mutates process-global refs such as
  `Utils.verbose`

## Behavior Constraints

- Help and parse errors must not surface as uncaught OCaml exceptions.
- Version/help/error should all be represented by the same explicit CLI
  outcome layer.
- Help and parse errors should preserve `Arg`-style wording and usage text as
  closely as practical.
- Successful CLI invocations should keep the same parsed params and FUSE argv
  as before this fix.
- The change should not alter `GdfuseFlow` orchestration contracts.

## Assumptions

- The preferred direction is to keep CLI parsing testable from `src/`, not to
  move behavior back into an executable-private `let () =` block.
- A small `GdfuseApp`-style runner in `src/` is worth the extra module if it
  lets tests cover the user-visible CLI entry behavior directly.
- If exact exit codes for help and parse errors are not already documented,
  implementation should choose one consistent policy and lock it down with
  tests rather than leaving it implicit.
