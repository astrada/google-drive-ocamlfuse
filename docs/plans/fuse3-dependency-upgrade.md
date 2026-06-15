# FUSE 3 Dependency Upgrade Plan

Status: planned; recommendations accepted.

## Goal

Upgrade `google-drive-ocamlfuse` from the libfuse 2 `ocamlfuse` binding to the
libfuse 3 `fuse3` binding, while using the application as a realistic
integration test for the upgraded binding.

The first implementation should keep the application behavior stable by using
`Fuse.Fuse_compat`. A later milestone can migrate the adapter to the native
FUSE 3 callback shape.

## Current State

- `google-drive-ocamlfuse.opam` depends on `ocamlfuse >= 2.7.2`.
- `src/dune` links the library with `ocamlfuse`.
- `bin/gdfuseFuse.ml` is the central FUSE adapter.
- The adapter currently uses the old callback shape directly through
  `Fuse.main` and `Fuse.default_operations`.
- `fuse3` keeps the OCaml top-level module name `Fuse`, but the public Dune
  and opam package is `fuse3`.
- `fuse3` provides `Fuse.Fuse_compat`, whose callback record matches the
  current adapter shape.
- `fuse3` defaults `Fuse.main` and `Fuse.Fuse_compat.main` to multithreaded
  libfuse loop mode.
- The application CLI already treats multithreaded mode as the default in help
  text, and `-s` remains the single-threaded opt-out.
- `ConfigRuntime.resolve` uses the application-level `multi_threading` flag to
  raise the SQLite busy timeout for multithreaded operation.
- The live e2e harness mounts the working-tree executable without `-s`, so it
  should exercise the multithreaded default after the upgrade.

## Accepted Decisions

### First Migration Surface

Use `Fuse.Fuse_compat` for the first dependency switch.

This keeps the initial diff small: package metadata changes, Dune dependency
changes, and FUSE adapter wiring should be enough to compile against `fuse3`.
It also separates binding integration risk from native FUSE 3 callback-shape
cleanup.

### Application Threading Default

Align the application-level `multi_threading` default with the binding default:
default mount mode should be multithreaded, and `-s` should remain the explicit
single-threaded opt-out.

This is needed because `ConfigRuntime.resolve` raises the SQLite busy timeout
only when `multi_threading` is true. After the binding default changes to
multithreaded execution, the application-level flag should describe the actual
runtime mode.

### e2e Coverage

Keep the existing default e2e run as the multithreaded-default coverage path.
Add an explicit single-threaded e2e path after the dependency switch is
building.

The e2e harness does not currently expose extra executable arguments. Add a
small setting such as `GDFUSE_E2E_GDFUSE_ARGS="-s"` or an equivalent
single-threaded Makefile target when adding this coverage.

### Native FUSE 3 API

Defer the native `Fuse.operations` migration to a later milestone.

When that milestone starts, adapt `bin/gdfuseFuse.ml` to the native FUSE 3
record:

- callbacks with `file_info` should ignore it initially where the application
  has no file-handle state;
- `fopen` and `opendir` should return `Fuse.default_file_info_update`;
- `rename` should reject unsupported nonzero flags or map supported flags
  deliberately;
- `utimens` should convert `Fuse.timestamp` to the existing float timestamp
  behavior, rejecting unsupported sentinels if needed;
- `readdir` should return `Fuse.dir_entry list`.

## Milestones

### M0: Baseline

Run the current build and unit tests before dependency changes.

Tasks:

- Run `dune build @install`.
- Run `dune runtest`.
- Run `make e2e-preflight` if local e2e credentials are configured.
- Confirm the local or opam `fuse3` package is installable.

Exit criteria:

- The pre-upgrade baseline is known.
- Any existing failures are recorded before changing dependencies.

### M1: Compatibility-Mode Dependency Switch

Switch from `ocamlfuse` to `fuse3` while keeping the old callback shape through
`Fuse.Fuse_compat`.

Tasks:

- Change `google-drive-ocamlfuse.opam` dependency from `ocamlfuse` to `fuse3`.
- Change `src/dune` library dependency from `ocamlfuse` to `fuse3`.
- Update `bin/gdfuseFuse.ml` to use `Fuse.Fuse_compat.main`.
- Update `bin/gdfuseFuse.ml` to use `Fuse.Fuse_compat.default_operations`.
- Keep top-level `Fuse.xattr_flags`, `Fuse.Unix_util`, and Bigarray buffer use
  unchanged.
- Update README requirements and relevant agent docs from `ocamlfuse` to
  `fuse3`.

Exit criteria:

- The application builds against `fuse3`.
- The existing FUSE adapter behavior remains on the compatibility callback
  shape.

### M2: Threading Default Alignment

Make application-level threading state match the binding default.

Tasks:

- Make default CLI parsing set `multi_threading = true`.
- Keep `-m` accepted and idempotent.
- Keep `-s` prepending `-s` to FUSE args and setting `multi_threading = false`.
- Update CLI tests for default, `-m`, and `-s`.
- Add or update `ConfigRuntime` tests for default multithreaded timeout
  behavior.
- Update user and agent docs that describe `-m`, `-s`, and multithreaded
  default behavior.

Exit criteria:

- A default mount uses the `fuse3` multithreaded loop and application runtime
  config reflects multithreaded mode.
- `-s` remains the opt-out and keeps the legacy SQLite busy-timeout behavior.

### M3: e2e Harness Single-Threaded Path

Keep the default e2e path as multithreaded-default coverage and add explicit
single-threaded coverage.

Tasks:

- Add an e2e setting for extra executable args or loop mode selection.
- Pass `-s` through that setting for single-threaded e2e runs.
- Add `make e2e-single-threaded` or document the explicit environment command.
- Update `test/e2e/README.md`.

Exit criteria:

- `make e2e` exercises the default multithreaded mode.
- There is a documented and testable single-threaded e2e path.

### M4: Native FUSE 3 Adapter

Move `bin/gdfuseFuse.ml` from `Fuse.Fuse_compat` to native `Fuse.operations`.

Tasks:

- Update adapter callback signatures to FUSE 3 shapes.
- Convert `utime` wrapper to `utimens`.
- Convert string-list `readdir` to `Fuse.dir_entry list`.
- Return `Fuse.default_file_info_update` from `fopen` and `opendir`.
- Ignore `file_info` in callbacks that do not currently need file handles.
- Make rename flag handling explicit.
- Update boundary docs and tests.

Exit criteria:

- The application no longer depends on `Fuse.Fuse_compat`.
- Native FUSE 3 callbacks are covered by unit tests and the live e2e suite.

## Verification

Run sequentially:

```sh
tools/format_ocaml
dune build @install
dune runtest
```

When local e2e credentials are configured and FUSE is available:

```sh
make e2e-preflight
make e2e
make e2e-single-threaded
```

If the single-threaded target has not been added yet, use the documented e2e
environment override for `-s` instead.
