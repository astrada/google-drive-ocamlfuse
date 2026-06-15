# Repository Guidelines

## Project Structure & Module Organization
`src/` holds the flat OCaml library that implements config, cache, OAuth, threading, and Drive/FUSE behavior. `bin/` contains the executable wiring: `gdfuse.ml` is the entrypoint, with runtime dependency and FUSE adapters beside it. `test/` contains OUnit suites; add new tests as `test/test<Name>.ml` and register them in `test/testSuite.ml`. `docs/wiki/` is user-facing documentation, while `docs/agent-docs/` documents architecture and common maintenance workflows. `tools/format_ocaml` formats tracked `.ml` and `.mli` files.

## Build, Test, and Development Commands
Use `dune build @install` to build the library and installable executable. Run `dune runtest` to execute the OUnit suite. Run build and test *sequentially* because `dune` uses a lock file that prevents running more than one `dune` command in parallel. `make build`, `make test`, and `make format` are thin wrappers around the main dune and formatting commands. Use `dune install` and `dune uninstall` for local installs, and `dune clean` to remove `_build/`. Run live e2e commands such as `make e2e-preflight` and `make e2e` outside the sandbox because they need network access, `/dev/fuse`, and mount permissions.

## Coding Style & Naming Conventions
Format OCaml sources with `ocamlformat` using the checked-in `.ocamlformat` (`version=0.29.0`, `profile=conventional`). Follow the existing two-space indentation and keep changes consistent with the repo’s flat module layout. Source files use lower camel case names such as `gdfuseFlow.ml`; public interfaces should stay in sync with matching `.mli` files. Prefer small helpers and explicit control flow over dense nested branches, especially in high-risk modules like `src/drive.ml`.

## Testing Guidelines
Tests use `ounit2`. Each test module should expose a `suite`, use the `test<Name>.ml` naming pattern, and be added to `test/testSuite.ml`. Favor unit tests for CLI parsing, config resolution, cache behavior, and startup flow. There are no end-to-end tests for live FUSE mounts, OAuth flows, or real Google Drive integration, so changes in those areas should also be validated manually.

## Commit & Pull Request Guidelines
Recent history uses short, imperative, capitalized subjects such as `Update docs`, `Refactor gdfuse for readability`, and `Bump gapi-ocaml dependency`. Keep commits focused and explain behavior changes in the body when the subject is not enough. Pull requests should summarize user-visible impact, list validation performed (`dune runtest`, manual mount/auth checks when relevant), and link issues when applicable. Screenshots are usually unnecessary for this CLI project; logs or repro steps are more useful.

## Security & Configuration Tips
Do not commit OAuth client secrets, tokens, or local config from `~/.gdfuse/`. When changing config handling, update both the OCaml implementation and the related documentation in `docs/wiki/Configuration.md`.

## Documentation Guidelines
When updating the documentations, keep the docs descriptive of the current code rather than describing the last modification or refactoring.
