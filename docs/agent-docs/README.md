# Agent Docs

This directory contains technical documentation for coding agents working in
`google-drive-ocamlfuse`.

Read these files in order:

1. `repo-map.md`
2. `architecture.md`
3. `drive-init-filesystem.md` for the first FUSE filesystem implementation note
4. `drive-read-dir.md` for directory listing, cache refresh, and virtual roots
5. `drive-get-resource.md` for path resolution and negative-cache behavior
6. `drive-get-metadata.md` for freshness boundaries and change-feed replay
7. `drive-upload-path.md` for dirty-state, dispatch, and content upload flow
8. `drive-create-remote-resource.md` for file/folder/symlink creation semantics
9. `drive-rename.md` for move/rename semantics and cache surgery
10. `drive-update-remote-resource.md` for shared metadata-mutation and delete flow
11. `drive-download-resource.md` for local content materialization and reuse
12. `drive-read.md` for stream-vs-cache read policy and read-ahead behavior
13. `application-flow.md`
14. `workflows.md`
15. `config-file-handling.md` for config-format and parser refactors

The existing user-facing docs under `docs/wiki/` are still the source for
installation, authorization, and end-user configuration details. The files in
`docs/agent-docs/` are focused on implementation and maintenance.
