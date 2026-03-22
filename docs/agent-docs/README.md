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
13. `drive-write.md` for local mutation, dirty-state updates, and write buffers
14. `drive-truncate.md` for local size mutation and signed cache-size updates
15. `drive-flush-fsync-release.md` for the file-close/sync callbacks that trigger upload dispatch
16. `drive-fopen.md` for open-time access validation and write-permission checks
17. `drive-get-attr.md` for stat synthesis, virtual roots, and visible metadata policy
18. `drive-read-link.md` for symlink/shortcut target resolution and cache fill
19. `drive-mknod-mkdir.md` for the thin file/folder creation entrypoints over `create_remote_resource`
20. `drive-symlink.md` for the FUSE symlink entrypoint and create-link delegation
21. `drive-xattr.md` for extended-attribute storage, reads, and mutations
22. `drive-delete-remote-resource.md` for delete-vs-trash policy selection
23. `drive-check-if-empty.md` for the folder emptiness guard used by deletion
24. `drive-unlink-rmdir.md` for the thin FUSE delete entrypoints and `is_folder` contract
25. `application-flow.md`
26. `workflows.md`
27. `config-file-handling.md` for config-format and parser refactors

The existing user-facing docs under `docs/wiki/` are still the source for
installation, authorization, and end-user configuration details. The files in
`docs/agent-docs/` are focused on implementation and maintenance.
