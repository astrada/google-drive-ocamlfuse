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
11. `drive-chmod-chown-utime.md` for the thin metadata-mutation entrypoints over `update_remote_resource`
12. `drive-download-resource.md` for local content materialization and reuse
13. `drive-read.md` for stream-vs-cache read policy and read-ahead behavior
14. `drive-write.md` for local mutation, dirty-state updates, and write buffers
15. `drive-truncate.md` for local size mutation and signed cache-size updates
16. `drive-flush-fsync-release.md` for the file-close/sync callbacks that trigger upload dispatch
17. `drive-fopen.md` for open-time access validation and write-permission checks
18. `drive-opendir.md` for directory-open validation and its lookup-only contract
19. `drive-statfs.md` for filesystem-wide quota reporting and synthetic `statvfs` fields
20. `drive-get-attr.md` for stat synthesis, virtual roots, and visible metadata policy
21. `drive-read-link.md` for symlink/shortcut target resolution and cache fill
22. `drive-mknod-mkdir.md` for the thin file/folder creation entrypoints over `create_remote_resource`
23. `drive-symlink.md` for the FUSE symlink entrypoint and create-link delegation
24. `drive-xattr.md` for extended-attribute storage, reads, and mutations
25. `drive-delete-remote-resource.md` for delete-vs-trash policy selection
26. `drive-check-if-empty.md` for the folder emptiness guard used by deletion
27. `drive-unlink-rmdir.md` for the thin FUSE delete entrypoints and `is_folder` contract
28. `application-flow.md`
29. `workflows.md`
30. `config-file-handling.md` for config-format and parser refactors

The existing user-facing docs under `docs/wiki/` are still the source for
installation, authorization, and end-user configuration details. The files in
`docs/agent-docs/` are focused on implementation and maintenance.
