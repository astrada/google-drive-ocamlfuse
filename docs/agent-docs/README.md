# Agent Docs

This directory contains technical documentation for coding agents working in
`google-drive-ocamlfuse`.

Read these files in order:

1. `repo-map.md`
2. `architecture.md`
3. `gdfuse-fuse-boundary.md` for the adapter layer that maps `Drive` exceptions to Unix/FUSE errors
4. `drive-init-filesystem.md` for the first FUSE filesystem implementation note
5. `drive-read-dir.md` for directory listing, cache refresh, and virtual roots
6. `drive-get-resource.md` for path resolution and negative-cache behavior
7. `drive-get-metadata.md` for freshness boundaries and change-feed replay
8. `drive-upload-path.md` for dirty-state, dispatch, and content upload flow
9. `drive-create-remote-resource.md` for file/folder/symlink creation semantics
10. `drive-rename.md` for move/rename semantics and cache surgery
11. `drive-update-remote-resource.md` for shared metadata-mutation and delete flow
12. `drive-chmod-chown-utime.md` for the thin metadata-mutation entrypoints over `update_remote_resource`
13. `drive-download-resource.md` for local content materialization and reuse
14. `drive-read.md` for stream-vs-cache read policy and read-ahead behavior
15. `drive-write.md` for local mutation, dirty-state updates, and write buffers
16. `drive-truncate.md` for local size mutation and signed cache-size updates
17. `drive-flush-fsync-release.md` for the file-close/sync callbacks that trigger upload dispatch
18. `drive-fopen.md` for open-time access validation and write-permission checks
19. `drive-opendir.md` for directory-open validation and its lookup-only contract
20. `gdfuse-noop-dir-callbacks.md` for the adapter-level `releasedir` / `fsyncdir` no-op hooks
21. `drive-statfs.md` for filesystem-wide quota reporting and synthetic `statvfs` fields
22. `drive-get-attr.md` for stat synthesis, virtual roots, and visible metadata policy
23. `drive-read-link.md` for symlink/shortcut target resolution and cache fill
24. `drive-mknod-mkdir.md` for the thin file/folder creation entrypoints over `create_remote_resource`
25. `drive-symlink.md` for the FUSE symlink entrypoint and create-link delegation
26. `drive-xattr.md` for extended-attribute storage, reads, and mutations
27. `drive-delete-remote-resource.md` for delete-vs-trash policy selection
28. `drive-check-if-empty.md` for the folder emptiness guard used by deletion
29. `drive-unlink-rmdir.md` for the thin FUSE delete entrypoints and `is_folder` contract
30. `application-flow.md`
31. `workflows.md`
32. `config-file-handling.md` for config-format and parser refactors

The existing user-facing docs under `docs/wiki/` are still the source for
installation, authorization, and end-user configuration details. The files in
`docs/agent-docs/` are focused on implementation and maintenance.
