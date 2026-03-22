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
17. `drive-start-uploading-if-dirty.md` for the tiny `ToUpload -> Uploading` gate behind upload dispatch
18. `drive-upload-if-dirty.md` for the bridge from the local dirty-state gate into `do_request (upload_with_retry ...)`
19. `drive-upload-with-retry.md` for the path-based bridge from `do_request` into `get_resource` and `queue_upload`
20. `drive-queue-upload.md` for the sync-vs-async dispatcher over `upload_resource_with_retry` and `UploadQueue.queue_resource`
21. `upload-queue-start-async-upload-thread.md` for the runtime-state installation and poll-thread startup of the async upload subsystem
22. `upload-queue-stop-async-upload-thread.md` for the stop-flag request that asks the async upload poll loop to drain and exit
23. `upload-queue-poll-upload-queue.md` for the long-lived async-upload poll loop and its drain-on-stop contract
24. `upload-queue-upload-resource.md` for the poll-thread helper that selects one queued entry and hands it to a worker thread
25. `drive-upload-resource-by-id.md` for the async-worker bridge from queue entries back into the request/session upload path
26. `drive-upload-resource-with-retry.md` for the common flush-and-retry wrapper around the real upload
27. `drive-upload.md` for the actual `FilesResource.update` upload attempt and cache reconciliation
28. `drive-flush-fsync-release.md` for the file-close/sync callbacks that trigger upload dispatch
29. `drive-fopen.md` for open-time access validation and write-permission checks
30. `drive-opendir.md` for directory-open validation and its lookup-only contract
31. `gdfuse-noop-dir-callbacks.md` for the adapter-level `releasedir` / `fsyncdir` no-op hooks
32. `drive-statfs.md` for filesystem-wide quota reporting and synthetic `statvfs` fields
33. `drive-get-attr.md` for stat synthesis, virtual roots, and visible metadata policy
34. `drive-read-link.md` for symlink/shortcut target resolution and cache fill
35. `drive-mknod-mkdir.md` for the thin file/folder creation entrypoints over `create_remote_resource`
36. `drive-symlink.md` for the FUSE symlink entrypoint and create-link delegation
37. `drive-xattr.md` for extended-attribute storage, reads, and mutations
38. `drive-delete-remote-resource.md` for delete-vs-trash policy selection
39. `drive-check-if-empty.md` for the folder emptiness guard used by deletion
40. `drive-unlink-rmdir.md` for the thin FUSE delete entrypoints and `is_folder` contract
41. `application-flow.md`
42. `workflows.md`
43. `config-file-handling.md` for config-format and parser refactors

The existing user-facing docs under `docs/wiki/` are still the source for
installation, authorization, and end-user configuration details. The files in
`docs/agent-docs/` are focused on implementation and maintenance.
