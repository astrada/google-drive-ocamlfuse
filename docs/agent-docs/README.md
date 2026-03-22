# Agent Docs

This directory contains technical documentation for coding agents working in
`google-drive-ocamlfuse`.

Read these files in order:

1. `repo-map.md`
2. `architecture.md`
3. `gdfuse-fuse-boundary.md` for the adapter layer that maps `Drive` exceptions to Unix/FUSE errors
4. `drive-init-filesystem.md` for the first FUSE filesystem implementation note
5. `drive-read-dir.md` for directory listing, cache refresh, and virtual roots
6. `background-folder-fetching-start-thread.md` for the startup boundary of background folder prefetching
7. `background-folder-fetching-fetch-next-folder.md` for the runtime folder-selection and callback step in background prefetching
8. `background-folder-fetching-stop-thread.md` for the stop-request half of the background folder prefetch lifecycle
9. `drive-get-resource.md` for path resolution and negative-cache behavior
10. `drive-get-metadata.md` for freshness boundaries and change-feed replay
11. `drive-upload-path.md` for dirty-state, dispatch, and content upload flow
12. `drive-create-remote-resource.md` for file/folder/symlink creation semantics
13. `drive-rename.md` for move/rename semantics and cache surgery
14. `drive-update-remote-resource.md` for shared metadata-mutation and delete flow
15. `drive-chmod-chown-utime.md` for the thin metadata-mutation entrypoints over `update_remote_resource`
16. `drive-download-resource.md` for local content materialization and reuse
17. `drive-read.md` for stream-vs-cache read policy and read-ahead behavior
18. `drive-write.md` for local mutation, dirty-state updates, and write buffers
19. `drive-truncate.md` for local size mutation and signed cache-size updates
20. `drive-start-uploading-if-dirty.md` for the tiny `ToUpload -> Uploading` gate behind upload dispatch
21. `drive-upload-if-dirty.md` for the bridge from the local dirty-state gate into `do_request (upload_with_retry ...)`
22. `drive-upload-with-retry.md` for the path-based bridge from `do_request` into `get_resource` and `queue_upload`
23. `drive-queue-upload.md` for the sync-vs-async dispatcher over `upload_resource_with_retry` and `UploadQueue.queue_resource`
24. `upload-queue-queue-resource.md` for the async enqueue step that persists and deduplicates upload-queue rows
25. `upload-queue-start-async-upload-thread.md` for the runtime-state installation and poll-thread startup of the async upload subsystem
26. `upload-queue-stop-async-upload-thread.md` for the stop-flag request that asks the async upload poll loop to drain and exit
27. `upload-queue-poll-upload-queue.md` for the long-lived async-upload poll loop and its drain-on-stop contract
28. `upload-queue-upload-resource.md` for the poll-thread helper that selects one queued entry and hands it to a worker thread
29. `thread-pool-create.md` for generic worker-pool construction, defaults, and injected synchronization primitives
30. `thread-pool-add-work.md` for the generic worker-pool admission and exception-safe cleanup boundary
31. `thread-pool-pending-threads.md` for the observable worker-count view over the live thread-pool table
32. `thread-pool-shutdown.md` for the generic worker-pool join step used during async-upload shutdown
33. `drive-upload-resource-by-id.md` for the async-worker bridge from queue entries back into the request/session upload path
34. `drive-upload-resource-with-retry.md` for the common flush-and-retry wrapper around the real upload
35. `drive-upload.md` for the actual `FilesResource.update` upload attempt and cache reconciliation
36. `drive-flush-fsync-release.md` for the file-close/sync callbacks that trigger upload dispatch
37. `drive-fopen.md` for open-time access validation and write-permission checks
38. `drive-opendir.md` for directory-open validation and its lookup-only contract
39. `gdfuse-noop-dir-callbacks.md` for the adapter-level `releasedir` / `fsyncdir` no-op hooks
40. `drive-statfs.md` for filesystem-wide quota reporting and synthetic `statvfs` fields
41. `drive-get-attr.md` for stat synthesis, virtual roots, and visible metadata policy
42. `drive-read-link.md` for symlink/shortcut target resolution and cache fill
43. `drive-mknod-mkdir.md` for the thin file/folder creation entrypoints over `create_remote_resource`
44. `drive-symlink.md` for the FUSE symlink entrypoint and create-link delegation
45. `drive-xattr.md` for extended-attribute storage, reads, and mutations
46. `drive-delete-remote-resource.md` for delete-vs-trash policy selection
47. `drive-check-if-empty.md` for the folder emptiness guard used by deletion
48. `drive-unlink-rmdir.md` for the thin FUSE delete entrypoints and `is_folder` contract
49. `application-flow.md`
50. `workflows.md`
51. `config-file-handling.md` for config-format and parser refactors

The existing user-facing docs under `docs/wiki/` are still the source for
installation, authorization, and end-user configuration details. The files in
`docs/agent-docs/` are focused on implementation and maintenance.
