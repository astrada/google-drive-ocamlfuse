# Agent Docs

This directory contains technical documentation for coding agents working in
`google-drive-ocamlfuse`.

Read these files in order:

1. `repo-map.md`
2. `architecture.md`
3. `gdfuse-fuse-boundary.md` for the adapter layer that maps `Drive` exceptions to Unix/FUSE errors
4. `drive-init-filesystem.md` for the first FUSE filesystem implementation note
5. `drive-read-dir.md` for directory listing, cache refresh, and virtual roots in the directory-read core
6. `background-folder-fetching-start-thread.md` for the startup boundary of background folder prefetching
7. `background-folder-fetching-fetch-next-folder.md` for the runtime folder-selection and callback step in background prefetching
8. `background-folder-fetching-folder-fetch.md` for the polling cadence, stop checks, and failure boundary around background prefetching
9. `background-folder-fetching-stop-thread.md` for the stop-request half of the background folder prefetch lifecycle
10. `drive-get-resource.md` for path resolution and negative-cache behavior
11. `drive-get-metadata.md` for freshness boundaries and change-feed replay
12. `drive-upload-path.md` for dirty-state, dispatch, and content upload flow
13. `drive-create-remote-resource.md` for file/folder/symlink creation semantics in the mutation core
14. `drive-rename.md` for move/rename semantics and cache surgery in the mutation core
15. `drive-update-remote-resource.md` for the shared mutation-wrapper pattern used by metadata mutations, xattr mutations, and the delete/rename mutation core
16. `drive-chmod-chown-utime.md` for `DriveMetadataMutations` timestamp, mode, and owner metadata updates
17. `drive-download-resource.md` for `DriveDownloads` local content materialization and reuse
18. `drive-read.md` for `DriveReads` stream-vs-cache read policy and read-ahead behavior
19. `drive-write.md` for local mutation, dirty-state updates, and write buffers in the file-mutation core
20. `drive-truncate.md` for local size mutation and signed cache-size updates in the file-mutation core
21. `drive-start-uploading-if-dirty.md` for the tiny `ToUpload -> Uploading` gate in the upload-dispatch core
22. `drive-upload-if-dirty.md` for the `Drive`-level bridge from file callbacks into the upload-dispatch request path
23. `drive-upload-with-retry.md` for the path-based bridge from the upload-dispatch gate into `get_resource` and `queue_upload`
24. `drive-queue-upload.md` for the sync-vs-async dispatcher over the direct-upload port and the async enqueue helper
25. `upload-queue-queue-resource.md` for the async enqueue step that persists and deduplicates upload-queue rows
26. `upload-queue-start-async-upload-thread.md` for the runtime-state installation and poll-thread startup of the async upload subsystem
27. `upload-queue-stop-async-upload-thread.md` for the stop-flag request that asks the async upload poll loop to drain and exit
28. `upload-queue-poll-upload-queue.md` for the long-lived async-upload poll loop and its drain-on-stop contract
29. `upload-queue-upload-resource.md` for the poll-thread helper that selects one queued entry and hands it to a worker thread
30. `thread-pool-create.md` for generic worker-pool construction, defaults, and injected synchronization primitives
31. `thread-pool-add-work.md` for the generic worker-pool admission and exception-safe cleanup boundary
32. `thread-pool-pending-threads.md` for the observable worker-count view over the live thread-pool table
33. `thread-pool-shutdown.md` for the generic worker-pool join step used during async-upload shutdown
34. `drive-upload-resource-by-id.md` for the async-worker bridge from queue entries back into the request/session upload path
35. `drive-upload-resource-with-retry.md` for the common flush-and-retry wrapper around the real upload
36. `drive-upload.md` for the actual `FilesResource.update` upload attempt and cache reconciliation
37. `drive-flush-fsync-release.md` for the file-close/sync callbacks that trigger upload dispatch
38. `drive-fopen.md` for `DriveOpens` open-time access validation and write-permission checks
39. `drive-opendir.md` for directory-open validation in the read-side view core
40. `gdfuse-noop-dir-callbacks.md` for the adapter-level `releasedir` / `fsyncdir` no-op hooks
41. `drive-statfs.md` for filesystem-wide quota reporting and synthetic `statvfs` fields
42. `drive-get-attr.md` for stat synthesis, virtual roots, and visible metadata policy in the read-side view core
43. `drive-read-link.md` for symlink/shortcut target resolution and cache fill in the read-side view core
44. `drive-mknod-mkdir.md` for the thin file/folder creation entrypoints over `create_remote_resource`
45. `drive-symlink.md` for the FUSE symlink entrypoint and create-link delegation
46. `drive-xattr.md` for `DriveXattrs` extended-attribute storage, reads, and mutations
47. `drive-delete-remote-resource.md` for delete-vs-trash policy selection
48. `drive-check-if-empty.md` for the folder emptiness probe used by deletion
49. `drive-unlink-rmdir.md` for the thin FUSE delete entrypoints and `is_folder` contract
50. `application-flow.md`
51. `workflows.md`
52. `config-file-handling.md` for config-format and parser maintenance

The user-facing docs under `docs/wiki/` are the source for installation,
authorization, and end-user configuration details. The files in
`docs/agent-docs/` focus on implementation and maintenance.
