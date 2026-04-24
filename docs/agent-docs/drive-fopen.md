# `Drive.fopen`

## Purpose

`Drive.fopen` is the access-validation path used by the FUSE file-open
callback.

Its directory-side counterpart is `Drive.opendir`; see
`docs/agent-docs/drive-opendir.md`.

The production entrypoint in `src/drive.ml` is a thin adapter over
`DriveOpens`:

```ocaml
let fopen path flags =
  do_request (OpenOps.fopen (drive_open_runtime ()) path flags) |> ignore;
  None
```

`DriveOpens` owns open-time access policy. `DriveOpenPorts` connects that
policy to production path normalization and resource resolution.

It does not allocate or return a persistent open-file object. Its real job is
to validate that the requested path exists and that the requested access mode is
allowed before later operations like `read`, `write`, and `truncate` run.

In practice, `Drive.fopen` answers two questions:

- does this path resolve to an existing resource?
- if the caller wants write-capable access, is that resource writable here?

## Signature

```ocaml
val fopen : string -> Unix.open_flag list -> 'a option
```

The arguments are:

- `path`: visible FUSE path
- `flags`: requested open flags from FUSE / the Unix open-mode layer

The return value is always `None`.

That is an intentional part of the current design: `fopen` acts as a gate, not
as a handle-construction step.

## FUSE Boundary

The production FUSE adapter in `bin/gdfuseFuse.ml` wires:

```ocaml
let fopen path flags =
  with_drive_op ~label:"fopen" ~param:path (fun () -> Drive.fopen path flags)
```

So any exception raised by `Drive.fopen` is translated at the boundary into a
Unix/FUSE error.

In particular:

- `Permission_denied` becomes `EACCES`
- `File_not_found` becomes `ENOENT`

This is why `fopen` can stay focused on repository exceptions instead of Unix
error codes directly.

## High-Level Flow

At a high level, `fopen` does this:

1. normalize the visible path into `(path_in_cache, trashed)`
2. inspect the open flags to decide whether the request is read-only
3. if the request is write-capable, reject it immediately when the filesystem
   is globally read-only
4. resolve the resource with `get_resource`
5. if the request is write-capable, reject it when the resource is considered
   read-only
6. return `None`

`DriveOpens.fopen` returns `unit` on success. The public `Drive.fopen` wrapper
then returns `None`.

So this is a validation path with side effects only through exceptions and
through any metadata refresh that `get_resource` triggers.

## Path Normalization

Like the other FUSE-facing entrypoints, `fopen` starts with:

```ocaml
let path_in_cache, trashed = get_path_in_cache path config
```

That means the normal path-mapping rules apply here too:

- trash-view paths are mapped into `(path, trashed)` cache coordinates
- synthetic namespaces are resolved the same way they are for later read/write
  operations

## Read-Only Request Detection

The implementation decides whether the open is read-only with:

```ocaml
let is_read_only_request = List.mem Unix.O_RDONLY flags
```

This is a narrow implementation detail worth documenting as-is.

For the rest of the function:

- if `is_read_only_request = true`, the call is treated as a read-only open
- otherwise it is treated as write-capable

So requests such as `O_WRONLY` or `O_RDWR` go down the write-capable path.

## Global Read-Only Guard

Before resolving the resource, `fopen` checks:

```ocaml
if (not is_read_only_request) && runtime.config.Config.read_only then
  raise Permission_denied
```

This is the earliest and cheapest rejection path.

If the mounted filesystem is globally read-only and the caller wants a
write-capable open, the function fails immediately without calling
`get_resource`.

Read-only opens are still allowed to continue past this point.

## Resource Resolution Always Happens Otherwise

If the global read-only guard does not reject the request, `fopen` runs:

```ocaml
get_resource path_in_cache trashed
```

That means even a read-only open still performs full path resolution and
existence checking.

Consequences:

- missing files fail at open time with `File_not_found`
- stale resource rows may be refreshed before the open succeeds
- metadata refresh may run first through `get_metadata`

See `docs/agent-docs/drive-get-resource.md` for the resolution semantics this
depends on.

## File-Level Writability Rule

After the resource is resolved, `fopen` only performs an extra check for
write-capable opens:

```ocaml
if (not is_read_only_request) && is_file_read_only runtime.config resource then
  Utils.raise_m Permission_denied
```

So read-only opens only validate existence.

Write-capable opens must also pass
`DriveOpens.is_file_read_only config resource = false`.

## What `is_file_read_only` Means Here

`DriveOpens.is_file_read_only` returns true when any of these hold:

- `resource.can_edit = Some false`
- the resource is a Google document and either `config.editable_docs = false`
  or its configured export format is `desktop`
- `config.large_file_read_only = true` and the resource is considered a large
  file under the current config

This is the real write-access policy enforced by `fopen`.

A few implications follow:

- Google documents are openable for reading by default
- Google documents become writable only when `editable_docs = true` and the
  current per-type export format is not `desktop`
- large-file streaming policy can also become a write-denial policy when
  `large_file_read_only` is enabled
- resources lacking an explicit `can_edit = false` are treated as editable by
  default in this check

## No Open Handle State

After validation, the public wrapper returns:

```ocaml
None
```

It does not:

- cache an open handle
- pin the resource
- snapshot the open flags for later calls
- establish any per-open cleanup contract

This matches the rest of the current `Drive` design, where later operations like
`read` and `write` re-resolve by path instead of dereferencing an object created
by `fopen`.

## Relationship To `Drive.read`, `Drive.write`, And `Drive.truncate`

`fopen` is the main front door for access-mode enforcement before the later
data paths run.

That matters because:

- `Drive.read` does not depend on write permissions
- `Drive.write` does not re-check filesystem read-only mode or per-file
  editability itself
- `Drive.truncate` likewise assumes permission checks have already happened
  outside its own body

So if write-permission behavior changes, `fopen` is one of the first places to
inspect.

See:

- `docs/agent-docs/drive-read.md`
- `docs/agent-docs/drive-write.md`
- `docs/agent-docs/drive-truncate.md`

## Maintenance Notes

### `fopen` Is Validation, Not Resource Preparation

Do not read this function as the place where later reads/writes obtain their
working state. It only checks access and existence.

### Read-Only Opens Still Refresh Metadata

Because `get_resource` still runs for read-only opens, a seemingly harmless open
can still trigger metadata refresh and cache reconciliation work.

### Access Policy Is Split Across Two Levels

Write-capable opens are rejected by:

1. filesystem-wide read-only mode
2. per-resource `is_file_read_only`

Keep both levels in mind when changing access behavior.

### Later Paths Depend On This Gate

The current design relies on `fopen` to reject disallowed write-capable access
before `write` or `truncate` mutate local state. If that assumption changes,
those later paths may need their own stronger guards.

### Test Boundary

`test/testDriveOpens.ml` exercises `DriveOpens` through fake ports. The tests
cover read-only and write-capable opens, global read-only mode, per-resource
editability, Google document editability, large-file read-only policy, trash
path normalization, missing-resource propagation, and the current
`List.mem Unix.O_RDONLY flags` request-mode rule.
