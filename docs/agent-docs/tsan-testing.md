# ThreadSanitizer Testing

## Purpose

Use this note when testing `google-drive-ocamlfuse` with an OCaml switch built
with ThreadSanitizer, such as an `ocaml-option-tsan` switch.

## Compiler Startup

If even simple compiler commands fail with:

```text
FATAL: ThreadSanitizer: unexpected memory mapping
```

reduce Linux address-space randomization for the current boot:

```sh
sudo sysctl vm.mmap_rnd_bits=30
```

After that, `ocamlopt -config` should run normally and should report:

```text
tsan: true
```

The native compiler flags should also include `-fsanitize=thread`.

## Current Test Results

The `5.4.0-tsan` switch can build this repository:

```sh
dune clean
dune build @install
```

Add the missing dependencies to the switch:

```sh
opam install --deps-only --with-test .
```

`make e2e-preflight` can pass under the TSAN switch when run outside the
sandbox with a valid local e2e config.

`dune runtest` and mounted e2e execution are currently inconclusive under
TSAN. They abort with:

```text
ThreadSanitizer:DEADLYSIGNAL
ERROR: ThreadSanitizer: SEGV
ThreadSanitizer: nested bug in the same thread, aborting.
```

The unit-test crash reproduces outside the sandbox. A verbose run reaches
`ConfigStore test:19:test_invalid_max_upload_chunk_size_is_rejected`, but that
test passes when run alone. This points to cumulative TSAN/runtime process
state rather than a normal OUnit assertion failure.

The minimal mounted case:

```sh
make e2e CASE="mount root listing"
```

fails during mount startup because the TSAN-built `gdfuse.exe` exits with the
same sanitizer SEGV pattern. Simple startup commands such as:

```sh
_build/default/bin/gdfuse.exe -version
_build/default/bin/gdfuse.exe -help
```

work outside the sandbox.

## Interpreting The Failure

Treat the current TSAN runtime failures as inconclusive. They do not look like
ordinary data-race reports in this project. They match a known sanitizer failure
shape where ThreadSanitizer crashes or misbehaves with an unsupported C
threading/runtime implementation:

<https://groups.google.com/g/thread-sanitizer/c/0xA7qH2Pr-o>

That thread describes `ThreadSanitizer:DEADLYSIGNAL` / nested sanitizer crashes
on threaded programs when the underlying C threading library is not supported
correctly by TSAN. The google-drive-ocamlfuse failures have the same shape:

- fatal sanitizer SEGV instead of a race report
- no useful application stack
- cumulative failures in otherwise ordinary unit-test execution
- mount startup failure with the same sanitizer pattern

So a successful TSAN build is useful signal, but failed TSAN runtime tests are
not currently actionable as google-drive-ocamlfuse race reports without a real
TSAN stack that points into application or binding code.
