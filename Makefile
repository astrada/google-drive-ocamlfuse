.PHONY: build clean test e2e e2e-list e2e-preflight e2e-single-threaded doc format

E2E_SUITE := _build/default/test/e2e/e2eSuite.exe
E2E_BUILD_TARGETS := test/e2e/e2eSuite.exe bin/gdfuse.exe

build:
	dune build @install

test:
	dune runtest

e2e:
	dune build $(E2E_BUILD_TARGETS)
ifdef CASE
	GDFUSE_E2E_ONLY="$(CASE)" $(E2E_SUITE)
else
	$(E2E_SUITE)
endif

e2e-list:
	dune build test/e2e/e2eSuite.exe
	$(E2E_SUITE) --list

e2e-preflight:
	dune build $(E2E_BUILD_TARGETS)
	$(E2E_SUITE) --preflight

e2e-single-threaded:
	dune build $(E2E_BUILD_TARGETS)
ifdef CASE
	GDFUSE_E2E_GDFUSE_ARGS="-s" GDFUSE_E2E_ONLY="$(CASE)" $(E2E_SUITE)
else
	GDFUSE_E2E_GDFUSE_ARGS="-s" $(E2E_SUITE)
endif

format:
	tools/format_ocaml

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
