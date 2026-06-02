.PHONY: build clean test e2e e2e-list e2e-preflight doc format

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

format:
	tools/format_ocaml

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
