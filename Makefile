.PHONY: build clean test e2e e2e-list e2e-preflight doc format

build:
	dune build @install

test:
	dune runtest

e2e:
ifdef CASE
	GDFUSE_E2E_ONLY="$(CASE)" dune build @e2e --force
else
	dune build @e2e
endif

e2e-list:
	dune build @e2e-list --force

e2e-preflight:
	dune build @e2e-preflight

format:
	tools/format_ocaml

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
