.PHONY: build clean test e2e e2e-preflight doc format

build:
	dune build @install

test:
	dune runtest

e2e:
	dune build @e2e

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
