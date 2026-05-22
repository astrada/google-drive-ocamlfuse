.PHONY: build clean test e2e doc format

build:
	dune build @install

test:
	dune runtest

e2e:
	dune build @e2e

format:
	tools/format_ocaml

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
