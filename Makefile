.PHONY: build clean test doc format

build:
	dune build @install

test:
	dune runtest

format:
	tools/format_ocaml

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
