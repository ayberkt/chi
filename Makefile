# Frontend to dune.

.PHONY: default build install uninstall test clean deps

default: build

deps:
	opam install core alcotest

build:
	cd parser/lib && $(MAKE)
	dune build

test:
	dune runtest -f

install:
	dune install

uninstall:
	dune uninstall

clean:
	cd parser/lib && $(MAKE) clean
	dune clean
	rm -f chi.install
