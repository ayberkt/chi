# χ

Interpreter for a tiny language that makes it easy to define a self-interpreter.

χ was first  formulated by Bengt Nordström. This interpreter follows [his notes](http://www.cse.chalmers.se/edu/year/2018/course/DIT311_Models_of_computation/reading/The_language_chi.pdf).

There is also [an Agda formalization of χ][agda-formalization] by Nils Anders Danielsson. This is useful for formalizing uncomputability proofs in Agda.

## Building

You need [BNFC][BNFC] to generate code for the parser so make sure you have
that installed.

You also need `opam` and `dune`. If you have these installed, run `make deps`
followed by `make` to build. This will create an executable in
`/_build/default/interpreter/bin/chi.exe`

To run the tests run `make test`.

[agda-formalization]: http://www.cse.chalmers.se/~nad/listings/chi/README.html
[BNFC]: https://bnfc.digitalgrammars.com/
