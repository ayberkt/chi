# χ

Interpreter for a tiny language that makes it easy to define a self-interpreter.

χ was first  formulated by Bengt Nordström. This interpreter follows [his notes](http://www.cse.chalmers.se/edu/year/2018/course/DIT311_Models_of_computation/reading/The_language_chi.pdf).

There is also [an Agda formalization of χ][agda-formalization] by Nils Anders Danielsson. This is useful for formalizing uncomputability proofs in Agda.

## Building

You need [BNFC][BNFC] to generate code for the parser so make sure you have
that installed.

You also need `opam` and `dune`. If you have these installed, run `make deps`
followed by `make` to build. This will create an executable in
`_build/default/interpreter/bin/chi.exe`

To run the tests run `make test`.

[agda-formalization]: http://www.cse.chalmers.se/~nad/listings/chi/README.html
[BNFC]: https://bnfc.digitalgrammars.com/

## Some Examples

Let's first alias the executable for convenience.
```
alias chi=./_build/default/interpreter/bin/chi.exe
```

As an example, let us define addition in χ:
```
rec add = \m. \n.
  case m of
    { Zero() -> n
    ; Suc(m) -> Suc(add m n)
    }
```

We can apply this to `Suc(Suc(Suc(Zero())))` and `Suc(Suc(Zero()))` and
evaluate it by writing the following program in some file `foo.chi`
```
(rec add = \m. \n.
  case m of
    { Zero() -> n
    ; Suc(m) -> Suc(add m n)
    }) Suc(Suc(Suc(Zero()))) Suc(Suc(Zero()))
```
and executing `chi foo.chi` which yields
```
Suc (Suc (Suc (Suc (Suc (Zero ())))))
```
as expected.

What makes χ interesting is that it is small enough that it is quite easy to
represent χ programs in itself. We encode every variable and constructor as a
natural number and then use χ constructors to represent the abstract syntax.
If the `-r` flag is given to χ it will print out the χ-representation instead
of evaluating the program. Here is an example:
```
> chi -r foo.chi

Using the following encodings
-----------------------------
n |-> 0
Zero |-> 0
add |-> 2
Suc |-> 1
m |-> 1
-----------------------------
Apply (Apply (Rec (Suc (Suc (Zero ())), Lambda (Suc (Zero ()), Lambda (Zero (), Case (Var (Suc (Zero ())), Cons (Branch (Zero (), Nil (), Var (Zero ())), Cons (Branch (Suc (Zero ()), Cons (Suc (Zero ()), Nil ()), Const (Suc (Zero ()), Cons (Apply (Apply (Var (Suc (Suc (Zero ()))), Var (Suc (Zero ()))), Var (Zero ())), Nil ()))), Nil ())))))), Const (Suc (Zero ()), Cons (Const (Suc (Zero ()), Cons (Const (Suc (Zero ()), Cons (Const (Zero (), Nil ()), Nil ())), Nil ())), Nil ()))), Const (Suc (Zero ()), Cons (Const (Suc (Zero ()), Cons (Const (Zero (), Nil ()), Nil ())), Nil ())))
``` as the example shows the encodings are not terribly fun to deal with. Of
course we could keep going with this and look at the representation of the
representation (`-rs` prevents the encodings from being printed):
```
> chi -rs foo.chi > bar.chi && chi -r bar.chi

Using the following encodings
-----------------------------
Const |-> 2
Case |-> 8
Branch |-> 7
Zero |-> 1
Rec |-> 10
Nil |-> 0
Var |-> 5
Suc |-> 4
Cons |-> 3
Apply |-> 6
Lambda |-> 9
-----------------------------
Const (Suc (Suc (Suc (Suc (Suc (Suc (Zero ())))))), Cons (Const (Suc (Suc (Suc (Suc (Suc (Suc (Zero ())))))), Cons (Const (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Zero ())))))))))), Cons (Const (Suc (Suc (Suc (Suc (Zero ())))), Cons (Const (Suc (Suc (Suc (Suc (Zero ())))), Cons (Const (Suc (Zero ()), Nil ()), Nil ())), Nil ())), Cons (Const (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Zero ()))))))))), Cons (Const (Suc (Suc (Suc (Suc (Zero ())))), Cons (Const (Suc (Zero ()), Nil ()), Nil ())), Cons (Const (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Zero ()))))))))), Cons (Const (Suc (Zero ()), Nil ()), Cons (Const (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Zero ())))))))), Cons (Const (Suc (Suc (Suc (Suc (Suc (Zero ()))))), Cons (Const (Suc (Suc (Suc (Suc (Zero ())))), Cons (Const (Suc (Zero ()), Nil ()), Nil ())), Nil ())), Cons (Const (Suc (Suc (Suc (Zero ()))), Cons (Const (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Zero ()))))))), Cons (Const (Suc (Zero ()), Nil ()), Cons (Const (Zero (), Nil ()), Cons (Const (Suc (Suc (Suc (Suc (Suc (Zero ()))))), Cons (Const (Suc (Zero ()), Nil ()), Nil ())), Nil ())))), Cons (Const (Suc (Suc (Suc (Zero ()))), Cons (Const (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Zero ()))))))), Cons (Const (Suc (Suc (Suc (Suc (Zero ())))), Cons (Const (Suc (Zero ()), Nil ()), Nil ())), Cons (Const (Suc (Suc (Suc (Zero ()))), Cons (Const (Suc (Suc (Suc (Suc (Zero ())))), Cons (Const (Suc (Zero ()), Nil ()), Nil ())), Cons (Const (Zero (), Nil ()), Nil ()))), Cons (Const (Suc (Suc (Zero ())), Cons (Const (Suc (Suc (Suc (Suc (Zero ())))), Cons (Const (Suc (Zero ()), Nil ()), Nil ())), Cons (Const (Suc (Suc (Suc (Zero ()))), Cons (Const (Suc (Suc (Suc (Suc (Suc (Suc (Zero ())))))), Cons (Const (Suc (Suc (Suc (Suc (Suc (Suc (Zero ())))))), Cons (Const (Suc (Suc (Suc (Suc (Suc (Zero ()))))), Cons (Const (Suc (Suc (Suc (Suc (Zero ())))), Cons (Const (Suc (Suc (Suc (Suc (Zero ())))), Cons (Const (Suc (Zero ()), Nil ()), Nil ())), Nil ())), Nil ())), Cons (Const (Suc (Suc (Suc (Suc (Suc (Zero ()))))), Cons (Const (Suc (Suc (Suc (Suc (Zero ())))), Cons (Const (Suc (Zero ()), Nil ()), Nil ())), Nil ())), Nil ()))), Cons (Const (Suc (Suc (Suc (Suc (Suc (Zero ()))))), Cons (Const (Suc (Zero ()), Nil ()), Nil ())), Nil ()))), Cons (Const (Zero (), Nil ()), Nil ()))), Nil ()))), Nil ())))), Cons (Const (Zero (), Nil ()), Nil ()))), Nil ()))), Nil ()))), Nil ()))), Nil ()))), Nil ()))), Cons (Const (Suc (Suc (Zero ())), Cons (Const (Suc (Suc (Suc (Suc (Zero ())))), Cons (Const (Suc (Zero ()), Nil ()), Nil ())), Cons (Const (Suc (Suc (Suc (Zero ()))), Cons (Const (Suc (Suc (Zero ())), Cons (Const (Suc (Suc (Suc (Suc (Zero ())))), Cons (Const (Suc (Zero ()), Nil ()), Nil ())), Cons (Const (Suc (Suc (Suc (Zero ()))), Cons (Const (Suc (Suc (Zero ())), Cons (Const (Suc (Suc (Suc (Suc (Zero ())))), Cons (Const (Suc (Zero ()), Nil ()), Nil ())), Cons (Const (Suc (Suc (Suc (Zero ()))), Cons (Const (Suc (Suc (Zero ())), Cons (Const (Suc (Zero ()), Nil ()), Cons (Const (Zero (), Nil ()), Nil ()))), Cons (Const (Zero (), Nil ()), Nil ()))), Nil ()))), Cons (Const (Zero (), Nil ()), Nil ()))), Nil ()))), Cons (Const (Zero (), Nil ()), Nil ()))), Nil ()))), Nil ()))), Cons (Const (Suc (Suc (Zero ())), Cons (Const (Suc (Suc (Suc (Suc (Zero ())))), Cons (Const (Suc (Zero ()), Nil ()), Nil ())), Cons (Const (Suc (Suc (Suc (Zero ()))), Cons (Const (Suc (Suc (Zero ())), Cons (Const (Suc (Suc (Suc (Suc (Zero ())))), Cons (Const (Suc (Zero ()), Nil ()), Nil ())), Cons (Const (Suc (Suc (Suc (Zero ()))), Cons (Const (Suc (Suc (Zero ())), Cons (Const (Suc (Zero ()), Nil ()), Cons (Const (Zero (), Nil ()), Nil ()))), Cons (Const (Zero (), Nil ()), Nil ()))), Nil ()))), Cons (Const (Zero (), Nil ()), Nil ()))), Nil ()))), Nil ())))
```
but you probably don't want to.

There is a χ-interpreter written for such encodings and it is in
`test/test_programs/self-interpreter.chi`. There are also some sample programs
written in χ-encodings such as
`test/test_programs/self_interpreter_tests/case-4.chi` which you can run with
the self-interpreter.
