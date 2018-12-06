open Chi_interpreter.Chi;;
open Chi_parser.AbsChi;;
open Chi_interpreter.Representation;;

open Core

module H = Hashtbl

let varx = Variable "x";;
let vary = Variable "y";;

let case1 () =
  Alcotest.(check bool)
    ""
    (isClosed [] (Var (Variable "x")))
    false
;;

let case2 () =
  Alcotest.(check bool)
    ""
    (isClosed [varx] (Var varx))
    true
;;

let case3 () =
  Alcotest.(check bool)
    ""
    (isClosed [] (Lambda (varx, Var varx)))
    true
;;

let case4 () =
  Alcotest.(check bool)
    ""
    (isClosed [] (Lambda (varx, Var vary)))
    false
;;

let case5 () =
  Alcotest.(check bool)
    ""
    (isClosed [] (Lambda (varx, Lambda (vary, Apply (Var varx, Var vary)))))
    true
;;

let eval_case1 () =
  let ctrue = Const (Constructor "True", []) in
  let exp   =
    (Apply (Lambda (Variable "m", Case (Var (Variable "m"),
     [Branch (Constructor "Zero", [], ctrue)])),
     Const (Constructor "Zero", [])))
  in
    Alcotest.(check bool)
      ""
      (eval exp = ctrue)
      true

let eval_case2 () =
  let
    f = Lambda (varx, (Var varx))
  in let
    arg = Const (Constructor "Zero", [])
  in
    Alcotest.(check bool)
      ""
      (eval (Apply (f, arg)) = arg)
      true

let eval_case3 () =
  let f    = Lambda (varx, Lambda (varx, (Var varx))) in
  let arg1 = Const (Constructor "Zero", [])           in
  let arg2 = Const (Constructor "True", [])           in
    Alcotest.(check bool)
      ""
      (eval (Apply (Apply (f, arg1), arg2)) = arg2)
      true

let eval_case4 () =
  let e =
    Apply (Apply (Rec (Variable "foo", Lambda (Variable "m", Lambda (Variable "n", Case (Var (Variable "m"), [Branch (Constructor "Zero", [], Case (Var (Variable "n"), [Branch (Constructor "Zero", [], Const (Constructor "True", [])); Branch (Constructor "Suc", [Variable "n"], Const (Constructor "False", []))])); Branch (Constructor "Suc", [Variable "m"], Case (Var (Variable "n"), [Branch (Constructor "Zero", [], Const (Constructor "False", [])); Branch (Constructor "Suc", [Variable "n"], Apply (Apply (Var (Variable "foo"), Var (Variable "m")), Var (Variable "n")))]))])))), Const (Constructor "Suc", [Const (Constructor "Suc", [Const (Constructor "Zero", [])])])), Const (Constructor "Suc", [Const (Constructor "Suc", [Const (Constructor "Zero", [])])]))
  in
    Alcotest.(check bool)
      ""
      (eval e = (Const (Constructor "True", [])))
      true

let subst_case1 () =
  let subst_term = Const (Constructor "Z", []) in
  let term       = Rec (varx, Var varx)       in
  Alcotest.(check bool)
    ""
    ((subst varx term subst_term) = term)
    true

let tests =
  [ "x not closed",            `Quick, case1
  ; "x closed, assuming x",    `Quick, case2
  ; "\\x. x closed",           `Quick, case3
  ; "\\x. y not closed",       `Quick, case4
  ; "\\x. \\y. x y closed",    `Quick, case5
  ];;

let eval_tests =
  [ "eval (1)",                 `Quick, eval_case1
  ; "(\\x. x) Zero() = Zero()", `Quick, eval_case2
  ; "Shadowing (1)",            `Quick, eval_case3
  ; "Complicated (1)",          `Quick, eval_case4
  ]

let subst_tests =
  [ "subst (1)", `Quick, subst_case1
  ]

let parse (c : In_channel.t) : Chi_parser.AbsChi.exp =
  let open Chi_parser in
  ParChi.pExp LexChi.token (Lexing.from_channel c)

let self_interpreter : exp =
  let channel = In_channel.create "../../../test/test_programs/self-interpreter.chi" in
  parse channel

let addition : exp =
  let channel = In_channel.create "../../../test/test_programs/addition.chi" in
  parse channel

let rep_one_plus_one x y =
  represent (Apply (Apply (addition, rep_nat x), rep_nat y)) None false

let self_intpt_add x y () =
  match rep_one_plus_one x y with
  | prog, rho ->
      let result   = eval (Apply (self_interpreter, prog)) in
      let expected = rep_nat (x + y) in
      Alcotest.(check bool)
        ""
        (result = fst (represent expected (Some rho) false))
        true

let self_interpreter_tests =
  [ "self-interpreter, 0 + 0",   `Quick, self_intpt_add 0   0
  ; "self-interpreter, 0 + 1",   `Quick, self_intpt_add 0   1
  ; "self-interpreter, 1 + 0",   `Quick, self_intpt_add 1   0
  ; "self-interpreter, 2 + 3",   `Quick, self_intpt_add 3   5
  ; "self-interpreter, 20 + 30", `Slow,  self_intpt_add 20  30
  ]
