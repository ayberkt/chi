open Chi_interpreter.Chi;;
open Chi_parser.AbsChi;;

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
