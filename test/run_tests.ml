(*
   Run all the OCaml test suites defined in the project.
*)

let test_suites: unit Alcotest.test list = [
  "Closed-checking", Test_interpreter.A.tests;
  "Substitution",    Test_interpreter.A.subst_tests;
  "Evaluation",      Test_interpreter.A.eval_tests;
];;

let () = Alcotest.run "chi" test_suites;;
