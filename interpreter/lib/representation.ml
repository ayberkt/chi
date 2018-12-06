open Core
open Chi_parser
open AbsChi

module L = List
module M = Map
module H = Hashtbl
module P = Printf

exception Todo

class environment =
  object (_)
    val vcount = ref 0
    val ccount = ref 0
    val rho : (string, int) H.t = String.Table.create ()

    method code_var (v : variable) : int =
      match v with
      | Variable x ->
          (match H.find rho x with
           | Some n -> n
           | None ->
               let vr = !vcount in
               H.set rho ~key:x ~data:!vcount;
               incr vcount;
               vr)

    method code_const (Constructor x : constructor) : int =
      match H.find rho x with
      | Some n -> n
      | None ->
          let cc = !ccount in
          H.set rho ~key:x ~data:!ccount;
          incr ccount;
          cc

    method explain_coding () : unit =
      let
        explain ~key:key ~data:data = P.printf "%s |-> %d\n" key data
      in
        P.printf "%s\n" "Using the following encodings";
        P.printf "%s\n" "-----------------------------";
        H.iteri rho ~f:explain;
        P.printf "%s\n" "-----------------------------"
  end

let rec rep_nat : int -> exp = function
  | 0 -> Const (Constructor "Zero", [])
  | n -> Const (Constructor "Suc", [rep_nat (n-1)])

exception Unbound

let represent (e : exp) (mrho : environment option) (verbose : bool) =
  let ccons    = Constructor "Cons"   in
  let cnil     = Constructor "Nil"    in
  let clam     = Constructor "Lambda" in
  let capp     = Constructor "Apply"  in
  let crec     = Constructor "Rec"    in
  let nil_rep  = Const (cnil, [])     in
  let rho =
    match mrho with
    | Some rho -> rho
    | None -> new environment
  in let rec rep (e : exp) : exp =
    match e with
    | Var v -> Const (Constructor "Var", [rep_nat (rho#code_var v)])
    | Lambda (v, e) -> Const (clam, [rep_nat (rho#code_var v); rep e])
    | Apply (e1, e2) -> Const (capp, [rep e1; rep e2])
    | Case (e, bs) -> Const (Constructor "Case", [rep e; rep_bs bs])
    | Rec (v, e) -> Const (crec, [rep_nat (rho#code_var v); rep e])
    | Const (c, es) ->
        Const (Constructor "Const",
              [ rep_nat (rho#code_const c);  represent_multi' es ])
  and represent_multi' es : exp =
    L.fold_right es ~f:(fun x y -> Const (ccons, [rep x; y])) ~init:nil_rep
  and rep_bs (bs : br list) : exp =
    L.fold_right bs ~f:(fun x r -> Const (ccons, [rep_branch x; r])) ~init:nil_rep
  and rep_vs : variable list -> exp = function
    | [] -> Const (Constructor "Nil", [])
    | v::xs ->
        Const (ccons, [rep_nat (rho#code_var v); rep_vs xs])
  and rep_branch : br -> exp = function
    | Branch (c, xs, e) ->
        Const (Constructor "Branch",
               [rep_nat (rho#code_const c);rep_vs xs; rep e])
  in
    let e_rep = rep e in
    if verbose then rho#explain_coding () else ();
    (e_rep, rho)
