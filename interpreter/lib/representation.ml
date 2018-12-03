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

    method add_var (v : variable) : int =
      match v with
      | Variable x ->
          (match H.find rho x with
           | Some n -> n
           | None ->
               let vr = !vcount in
               H.set rho ~key:x ~data:!vcount;
               incr vcount;
               vr)

    method add_const (c : constructor) : int =
      match c with
      | Constructor x ->
          (match H.find rho x with
           | Some n -> n
           | None ->
               let cc = !ccount in
               H.set rho ~key:x ~data:!ccount;
               incr ccount;
               cc)

    method explain_coding () : unit =
      let
        explain ~key:(key : string) ~data:(data : int) : unit =
          P.printf "%s |-> %d\n" key data; Out_channel.flush Out_channel.stdout;
      in
        P.printf "%s\n" "Using the following encodings";
        H.iteri rho ~f:explain
  end

type code = (variable, int) Map.Poly.t

let rec rep_nat : int -> exp = function
  | 0 -> Const (Constructor "Zero", [])
  | n -> Const (Constructor "Suc", [rep_nat (n-1)])

exception Unbound

let represent : exp -> exp = fun e ->
  let rho    = new environment in
  let rec rep (e : exp) : exp =
    match e with
    | Var v -> Const (Constructor "Var", [rep_nat (rho#add_var v)])
    | Lambda (v, e) ->
        Const (Constructor "Lambda", [rep_nat (rho#add_var v) ; rep e])
    | Apply (e1, e2) -> Const (Constructor "Apply", [rep e1; rep e2])
    | Case (e, bs) -> Const (Constructor "Case", [rep e; represent_bs bs])
    | Rec (v, e) ->
        Const (Constructor "Rec", [rep_nat (rho#add_var v); rep e])
    | Const (c, es) ->
      Const (Constructor "Const",
             [ rep_nat (rho#add_const c);  represent_multi es ])
  and represent_multi (es : exp list) : exp =
    match es with
    | [] -> Const (Constructor "Nil", [])
    | e::es -> Const (Constructor "Cons", [rep e; represent_multi es])
  and represent_bs (bs : br list) : exp =
    match bs with
    | [] -> Const (Constructor "Nil", [])
    | b::bs -> Const (Constructor "Cons", [rep_branch b; represent_bs bs])
  and rep_vs : variable list -> exp = function
    | [] -> Const (Constructor "Nil", [])
    | v::xs ->
        Const (Constructor "Cons", [rep_nat (rho#add_var v); rep_vs xs])
  and rep_branch : br -> exp = function
    | Branch (c, xs, e) ->
        Const (Constructor "Branch", [rep_nat (rho#add_const c); rep_vs xs; rep e])
  in
    let e_rep = rep e in
    rho#explain_coding ();
    e_rep
