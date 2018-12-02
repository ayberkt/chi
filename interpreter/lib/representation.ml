open Core
open Chi_parser
open AbsChi

module L = List
module M = Map
module H = Hashtbl

exception Todo

type code = (variable, int) Map.Poly.t

let rec represent : int -> exp = function
  | 0 -> Const (Constructor "Zero", [])
  | n -> Const (Constructor "Suc", [represent n])

let represent : exp -> exp =
  let vcount = ref 0 in
  let ccount = ref 0 in
  let rho    = String.Table.create () ~size:20 in
  let rec represent' (e : exp) : exp =
    match e with
    | Var (Variable x) ->
        (match H.find rho x with
         | Some n -> Const (Constructor "Var", [represent n])
         | None -> raise Todo)
    | Lambda (Variable x, e) ->
        H.set rho ~key:x ~data:!vcount;
        incr vcount;
        Const (Constructor "Lambda", [represent' e])
    | Apply (e1, e2) -> Apply (represent' e1, represent' e2)
    | Case (e, bs) -> Const (Constructor "Case", [represent' e; represent_bs bs])
    | Rec (Variable x, e) ->
        H.set rho ~key:x ~data:!vcount;
        incr vcount;
        Const (Constructor "Rec", [represent' e])
    | Const (Constructor c, es) ->
        (match H.find rho c with
         | Some cr -> Const (Constructor "Const", [represent cr; represent' e])
         | None ->
             let cr = !ccount in
             H.set rho ~key:c ~data:!ccount;
             incr ccount;
             Const (Constructor "Const", [represent cr; represent_multi es]))
  and represent_multi (es : exp list) : exp =
    match es with
    | [] -> Const (Constructor "Nil", [])
    | e::es -> Const (Constructor "Cons", [represent' e; represent_multi es])
  and represent_bs (bs : br list) : exp =
    match bs with
    | [] -> Const (Constructor "Nil", [])
    | b::bs -> Const (Constructor "Cons", [rep_branch b; represent_bs bs])
  and rep_vs : variable list -> exp = function
    | [] -> Const (Constructor "Nil", [])
    | (Variable x)::xs ->
        match H.find rho x with
          | Some n -> Const (Constructor "Cons", [represent n; rep_vs xs])
        | None ->
            let vc = !vcount in
            H.set rho ~key:x ~data:!vcount;
            incr vcount;
            Const (Constructor "Cons", [represent vc; rep_vs xs])
  and rep_branch : br -> exp = function
    | Branch (Constructor c, xs, e) ->
        match H.find rho c with
          | Some n ->
              Const (Constructor "Branch", [represent n; rep_vs xs; represent' e])
          | None ->
              let cc = !ccount in
              H.set rho ~key:c ~data:!ccount;
              incr ccount;
              Const (Constructor "Branch", [represent cc; rep_vs xs; represent' e])
  in
    represent'
