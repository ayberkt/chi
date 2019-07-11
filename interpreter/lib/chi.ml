open Chi_parser
open AbsChi
open PrintChi

module L = Core.List

let sprintf = Core.Printf.sprintf

(* Infix wrapper for map. *)
let (<$>) f xs = L.map ~f:f xs

(* Wrapper for list membership. *)
let mem x xs = L.mem xs x ~equal:(=)

(* Should be read as "subst x in e with e'". *)
let rec subst (x : variable) (e : exp) (e' : exp) : exp =
  match e with
  | Var x' when x = x' -> e'
  | Var x' -> Var x'
  | Apply (e1, e2) -> Apply (subst x e1 e', subst x e2 e')
  | Lambda (x', e1) when x = x' -> Lambda (x', e1)
  | Lambda (x', e1) -> Lambda (x', subst x e1 e')
  | Case (e1, brs) -> Case (subst x e1 e', (fun br -> substBr x br e') <$> brs)
  | Rec (x', e1) when x = x' -> Rec (x', e1)
  | Rec (x', e1) -> Rec (x', subst x e1 e')
  | Const (c, es) -> Const (c, (fun e0 -> subst x e0 e') <$> es)
and substBr (x : variable) (br : br) (e' : exp) : br =
  match br with
  | Branch (c, xs, e) when mem x xs -> Branch (c, xs, e)
  | Branch (c, xs, e) -> Branch (c, xs, subst x e e')

exception SubstMultiMismatch of string

let rec substMulti ((Constructor cs) : constructor) (xs : variable list) (e : exp) (es : exp list) : exp =
  match xs, es with
  | [], [] -> e
  | x::xs,  _::es when mem x xs -> substMulti (Constructor cs) xs  e es
  | x::xs, e'::es -> substMulti (Constructor cs) xs  (subst x e e') es
  | _ -> raise (SubstMultiMismatch cs)

exception NonFunctionAppliedToArg
exception MatchingOnNonConst of string
exception NoMatchingPattern of string

let rec isBranchClosed (xs : variable list) (br : br) : bool =
  match br with
  | Branch (_, xs', e) -> isClosed (L.append xs xs') e
and isClosed (ctx : variable list) (e : exp) =
  match e with
  | Var    x        -> List.mem x ctx
  | Const  (_, es)  -> L.for_all es ~f:(isClosed ctx)
  | Apply  (e0, e1) -> isClosed ctx e0 && isClosed ctx e1
  | Lambda (x, e)   -> isClosed (x::ctx) e
  | Case   (e, brs) -> isClosed ctx e && L.for_all brs ~f:(isBranchClosed ctx)
  | Rec    (x, e)   -> isClosed (x::ctx) e

let rec lookup (c : constructor) (bs : br list) : (variable list * exp) option =
  match bs with
  | [] -> None
  | Branch (c', xs, e')::_  when c = c' -> Some (xs, e')
  | Branch (_,   _,  _)::bs -> lookup c bs

let rec eval : exp -> exp = function
  | Lambda (x, e) -> Lambda (x, e)
  | Apply (e1, e2) ->
      (match eval e1 with
       | Lambda (x, e) -> eval (subst x e (eval e2))
       | _ -> raise NonFunctionAppliedToArg)
  | Case (e, bs) ->
      (match eval e with
       | Const (c, vs) ->
           (match lookup c bs with
            | Some (xs, e') -> eval (substMulti c xs e' vs)
            | None -> let Constructor s = c in raise (NoMatchingPattern s))
       | _ -> raise (MatchingOnNonConst (printTree prtExp e)))
  | Rec (x, e) -> eval (subst x e (Rec (x, e)))
  | Var x -> Var x
  | Const (c, es) -> Const (c, eval <$> es)

type result = Success of exp | Error of string

let eval_top (e : exp) : result =
  try
    Success (eval e)
  with
  | NonFunctionAppliedToArg -> Error "non-function applied"
  | MatchingOnNonConst s    -> Error (sprintf "attempt to match on non-constructor: %s" s)
  | NoMatchingPattern  s    -> Error (sprintf "no matching pattern for %s" s)
  | SubstMultiMismatch s    -> Error (sprintf "matching case with unidentical arity: %s" s)
