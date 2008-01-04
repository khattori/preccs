(**
   パタンマッチコンパイルモジュール
   
   概要：パタンマッチコンパイルを行なう

   @author Hattori Kenta
   @version $Id: pi.ml,v 1.6 2006/07/27 00:07:18 hattori Exp $
*)
module A = Syntax
module L = List
module P = Pi

exception Not_exhaustive

let divide_list f ls =
  let rec iter l1 = function
      l::ls when f l -> iter (l::l1) ls
    | ls -> L.rev l1,ls
  in
    iter [] ls

let is_wild_or_var_pat (ps,_) =
  match L.hd ps with
      A.PatAny _ | A.PatIdent _ -> true
    | _ -> false
let is_unit_pat (ps,_) =
  match L.hd ps with
      A.PatConst(A.ConUnit _) -> true
    | _ -> false

let is_const_pat (ps,_) =
  match L.hd ps with
      A.PatConst _ -> true
    | _ -> false
let is_tuple_pat (ps,_) =
  match L.hd ps with
      A.PatTuple _ | A.PatRecord _ -> true
    | _ -> false
let is_variant_pat (ps,_) =
  match L.hd ps with
      A.PatVariant _ -> true
    | _ -> false

let make_let x (pat,pexp) =
  match L.hd pat with
      A.PatAny _ -> L.tl pat,pexp
    | A.PatIdent(_,s) -> L.tl pat,P.Let(s,P.Var x,pexp)
    | _ -> assert false

let top_left pmat = L.hd (fst (L.hd pmat))
let get_args = function
    A.PatTuple ls -> L.length ls | A.PatRecord ls -> L.length ls
  | _ -> assert false

let list_case_const pmat =
  let rec iter result = function
      [] -> L.rev result
    | (A.PatConst(A.ConBool(_,b))::_,_)::rs -> iter (if L.mem (P.Bool b) result then result else (P.Bool b)::result) rs
    | (A.PatConst(A.ConInt(_,i))::_,_)::rs -> iter (if L.mem (P.Int i) result then result else (P.Int i)::result) rs
    | (A.PatConst(A.ConStr(_,s))::_,_)::rs -> iter (if L.mem (P.String s) result then result else (P.String s)::result) rs
    | (A.PatConst(A.ConUnit _)::_,_)::_ -> [P.Unit]
    | _ -> assert false
  in
    iter [] pmat

let list_case_variant pmat =
  let rec iter result = function
      [] -> L.rev result
    | (A.PatVariant(_,s,_)::_,_)::rs ->
	iter (if L.mem s result then result else s::result) rs
    | _ -> assert false
  in
    iter [] pmat

let select_variant_pat sym pmat =
  let rec iter result = function
      [] -> L.rev result
    | (A.PatVariant(_,s,p)::ps,pexp)::rs ->
	iter (if s = sym then (p::ps,pexp)::result else result) rs
    | _ -> assert false
  in
    iter [] pmat

let select_bool_pat b pmat =
  let rec iter result = function
      [] -> L.rev result
    | (A.PatConst(A.ConBool(_,b'))::ps,pexp)::rs ->
	iter (if b = b' then (ps,pexp)::result else result) rs
    | _ -> assert false
  in
    iter [] pmat

let select_int_pat i pmat =
  let rec iter result = function
      [] -> L.rev result
    | (A.PatConst(A.ConInt(_,i'))::ps,pexp)::rs ->
	iter (if i = i' then (ps,pexp)::result else result) rs
    | _ -> assert false
  in
    iter [] pmat

let select_string_pat s pmat =
  let rec iter result = function
      [] -> L.rev result
    | (A.PatConst(A.ConStr(_,s'))::ps,pexp)::rs ->
	iter (if s = s' then (ps,pexp)::result else result) rs
    | _ -> assert false
  in
    iter [] pmat

let select_unit_pat pmat =
  let rec iter result = function
      [] -> L.rev result
    | (A.PatConst(A.ConUnit _)::ps,pexp)::rs -> iter ((ps,pexp)::result) rs
    | _ -> assert false
  in
    iter [] pmat


let flatten_pat = function
    A.PatTuple ps::pats,pexp  -> ps@pats,pexp
  | A.PatRecord rs::pats,pexp -> (L.map (fun (_,_,p) -> p) rs)@pats,pexp
  | _ -> assert false

let rec trans vars pmat =
  match vars with
      [] -> snd (L.hd pmat)
    | x::xs when L.for_all is_wild_or_var_pat pmat
	  -> trans xs (L.map (make_let x) pmat)
    | x::xs when L.for_all is_unit_pat pmat
          -> trans xs pmat
    | x::xs when L.for_all is_const_pat pmat
	  -> P.Case(P.Var x,
                    L.map
                    (make_case_const_clause x xs pmat)
                    (list_case_const pmat) @ [None,P.Exit])
    | x::xs when L.for_all is_tuple_pat pmat
	  -> let n = get_args (top_left pmat) in
	       make_tuple_pat n x xs pmat
    | x::xs when L.for_all is_variant_pat pmat
	  -> P.Case(P.Select(P.Var x,P.Const(P.Int 0)),
		   L.map
		   (make_case_variant_clause x xs pmat)
		   (list_case_variant pmat) @ [None,P.Exit])
    | _ -> let p1,p2 = split_pat pmat in P.Catch(trans vars p1, trans vars p2)

and make_tuple_pat nargs x xs pmat =
  let rec iter i fs =
    if i < nargs then
      let v_fld = Symbol.genid "fld" in
	P.Let(v_fld,P.Select(P.Var x,P.Const(P.Int i)),iter (i+1) (v_fld::fs))
    else
      trans ((L.rev fs) @ xs) (L.map flatten_pat pmat)
  in
    iter 0 []
    
and make_case_const_clause x xs pmat = function
    P.Bool b   -> Some(P.Bool b),trans xs (select_bool_pat b pmat)
  | P.Int i    -> Some(P.Int i),trans xs (select_int_pat i pmat)
  | P.String s -> Some(P.String s),trans xs (select_string_pat s pmat)
  | P.Unit     -> Some(P.Unit),trans xs (select_unit_pat pmat)

and make_case_variant_clause x xs pmat s =
  let v_fld = Symbol.genid "fld" in
    Some(P.Int(Symbol.id s)),
      P.Let(v_fld, P.Select(P.Var x,P.Const(P.Int 1)),
	   trans (v_fld::xs) (select_variant_pat s pmat))

and split_pat pmat =
  match top_left pmat with
      A.PatAny _ | A.PatIdent _ ->
	divide_list
	  (fun (pat,_) -> match L.hd pat with
	      A.PatAny _ | A.PatIdent _ -> true
	    | _ -> false)
	  pmat
    | _ ->
	divide_list
	  (fun (pat,_) -> match L.hd pat with
	      A.PatAny _ | A.PatIdent _ -> false
	    | _ -> true)
	  pmat

