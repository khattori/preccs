(**
   遷移条件モジュール

   @author Hattori Kenta
   @version $Id: cond.ml,v 1.2 2006/06/21 00:14:14 hattori Exp $
*)
module P = Prop
module Lm = Map.Make(Label)

(* 遷移条件の型定義 *)
type t =
    Const of bool
  | Prop  of var P.t

(* 値参照種別 *)
and var =
    Counter of Label.t (* カウンタ値を参照 *)
  | Value   of Label.t (* ラベル値を参照   *)

let conj = function
    c1,Const(b2)      -> if b2 then c1 else Const false
  | Const(b1),c2      -> if b1 then c2 else Const false
  | Prop(p1),Prop(p2) -> Prop(P.Conj(p1,p2))
let disj = function
    c1,Const(b2)      -> if b2 then Const true else c1
  | Const(b1),c2      -> if b1 then Const true else c2
  | Prop(p1),Prop(p2) -> Prop(P.Disj(p1,p2))
let neg = function
    Const(b) -> Const(not b)
  | Prop(p)  -> Prop(P.Neg(p))
let impl c1 c2 = disj(neg c1,c2)
let is_true = function
    Const(b) -> b
  | Prop(p)  -> P.taut p

let get_vars = function
    Const _ -> []
  | Prop p  -> P.get_atoms p

let rec gen_conds = function
    [] -> [Const true]
  | v::vs ->
      let cs  = gen_conds vs in
      let vs1 = List.map (fun c -> conj(Prop(P.Atom v),c)) cs in
      let vs2 = List.map (fun c -> conj(Prop(P.Neg (P.Atom v)),c)) cs in
	vs1 @ vs2

(*
 * 遷移条件のラベルのα変換を行う
 * 
 *   引　数：lm  : Lm.t   --- ラベル変換マップ
 *           cond: Cond.t --- 遷移条件
 * 
 *)
let alpha lm = function
    Const b -> Const b
  | Prop p  -> Prop(P.map (
                      function 
                          Counter l when Lm.mem (Label.deref l) lm -> Counter (Lm.find (Label.deref l) lm)
                        | Value   l when Lm.mem (Label.deref l) lm -> Value   (Lm.find (Label.deref l) lm)
                        | v -> v
                    ) p)

let print_var = function
    Counter l -> Printf.printf "C(%d" l; print_string ")"
  | Value l   -> Printf.printf "V(%d" l; print_string ")"

let show = function
    Const true -> print_string "T"
  | Const false -> print_string "F"
  | Prop p -> Prop.show print_var p
