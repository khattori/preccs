(**
   遷移条件モジュール

   @author Hattori Kenta
   @version $Id: cond.ml,v 1.2 2006/06/21 00:14:14 hattori Exp $
*)
module P = Prop
module PosSet = Set.Make(Pos)
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
                          Counter l when Lm.mem l lm -> Counter (Lm.find l lm)
                        | Value   l when Lm.mem l lm -> Value   (Lm.find l lm)
                        | v -> v
                    ) p)

(*
 * 遷移条件リストから命題論理式の組み合わせを抽出
 * 
 *   引　数：cs : Cond.t list --- 遷移条件の組のリスト
 * 
 *   戻り値：生成した論理式のリスト：Cond.t list
 *)
let get_vars cs = 
  let ps = List.fold_left (                      (* 命題変数を抽出 *)
    fun ps -> function
        Const true -> ps
      | Prop p     -> P.get_vars p @ ps
      | _          -> assert false (* Const false は除去済み *)
  ) [] cs in
  let ps' = List.fold_left (                     (* 重複を取り除く *)
    fun r p -> if List.mem p r then r else p::r
  ) [] ps
  in List.map (fun p -> Prop p) (P.gen_comb ps') (* 組み合わせを生成 *)

