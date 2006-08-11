(**
   NFAモジュール

   @author Hattori Kenta
   @version $Id: nfa.ml,v 1.2 2006/06/21 00:14:15 hattori Exp $
*)

module R  = Regex
module C  = Cond
module Ht = Hashtbl
module Ps = Set.Make(Pos)

(*
 * 条件付き遷移の型定義
 *)
type trans = Cond.t * Pos.t

(*
 * 条件付き遷移リストの正規化を行う：
 *     矛盾（恒偽）となる命題論理式を含む要素を取り除く
 * 
 *   引　数： ---: trans list --- 条件付き遷移リスト
 * 
 *   戻り値： 正規化した条件付き遷移リスト
 * 
 *)
let normalize tls =
  List.fold_left (
    fun cp (c,p) ->
      match c with
          C.Const(false) -> cp
        | C.Prop(p) when Prop.taut (Prop.Neg p) -> cp
        | _ -> (c,p)::cp
  ) [] tls

(*
 * 条件付遷移リストに条件を追加する
 * 
 *   引　数：c  : Cond.t     --- 追加する条件
 *           tr : trans list --- 条件付き遷移リスト
 * 
 *   戻り値：追加後の条件付き遷移リスト
 * 
 *)
let prod c tr = normalize (List.map (fun (c',p) -> C.conj(c,c'),p) tr)

(*
 * 二つの条件付き遷移リストをマージする
 * 
 *   引　数：tr1 : trans list --- 条件付き遷移リスト1
 *           tr2 : trans list --- 条件付き遷移リスト2
 * 
 *   戻り値：マージした結果の条件付き遷移リスト
 * 
 *)
let rec union tr1 tr2 = normalize (
  match tr1,tr2 with
      [],_ -> tr2
    | _,[] -> tr1
    | (c1,p1)::cp1,(c2,p2)::cp2 when p1=p2 -> (C.disj(c1,c2),p1)::(union cp1 cp2)
    | (c1,p1)::cp1,(c2,p2)::cp2 when p1<p2 -> (c1,p1)::(c2,p2)::(union cp1 cp2)
    | (c1,p1)::cp1,(c2,p2)::cp2            -> (c2,p2)::(c1,p1)::(union cp1 cp2)
)

let rec nullable = function
    R.EPS         -> C.Const true
  | R.CHARS _     -> C.Const false
  | R.SEQ (r1,r2) -> C.conj (nullable r1, nullable r2)
  | R.ALT (r1,r2) -> C.disj (nullable r1, nullable r2)
  | R.CLOS _      -> C.Const true
  | R.LBL (r,_)   -> nullable r
  | R.REP (r,lbl) -> C.disj (C.Prop(Prop.Atom(C.Value lbl)), nullable r)

let rec firstpos = function
    R.EPS         -> []
  | R.CHARS p     -> [C.Const true, p]
  | R.SEQ (r1,r2) -> union (firstpos r1) (prod (nullable r1) (firstpos r2))
  | R.ALT (r1,r2) -> union (firstpos r1) (firstpos r2)
  | R.CLOS r      -> firstpos r
  | R.LBL (r,_)   -> firstpos r
  | R.REP (r,lbl) -> prod (C.neg(C.Prop(Prop.Atom(C.Value lbl)))) (firstpos r)

(** フォロー位置の集合を返す関数を返す
    Regex.t -> Pos.t -> CPs.t *)
let followpos (re:Pos.t Regex.t) =
  let tbl = Ht.create 17 in
  let follow p = Ht.find tbl p in
  let rec fill s = function
      R.EPS         -> ()
    | R.CHARS p     -> Ht.add tbl p s
    | R.SEQ (r1,r2) ->
        fill (union (prod (nullable r2) s) (firstpos r2)) r1;
        fill s r2
    | R.ALT (r1,r2) -> fill s r1; fill s r2
    | R.CLOS r      -> fill (union (firstpos r) s) r
    | R.LBL (r,_)   -> fill s r
    | R.REP (r,lbl) -> fill (union
                               (* 繰返し条件 *)
                               (prod (C.neg(C.Prop(Prop.Atom(C.Counter lbl))))
                                  (firstpos r))
                               (* 脱出条件 *)
                               (prod (C.Prop(Prop.Atom(C.Counter lbl))) s)) r
  in fill [] re; follow

(*
 * 条件付き遷移の判定：
 *     tr1の遷移のなかで対応するtr2でも可能な遷移を取得する
 * 
 *   引　数：tr1 : trans list --- 条件付き遷移1
 *           tr2 : trans list --- 条件付き遷移2
 * 
 *   戻り値：対応する遷移先の組
 *)
let transable tr1 tr2 =
  let cndflt cp cnd =
    List.fold_left (
      fun ps (c,p) -> if C.is_true (C.impl cnd c) then Ps.add p ps else ps
    ) Ps.empty cp in

    List.fold_left
      ( fun pairs (c,p) ->
          let p1 = (cndflt tr1 c) in
          let p2 = (cndflt tr2 c) in
            if Ps.is_empty p2 then raise Exit else (p1,p2)::pairs ) [] tr1

let cp2ps cp = List.fold_left (fun ps (_,p) -> Ps.add p ps) Ps.empty cp
let ps2cset ps = Ps.fold (fun p cs -> Cset.union (Pos.pos2cs p) cs) ps Cset.empty
