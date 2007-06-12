(**
   NFAモジュール

   @author Hattori Kenta
   @version $Id: nfa.ml,v 1.2 2006/06/21 00:14:15 hattori Exp $
*)
module R  = Regex
module C  = Cond
module Ht = Hashtbl
module Ps = Set.Make(Pos)
module Ls = Set.Make(Label)
module Lm = Map.Make(Label)

(*
 * 条件付き遷移の型定義
 *)
type trans = Cond.t * Ls.t * Pos.t

(*
 * ラベルをα変換する
 * 
 *   引　数：lm  : Lm.t       --- ラベル変換マップ
 *           trs : trans list --- 条件付き遷移
 *)
let alpha lm trs =
  List.map (fun (cond,ls,pos) -> C.alpha lm cond,ls,pos) trs

(*
 * 新規ラベルを作成してラベル変換表に追加する
 * 
 *   引　数：ls : Ls.t --- ラベル集合
 *           lm : Lm.t --- 元となるラベル変換表
 * 
 *   戻り値：ラベル変換表Label.t -> Label.t (Lm.t)
 * 
 *)
let add_lblmap ls lm =
  let sztbl = Ht.create(13) in
    (* サイズ別にラベルを分類する *)
    Ls.iter (fun l ->
               let sz = Label.size l in
                 if Ht.mem sztbl sz then
                   Ht.replace sztbl sz (Ls.add l (Ht.find sztbl sz))
                 else
                   Ht.add sztbl sz (Ls.singleton l)) ls;
    (* ラベル変換表に登録 *)
    Ht.fold (fun sz ls lm' -> 
               let l = Label.create sz in
                 Ls.fold (fun l' lm'' -> Lm.add l' l lm'') ls lm'
            ) sztbl lm


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
    fun cp (c,l,p) ->
      match c with
          C.Const(false) -> cp
        | C.Prop(p) when Prop.taut (Prop.Neg p) -> cp
        | _ -> (c,l,p)::cp
  ) [] tls

(*
 * 条件付遷移リストに条件を追加する
 * 
 *   引　数：c  : Cond.t         --- 追加する条件
 *           tr : trans list     --- 条件付き遷移リスト
 * 
 *   戻り値：追加後の条件付き遷移リスト
 * 
 *)
let cprod c tr = normalize (List.map (fun (c',l,p) -> C.conj(c,c'),l,p) tr)

(*
 * 条件付遷移リストにラベルを追加する
 * 
 *   引　数：l  : Label.t        --- 追加するラベル
 *           tr : trans list     --- 条件付き遷移リスト
 * 
 *   戻り値：追加後の条件付き遷移リスト
 * 
 *)
let lprod l tr = List.map (fun (c,ls,p) -> c,Ls.add l ls,p) tr

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
    | (c1,l1,p1)::cp1,(c2,l2,p2)::cp2 when p1=p2
        -> (C.disj(c1,c2),Ls.union l1 l2,p1)::(union cp1 cp2)
    | (c1,l1,p1)::cp1,(c2,l2,p2)::cp2 when p1<p2
        -> (c1,l1,p1)::(c2,l2,p2)::(union cp1 cp2)
    | (c1,l1,p1)::cp1,(c2,l2,p2)::cp2
        -> (c2,l2,p2)::(c1,l1,p1)::(union cp1 cp2)
)

let rec nullable = function
    R.EPS         -> C.Const true
  | R.CHARS _     -> C.Const false
  | R.SEQ (r1,r2) -> C.conj (nullable r1, nullable r2)
  | R.ALT (r1,r2) -> C.disj (nullable r1, nullable r2)
  | R.CLOS _      -> C.Const true
  | R.LBL (r,_)   -> nullable r
  | R.REP (r,l)   -> C.disj (C.Prop(Prop.Atom(C.Value l)), nullable r)

let rec firstpos = function
    R.EPS         -> []
  | R.CHARS p     -> [C.Const true, Ls.empty, p]
  | R.SEQ (r1,r2) -> union (firstpos r1) (cprod (nullable r1) (firstpos r2))
  | R.ALT (r1,r2) -> union (firstpos r1) (firstpos r2)
  | R.CLOS r      -> firstpos r
  | R.LBL (r,lbl) -> lprod lbl (firstpos r)
  | R.REP (r,l)   -> cprod (C.neg(C.Prop(Prop.Atom(C.Value l)))) (firstpos r)

(** フォロー位置の集合を返す関数を返す
    Regex.t -> Pos.t -> CPs.t *)
let followpos (re:Pos.t Regex.t) =
  let tbl = Ht.create 17 in
  let follow p = Ht.find tbl p in
  let rec fill s = function
      R.EPS         -> ()
    | R.CHARS p     -> Ht.add tbl p s
    | R.SEQ (r1,r2) ->
        fill (union (cprod (nullable r2) s) (firstpos r2)) r1;
        fill s r2
    | R.ALT (r1,r2) -> fill s r1; fill s r2
    | R.CLOS r      -> fill (union (firstpos r) s) r
    | R.LBL (r,_)   -> fill s r
    | R.REP (r,l)   -> fill (union
                               (* 繰返し条件 *)
                               (cprod (C.neg(C.Prop(Prop.Atom(C.Counter l))))
                                  (firstpos r))
                               (* 脱出条件 *)
                               (cprod (C.Prop(Prop.Atom(C.Counter l))) s)) r
  in fill [] re; follow

(*
 * 条件付き遷移の判定：
 *     tr1の遷移のなかで対応するtr2でも可能な遷移を取得する
 * 
 *   引　数：tr1 : trans list             --- 条件付き遷移1
 *           tr2 : trans list             --- 条件付き遷移2
 * 
 *   戻り値：対応する遷移先の組
 *)
let transable tr1 tr2 =
  (* 遷移先の位置集合を取得 *)
  let cndflt cp cnd =
    List.fold_left (
      fun (ps,ls) (c,ls',p) ->
        if C.is_true (C.impl cnd c) then (Ps.add p ps,Ls.union ls ls') else (ps,ls)
    ) (Ps.empty,Ls.empty) cp in
    List.fold_left
      ( fun pairs (c,ls,p) ->
          let p1,ls1 = (cndflt tr1 c) in
          let p2,ls2 = (cndflt tr2 c) in
            if Ps.is_empty p2 then raise Exit else (p1,ls1,p2,ls2)::pairs ) [] tr1

let cp2ps cp = List.fold_left (fun ps (_,_,p) -> Ps.add p ps) Ps.empty cp
let cp2ls cp = List.fold_left (fun ls (_,ls',_) -> Ls.union ls ls') Ls.empty cp
let ps2cset ps = Ps.fold (fun p cs -> Cset.union (Pos.pos2cs p) cs) ps Cset.empty
