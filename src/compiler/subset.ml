(**
   正規表現の包含判定モジュール

   @author Hattori Kenta
   @version $Id: subset.ml,v 1.3 2006/06/21 00:14:15 hattori Exp $
*)

module R  = Regex
module N  = Nfa
module Ht = Hashtbl

(** 位置集合 *)
module Ps = Set.Make(Pos)
module Lm = Map.Make(Label)   (* ラベル→ラベル *)
module Ls = Set.Make(Label)

(*
 * ラベルの開始ノードを記録する
 * 
 *   引　数：tbl: Ht.t    --- ラベル記録表 (Pos.t -> Ls.t)
 *           re : Regex.t --- 正規表現ツリー
 * 
 *)
let rec rcdlabel tbl = function
    R.EPS | R.CHARS _ -> ()
  | R.SEQ(r1,r2) | R.ALT(r1,r2) -> rcdlabel tbl r1; rcdlabel tbl r2
  | R.CLOS(r)    | R.REP(r,_)   -> rcdlabel tbl r
  | R.LBL(r,l) -> List.iter (
      fun (_,p) ->
        if Ht.mem tbl p then
          Ht.replace tbl p (Ls.add l (Ht.find tbl p))
        else
          Ht.add tbl p (Ls.singleton l)
    ) (N.firstpos r)

(*
 * 位置集合から新規ラベルを作成してラベル変換表に追加する
 * 
 *   引　数：ps : Ps.t --- 位置集合
 *           tbl: Ht.t --- Pos.t -> Ls.tの表
 *           lm : Lm.t --- 元となるラベル変換表
 * 
 *   戻り値：ラベル変換表Label.t -> Label.t (Lm.t)
 * 
 *)
let add_lblmap ps tbl lm =
  (* 位置に関連付けられているラベルを全て求める *)
  let lblset = Ps.fold (fun p ls ->
                          if Ht.mem tbl p then
                            Ls.union (Ht.find tbl p) ls
                          else
                            ls) ps Ls.empty in

  (* サイズ別にラベルを分類する *)
  let sztbl = Ht.create(13) in
    Ls.iter (fun l ->
               let sz = Label.size l in
                 if Ht.mem sztbl sz then
                   Ht.replace sztbl sz (Ls.add l (Ht.find sztbl sz))
                 else
                   Ht.add sztbl sz (Ls.singleton l)) lblset;

      List.fold_left (fun lm' (sz,ls) ->
               let l = Label.create sz in
                 Ls.fold (fun l' lm'' -> Lm.add l' l lm'') ls lm'
            ) lm (Ht.fold (fun sz ls lslst -> (sz,ls)::lslst) sztbl [])

(*
 * 正規表現の包含関係判定(r1⊆r2かどうか)
 * 
 *   引　数：r1 : Regex.t --- 正規表現1
 *           r2 : Regex.t --- 正規表現2
 * 
 *   戻り値：判定結果を真偽値で返す
 * 
 *)
let subset r1 r2 =
  let lpos1 = Pos.create Cset.fin in
  let lpos2 = Pos.create Cset.fin in
  let r1' = R.SEQ(R.posify r1,R.CHARS lpos1) in
  let r2' = R.SEQ(R.posify r2,R.CHARS lpos2) in
  let follow1 = N.followpos r1' in
  let follow2 = N.followpos r2' in
  let ltbl = Ht.create(13) in  (* Pos.t -> Ls.t のテーブル *)
  let marked = ref [] in
    (** CPs.t -> C.t -> Ps.t *) 
  let rec check lm = function
      [] -> ()
    | (s1,s2)::rs ->
        (* 等価なラベルを検索 *)
        let lm' = add_lblmap (Ps.union s1 s2) ltbl lm in
          marked := (s1,s2)::!marked;
          Cset.iter (
            fun a ->
              let f flw p cp =
                if Cset.mem a (Pos.pos2cs p) then N.union cp (flw p) else cp in
              let cp1 = Ps.fold (f follow1) s1 [] in (* R1のフォロー集合 *)
              let cp2 = Ps.fold (f follow2) s2 [] in (* R2のフォロー集合 *)
              let cp1' = N.alpha lm' cp1 in
              let cp2' = N.alpha lm' cp2 in
                List.iter (
                  fun (ps1,ps2) ->
                    if List.mem (ps1,ps2) !marked then ()
                    else check lm' ((ps1,ps2)::rs)
                ) (N.transable cp1' cp2')
          ) (N.ps2cset s1)
  in
    try
      rcdlabel ltbl r1';
      rcdlabel ltbl r2';
      check Lm.empty [N.cp2ps (N.firstpos r1'), N.cp2ps (N.firstpos r2')];
      List.for_all
        ( function s1,s2 -> if Ps.mem lpos1 s1 then Ps.mem lpos2 s2 else true )
        !marked
    with
        Exit -> false

(* テスト用データ *)
(*------------------------------------------------------------*)
(*
open Regex
(* {l:octet;m:octet[l]}* *)
let l1 = Label.create 1
let l2 = Label.create 2
let l2' = Label.create 1
let r1 = CLOS(SEQ(LBL(CHARS(Cset.all),l1),
                  REP(CHARS(Cset.all),l1)))
(* {l:octet[2];m:octet[l]}|{l:octet;m:octet[l]}* *)
let r2 = ALT(SEQ(LBL(CHARS(Cset.all),l2),
                  REP(CHARS(Cset.all),l2)),
             CLOS(SEQ(LBL(CHARS(Cset.all),l2'),
                      REP(CHARS(Cset.all),l2'))))


(* (a|b)*abb *)
let r = SEQ(SEQ(CLOS(ALT(CHARS(Pos.create (Cset.singleton 'a')),
                         CHARS(Pos.create (Cset.singleton 'b')))),
                SEQ(CHARS(Pos.create (Cset.singleton 'a')),
                    SEQ(CHARS(Pos.create (Cset.singleton 'b')),
                        CHARS(Pos.create (Cset.singleton 'b'))))),
            CHARS (Pos.create Cset.fin))
let f = N.followpos r
(* ab* *)
let r1 = SEQ(SEQ(CHARS(Pos.create (Cset.singleton 'a')),
                 CLOS(CHARS(Pos.create (Cset.singleton 'b')))),
             CHARS (Pos.create Cset.fin))
let f1 = N.followpos r1
(* ab** *)
let r1' = SEQ(SEQ(CHARS(Pos.create (Cset.singleton 'a')),
                  CLOS(CLOS(CHARS(Pos.create (Cset.singleton 'b'))))),
             CHARS (Pos.create Cset.fin))
let f1' = N.followpos r1'

(* ab[l] *)
let r2 = SEQ(SEQ(CHARS(Pos.create (Cset.singleton 'a')),
                 REP(CHARS(Pos.create (Cset.singleton 'b')),0)),
             CHARS (Pos.create Cset.fin))
let f2 = N.followpos r2
  (* ab[l]* *)
let r3 = SEQ(SEQ(CHARS(Pos.create (Cset.singleton 'a')),
                 CLOS(REP(CHARS(Pos.create (Cset.singleton 'b')),0))),
             CHARS (Pos.create Cset.fin))
let f3 = N.followpos r3
  (* ab*[l] *)
let r4 = SEQ(SEQ(CHARS(Pos.create (Cset.singleton 'a')),
                 REP(CLOS(CHARS(Pos.create (Cset.singleton 'b'))),0)),
             CHARS (Pos.create Cset.fin))
let f4 = N.followpos r4

(* a(b|c)[l] *)
let r5 = SEQ(SEQ(CHARS(Pos.create (Cset.singleton 'a')),
                 REP(ALT(CHARS(Pos.create (Cset.singleton 'b')),
                         CHARS(Pos.create (Cset.singleton 'c'))),0)),
             CHARS (Pos.create Cset.fin))
let f5 = N.followpos r5

(* a(b[l]|c[l]) *)
let r6 = SEQ(SEQ(CHARS(Pos.create (Cset.singleton 'a')),
                 ALT(REP(CHARS(Pos.create (Cset.singleton 'b')),0),
                     REP(CHARS(Pos.create (Cset.singleton 'c')),0))),
             CHARS (Pos.create Cset.fin))
let f6 = N.followpos r6
(* ab[l][l] *)
let r7 = SEQ(SEQ(CHARS(Pos.create (Cset.singleton 'a')),
                 REP(REP(CHARS(Pos.create (Cset.singleton 'b')),0),0)),
             CHARS (Pos.create Cset.fin))
let f7 = N.followpos r7
(* ab[l][m] *)
let r8 = SEQ(SEQ(CHARS(Pos.create (Cset.singleton 'a')),
                 REP(REP(CHARS(Pos.create (Cset.singleton 'b')),0),0)),
             CHARS (Pos.create Cset.fin))
let f8 = N.followpos r8
(* ab[m][l] *)
let r8' = SEQ(SEQ(CHARS(Pos.create (Cset.singleton 'a')),
                 REP(REP(CHARS(Pos.create (Cset.singleton 'b')),0),0)),
             CHARS (Pos.create Cset.fin))
let f8' = N.followpos r8'
*)
