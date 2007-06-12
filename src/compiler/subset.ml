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
  let marked = ref [] in
    (** CPs.t -> C.t -> Ps.t *) 
  let rec check lm = function
      [] -> ()
    | (s1,s2)::rs ->
        marked := (s1,s2)::!marked;
        Cset.iter (
          fun a ->
            let f flw p cp =
              if Cset.mem a (Pos.pos2cs p) then N.union cp (flw p) else cp in
            let cp1 = Ps.fold (f follow1) s1 [] in (* R1のフォロー集合 *)
            let cp2 = Ps.fold (f follow2) s2 [] in (* R2のフォロー集合 *)
            let cp1' = N.alpha lm cp1 in
            let cp2' = N.alpha lm cp2 in
              List.iter (
                fun (ps1,ls1,ps2,ls2) ->
                  if List.mem (ps1,ps2) !marked then ()
                  else check (N.add_lblmap (Ls.union ls1 ls2) lm) ((ps1,ps2)::rs)
              ) (N.transable cp1' cp2')
        ) (N.ps2cset s1)
  in
  let fp1 = N.firstpos r1' in
  let fp2 = N.firstpos r2' in
  let s1 = N.cp2ps fp1 in (* 位置集合 *)
  let s2 = N.cp2ps fp2 in
  let l1 = N.cp2ls fp1 in (* ラベル集合 *)
  let l2 = N.cp2ls fp2 in
  let lm = N.add_lblmap (Ls.union l1 l2) Lm.empty in
    try
      check lm [s1,s2];
      List.for_all
        ( function s1,s2 -> if Ps.mem lpos1 s1 then Ps.mem lpos2 s2 else true )
        !marked
    with
        Exit -> false

(*
 * 正規表現の共通部分判定(r1∩r2かどうか)
 * 
 *   引　数：r1 : Regex.t --- 正規表現1
 *           r2 : Regex.t --- 正規表現2
 * 
 *   戻り値：判定結果を真偽値で返す
 * 
 *)
let inter r1 r2 = false (* not yet *)

(* テスト用データ *)
(*------------------------------------------------------------*)
(*
open Regex

(* {l:octet;m:octet[l]}* *)
let l1   = Label.create 1
let l1'  = Label.create 1
let l1'' = Label.create 1
let l2  = Label.create 2
let l2' = Label.create 2

let r1 = CLOS(SEQ(LBL(CHARS(Cset.all),l1),
                  REP(CHARS(Cset.all),l1)))
(* {l:octet[2];m:octet[l]}|{l:octet;m:octet[l]}* *)
let r1' = ALT(SEQ(LBL(SEQ(CHARS(Cset.all),CHARS(Cset.all)),l2),
                  REP(CHARS(Cset.all),l2)),
              CLOS(SEQ(LBL(CHARS(Cset.all),l1'),
                       REP(CHARS(Cset.all),l1'))))
let () =
  assert (not (subset r1' r1));
  assert (subset r1 r1')

(* (a|b)*abb *)
let r2 = SEQ(CLOS(ALT(CHARS(Cset.singleton 'a'),
                      CHARS(Cset.singleton 'b'))),
             SEQ(CHARS(Cset.singleton 'a'),
                 SEQ(CHARS(Cset.singleton 'b'),
                     CHARS(Cset.singleton 'b'))))
(* (a|b)* *)
let r2' = CLOS(ALT(CHARS(Cset.singleton 'a'),
                   CHARS(Cset.singleton 'b')))

let () =
  assert (subset r2 r2');
  assert (not (subset r2' r2))

(* {l:(b|c);octet[l]} *)
let r3 = SEQ(LBL(ALT(CHARS(Cset.singleton 'b'),CHARS(Cset.singleton 'c')),l1),
             REP(CHARS(Cset.all),l1))
(* a*{l:(b|c);octet[l]} *)
let r3' = SEQ(CLOS(CHARS(Cset.singleton 'a')),
              SEQ(LBL(ALT(CHARS(Cset.singleton 'b'),CHARS(Cset.singleton 'c')),l1'),
                  REP(CHARS(Cset.all),l1')))
let () =
  assert (subset r3 r3');
  assert (not (subset r3' r3))


(*
  p1  {l:octet;octet[l]}
  p2  {l:octet;octet[l]*}
  p3  {l:octet;octet*[l]}
  p4  {octet;octet*}
  p5  {octet*}
  p6  {octet}
*)
let p1 = SEQ(LBL(CHARS(Cset.all),l1),
             REP(CHARS(Cset.all),l1))
let p2 = SEQ(LBL(CHARS(Cset.all),l1'),
             CLOS(REP(CHARS(Cset.all),l1')))
let p3 = SEQ(LBL(CHARS(Cset.all),l1''),
             REP(CLOS(CHARS(Cset.all)),l1''))
let p4 = SEQ(CHARS(Cset.all),
             CLOS(CHARS(Cset.all)))
let p5 = CLOS(CHARS(Cset.all))
let p6 = CHARS(Cset.all)

let () =
  assert(subset p1 p2);
  assert(not(subset p2 p1));
  assert(subset p1 p3);
  assert(not(subset p3 p1));
  assert(subset p1 p4);
  assert(not(subset p4 p1));
  assert(subset p1 p5);
  assert(not(subset p5 p1));
  assert(not(subset p1 p6));
  assert(not(subset p6 p1));
  assert(subset p2 p3);
  assert(not(subset p3 p2));
  assert(subset p2 p4);
  assert(not(subset p4 p2));
  assert(subset p2 p5);
  assert(not(subset p5 p2));
  assert(not(subset p2 p6));
  assert(subset p6 p2);
  assert(subset p3 p4);
  assert(not(subset p4 p3));
  assert(subset p3 p5);
  assert(not(subset p5 p3));
  assert(not(subset p3 p6));
  assert(subset p6 p3);
  assert(subset p4 p5);
  assert(not(subset p5 p4));
  assert(not(subset p4 p6));
  assert(subset p6 p4);
  assert(not(subset p5 p6));
  assert(subset p6 p5)
*)
