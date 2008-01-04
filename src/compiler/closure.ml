(**
   クロージャ変換モジュール

   @author Hattori Kenta
   @version $Id: closure.ml,v 1.3 2006/04/28 04:03:40 hattori Exp $
*)

module C = Cps
module Ss = Set.Make(Symbol)
(** シンボルリストからシンボル集合を作成 *)
let createSet = List.fold_left ( fun s v -> Ss.add v s ) Ss.empty


(** FIXで定義される関数名の集合を取得 *)
let fixnames = List.fold_left (fun ss (f,_,_) -> Ss.add f ss) Ss.empty
(** FIXで定義される関数ラベルのリストを生成 *)
let fixlabels = List.map (fun (f,_,_) -> (C.Label f))
(** CPSオペランドリストの変数集合を取得 *)
let vars =
  List.fold_left
    ( fun ss v -> match v with C.Var s -> Ss.add s ss | _ -> ss ) Ss.empty

(** 自由変数を取得 *)
let rec freeVal = function
    C.Prim(p,ops,rs,cs),_ ->
      Ss.union (vars ops) (Ss.diff (freeVals cs) (createSet rs))
  | C.App(f,ops),_ -> vars (f::ops)
  | C.Fix(bs,c),_ -> Ss.diff (Ss.union (freeValFbs bs) (freeVal c)) (fixnames bs)
  | C.Switch(v,bcs),_ -> let _,cs = List.split bcs in Ss.union (vars [v]) (freeVals cs)
  | C.Cblk(_,vs,c),_ -> Ss.union (vars vs) (freeVal c)
and freeVals cs =
  List.fold_left (fun s c -> Ss.union (freeVal c) s) Ss.empty cs
and freeValFbs bs = 
  List.fold_left
    ( fun s b -> Ss.union (freeValFb b) s ) Ss.empty bs
and freeValFb (f,ps,c) = Ss.diff (freeVal c) (createSet ps)

(** 自由変数をセット *)
let rec attachFv (cexp,fv) =
  fv := Ss.elements (freeVal(cexp,fv));
  match cexp with
      C.Prim(_,_,_,cs) -> List.iter attachFv cs
    | C.App _          -> ()
    | C.Fix(bs,c)      -> List.iter (fun (_,_,c') -> attachFv c') bs; attachFv c
    | C.Switch(_,vcs)  -> List.iter (fun (_,c) -> attachFv c) vcs
    | C.Cblk(_,_,c)    -> attachFv c

(** クロージャレコード形式の生成 *)
let closFormat bs d =
  (fixlabels bs) @
    (List.map
        ( fun s -> C.Var s )
        (Ss.elements (Ss.inter (Ss.diff (freeValFbs bs) (fixnames bs)) d)))

(** クロージャ変換関数 *)
let rec conv f bb dd ss ff = function
    C.Prim(p,ops,rs,cs),_ ->
      C.Prim(p,ops,rs,
             List.map
               (conv f (Ss.union bb (createSet rs)) (Ss.union dd (createSet rs))
                  ss ff) cs),ref []
  | C.App(f,ops),_ ->
      let g = Symbol.genid "g" in
        C.Prim(C.Select,[f;C.Const(C.Int 0)],[g],[C.App(C.Var g,f::ops),ref []]),ref []
  | C.Fix(bs,c),_ ->
      let fs = List.map ( fun (b,_,_) -> b ) bs in
      let hd = List.hd fs in
      let rec roff n c = function
          []   -> c
        | h::r -> C.Prim(C.Offset,[C.Var hd;C.Const(C.Int n)],[h],[roff (n+1) c r]),ref []
      in
      let c' = C.Prim(C.Record,closFormat bs dd,[hd],
                      [roff 1 (conv f (Ss.union bb (fixnames bs))
                                 (Ss.union dd (fixnames bs)) ss ff c)
                         (List.tl fs)]),ref []
      and bs' =
        List.map (convFb dd (fixlabels bs) (closFormat bs dd)) bs
      in C.Fix(bs',c'),ref []
  | C.Switch(v,vcs),_ ->
      C.Switch(v,List.map (fun (v',c) -> v',conv f bb dd ss ff c) vcs),ref []
  | C.Cblk(cs,vs,c),_ -> C.Cblk(cs,vs,conv f bb dd ss ff c),ref []

and convFb dd ss ff (f,ps,b) =
  (f,(f::ps),
   Ss.fold
     (subst f ss ff)
     (freeValFb (f,ps,b))
     (conv f Ss.empty (freeVal b) ss ff b))

and subst f ss ff v bb =
  let rec pos e = function
      []   -> raise Not_found
    | h::r -> if e = h then 0 else 1 + (pos e r) in

    if v = f then bb
    else if (List.mem (C.Label v) ss) then 
      let o = (pos (C.Label v) ss) - (pos (C.Label f) ss) in
        C.Prim(C.Offset,[C.Var f;C.Const(C.Int o)],[v],[bb]),ref []
    else if (List.mem (C.Var v) ff) then
      let o = (pos (C.Var v) ff) - (pos (C.Label f) ss) in
        C.Prim(C.Select,[C.Var f;C.Const(C.Int o)],[v],[bb]),ref []
    else C.Prim(C.Select,[C.Label v;C.Const(C.Int 0)],[v],[bb]),ref []


(** 局所関数定義をトップレベルに移動 *)
let liftUp cexp =
  let rec lift = function
      C.Prim(p,ops,rs,cs),_ ->
        let lbs,rcs =
          List.fold_right
            ( fun c (bs,cs) -> let bs',c' = lift c in (bs'@bs),(c'::cs) )
            cs ([],[]) in
          lbs,(C.Prim(p,ops,rs,rcs),ref [])
    | C.Fix(bs,c),_ ->
        let lbs,rbs =
          List.fold_right
            ( fun (f,a,c) (lbs,rbs) ->
                let bs',c' = lift c in (bs'@lbs),((f,a,c')::rbs) )
            bs ([],[]) in
        let lbs',rc = lift c in
          (rbs@lbs@lbs'),rc
    | C.Cblk(cs,vs,c),_ ->
        let lbs,rc = lift c in lbs,(C.Cblk(cs,vs,rc), ref [])
    | C.Switch(v,bcs),_ ->
        let lbs,rcs =
          List.fold_right
            ( fun (b,c) (bs,cs) -> let bs',c' = lift c in (bs'@bs),((b,c')::cs) )
	    bcs ([],[]) in
          lbs,(C.Switch(v,rcs),ref [])
    | c -> [],c (* C.Appの場合 *)
  in match lift cexp with
      [],c -> c
    | bs,c -> C.Fix(bs,c),ref []

(** クロージャ変換→局所関数定義のリフトアップ *)
let convert cexp =
  (liftUp
     (conv (Symbol.symbol "__main") Ss.empty Ss.empty [] [] cexp))
