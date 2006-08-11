(**
   �N���[�W���ϊ����W���[��

   @author Hattori Kenta
   @version $Id: closure.ml,v 1.3 2006/04/28 04:03:40 hattori Exp $
*)

module C = Cps
module Ss = Set.Make(Symbol)
(** �V���{�����X�g����V���{���W�����쐬 *)
let createSet = List.fold_left ( fun s v -> Ss.add v s ) Ss.empty


(** FIX�Œ�`�����֐����̏W�����擾 *)
let fixnames = List.fold_left (fun ss (f,_,_) -> Ss.add f ss) Ss.empty
(** FIX�Œ�`�����֐����x���̃��X�g�𐶐� *)
let fixlabels = List.map (fun (f,_,_) -> (C.Label f))
(** CPS�I�y�����h���X�g�̕ϐ��W�����擾 *)
let vars =
  List.fold_left
    ( fun ss v -> match v with C.Var s -> Ss.add s ss | _ -> ss ) Ss.empty

(** ���R�ϐ����擾 *)
let rec freeVal = function
    C.Prim(p,ops,rs,cs),_ ->
      Ss.union
        (vars ops)
        (Ss.diff
           (List.fold_left (fun s c -> Ss.union (freeVal c) s) Ss.empty cs)
           (createSet rs))
  | C.App(f,ops),_ -> vars (f::ops)
  | C.Fix(bs,c),_ -> Ss.diff (Ss.union (freeValFbs bs) (freeVal c)) (fixnames bs)
  | C.Cblk(_,vs,c),_ -> Ss.union (vars vs) (freeVal c)

and freeValFbs bs = 
  List.fold_left
    ( fun s b -> Ss.union (freeValFb b) s ) Ss.empty bs
and freeValFb (f,ps,c) = Ss.diff (freeVal c) (createSet ps)

(** ���R�ϐ����Z�b�g *)
let rec attachFv (cexp,fv) =
  fv := Ss.elements (freeVal(cexp,fv));
  match cexp with
      C.Prim(_,_,_,cs) -> List.iter attachFv cs
    | C.App _          -> ()
    | C.Fix(bs,c)      -> List.iter (fun (_,_,c') -> attachFv c') bs; attachFv(c)
    | C.Cblk(_,_,c)    -> attachFv(c)

(** �N���[�W�����R�[�h�`���̐��� *)
let closFormat bs d =
  (fixlabels bs) @
    (List.map
        ( fun s -> C.Var s )
        (Ss.elements (Ss.inter (Ss.diff (freeValFbs bs) (fixnames bs)) d)))

(** �N���[�W���ϊ��֐� *)
let rec conv f bb dd ss ff = function
    C.Prim(p,ops,rs,cs),_ ->
      C.Prim(p,ops,rs,
             List.map
               (conv f (Ss.union bb (createSet rs)) (Ss.union dd (createSet rs))
                  ss ff) cs),ref []
  | C.App(f,ops),_ ->
      let g = C.genid "g" in
        C.Prim(C.Select,[f;C.Int 0],[g],[C.App(C.Var g,f::ops),ref []]),ref []
  | C.Fix(bs,c),_ ->
      let fs = List.map ( fun (b,_,_) -> b ) bs in
      let hd = List.hd fs in
      let rec roff n c = function
          []   -> c
        | h::r -> C.Prim(C.Offset,[C.Var hd;C.Int n],[h],[roff (n+1) c r]),ref []
      in
      let c' = C.Prim(C.Record,closFormat bs dd,[hd],
                      [roff 1 (conv f (Ss.union bb (fixnames bs))
                                 (Ss.union dd (fixnames bs)) ss ff c)
                         (List.tl fs)]),ref []
      and bs' =
        List.map (convFb dd (fixlabels bs) (closFormat bs dd)) bs
      in C.Fix(bs',c'),ref []
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
        C.Prim(C.Offset,[C.Var f;C.Int o],[v],[bb]),ref []
    else if (List.mem (C.Var v) ff) then
      let o = (pos (C.Var v) ff) - (pos (C.Label f) ss) in
        C.Prim(C.Select,[C.Var f;C.Int o],[v],[bb]),ref []
    else C.Prim(C.Select,[C.Label v;C.Int 0],[v],[bb]),ref []


(** �Ǐ��֐���`���g�b�v���x���Ɉړ� *)
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
    | c -> [],c (* C.App�̏ꍇ *)
  in match lift cexp with
      [],c -> c
    | bs,c -> C.Fix(bs,c),ref []

(** �N���[�W���ϊ����Ǐ��֐���`�̃��t�g�A�b�v *)
let convert cexp =
  (liftUp
     (conv (Symbol.symbol "__main") Ss.empty Ss.empty [] [] cexp))
