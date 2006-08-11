(**
   NFA���W���[��

   @author Hattori Kenta
   @version $Id: nfa.ml,v 1.2 2006/06/21 00:14:15 hattori Exp $
*)

module R  = Regex
module C  = Cond
module Ht = Hashtbl
module Ps = Set.Make(Pos)

(*
 * �����t���J�ڂ̌^��`
 *)
type trans = Cond.t * Pos.t

(*
 * �����t���J�ڃ��X�g�̐��K�����s���F
 *     �����i�P�U�j�ƂȂ閽��_�������܂ޗv�f����菜��
 * 
 *   ���@���F ---: trans list --- �����t���J�ڃ��X�g
 * 
 *   �߂�l�F ���K�����������t���J�ڃ��X�g
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
 * �����t�J�ڃ��X�g�ɏ�����ǉ�����
 * 
 *   ���@���Fc  : Cond.t     --- �ǉ��������
 *           tr : trans list --- �����t���J�ڃ��X�g
 * 
 *   �߂�l�F�ǉ���̏����t���J�ڃ��X�g
 * 
 *)
let prod c tr = normalize (List.map (fun (c',p) -> C.conj(c,c'),p) tr)

(*
 * ��̏����t���J�ڃ��X�g���}�[�W����
 * 
 *   ���@���Ftr1 : trans list --- �����t���J�ڃ��X�g1
 *           tr2 : trans list --- �����t���J�ڃ��X�g2
 * 
 *   �߂�l�F�}�[�W�������ʂ̏����t���J�ڃ��X�g
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

(** �t�H���[�ʒu�̏W����Ԃ��֐���Ԃ�
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
                               (* �J�Ԃ����� *)
                               (prod (C.neg(C.Prop(Prop.Atom(C.Counter lbl))))
                                  (firstpos r))
                               (* �E�o���� *)
                               (prod (C.Prop(Prop.Atom(C.Counter lbl))) s)) r
  in fill [] re; follow

(*
 * �����t���J�ڂ̔���F
 *     tr1�̑J�ڂ̂Ȃ��őΉ�����tr2�ł��\�ȑJ�ڂ��擾����
 * 
 *   ���@���Ftr1 : trans list --- �����t���J��1
 *           tr2 : trans list --- �����t���J��2
 * 
 *   �߂�l�F�Ή�����J�ڐ�̑g
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
