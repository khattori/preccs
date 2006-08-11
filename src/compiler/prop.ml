(**
   ����_�����W���[��

   @author Hattori Kenta
   @version $Id: prop.ml,v 1.4 2006/06/06 04:39:15 hattori Exp $
*)

(*
 * ����_�����̌^��`�i'a�͖���ϐ��̌^�j
 *)
type 'a t =
    Atom of 'a
  | Neg  of 'a t
  | Conj of 'a t * 'a t
  | Disj of 'a t * 'a t

(*
 * �܈ӂ��\������Fp��q �� ��p��q
 *)
let implies p q = Disj(Neg(p),q)

(*
 * �g�[�g���W�[�`�F�b�J�[�F
 *     Wang�̃A���S���Y���ɂ��C�^����ꂽ����_�����̍P�^������s��
 * 
 *   ���@���Fp : 'a Prop.t --- ����_����
 * 
 *   �߂�l�F�P�^�Ȃ�true�C�����łȂ����false��Ԃ�
 * 
 *)
let taut p = 
  let rec common = function
      [], s      -> false
    | (hd::tl),s -> if List.exists ((=) hd) s then true else common(tl,s) in
  let rec wang = function
      ([], la, [], ra) -> common (la, ra)
   |  (lt, la, Atom(a)::tl, ra) -> wang(lt, la, tl, Atom(a)::ra)
   |  (Atom(a)::tl, la, rt, ra) -> wang(tl, Atom(a)::la, rt, ra)
   |  (lt, la, Neg(a)::tl, ra) -> wang(a::lt, la, tl, ra)         (* �ʉE *)
   |  (Neg(a)::tl, la, rt, ra) -> wang(tl, la, a::rt, ra)         (* �ʍ� *)
   |  (lt, la, Conj(a,b)::tl, ra) -> wang(lt, la, a::tl, ra)
                                   & wang(lt, la, b::tl, ra)      (* �ȉE *)
   |  (Conj(a,b)::tl, la, rt, ra) -> wang(a::b::tl, la, rt, ra)   (* �ȍ� *)
   |  (lt, la, Disj(a,b)::tl, ra) -> wang(lt, la, a::b::tl, ra)   (* �ɉE *)
   |  (Disj(a,b)::tl, la, rt, ra) -> wang(a::tl, la, rt, ra)
                                   & wang(b::tl, la, rt, ra)      (* �ɍ� *)
  in
    wang ([],[],[p],[])

(*
 * ����ϐ��̃��X�g����C���ׂĂ̑g�����̖���_�����𐶐�����
 * 
 *   ���@���F---: 'a list --- ����ϐ��̃��X�g
 * 
 *   �߂�l�F������������_�����̃��X�g
 * 
 *)
let rec gen_comb = function
    []    -> []
  | [x]   -> [Atom x;Neg(Atom x)]
  | x::xs ->
      let ps = gen_comb xs in
        List.map (fun p -> Conj(Atom x,p)) ps
        @ List.map (fun p -> Conj(Neg(Atom x),p)) ps

(*
 * ����ϐ��̃��X�g���擾����
 * 
 *   ���@���Fp: Prop.t --- ����_����
 * 
 *   �߂�l�F����ϐ��̃��X�g
 * 
 *)
let get_vars p =
  let rec trav vs = function
      Atom v      -> v::vs
    | Neg p       -> trav vs p
    | Conj(p1,p2) -> trav (trav vs p1) p2
    | Disj(p1,p2) -> trav (trav vs p1) p2
  in
    trav [] p

