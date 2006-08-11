(**
   �J�ڏ������W���[��

   @author Hattori Kenta
   @version $Id: cond.ml,v 1.2 2006/06/21 00:14:14 hattori Exp $
*)
module P = Prop
module PosSet = Set.Make(Pos)

(* �J�ڏ����̌^��` *)
type t =
    Const of bool
  | Prop  of var P.t

(* �l�Q�Ǝ�� *)
and var =
    Counter of Label.t (* �J�E���^�l���Q�� *)
  | Value   of Label.t (* ���x���l���Q��   *)

let conj = function
  | c1,Const(b2)      -> if b2 then c1 else Const false
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
 * �J�ڏ������X�g���疽��_�����̑g�ݍ��킹�𒊏o
 * 
 *   ���@���Fcs : Cond.t list --- �J�ڏ����̑g�̃��X�g
 * 
 *   �߂�l�F���������_�����̃��X�g�FCond.t list
 *)
let get_vars cs = 
  let ps = List.fold_left (                      (* ����ϐ��𒊏o *)
    fun ps -> function
        Const true -> ps
      | Prop p     -> P.get_vars p @ ps
      | _          -> assert false (* Const false �͏����ς� *)
  ) [] cs in
  let ps' = List.fold_left (                     (* �d������菜�� *)
    fun r p -> if List.mem p r then r else p::r
  ) [] ps
  in List.map (fun p -> Prop p) (P.gen_comb ps') (* �g�ݍ��킹�𐶐� *)

