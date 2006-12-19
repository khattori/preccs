(**
   �����t�J�ڃ��W���[��

   @author Hattori Kenta
   @version $Id: dtrans.ml,v 1.3 2006/07/06 04:15:36 hattori Exp $
*)
module TcondSet  = Set.Make(Tcond)
module LabelSet  = Set.Make(Label)
module PosSet    = Set.Make(Pos)
module DstateSet = Set.Make(Dstate)

(* �����t���J�ڏW���̗v�f *)
module E = struct
  (* �����W�� �~ �J�ڏ��� �~ �L�^���x���W�� �~ ����� *)
  type t = Cset.t * TcondSet.t * LabelSet.t * Dstate.t

  let compare (c1,t1,l1,s1) (c2,t2,l2,s2) =
    let ret = Cset.compare c1 c2 in
      if ret = 0 then let ret = TcondSet.compare t1 t2 in
        if ret = 0 then let ret = LabelSet.compare l1 l2 in
          if ret = 0 then Dstate.compare s1 s2
          else ret
        else ret
      else ret

end
module S = Set.Make(E)

type t = S.t
let empty = S.empty
let is_empty = S.is_empty
let create cs ts ls st = S.add (cs,ts,ls,st) S.empty

let tmap f s =
  TcondSet.fold (fun e s' -> TcondSet.add (f e) s') s TcondSet.empty
let tseq (_,t1,_,s1) (_,t2,_,s2) =
  (TcondSet.compare t1 t2) == 0 && (Dstate.compare s1 s2) == 0

(* �J�ڕ\�ɑJ�ڂ�ǉ�����D
   �� �J�ڏ����Ǝ���Ԃ���������΁C�����W���ƃ��x���W�����}�[�W *)
let add cs ts ls st dt = 
  if S.exists (tseq (cs,ts,ls,st)) dt then
    let s1,s2 = S.partition (tseq (cs,ts,ls,st)) dt in
    let cs',ls' = S.fold (fun (c,_,l,_) (c',l') ->
                            Cset.union c c',LabelSet.union l l') s1 (cs,ls) in
      S.add (cs',ts,ls',st) s2
  else
    S.add (cs,ts,ls,st) dt

(* ���̏�Ԃ̃��X�g���擾���� *)
let next_all dt =
  S.fold (fun (_,_,_,s) ns -> DstateSet.add s ns) dt DstateSet.empty

(* �^����ꂽ�����W���ɂ��Ď��̏����ƃ��x���W���C��Ԃ̃��X�g���擾���� *)
let next_list cs dt =
  let dt',_ = S.partition (fun (c',_,_,_) -> c' = cs) dt in
    List.map (fun (_,t,l,s) -> (t,l,s)) (S.elements dt')

(* ���J�ڂ̂��߂̕����W���̃��X�g���擾 *)
let cset_list s =
  S.fold (fun (c,_,_,_) cs -> if List.mem c cs then cs else c::cs) s []

(* �J�ډ\���ǂ����̔��� *)
let transable (cs1,ts1,ls1,ns1) (cs2,ts2,ls2,ns2) lm =
  let ts1' = tmap (Tcond.alpha lm) ts1 in
  let ts2' = tmap (Tcond.alpha lm) ts2 in
    not (Cset.is_empty (Cset.inter cs1 cs2))
    && (TcondSet.subset ts1' ts2' || TcondSet.subset ts2' ts1')
    
(*
 * �X�L�b�v�������{��
 * 
 *   ��  ���F dt1 : �X�L�b�v�ΏۂƂȂ��ԑJ��
 *            dt2 : ��r���ƂȂ��ԑJ��
 *            lm  : ���x���}�b�v
 * 
 *   �߂�l�F�V������ԑJ�ځ~�V�������x���}�b�v�~����ԃy�A�̃��X�g
 *
 *)
let skip dt1 dt2 lm =
  let dt,ls,pairs =
    S.fold (
      fun ((cs1,ts1,ls1,ns1) as tr1) (dt',ls,pairs') ->
        S.fold (
          fun ((cs2,ts2,ls2,ns2) as tr2) (dt',ls,pairs') ->
            if transable tr1 tr2 lm then
              S.add tr1 dt',LabelSet.union ls2 ls,(ns1,ns2)::pairs'
            else
              dt',ls,pairs'
        ) dt2 (dt',LabelSet.union ls1 ls,pairs')
    ) dt1 (empty,LabelSet.empty,[]) in
  let dt' =
    if List.length (cset_list dt) == 1 then
      S.fold (fun (cs,ts,ls,ns) dt -> S.add (Cset.all,ts,ls,ns) dt) S.empty dt
    else
      dt
  in
    dt',(Nfa.add_lblmap ls lm),pairs

let show dt =
  S.iter (
    fun (cs,ts,ls,ns)
      -> Cset.show cs;
        TcondSet.iter (fun tc -> Tcond.show tc) ts;
        Dstate.show ns;
        print_newline()
  ) dt
