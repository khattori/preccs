(**
   ��ԑJ�ڕ\���W���[��

   @author Hattori Kenta
   @version $Id: dtable.ml,v 1.3 2006/07/06 04:15:36 hattori Exp $
*)
module Ht = Hashtbl
module T  = Types

module PosSet    = Set.Make(Pos)
module LabelSet  = Set.Make(Label)
module TcondSet  = Set.Make(Tcond)
module DstateMap = Map.Make(Dstate)

(*
 * �󗝏�ԊǗ����W���[��
 * 
 *   note:  �󗝏�Ԏ��̎󗝔ԍ��Ƒg���Ă�t�B�[���h�̊Ǘ�
 *)
module Accept = struct
  (* �󗝏�ԃe�[�u�� : �ʒu �� �󗝔ԍ��~�t�B�[���h���X�g *)
  let table:(Pos.t, int * T.field list) Ht.t = Ht.create(13)

  (* �󗝏�ԃe�[�u���ɓo�^���� *)
  let register ps fs =
    let idx = ref 0 in
      List.iter2 (fun p f ->
                    Ht.add table p (!idx,f); incr idx
                 ) ps fs
        
  (* �ŏ��̎󗝔ԍ��ƃt�B�[���h���X�g��T�� *)
  let find ps =
    let num,fs =
      PosSet.fold (
        fun p (n,fs) ->
          let n',fs' = try Ht.find table p with Not_found -> n,fs in
            if n < n' then n,fs else n',fs'
      ) ps (max_int,[]) in
      if num == max_int then raise Not_found else num,fs
end

type act =
    ACT_NULL          (* ������ *)
  | ACT_MATCH         (* �����W���}�b�`���� *)
  | ACT_SKIP          (* �X�L�b�v���� *)
  | ACT_TRANS         (* ��ԑJ�� *)
  | ACT_FINAL         (* �I������ *)
  | ACT_RECORD        (* ���x���L�^���� *)
  | ACT_COND_VALZERO  (* ��������(V_l==0) *)
  | ACT_COND_VALNONZ  (* ��������(V_l!=0) *)
  | ACT_COND_CNTZERO  (* ��������(C_l==0) *)
  | ACT_COND_CNTNONZ  (* ��������(C_l!=0) *)
  | ACT_COUNT_SET     (* �J�E���^����(C_l=V_l-1) *)
  | ACT_COUNT_DECR    (* �J�E���^����(C_l--) *)
let act_string = function
    ACT_NULL          -> "ACT_NULL        "
  | ACT_MATCH         -> "ACT_MATCH       "
  | ACT_SKIP          -> "ACT_SKIP        "
  | ACT_TRANS         -> "ACT_TRANS       "
  | ACT_FINAL         -> "ACT_FINAL       "
  | ACT_RECORD        -> "ACT_RECORD      "
  | ACT_COND_VALZERO  -> "ACT_COND_VALZERO"
  | ACT_COND_VALNONZ  -> "ACT_COND_VALNONZ"
  | ACT_COND_CNTZERO  -> "ACT_COND_CNTZERO"
  | ACT_COND_CNTNONZ  -> "ACT_COND_CNTNONZ"
  | ACT_COUNT_SET     -> "ACT_COUNT_SET   "
  | ACT_COUNT_DECR    -> "ACT_COUNT_DECR  "

type next  = act  * int
type stent = next * int
type fent  = string * int
type ment  = next * int * int  (* ������ * ��}�b�`���G���g�� * Cset�G���g�� *)
type csent = Cset.t
type rent  = next * int * int  (* ������ * ���x��ID * �T�C�Y *)
type cond  = next * next * int (* TRUE������ * FALSE������ * ���x��ID *)
type cact  = next * int        (* ������ * ���x��ID *)

(* �e�[�u�� *)
class ['a] table =
object
  val tbl = (Ht.create(29) : (int,'a) Ht.t)
  val tbl' = (Ht.create(29) : ('a,int) Ht.t)
  val mutable num = -1
  method num = num
  method add ent =
    try
      Ht.find tbl' ent 
    with Not_found ->
      num <- num + 1;
      Ht.add tbl num ent;
      Ht.add tbl' ent num;
      num
  method find idx = Ht.find tbl idx
end

let state_map = Ht.create(29) (* Dstate.t -> Index *)
let state_num = ref (-1)
let state_index st =
  try
    Ht.find state_map st
  with Not_found ->
    incr state_num;
    Ht.add state_map st !state_num;
    !state_num

let state_table = Ht.create(29) (* Index -> stent *)
let create_stent st (next,fidx) =
  let idx = state_index st in
    Ht.add state_table idx (next,fidx)

let fact_table = new table
let mact_table = new table
let cset_table = new table
let ract_table = new table
let cond_table = new table
let cact_table = new table

let max_label = ref 0
let update_max_label n =
  if n > !max_label then max_label := n
(*
 * ��ԑJ�ڕ\�𐶐�����
 * 
 *   ���@���Finit_st : Dstate.t    --- ������Ԕԍ�
 *           init_ls : LabelSet.t  --- �������x���W��
 *           st_map  : DstateMap.t --- �J�ڕ\
 *   �߂�l�F������Ԃ�ID
 * 
 *)
let create init_st init_ls st_map =
  let label_map = Label.map_create() in (* ���x��ID -> ���x���ԍ��i���[�J���j *)
  let rec create_next tm =
    let cset_list = Dtrans.cset_list tm in (* Cset.t�̃��X�g���擾 *)
      List.fold_left
        (fun next cs -> create_cset_next next cs tm) (ACT_NULL,0) cset_list
  and create_cset_next (nact,nidx) cs tm =
    let tlp_list = Dtrans.next_list cs tm in
    let next' = List.fold_left (
      fun nxt (ts,ls,st) -> create_trans_next nxt (ts,ls,st)
    ) (create_trans_next (ACT_NULL,0) (List.hd tlp_list)) (List.tl tlp_list) in
      if Cset.all = cs then
        ACT_SKIP,mact_table#add (next',0,0)
      else
        ACT_MATCH,mact_table#add (next',nidx,cset_table#add cs)
  and create_trans_next next (ts,ls,st) =
    let idx = state_index st in
    let nxt = ACT_TRANS,idx in
    let nxt' = create_rcd_next nxt ls in
      if TcondSet.is_empty ts then
        nxt'
      else
        create_tcond_next nxt' next ts
  and create_rcd_next next ls =
    if LabelSet.is_empty ls then
      next
    else
      LabelSet.fold (
        fun lbl nxt ->
          ACT_RECORD,
          ract_table#add (nxt,Label.map_find label_map lbl,Label.size lbl)
      ) ls next
  and create_tcond_next tnxt fnxt tcset =
    let next = TcondSet.fold (
      fun tc nxt ->
        match tc with
          | Tcond.ValNonz(l) ->
              ACT_COUNT_SET,cact_table#add (nxt,Label.map_find label_map l)
          | Tcond.CntNonz(l) ->
              ACT_COUNT_DECR,cact_table#add (nxt,Label.map_find label_map l)
          | _ -> nxt
    ) tcset tnxt in
    let next' = TcondSet.fold (
      fun tc nxt ->
        match tc with
            Tcond.ValZero(l) ->
              ACT_COND_VALZERO,cond_table#add (nxt,fnxt,Label.map_find label_map l)
          | Tcond.ValNonz(l) ->
              ACT_COND_VALNONZ,cond_table#add (nxt,fnxt,Label.map_find label_map l)
          | Tcond.CntZero(l) ->
              ACT_COND_CNTZERO,cond_table#add (nxt,fnxt,Label.map_find label_map l)
          | Tcond.CntNonz(l) ->
              ACT_COND_CNTNONZ,cond_table#add (nxt,fnxt,Label.map_find label_map l)
    ) tcset next in
      next'
  and create_final st =
    try
      let num,field_list = Accept.find (Dstate.to_posset st) in
        fact_table#add (T.encode_field_list label_map field_list,num)
    with Not_found -> -1
  in
  let next = create_rcd_next (ACT_TRANS,state_index init_st) init_ls in
  let init_st' = Dstate.create() in
    DstateMap.iter (
      fun st tm ->
        let next = create_next tm in
        let fidx = create_final st in
          create_stent st (next,fidx);
    ) st_map;
    create_stent init_st' (next,0);
    update_max_label (Label.map_size label_map);
    state_index init_st'

(*
 * DFA�̏o��
 *
 *   ���@���F�Ȃ�
 *   �߂�l�F�Ȃ�
 * 
 *)
let emit () =
  (* ��ԃe�[�u�� *)
  Printf.printf "state_t __prc__state_table[] = {\n";
  for i = 0 to !state_num do
    let (act,nidx),fidx = Ht.find state_table i in
      Printf.printf "/* [%03d] */ { %s,%4d,%4d },\n" i (act_string act) nidx fidx
  done;
  Printf.printf "};\n";
  (* �I�������G���g�� *)
  Printf.printf "fact_t __prc__fact_table[] = {\n";
  for i = 0 to fact_table#num do
    let cons,num = fact_table#find i in
      if String.length cons == 0 then
        Printf.printf "/* [%03d] */ { %2d, NULL },\n" i num
      else
        Printf.printf "/* [%03d] */ { %2d, \"%s\" },\n" i num cons
  done;
  Printf.printf "};\n";
  (* �}�b�`�e�[�u�� *)
  Printf.printf "mact_t __prc__mact_table[] = {\n";
  for i = 0 to mact_table#num do
    let (act,nidx),midx,cidx = mact_table#find i in
      Printf.printf "/* [%03d] */ { %s,%4d,%4d,%4d },\n"
        i (act_string act) nidx midx cidx
  done;
  Printf.printf "};\n";
  (* �����W���e�[�u�� *)
  Printf.printf "cset_t __prc__cset_table[] = {\n";
  for i = 0 to cset_table#num do
    let cs = cset_table#find i in
      Printf.printf "/* [%03d] */ %s,\n" i (Cset.encode cs);
  done;
  Printf.printf "};\n";
  (* ���x���L�^�����e�[�u�� *)
  Printf.printf "ract_t __prc__ract_table[] = {\n";
  for i = 0 to ract_table#num do
    let (act,nidx),lid,lsiz = ract_table#find i in
      Printf.printf "/* [%03d] */ { %s,%4d,%2d,%2d },\n"
        i (act_string act) nidx lid lsiz
  done;
  Printf.printf "};\n";
  (* ���������e�[�u�� *)
  Printf.printf "cond_t __prc__cond_table[] = {\n";
  for i = 0 to cond_table#num do
    let (tact,tidx),(fact,fidx),lid = cond_table#find i in
      Printf.printf "/* [%03d] */ { %s,%4d, %s,%4d,%2d },\n"
        i (act_string tact) tidx (act_string fact) fidx lid
  done;
  Printf.printf "};\n";
  (* �J�E���^�����e�[�u�� *)
  Printf.printf "cact_t __prc__cact_table[] = {\n";
  for i = 0 to cact_table#num do
    let (act,nidx),lid = cact_table#find i in
      Printf.printf "/* [%03d] */ { %s,%4d,%2d },\n"
        i (act_string act) nidx lid
  done;
  Printf.printf "};\n";
  (* ���x�����W�X�^ *)
  Printf.printf "#define MAX_LABEL (%d)\n" !max_label;
