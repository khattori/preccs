(**
   DFA���W���[��

   @author Hattori Kenta
   @version $Id: dfa.ml,v 1.3 2006/07/06 04:15:36 hattori Exp $
*)
module T  = Types
module R  = Regex
module P  = Prop
module C  = Cond
module N  = Nfa

module Ht = Hashtbl

module PosSet    = Set.Make(Pos)
module PosSetSet = Set.Make(PosSet)
module PosSetMap = Map.Make(PosSet)

module LabelSet  = Set.Make(Label)

(* �X�L�b�v���[�h *)
let skipMode = ref false

(* �󗝏�ԃe�[�u�� *)
let acc_tbl:(Pos.t, int * T.field list) Ht.t = Ht.create(13)

let show_posset ps =
  Printf.printf "[";
  PosSet.iter (
    fun p ->
      if Ht.mem acc_tbl p then
        let n,fs = Ht.find acc_tbl p in
          Printf.printf "%d(%d:" p n;
          T.show_field_list fs;
          print_string ");"
      else
        Printf.printf "%d;" p
  ) ps;
  Printf.printf "]"

let show_lblset ls =
  Printf.printf "(";
  LabelSet.iter (fun l -> Label.show l; print_string ";") ls;
  Printf.printf ")"

(* �J�ڏ��� *)
module Tcond =
struct
  type t =
      ValZero of Label.t
    | ValNonz of Label.t (* C[l] <- val_of_l - 1 *)
    | CntZero of Label.t
    | CntNonz of Label.t (* C[l]-- *)
  let compare = Pervasives.compare

  let show = function
      ValZero(l) -> Printf.printf "ValZero("; Label.show l; print_string ")"
    | ValNonz(l) -> Printf.printf "ValNonz("; Label.show l; print_string ")"
    | CntZero(l) -> Printf.printf "CntZero("; Label.show l; print_string ")"
    | CntNonz(l) -> Printf.printf "CntNonz("; Label.show l; print_string ")"

end
(* �J�ڏ����̏W�� *)
module TcondSet =
struct
  module T  = Tcond
  module TS = Set.Make(Tcond)
  type t = TS.t
  let compare = TS.compare
  let empty = TS.empty
  let is_empty = TS.is_empty
  let fold = TS.fold

  let of_cond = function
      C.Prop(p) ->
        let rec trav cs = function
            P.Atom(C.Counter(l)) -> TS.add (T.CntZero l) cs
          | P.Atom(C.Value(l))   -> TS.add (T.ValZero l) cs
          | P.Neg(P.Atom(C.Counter(l))) -> TS.add (T.CntNonz l) cs
          | P.Neg(P.Atom(C.Value(l)))   -> TS.add (T.ValNonz l) cs
          | P.Conj(p1,p2) -> trav (trav cs p1) p2
          | _ -> assert false
        in
          trav empty p 
    | C.Const(true) -> TS.empty
    | _             -> assert false

  (* tc1��tc2���ǂ����̊ȈՔ��� --- TODO:������Ƃ�������ł͂Ȃ� *)
  let imply tc1 tc2 = TS.subset tc2 tc1

  let show ts =
    print_string "[";
    TS.iter (fun tc -> T.show tc;print_string ";") ts;
    print_string "]";
end

(* DFA�J�ڂ̌^ *)
module TransMap =
struct
  module S = Set.Make(
    struct
      type t = Cset.t * TcondSet.t * PosSet.t
      let compare (c1,t1,p1) (c2,t2,p2) =
        let c = Cset.compare c1 c2 in
          if c = 0 then
            let c = TcondSet.compare t1 t2 in
              if c = 0 then PosSet.compare p1 p2 else c
          else
            c
    end
  )

  type t = S.t    (* �������W���~�J�ڏ����W���~����ԁ� �̏W�� *)
  let empty = S.empty
  let tpeq (_,t1,p1) (_,t2,p2) =
    (TcondSet.compare t1 t2) == 0 && (PosSet.compare p1 p2) == 0
      
  (* �J�ڕ\�ɑJ�ڂ�ǉ�����D
     �� �J�ڏ����Ǝ���Ԃ���������΁C�����W�����}�[�W *)
  let add (cset,condset) posset s = 
    if S.exists (tpeq (cset,condset,posset)) s then
      let s1,s2 = S.partition (tpeq (cset,condset,posset)) s in
      let cs = S.fold (fun (c,_,_) cs' -> Cset.union c cs') s1 cset in
        S.union s2 (S.singleton (cs,condset,posset))
    else
      S.add (cset,condset,posset) s

  (* ���̏�Ԃ̃��X�g���擾���� *)
  let next_all s =
    PosSetSet.elements
      (S.fold (fun (c,t,p) pss -> PosSetSet.add p pss) s PosSetSet.empty)

  (* �^����ꂽ�����W���ɂ��Ď��̏����Ə�Ԃ̃��X�g���擾���� *)
  let next_list cs s =
    let s',_ = S.partition (fun (c',_,_) -> c' = cs) s in
      List.map (fun (_,t,p) -> (t,p)) (S.elements s')

  (* �����W�����T�u�Z�b�g�ł�OK *)
  let next_list2 cs s =
    let s',_ = S.partition (fun (c',_,_) -> Cset.subset cs c') s in
      List.map (fun (_,t,p) -> (t,p)) (S.elements s')

  (* �X�L�b�v�����ɕύX���� *)
  let skip cs s =
    let s1,s2 = S.partition (fun (c',_,_) -> c' = cs) s in
      List.fold_left (fun s' e -> S.add e s') s2
        (List.map (fun (c,t,p) -> (Cset.all,t,p)) (S.elements s1))

  let cset_list s =
    S.fold (fun (c,t,p) cs -> if List.mem c cs then cs else c::cs) s []

  let show s =
    let cs = cset_list s in
      List.iter (
        fun c ->
          (* s'��c�Ŏn�܂�J�� *)
          let s',_ = S.partition (fun (c',_,_) -> c' = c) s in
            print_string "\t->";
            Cset.show c;
            print_newline();
            S.iter (
              fun (_,t,p) ->
                print_string "\t\t";
                TcondSet.show t;
                show_posset p;
                print_newline()
            ) s'
      ) cs
end

(*
 * DFA���_���v����
 * 
 *   ���@���Finit   : PosSet.t    --- DFA�������
 *           ps_map : PosSetMap.t --- ��ԑJ�ڕ\
 *           ls_map : PosSetMap.t --- ���x���\
 * 
 *)
let show (init,(ps_map,ls_map)) =
  show_posset init; print_newline();
  PosSetMap.iter (
    fun ps tm ->
      show_posset ps;
      show_lblset (PosSetMap.find ps ls_map);
      print_newline();
      TransMap.show tm
  ) ps_map

(*
 * ��̏����t���J�ڃ��X�g���}�[�W����
 * 
 *   ���@���Ftr1 : trans list --- �����t���J�ڃ��X�g1
 *           tr2 : trans list --- �����t���J�ڃ��X�g2
 * 
 *   �߂�l�F�}�[�W�������ʂ̏����t���J�ڃ��X�g
 * 
 *)
let rec union tr1 tr2 = N.normalize (
  match tr1,tr2 with
      [],_ -> tr2
    | _,[] -> tr1
    | (c1,(p1,l1))::cp1,(c2,(p2,l2))::cp2 when p1=p2
        -> (C.disj(c1,c2),(p1,LabelSet.union l1 l2))::(union cp1 cp2)
    | (c1,(p1,l1))::cp1,(c2,(p2,l2))::cp2 when p1<p2
        -> (c1,(p1,l1))::(c2,(p2,l2))::(union cp1 cp2)
    | (c1,(p1,l1))::cp1,(c2,(p2,l2))::cp2
        -> (c2,(p2,l2))::(c1,(p1,l1))::(union cp1 cp2)
)

(*
 * �J�n���x���̌v�Z
 * 
 *   ���@���Fls1 : LabelSet.t --- �J�ڌ��m�[�h�ɑΉ����郉�x���W��
 *           ls2 : LabelSet.t --- �J�ڐ�m�[�h�ɑΉ����郉�x���W��
 * 
 *   �߂�l�F�J�n���x���̏W��
 * 
 *)
let start_labels ls1 ls2 = 
  LabelSet.fold (
    fun lbl ls -> if LabelSet.mem lbl ls1 then ls else LabelSet.add lbl ls
  ) ls2 LabelSet.empty

(*
 * �J�n��Ԃ̃��x�����v�Z
 * 
 *   ���@���Fps  : PosSet.t --- �J�n��ԂɑΉ�����ʒu�W��
 *           ltbl: (Pos.t,LabelSet.t) Ht.t --- ���x���\
 * 
 *   �߂�l�F�J�n��Ԃ̊J�n���x���̏W��
 * 
 *)
let init_labels ps ltbl =
  PosSet.fold (
    fun p ls ->
      if Ht.mem ltbl p then
        LabelSet.union ls (Ht.find ltbl p)
      else
        ls
  ) ps LabelSet.empty

(*
 * ���x���m�[�h��艺�Ɉʒu����t�iPos.t�j�ɑ΂��ă��x�����L�^����
 * 
 *   ���@���Fre : Regex.t --- ���K�\���c���[
 * 
 *   �߂�l�F���x���L�^�\
 * 
 *)
let rcdlabel re =
  let tbl = Ht.create(13) in
  let rec rcd lbls = function
      R.EPS      -> ()
    | R.CHARS(p) -> Ht.add tbl p lbls
    | R.SEQ(r1,r2) | R.ALT(r1,r2) -> rcd lbls r1; rcd lbls r2
    | R.CLOS(r)    | R.REP(r,_)   -> rcd lbls r
    | R.LBL(r,l) -> rcd (LabelSet.add l lbls) r
  in
    rcd LabelSet.empty re; tbl

(*
 * �J�ڏ����̃��X�g�𒼌𕪉�����
 *
 *   ���@���Fcs :(Cond.t * (Pos.t * LabelSet.t)) list --- �J�ڏ����ƒl�̑g�̃��X�g
 * 
 *   �߂�l�F���𕪉��������ʂ̃��X�g�F(Cond.t * (PosSet.t * LabelSet.t)) list
 * 
 *)
let decomp cs =
  (* �J�ڏ������X�g���疽��_�����̑g�ݍ��킹�𒊏o *)
  let ps = C.get_vars (fst (List.split cs)) in
    match cs,ps with
        [],_ -> []
      | _,[] -> [C.Const true,
                 List.fold_left (
                   fun (ps,ls) (pos,lbl) ->
                     (PosSet.add pos ps),(LabelSet.union lbl ls)
                 ) (PosSet.empty,LabelSet.empty) (snd (List.split cs))]
      | _    ->
          (* �e�J�ڏ������`�F�b�N *)
          List.filter (fun (_,(ps,ls)) -> not (PosSet.is_empty ps)) (
            List.fold_left (
              fun rs p ->
                (p,( List.fold_left (
                       fun (ps,ls) (c,(pos,lbl)) ->
                         if C.is_true(C.impl p c) then
                           (PosSet.add pos ps),(LabelSet.union lbl ls)
                         else
                           ps,ls
                     ) (PosSet.empty,LabelSet.empty) cs ))::rs
            ) [] ps
          )


type act =
    ACT_NULL          (* ������ *)
  | ACT_MATCH         (* �����W���}�b�`���� *)
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
  | ACT_TRANS         -> "ACT_TRANS       "
  | ACT_FINAL         -> "ACT_FINAL       "
  | ACT_RECORD        -> "ACT_RECORD      "
  | ACT_COND_VALZERO  -> "ACT_COND_VALZERO"
  | ACT_COND_VALNONZ  -> "ACT_COND_VALNONZ"
  | ACT_COND_CNTZERO  -> "ACT_COND_CNTZERO"
  | ACT_COND_CNTNONZ  -> "ACT_COND_CNTNONZ"
  | ACT_COUNT_SET     -> "ACT_COUNT_SET   "
  | ACT_COUNT_DECR    -> "ACT_COUNT_DECR  "

type next  = act * int
type stent = next * int
type fent  = string * int
type ment  = next * Cset.t
type rent  = next * int * int  (* ������ * ���x��ID * �T�C�Y *)
type cond  = next * next * int (* TRUE������ * FALSE������ * ���x��ID *)
type cact  = next * int        (* ������ * ���x��ID *)

let state_map = ref PosSetMap.empty  (* PosSet.t -> State.ID *)
let state_table = Ht.create(29)      (* State.ID -> stent *)
let state_size  = ref 0
let new_state() =
  let id = !state_size in incr state_size; id
let update_stent st (next,fidx) =
  Ht.replace state_table st (next,fidx)

let fact_table  = Ht.create(29)
let fact_size   = ref 0
let create_fent (cons,num) =
  let idx = !fact_size in
    incr fact_size;
    Ht.add fact_table idx (cons,num);
    idx

let mact_table  = Ht.create(29)
let mact_size   = ref 0
let create_ment (next,cs) =
  let idx = !mact_size in
    incr mact_size;
    Ht.add mact_table idx (next,cs);
    idx

let ract_table  = Ht.create(29)
let ract_size   = ref 0
let create_rent (next,lid,lsiz) =
  let idx = !ract_size in
    incr ract_size;
    Ht.add ract_table idx (next,lid,lsiz);
    idx

let cond_table  = Ht.create(29)
let cond_size   = ref 0
let create_cond (tnxt,fnxt,lid) =
  let idx = !cond_size in
    incr cond_size;
    Ht.add cond_table idx (tnxt,fnxt,lid);
    idx

let cact_table  = Ht.create(29)
let cact_size   = ref 0
let create_cact (next,lid) =
  let idx = !cact_size in
    incr cact_size;
    Ht.add cact_table idx (next,lid);
    idx

let max_label = ref 0
let update_max_label n =
  if n > !max_label then max_label := n

(*
 * DFA�I�u�W�F�N�g�𐶐�����
 * 
 *   ���@���Finit   : PosSet.t    --- �������
 *           ps_map : PosSetMap.t --- �J�ڕ\
 *           ls_map : PosSetMap.t --- �L�^���x���\(���x�����L�^�����Ԃ̕\)
 *   �߂�l�F������Ԃ�ID
 * 
 *)
let create (init,(ps_map,ls_map)) =
  let label_map = Label.map_create() in
  let rec create_next tm =
    let cset_list = TransMap.cset_list tm in (* Cset.t�̃��X�g���擾 *)
    let next = create_cset_next (List.hd cset_list) tm in
      List.iter (fun cs -> ignore(create_cset_next cs tm)) (List.tl cset_list);
      next
  and create_cset_next cs tm =
    let tp_list = TransMap.next_list cs tm in
    let next = List.fold_left (
      fun nxt (tcset,posset) -> create_trans_next nxt (tcset,posset)
    ) (create_trans_next (ACT_NULL,0) (List.hd tp_list)) (List.tl tp_list)  in
      if Cset.all = cs then
        next
      else
        ACT_MATCH,create_ment(next,cs)
  and create_trans_next next (tcset,posset) =
    let nxt = ACT_TRANS,PosSetMap.find posset !state_map in
      if TcondSet.is_empty tcset then
        nxt
      else
        create_tcond_next nxt next tcset
  and create_rcd_next next posset =
    let ls = PosSetMap.find posset ls_map in
      if LabelSet.is_empty ls then
        next
      else
        LabelSet.fold (
          fun lbl nxt ->
            ACT_RECORD,
            create_rent(nxt,Label.map_find label_map lbl,Label.size lbl)
        ) ls next
  and create_tcond_next tnxt fnxt tcset =
    let next = TcondSet.fold (
      fun tc nxt ->
        match tc with
          | Tcond.ValNonz(l) ->
              ACT_COUNT_SET,create_cact(nxt,Label.map_find label_map l)
          | Tcond.CntNonz(l) ->
              ACT_COUNT_DECR,create_cact(nxt,Label.map_find label_map l)
          | _ -> nxt
    ) tcset tnxt in
    let next' = TcondSet.fold (
      fun tc nxt ->
        match tc with
            Tcond.ValZero(l) ->
              ACT_COND_VALZERO,create_cond(nxt,fnxt,Label.map_find label_map l)
          | Tcond.ValNonz(l) ->
              ACT_COND_VALNONZ,create_cond(nxt,fnxt,Label.map_find label_map l)
          | Tcond.CntZero(l) ->
              ACT_COND_CNTZERO,create_cond(nxt,fnxt,Label.map_find label_map l)
          | Tcond.CntNonz(l) ->
              ACT_COND_CNTNONZ,create_cond(nxt,fnxt,Label.map_find label_map l)
    ) tcset next in
      next'
  and create_final ps =
    let num,field_list = PosSet.fold ( (* �ŏ��̎󗝔ԍ���I������ *)
      fun p (n,fs) ->
        if Ht.mem acc_tbl p then
          let n',fs' = Ht.find acc_tbl p in
            if n < n' then n,fs else n',fs'
        else
          n,fs
    ) ps (max_int,[]) in
      create_fent (T.encode_field_list label_map field_list,num)
  in
  state_map :=
    PosSetMap.fold (
      fun ps a stmap ->
        if PosSetMap.mem ps stmap then
          stmap
        else
          PosSetMap.add ps (new_state()) stmap
    ) ps_map !state_map;
  PosSetMap.iter (
    fun ps tm ->
      let next  = create_next tm in
      let next' = create_rcd_next next ps in
      let fidx  = create_final ps in
        update_stent (PosSetMap.find ps !state_map) (next',fidx)
  ) ps_map;
  update_max_label (Label.map_size label_map);
  PosSetMap.find init !state_map

let gendfa re =
  let ltbl = rcdlabel re in
  let init_st = N.cp2ps (N.firstpos re) in
  let follow  = N.followpos re in
  let rec make marked unmarked labels =
    if PosSetSet.is_empty unmarked then marked,labels else
      let ps = PosSetSet.choose unmarked in
      let trans_map,label_map = 
        Cset.fold (
          fun (tm,lm) a ->
            let f flw p cp =
              let nxts = flw p in
              let nxts' = List.map (
                fun (c,p') ->
                  if Ht.mem ltbl p' then
                    let lbls  = Ht.find ltbl p in (* �J�ڌ��̃��x���W�� *)
                    let lbls' = Ht.find ltbl p' in (* �J�ڐ�̃��x���W�� *)
                      c,(p',start_labels lbls lbls') (* �J�n���x�����擾 *)
                  else c,(p',LabelSet.empty)
              ) nxts in
                if Cset.mem a (Pos.pos2cs p) then union cp nxts' else cp in
            let nexts = PosSet.fold (f follow) ps []
            in
              List.fold_left (
                fun (tm,lm) (cond,(ps,ls)) ->
                  (TransMap.add (Cset.create a,TcondSet.of_cond cond) ps tm),
                  (if PosSetMap.mem ps lm then
                     PosSetMap.add ps (LabelSet.union ls (PosSetMap.find ps lm)) lm
                   else
                     PosSetMap.add ps ls lm)
              ) (tm,lm) (decomp nexts)
        ) (TransMap.empty,labels) (N.ps2cset ps) in
        make
          (PosSetMap.add ps trans_map marked)
          (List.fold_left (
             fun us ns ->
               if PosSetMap.mem ns marked then us
               else PosSetSet.add ns us
           ) (PosSetSet.remove ps unmarked) (TransMap.next_all trans_map))
          label_map
  in
    init_st,(
      make
        PosSetMap.empty
        (PosSetSet.singleton init_st)
        (PosSetMap.add init_st (init_labels init_st ltbl) PosSetMap.empty)
    )

(*
 * ���K�\���^�̃��X�g����DFA�𐶐�����
 * 
 *   ���@���Fts : Types.rgx list --- ���K�\���^�̃��X�g
 * 
 *   �߂�l�Fint --- ��������DFA��ID
 * 
 *)
let generate ts =
  let rs,fs = List.split (List.map T.regexify2 ts) in
  let rs',ps = List.split(List.map (
                            fun r ->
                              let p  = Pos.create Cset.fin in
                                R.SEQ(R.posify r,R.CHARS p),p
                          ) rs) in
  let _ = let idx = ref 0 in
    List.iter2 (
      fun p f ->
        Ht.add acc_tbl p (!idx,f);
        incr idx
    ) ps fs in
  let re = List.fold_left (fun r' r -> R.ALT(r',r)) (List.hd rs') (List.tl rs') in
  let ltbl = rcdlabel re in
  let init_st = N.cp2ps (N.firstpos re) in
  let follow  = N.followpos re in
  let rec make marked unmarked labels =
    if PosSetSet.is_empty unmarked then marked,labels else
      let ps = PosSetSet.choose unmarked in
      let trans_map,label_map = 
        Cset.fold (
          fun (tm,lm) a ->
            let f flw p cp =
              let nxts = flw p in
              let nxts' = List.map (
                fun (c,p') ->
                  if Ht.mem ltbl p' then
                    let lbls  = Ht.find ltbl p in (* �J�ڌ��̃��x���W�� *)
                    let lbls' = Ht.find ltbl p' in (* �J�ڐ�̃��x���W�� *)
                      c,(p',start_labels lbls lbls') (* �J�n���x�����擾 *)
                  else c,(p',LabelSet.empty)
              ) nxts in
                if Cset.mem a (Pos.pos2cs p) then union cp nxts' else cp in
            let nexts = PosSet.fold (f follow) ps []
            in
              List.fold_left (
                fun (tm,lm) (cond,(ps,ls)) ->
                  (TransMap.add (Cset.create a,TcondSet.of_cond cond) ps tm),
                  (if PosSetMap.mem ps lm then
                     PosSetMap.add ps (LabelSet.union ls (PosSetMap.find ps lm)) lm
                   else
                     PosSetMap.add ps ls lm)
              ) (tm,lm) (decomp nexts)
        ) (TransMap.empty,labels) (N.ps2cset ps) in
        make
          (PosSetMap.add ps trans_map marked)
          (List.fold_left (
             fun us ns ->
               if PosSetMap.mem ns marked then us
               else PosSetSet.add ns us
           ) (PosSetSet.remove ps unmarked) (TransMap.next_all trans_map))
          label_map
  in
  let dfa = init_st,(
    make
      PosSetMap.empty
      (PosSetSet.singleton init_st)
      (PosSetMap.add init_st (init_labels init_st ltbl) PosSetMap.empty)
  )
  in
    create dfa

(*
 * ���K�\���^�̃��X�g����X�L�b�v�����t��DFA�𐶐�����
 * 
 *   ���@���Ft  : Types.rgx      --- ���K�\���^�i�e�p�^�[���j
 *           ts : Types.rgx list --- ���K�\���^�̃��X�g
 * 
 *   �߂�l�Fint --- ��������DFA��ID
 * 
 *)
let generate2_ t ts =
  let rs,fs = List.split (List.map T.regexify2 ts) in
  let rs',ps = List.split(List.map (
                            fun r ->
                              let p  = Pos.create Cset.fin in
                                R.SEQ(R.posify r,R.CHARS p),p
                          ) rs) in
  let _ = let idx = ref 0 in
    List.iter2 (
      fun p f ->
        Ht.add acc_tbl p (!idx,f);
        incr idx
    ) ps fs in
  let re = List.fold_left (fun r' r -> R.ALT(r',r)) (List.hd rs') (List.tl rs') in
  let re' = R.posify (T.regexify t) in
  let init,(ps_map,ls_map) = gendfa re in   (* �p�^���}�b�`��DFA *)
  let init',(ps_map',ls_map') = gendfa re' in (* �}�b�`�ς݂�DFA *)
(* DFA�̍\��
 *           init   : PosSet.t    --- �������
 *           ps_map : PosSetMap.t --- �J�ڕ\
 *           ls_map : PosSetMap.t --- �L�^���x���\(���x�����L�^�����Ԃ̕\)
 *)
  let pairs = ref [init,init'] in
  let checked = ref [] in
  let new_ps_map = ref PosSetMap.empty in
    (* tm <: tm' *)
  let next_pairs tm tm' =
    let cl = TransMap.cset_list tm in    (* ���͕����W���̃��X�g *)
      List.iter (
        fun cs ->
          let nl = TransMap.next_list cs tm in    (* ���J�ڏ����~����Ԃ̃��X�g *)
          let nl' = TransMap.next_list2 cs tm' in
            (* TODO: �J�ڏ����Ɋւ���`�F�b�N *)
            List.iter (
              fun (t,p) ->
                let t',p' = List.find (fun (t',p') -> TcondSet.imply t t') nl' in
                  if not (List.mem (p,p') !checked) then pairs := (p,p')::!pairs
            ) nl
      ) cl in
    while !pairs != []
    do
      let s,s' = List.hd !pairs in
      let tm = PosSetMap.find s ps_map in  (* �J�ڃe�[�u�� *)
      let tm' = PosSetMap.find s' ps_map' in 
      let cl = TransMap.cset_list tm in    (* ���͕����W���̃��X�g *)
      let cl' = TransMap.cset_list tm' in
        pairs := List.tl !pairs;
        checked := (s,s')::!checked;
        ( if (List.length cl)=1 && (List.length cl')=1 && (List.hd cl=List.hd cl')
          then
            let cs = List.hd cl in
            let tc = TransMap.next_list cs tm in
              new_ps_map := PosSetMap.add s (TransMap.skip cs tm) !new_ps_map
              (* if (List.length tc) = 1 then *)
          else
            new_ps_map := PosSetMap.add s tm !new_ps_map
        );
        next_pairs tm tm'
    done;
    create (init,(!new_ps_map,ls_map))

let generate2 t ts =
  if !skipMode then
    generate2_ t ts
  else
    generate ts

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
  for i = 0 to !state_size - 1 do
    let (act,nidx),fidx = Ht.find state_table i in
      Printf.printf "/* [%03d] */ { %s,%4d,%4d },\n" i (act_string act) nidx fidx
  done;
  Printf.printf "};\n";
  (* �I�������G���g�� *)
  Printf.printf "fact_t __prc__fact_table[] = {\n";
  for i = 0 to !fact_size - 1 do
    let cons,num = Ht.find fact_table i in
      if String.length cons == 0 then
        Printf.printf "/* [%03d] */ { %2d, NULL },\n" i num
      else
        Printf.printf "/* [%03d] */ { %2d, \"%s\" },\n" i num cons
  done;
  Printf.printf "};\n";
  (* �����W���e�[�u�� *)
  Printf.printf "mact_t __prc__mact_table[] = {\n";
  for i = 0 to !mact_size - 1 do
    let (act,nidx),cs = Ht.find mact_table i in
      Printf.printf "/* [%03d] */ { %s,%4d,%s },\n"
        i (act_string act) nidx (Cset.encode cs)
  done;
  Printf.printf "};\n";
  (* ���x���L�^�����e�[�u�� *)
  Printf.printf "ract_t __prc__ract_table[] = {\n";
  for i = 0 to !ract_size - 1 do
    let (act,nidx),lid,lsiz = Ht.find ract_table i in
      Printf.printf "/* [%03d] */ { %s,%4d,%2d,%2d },\n"
        i (act_string act) nidx lid lsiz
  done;
  Printf.printf "};\n";
  (* ���������e�[�u�� *)
  Printf.printf "cond_t __prc__cond_table[] = {\n";
  for i = 0 to !cond_size - 1 do
    let (tact,tidx),(fact,fidx),lid = Ht.find cond_table i in
      Printf.printf "/* [%03d] */ { %s,%4d, %s,%4d,%2d },\n"
        i (act_string tact) tidx (act_string fact) fidx lid
  done;
  Printf.printf "};\n";
  (* �J�E���^�����e�[�u�� *)
  Printf.printf "cact_t __prc__cact_table[] = {\n";
  for i = 0 to !cact_size - 1 do
    let (act,nidx),lid = Ht.find cact_table i in
      Printf.printf "/* [%03d] */ { %s,%4d,%2d },\n"
        i (act_string act) nidx lid
  done;
  Printf.printf "};\n";
  (* ���x�����W�X�^ *)
  Printf.printf "#define MAX_LABEL (%d)\n" !max_label;
  Printf.printf "u_char *__prc__lbl_ptr[MAX_LABEL];\n";
  Printf.printf "u_int __prc__lbl_value[MAX_LABEL];\n";
  Printf.printf "u_int __prc__lbl_count[MAX_LABEL];\n";

  
