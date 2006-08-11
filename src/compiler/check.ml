(**
   �^�������W���[��
   
   �T�v�F�^�����A�^���̕ϊ����s��

   @author Hattori Kenta
   @version $Id: check.ml,v 1.9 2006/08/07 09:08:06 hattori Exp $
*)
open Error

module A = Syntax
module E = Env
module T = Types
module R = Regex

(* t1 <: t2�ł��邱�Ƃ��m�F���� *)
let assert_type i t1 t2 =
  if T.subtype t1 t2 then () else errorAt i (ERR_TYPE_MISMATCH (t1,t2))

(* �t�B�[���h���擾���� *)
let get_field info t s =
  try
    T.field t s
  with
      Not_found -> errorAt info (ERR_UNDEF_LABEL s)
    | T.Ill_field -> errorAt info ERR_ILLEGAL_LABEL
    
(* �z��v�f���擾���� *)
let get_index info t =
  try
    T.index t
  with
      _ -> errorAt info ERR_ILLEGAL_ARRAY

(*
 * �v���Z�X�̖{�̂��擾����
 * 
 *   ���@���Finfo: Error.info --- �G���[���
 *           env : Env.t      --- ���O��
 *           s   : Symbol.t   --- ���O
 *   �߂�l�F�v���Z�X�̈����̌^�̕���
 * 
 *)
let lookup_proc info env s =
  try match E.find s env with
      E.ProcEntry ts -> ts
    | _ -> raise Not_found
  with
      Not_found -> errorAt info (ERR_UNDEF_PROC s)

(*
 * �^������^������������
 * 
 *   ���@���Finfo: Error.info --- �G���[���
 *           env : Env.t      --- ���O��
 *           s   : Symbol.t   --- ���O
 *   �߂�l�F�^���
 * 
 *)
let lookup_type info env s =
  try match E.find s env with
      E.TypeEntry t -> t
    | _ -> raise Not_found
  with
      Not_found -> errorAt info (ERR_UNDEF_TYPE s)

let lookup_rtype info env s =
  match lookup_type info env s with
      T.REGEX(re) -> re
    | _ -> errorAt info ERR_ILLEGAL_TYPE

(*
 * �`���l���̎��^���擾����
 * 
 *   ��  ���Finfo: Error.info --- �G���[���
 *             ch: Types.t    --- �`���l���^
 *   �Ԃ�l�F �`���l���̎��^
 * 
 *)
let type_of_chan info = function
    T.CHAN ty -> ty
  | _         -> errorAt info ERR_NOT_CHANNEL

(* �萔�̌^��Ԃ� *)
let type_of_const = function
    A.ConBool(_,b) -> T.BOOL
  | A.ConInt(_,n)  -> T.INT
  | A.ConStr(_,s)  -> T.STRING

(*
 * �^��`�̃`�F�b�N���s���^���ɕϊ�����
 * 
 *   ���@���Fenv: Env.t --- ���O��
 *           -  : S.typ --- �^�\��
 *   �߂�l�F�ϊ������^��
 * 
 *)
let rec check_type env =
  (* �h���^��`�p�̕ϐ��Q�� *)
  let rec deriv_var t = function
      A.VarSimple(i,s)    -> get_field i t s
    | A.VarField(i,v,s,_,_) ->
        let t' = get_field i (deriv_var t v) s in
          (* o := T.offset t' s; *) t'
    | A.VarProj(i,v,n) -> T.proj (deriv_var t v) n
    | A.VarSubscr(i,v,e)  ->
        errorAt i ERR_ILLEGAL_DERIV in
  let deriv_ap v =
    let rec trav ap = function
        A.VarSimple(_,s) -> s::ap
      | A.VarField(_,v,s,_,_) -> trav (s::ap) v
      | _ -> assert false in
      trav [] v in
    function
        A.TypName(i,s)  -> lookup_type i env s
      | A.TypChan(_,t)  -> T.CHAN(check_type env t)
      | A.TypRegex(_,r) -> T.REGEX(check_regex env r)
      | A.TypRecord fs  ->
          T.RECORD(List.map (fun (_,s,t) -> s,check_type env t) fs)
      | A.TypTuple ts  ->
          T.TUPLE(List.map (fun t -> check_type env t) ts)
      | A.TypArray(i,t,n) ->
          if n > 0
          then T.ARRAY(check_type env t,n)
          else errorAt i (ERR_ILLEGAL_ARRLEN n)
      | A.TypDeriv(i,s,ds) ->
          List.fold_left
            (fun ty (v,t) ->
               let t' = check_type env t in
               let t'' = deriv_var ty v in
                 assert_type i t' t'';
                 T.deriv ty (deriv_ap v) t')
            (lookup_type i env s) ds

(*
 * ���K�\���^�̃`�F�b�N���s���^���ɕϊ�����
 * 
 *   ���@���Fenv: Env.t --- ���O��
 *           -  : S.rgx --- ���K�\���^�\��
 *   �߂�l�F�ϊ������^��
 * 
 *)
and check_regex env = function
    A.RgxString(i,s)  -> T.REXP(R.of_string s)
  | A.RgxChrcls(i,s)  -> T.REXP(R.of_chrcls s)
  | A.RgxName(i,s)    -> lookup_rtype i env s
  | A.RgxCat(_,r1,r2) -> T.REXP(R.SEQ(T.regexify(check_regex env r1),
                                      T.regexify(check_regex env r2)))
  | A.RgxAlt(_,r1,r2) -> T.REXP(R.ALT(T.regexify(check_regex env r1),
                                      T.regexify(check_regex env r2)))
  | A.RgxClos(_,r)    -> T.REXP(R.CLOS(T.regexify(check_regex env r)))
  | A.RgxPclos(_,r)   -> T.REXP(R.pclos(T.regexify(check_regex env r)))
  | A.RgxOpt(_,r)     -> T.REXP(R.opt(T.regexify(check_regex env r)))
  | A.RgxArray(i,r,n) ->
      if n > 0
      then T.RARR(check_regex env r,n)
      else errorAt i (ERR_ILLEGAL_ARRLEN n)
  | A.RgxIter(_,r,s)  -> T.RITR(check_regex env r,s)
  | A.RgxRecord rs    -> T.RRCD(List.map (fun (_,s,r) -> s,check_regex env r) rs)

(*
 * �v���Z�X���̃`�F�b�N���s��
 * 
 *   ��  ���Fenv: Env.t --- ��
 *           -  : proc  --- �v���Z�X��
 *   �߂�l�F����Ԃ�
 *
 *)
and check_proc env = function
    A.ProcStop _ | A.ProcSkip _ -> env
  | A.ProcSeq(_,ps) -> List.fold_left (fun e p -> check_proc e p) env ps
  | A.ProcChoice(_,ps) ->
      List.iter (fun (g,p) -> ignore (check_proc (check_proc env g) p)) ps; env
  | A.ProcMatch(_,exp,ps,t) ->
      t := check_expr env exp;
      List.iter (
        fun (patn,proc) ->
          let env' = check_match env (!t) patn in
            ignore (check_proc env' proc)
      ) ps;
      env
  | A.ProcInput(i,v,s) ->
      let t = type_of_chan i (check_var env v) in E.add s (E.VarEntry t) env
  | A.ProcOutput(i,v,e) ->
      let t1 = type_of_chan i (check_var env v) in
      let t2 = check_expr env e in
        assert_type i t2 t1; env
  | A.ProcRun(i,s,es) ->
      let ts = lookup_proc i env s in
        if (List.length es) == (List.length ts) then
          (List.iter2 (fun e t -> assert_type i (check_expr env e) t) es ts; env)
        else
          errorAt i ERR_INVALID_ARGNUM
  | A.ProcVar(i,s,A.DeclExpr e) -> E.add s (E.VarEntry(check_expr env e)) env
  | A.ProcVar(i,s,A.DeclType(t,ty)) ->
      ty := check_type env t; E.add s (E.VarEntry !ty) env
  | A.ProcAsign(i,v,e) ->
      assert_type i (check_expr env e) (check_var env v); env
  | A.ProcCblock(i,_,vs) ->
      List.iter (fun v -> ignore (check_var env v)) vs; env

(*
 * ���̌^�`�F�b�N���s���C�^��Ԃ�
 *
 *   ��  ���Fenv: Env.t --- ��
 *           -  : exp   --- ��
 *   �߂�l�F�����œn���ꂽ���̎��^
 *
 *)
and check_expr env = function
    A.ExpConst(c) -> type_of_const c
  | A.ExpVar(v)   -> check_var env v
  | A.ExpRecord(fs) -> T.RECORD(List.map (fun (_,s,e) -> s,check_expr env e) fs)
  | A.ExpTuple es   -> T.TUPLE(List.map (fun e -> check_expr env e) es)
  | A.ExpBinop(i,A.BopAdd,e1,e2) | A.ExpBinop(i,A.BopSub,e1,e2)
  | A.ExpBinop(i,A.BopMul,e1,e2) | A.ExpBinop(i,A.BopDiv,e1,e2)
  | A.ExpBinop(i,A.BopMod,e1,e2) ->
      assert_type i (check_expr env e1) T.INT;
      assert_type i (check_expr env e2) T.INT;
      T.INT
  | A.ExpBinop(i,A.BopLt, e1,e2) | A.ExpBinop(i,A.BopLeq,e1,e2)
  | A.ExpBinop(i,A.BopGt, e1,e2) | A.ExpBinop(i,A.BopGeq,e1,e2) ->
      assert_type i (check_expr env e1) T.INT;
      assert_type i (check_expr env e2) T.INT;
      T.BOOL
  | A.ExpBinop(i,A.BopEq, e1,e2) | A.ExpBinop(i,A.BopNeq,e1,e2) ->
      let t1 = check_expr env e1 in
      let t2 = check_expr env e2 in
        assert_type i t1 t2;
        assert_type i t2 t1;
        T.BOOL
  | A.ExpBinop(i,A.BopAnd,e1,e2) | A.ExpBinop(i,A.BopOr, e1,e2) ->
      assert_type i (check_expr env e1) T.BOOL;
      assert_type i (check_expr env e2) T.BOOL;
      T.BOOL
  | A.ExpBinop(i,A.BopCat,e1,e2) ->
      assert_type i (check_expr env e1) T.STRING;
      assert_type i (check_expr env e2) T.STRING;
      T.STRING
  | A.ExpMonop(i,A.MopNeg,e) -> assert_type i (check_expr env e) T.INT; T.INT
  | A.ExpMonop(i,A.MopNot,e) -> assert_type i (check_expr env e) T.BOOL;T.BOOL

(*
 * �ϐ���`�̃`�F�b�N���s��
 * 
 *   ��  ���Fenv: Env.t --- ��
 *           -  : var   --- �ϐ�
 *   �߂�l�F�����œn���ꂽ�ϐ��̌^
 * 
 *)
and check_var env =
  (* �ϐ����������� *)
  let lookup_var info s =
    try
      match E.find s env with
          E.VarEntry(t) -> t
        | _             -> raise Not_found
    with
        Not_found -> errorAt info (ERR_UNDEF_VAR s) in
    (* �A�N�Z�X�p�X�����ǂ� *)
    function
        A.VarSimple(i,s)    -> lookup_var i s
      | A.VarField(i,v,s,o,r) ->
          let t = check_var env v in
          let t' = get_field i t s in
            r := t;
            o := T.offset t s; t'
      | A.VarProj(i,v,n) -> T.proj (check_var env v) n
      | A.VarSubscr(i,v,e)  ->
          assert_type i (check_expr env e) T.INT;  (* e��INT�^�ł��邱��   *)
          get_index i (check_var env v)


(*
 * �p�^�[�����̃`�F�b�N
 * 
 *   ���@���Fenv: Env.t   --- ���O��
 *           ty : Types.t --- �p�^�[���}�b�`����l�̌^���
 *           -  : pat     --- �p�^�[���\��
 *   �߂�l�F�V��������Ԃ�
*)
and check_match env ty = function
    A.PatAny _   -> env
  | A.PatConst c -> 
      if T.subtype ty (type_of_const c) then env
      else errorAt (A.info_of_const c) ERR_ILLEGAL_PATTERN
  | A.PatRegex(i,s,r,t) ->
      let rt = check_regex env r in
        if T.subtype (T.REGEX rt) ty then (
          t := rt;
          E.add s (E.VarEntry(T.REGEX rt)) env
        ) else errorAt i ERR_ILLEGAL_PATTERN

(*
 * �^�`�F�b�N���s���֐�
 *
 *   ��  ���Fdefs: toplevel --- �g�b�v���x����`�̃��X�g
 *   �߂�l�F�g�b�v���x����
 * 
 *)
let check defs =
  (* �g�b�v���x����`�̌^�`�F�b�N���s�� *)
  List.fold_left (
    fun e -> (
      function
          A.DefVar(_,s,A.DeclExpr ex) -> E.add s (E.VarEntry(check_expr e ex)) e
        | A.DefVar(_,s,A.DeclType(t,ty)) ->
            ty := check_type e t;E.add s (E.VarEntry !ty) e
        | A.DefType types ->
            List.fold_left (
              fun e (_,s,ty) -> E.add s (E.TypeEntry(check_type e ty)) e
            ) e types
        | A.DefProc procs ->
            (* �v���Z�X��`�w�b�_���̃`�F�b�N *)
            let e = List.fold_left (
              fun e (_,s,ds,_) ->
                E.add s (E.ProcEntry(List.map (fun (_,t) -> check_type e t) ds)) e
            ) e procs in
              List.iter (
                fun (i,s,ds,p) ->
                  match (E.find s e) with
                      (* �v���Z�X�{�̂̃`�F�b�N *)
                      E.ProcEntry(ts) -> ignore (
                        check_proc (
                          List.fold_left2 (
                            fun e (s,_) t -> E.add s (E.VarEntry t) e
                          ) e ds ts
                        ) p )
                    | _ -> assert false
              ) procs; e
    )
  ) E.base_env defs
