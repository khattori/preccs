(**
   型検査モジュール
   
   概要：型検査、型式の変換を行う

   @author Hattori Kenta
   @version $Id: check.ml,v 1.9 2006/08/07 09:08:06 hattori Exp $
*)
open Error

module A = Syntax
module E = Env
module T = Types
module R = Regex

(* t1 <: t2であることを確認する *)
let assert_type i t1 t2 =
  if T.subtype t1 t2 then () else errorAt i (ERR_TYPE_MISMATCH (t1,t2))

(* labelが二重定義されていないことを確認する *)
let assert_label ls =
  ignore (List.fold_left (fun ss (i,s,_) ->
    if List.mem s ss then errorAt i (ERR_REDEF_LABEL s) else s::ss
  ) [] ls)
    

(* フィールドを取得する *)
let get_field info t s =
  try
    T.field t s
  with
      Not_found -> errorAt info (ERR_UNDEF_LABEL s)
    | T.Ill_field -> errorAt info ERR_ILLEGAL_LABEL
    
(* 配列要素を取得する *)
let get_index info t =
  try
    T.index t
  with
      _ -> errorAt info ERR_ILLEGAL_ARRAY

(*
 * プロセスの本体を取得する
 * 
 *   引　数：info: Error.info --- エラー情報
 *           env : Env.t      --- 名前環境
 *           s   : Symbol.t   --- 名前
 *   戻り値：プロセスの引数の型の並び
 * 
 *)
let lookup_proc info env s =
  try match E.find s env with
      E.ProcEntry(ts,r) -> ts,r
    | _ -> raise Not_found
  with
      Not_found -> errorAt info (ERR_UNDEF_PROC s)

(*
 * 型名から型情報を検索する
 * 
 *   引　数：info: Error.info --- エラー情報
 *           env : Env.t      --- 名前環境
 *           s   : Symbol.t   --- 名前
 *   戻り値：型情報
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
 * チャネルの持つ型を取得する
 * 
 *   引  数：info: Error.info --- エラー情報
 *             ch: Types.t    --- チャネル型
 *   返り値： チャネルの持つ型
 * 
 *)
let type_of_chan info = function
    T.CHAN ty -> ty
  | _         -> errorAt info ERR_NOT_CHANNEL

(* 定数の型を返す *)
let type_of_const = function
    A.ConUnit _    -> T.UNIT	
  | A.ConBool(_,b) -> T.BOOL
  | A.ConInt(_,n)  -> T.INT
  | A.ConStr(_,s)  -> T.STRING

(*
 * 型定義のチェックを行い型式に変換する
 * 
 *   引　数：env: Env.t --- 名前環境
 *           -  : S.typ --- 型構文
 *   戻り値：変換した型式
 * 
 *)
let rec check_type env =
  (* 派生型定義用の変数参照 *)
(*
  let rec deriv_var t = function
      A.VarSimple(i,s) -> get_field i t s
    | v -> errorAt (A.info_of_var v) ERR_ILLEGAL_DERIV in
*)
  let deriv_ap v =
    let rec trav ap = function
        A.VarSimple(_,s) -> s::ap
      | _ -> assert false in
      trav [] v in
    function
        A.TypName(i,s)  -> lookup_type i env s
      | A.TypChan(_,t)  -> T.CHAN(check_type env t)
      | A.TypRegex(_,r) -> T.REGEX(check_regex env r)
      | A.TypRecord fs  ->
	  assert_label fs;
          T.RECORD(List.map (fun (_,s,t) -> s,check_type env t) fs)
      | A.TypVariant vs ->
	  assert_label vs;
          T.VARIANT(List.map (fun (_,s,t) -> s,check_type env t) vs)
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
(*               let t'' = deriv_var ty v in
                 assert_type i t' t''; *)
                 T.deriv ty (deriv_ap v) t')
            (lookup_type i env s) ds

(*
 * 正規表現型のチェックを行い型式に変換する
 * 
 *   引　数：env: Env.t --- 名前環境
 *           -  : S.rgx --- 正規表現型構文
 *   戻り値：変換した型式
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
  | A.RgxIter(_,r,s,f)  -> T.RITR(check_regex env r,s,f)
  | A.RgxRecord rs    ->
      assert_label rs;
      T.RRCD(List.map (fun (_,s,r) -> s,check_regex env r) rs)

(*
 * プロセス式のチェックを行う
 * 
 *   引  数：env: Env.t   --- 環境
 *           -  : proc    --- プロセス式
 *   戻り値：環境を返す
 *
 *)
and check_proc env r = function
    A.ProcStop _ | A.ProcSkip _ -> env
  | A.ProcReturn(i,e) -> (
      match r with
        None   -> errorAt i ERR_HAS_RETVAL
      | Some t -> assert_type i (check_expr env e) t; env
    )
  | A.ProcSeq(i,ps) ->
      List.fold_left (fun e p -> check_proc e r p) env ps
  | A.ProcChoice(_,ps) ->
      List.iter (fun (g,p) -> ignore (check_proc (check_proc env r g) r p)) ps; env
  | A.ProcMatch(i,exp,ps,ty) ->
      let t = check_expr env exp in
        ty := t;
        List.iter (
          fun (patn,proc) ->
            let env' = check_match env (t,patn) in ignore (check_proc env' r proc)
        ) ps;
        env
  | A.ProcInput(i,e,s) ->
      let t = type_of_chan i (check_expr env e) in E.add s (E.VarEntry t) env
  | A.ProcOutput(i,e1,e2) ->
      let t1 = type_of_chan i (check_expr env e1) in
      let t2 = check_expr env e2 in
        assert_type i t2 t1; env
  | A.ProcRun(i,s,es) ->
      let ts,_ = lookup_proc i env s in
        if (List.length es) == (List.length ts) then
          (List.iter2 (fun e t -> assert_type i (check_expr env e) t) es ts; env)
        else
          errorAt i ERR_INVALID_ARGNUM
  | A.ProcVar(i,s,A.DeclExpr e) -> E.add s (E.VarEntry(check_expr env e)) env
  | A.ProcVar(i,s,A.DeclType(t,ty)) ->
      ty := check_type env t; E.add s (E.VarEntry !ty) env
  | A.ProcAssign(i,e1,e2) -> 
      assert_type i (check_expr env e2) (check_expr env e1); env
  | A.ProcCblock(i,_,es) ->
      List.iter (fun e -> ignore (check_expr env e)) es; env

and check_proc_no_return = function
    A.ProcStop _ | A.ProcSkip _ | A.ProcAssign _
  | A.ProcInput _ | A.ProcOutput _
  | A.ProcRun _ | A.ProcVar _ | A.ProcCblock _ -> ()
  | A.ProcReturn(i,_) -> errorAt i ERR_HAS_RETVAL
  | A.ProcSeq(_,ps) -> List.iter (fun p -> check_proc_no_return p) ps
  | A.ProcChoice(_,ps) -> List.iter (fun (_,p) -> check_proc_no_return p) ps
  | A.ProcMatch(_,_,ps,_) -> List.iter (fun (_,p) -> check_proc_no_return p) ps

and check_proc_has_return = function
    A.ProcStop _ | A.ProcSkip _  | A.ProcAssign _
  | A.ProcInput _ | A.ProcOutput _
  | A.ProcRun _ | A.ProcVar _ | A.ProcCblock _ -> false
  | A.ProcReturn(i,e) -> true
  | A.ProcSeq(_,ps) -> List.exists (fun p -> check_proc_has_return p) ps
  | A.ProcChoice(_,ps) -> List.for_all (fun (_,p) -> check_proc_has_return p) ps
  | A.ProcMatch(_,_,ps,_) -> List.for_all (fun (_,p) -> check_proc_has_return p) ps

(*
 * 式の型チェックを行い，型を返す
 *
 *   引  数：env: Env.t --- 環境
 *           -  : exp   --- 式
 *   戻り値：引数で渡された式の持つ型
 *
 *)
and check_expr env = function
    A.ExpConst(c) -> type_of_const c
  | A.ExpVar(v)   -> check_var env v
  | A.ExpRecord(fs) -> T.RECORD(List.map (fun (_,s,e) -> s,check_expr env e) fs)
  | A.ExpVariant(_,s,e) -> T.VARIANT([s,check_expr env e])
  | A.ExpTuple es   -> T.TUPLE(List.map (fun e -> check_expr env e) es)
  | A.ExpArray es   -> T.ARRAY(check_expr env (List.hd es), List.length es)
  | A.ExpCall(i,s,es) ->
      let ts,r = lookup_proc i env s in
        if (List.length es) == (List.length ts) then
          (List.iter2 (fun e t -> assert_type i (check_expr env e) t) es ts;
           match r with
	       None -> errorAt i (ERR_NO_RETVAL s)
	     | Some t -> t)
        else
          errorAt i ERR_INVALID_ARGNUM
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
 * 変数定義のチェックを行う
 * 
 *   引  数：env: Env.t --- 環境
 *           -  : var   --- 変数
 *   戻り値：引数で渡された変数の型
 * 
 *)
and check_var env =
  (* 変数を検索する *)
  let lookup_var info s =
    try
      match E.find s env with
          E.VarEntry(t) -> t
        | _             -> raise Not_found
    with
        Not_found -> errorAt info (ERR_UNDEF_VAR s) in
    (* アクセスパスをたどる *)
    function
        A.VarSimple(i,s)    -> lookup_var i s
      | A.VarField(i,e,s,o,r) ->
          let t = check_expr env e in
          let t' = get_field i t s in
            r := t;
            o := T.offset t s; t'
      | A.VarProj(i,e,n) -> T.proj (check_expr env e) n
      | A.VarSubscr(i,e1,e2)  ->
          assert_type i (check_expr env e2) T.INT;  (* eはINT型であること   *)
          get_index i (check_expr env e1)


(*
 * パターン式全体の型チェック
 * 
 *   引　数：env: Env.t   --- 名前環境
 *           -  : pat     --- パターン構文
 *   戻り値：新しい環境を返す
 *)
and check_match env = function
    _,A.PatAny _   -> env
  | T.REGEX _,A.PatConst c -> assert_type (A.info_of_const c) (type_of_const c) T.STRING; env
  | t,A.PatConst c -> assert_type (A.info_of_const c) (type_of_const c) t; env
  | t,A.PatIdent(i,s) -> E.add s (E.VarEntry t) env
  | T.TUPLE ts,A.PatTuple ps when List.length ts == List.length ps
      -> List.fold_left2 (fun e t p -> check_match e (t,p)) env ts ps
  | T.RECORD rs,A.PatRecord ps when List.length rs == List.length ps
      -> List.fold_left2 (
           fun e (s,t) (i,s',p) ->
             if Symbol.equal s s' then
               check_match env (t,p)
             else
               errorAt i ERR_ILLEGAL_PATTERN
         ) env rs ps
  | T.VARIANT vs,A.PatVariant(i,s,p) when List.exists (fun (s',_) -> Symbol.equal s s') vs
      -> let _,t = List.find (fun (s',_) -> Symbol.equal s s') vs in
           check_match env (t,p)
  | T.STRING, A.PatRegex(i,s,r,t) | T.REGEX _, A.PatRegex(i,s,r,t) ->
      let rt = check_regex env r in
	t := rt;
        E.add s (E.VarEntry(T.REGEX rt)) env
  | _,p -> errorAt (A.info_of_pat p) ERR_ILLEGAL_PATTERN

(*
 * 型チェックを行う関数
 *
 *   引  数：defs: toplevel --- トップレベル定義のリスト
 *   戻り値：トップレベル環境
 * 
 *)
let check defs =
  (* トップレベル定義の型チェックを行う *)
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
            (* プロセス定義ヘッダ部のチェック *)
            let e = List.fold_left (
              fun e (i,s,ds,r,_) -> try
		  let _ = E.find s e in
		    errorAt i (ERR_REDEF_PROC s)
		with
		    Not_found ->
		      E.add s (E.ProcEntry(List.map (fun (_,t) -> check_type e t) ds,
                                          match r with
					      None -> None
					    | Some t' -> Some (check_type e t'))) e
            ) e procs in
              List.iter (
                fun (i,s,ds,_,p) ->
                  match (E.find s e) with
                      (* プロセス本体のチェック *)
                      E.ProcEntry(ts,None) -> ignore (
                        check_proc (
                          List.fold_left2 (
                            fun e (s,_) t -> E.add s (E.VarEntry t) e
                          ) e ds ts
                        ) None p );
			check_proc_no_return p
		    | E.ProcEntry(ts,Some r) ->
			let e' = List.fold_left2 (
                            fun e (s,_) t -> E.add s (E.VarEntry t) e
                          ) e ds ts
			in
			  ignore (check_proc e' (Some r) p);
			  if (check_proc_has_return p) then
				()
			  else
				errorAt i (ERR_NO_RETVAL s)
                    | _ -> assert false
              ) procs; e
    )
  ) E.base_env defs
