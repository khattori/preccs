(**
   中間言語変換モジュール
   
   概要：抽象構文木からπ中間言語へ変換を行う

   @author Hattori Kenta
   @version $Id: pi.ml,v 1.6 2006/07/27 00:07:18 hattori Exp $
*)
module A = Syntax
module P = Pi
module T = Types
module R = Regex
module L = List

let genid = Symbol.genid

let v_next = Symbol.symbol("next")
let v_junk = Symbol.symbol("junk")
let v_temp = Symbol.symbol("temp")
let v_mval = Symbol.symbol("mval")

let all_regex = L.for_all (fun (p,_) -> match p with A.PatRegex _ | A.PatAny _ -> true | _ -> false)

let rec make_list a = function
    0 -> []
  | n when n < 0 -> assert false
  | n -> a::make_list a (n-1)

let trans_const = function
    A.ConUnit _    -> P.Unit
  | A.ConBool(_,b) -> P.Bool b
  | A.ConInt(_,n)  -> P.Int n
  | A.ConStr(_,s)  -> P.String s
let trans_bop = function
    A.BopAdd -> P.Add | A.BopSub -> P.Sub
  | A.BopMul -> P.Mul | A.BopDiv -> P.Div
  | A.BopMod -> P.Mod
  | A.BopEq  -> P.Eq  | A.BopNeq -> P.Neq
  | A.BopLt  -> P.Lt  | A.BopLeq -> P.Leq
  | A.BopGt  -> P.Gt  | A.BopGeq -> P.Geq
  | A.BopAnd -> P.And | A.BopOr  -> P.Or
  | A.BopCat -> P.Cat
let trans_mop = function A.MopNeg -> P.Neg | A.MopNot -> P.Not

(*
 * 正規表現型のデフォルト文字列を取得
 *)
let rec defstr_of_regex = function
    T.REXP re -> R.to_string re
  | T.RARR(r,0) -> ""
  | T.RARR(r,1) -> defstr_of_regex r
  | T.RARR(r,n) -> (defstr_of_regex r)^(defstr_of_regex (T.RARR(r,n-1)))
  | T.RITR _    -> ""
  | T.RRCD fs -> L.fold_left (fun s (_,r) -> s ^ defstr_of_regex r) "" fs

let con_regex =
  let rec trav = function
      T.REXP re -> P.Const(P.Int (R.size re))
    | T.RARR(r,n) -> P.Const(P.Int ((R.size (T.regexify r)) * n))
    | T.RITR _ -> P.Const(P.Int 0)
    | T.RRCD fs -> P.Record (L.map (fun (_,r) -> trav r)  fs)
  in
    function
        T.REXP re -> P.Const(P.String (R.to_string re))
      | T.RARR(r,0) -> P.Const(P.String "")
      | T.RARR(r,1) -> P.Const(P.String (defstr_of_regex r))
      | T.RARR(r,n) -> P.Const(P.String ((defstr_of_regex r)^(defstr_of_regex (T.RARR(r,n-1)))))
      | T.RITR _    -> P.Const(P.String "")
      | T.RRCD fs ->
          let s = L.fold_left (fun s (_,r) -> s ^ defstr_of_regex r) "" fs in
            P.Rexrcd(s, L.map (fun (_,r) -> trav r) fs)


(*
 * 値式の変換
 * 
 *   引　数：env: Env.t --- 名前環境
 *           ---: exp   --- 値式
 *   戻り値：変換後の値式
 * 
 *)
let rec trans_expr env = function
    A.ExpConst c   -> P.Const(trans_const c)
  | A.ExpVar v     -> trans_var env v
  | A.ExpRecord fs -> P.Record(L.map (fun (_,_,e) -> trans_expr env e) fs)
  | A.ExpTuple es | A.ExpArray es
		   -> P.Record(L.map (trans_expr env) es)
  | A.ExpVariant(_,s,e) -> P.Record([P.Const(P.Int(Symbol.id s));trans_expr env e])
  | A.ExpBinop(_,bop,e1,e2) ->
      P.Prim(trans_bop bop,[trans_expr env e1; trans_expr env e2])
  | A.ExpMonop(_,mop,e) -> P.Prim(trans_mop mop,[trans_expr env e])
  | A.ExpCall(_,s,es) -> P.Apply(P.Var s,L.map (fun e -> trans_expr env e) es)

(*
 * 変数参照式の変換
 * 
 *   引　数：env: Env.t --- 名前環境
 *           ---: var   --- 変数参照式
 *   戻り値：変換後の式
 * 
 *)
and trans_var env = function
    A.VarSimple(_,s) -> P.Var s
  | A.VarField(_,e,s,o,r) -> (
      match !r with
          T.RECORD _        -> P.Select(trans_expr env e,P.Const(P.Int !o))
        | T.REGEX(T.RRCD _) -> P.Offset(P.Select(trans_expr env e,
						P.Const(P.Int 2)),
				       P.Const(P.Int !o))
        | _ -> assert false
    )
  | A.VarProj(_,e,n) -> P.Select(trans_expr env e,P.Const(P.Int n))
  | A.VarSubscr(_,e1,e2) -> P.Select(trans_expr env e1,trans_expr env e2)

(*
 * プロセス式の変換
 * 
 *   引　数：env: Env.t --- 名前環境
 *           nxt: proc  --- 逐次実行時の後続プロセス
 *           ---: proc  --- プロセス式（抽象構文）
 *   戻り値：プロセス式（π式）
 * 
 *)
and trans_proc env nxt = function
    A.ProcStop _ -> P.Stop
  | A.ProcSkip _ -> nxt
  | A.ProcSeq(_,ps) -> L.fold_right (fun p proc -> trans_proc env proc p) ps nxt
  | A.ProcChoice(_,ps) ->
      let proc = L.hd ps in
      let g = fst proc in
      let p = snd proc in
      let v_nprc = genid "nprc" in
	P.Fix([v_nprc,[],nxt],
             P.Guard(
               L.fold_left (
		 fun proc (g,p) ->
		   P.Alt(proc,trans_gproc env (trans_proc env (P.Call(P.Var v_nprc,[])) p) g)
               ) (trans_gproc env (trans_proc env (P.Call(P.Var v_nprc,[])) p) g) (L.tl ps)))
  | A.ProcMatch(i,e,ps,t) when all_regex ps -> ( 
      try 
        trans_rgxmatch env nxt (!t) e ps
      with
          Dfa.Not_exhaustive -> Error.errorAt i Error.ERR_NOT_EXHAUSTIVE )
  | A.ProcMatch(i,e,ps,_) -> (
      let v_nprc = genid "nprc" in
      let v_mexp = genid "mexp" in
      let pmat = L.map (fun (pat,pexp) -> [pat],trans_proc env (P.Call(P.Var v_nprc,[])) pexp) ps
      in
	try
	  P.Let(v_mexp, trans_expr env e,
	       P.Fix([v_nprc,[],nxt], Match.trans [v_mexp] pmat))
	with
	    Match.Not_exhaustive -> Error.errorAt i Error.ERR_NOT_EXHAUSTIVE )
  | A.ProcInput(_,e,s)  -> P.Guard(P.Recv(trans_expr env e,s,nxt))
  | A.ProcOutput(_,e1,e2) -> P.Guard(P.Send(trans_expr env e1,trans_expr env e2,nxt))
  | A.ProcRun(_,s,es) -> P.Par(P.Call(P.Var s,L.map (fun e -> trans_expr env e) es),nxt)
  | A.ProcReturn(_,e) -> P.Par(P.Retn(trans_expr env e),nxt)
  | A.ProcVar(_,s,A.DeclExpr e)  -> P.Let(s,trans_expr env e,nxt)
  | A.ProcVar(_,s,A.DeclType(_,ty)) when T.is_chan !ty -> P.New(s,nxt)
  | A.ProcVar(_,s,A.DeclType(_,ty)) -> P.Let(s,trans_type !ty,nxt)
  | A.ProcAssign(_,e1,e2) -> P.Asgn(trans_expr env e1, trans_expr env e2,nxt)
  | A.ProcCblock(_,cs,es) -> P.Cblk(cs,L.map (trans_expr env) es,nxt)

(* ガード式変換 *)
and trans_gproc env nxt = function
    A.ProcInput(_,e,s)  -> P.Recv(trans_expr env e,s,nxt)
  | A.ProcOutput(_,e1,e2) -> P.Send(trans_expr env e1,trans_expr env e2,nxt)
  | _ -> assert false

(*
 * 正規表パターンマッチ式の変換
 * 
 *   引　数  env: 
 *           nxt: proc      --- 逐次実行時の後続プロセス
 *           exp: exp       --- 値式
 *           pts: pat list  --- パタン式のリスト
 *   戻り値：プロセス式（π式）
 *)
and trans_rgxmatch env nxt ty exp pts =
  let pt_ls,pr_ls = L.split pts in
  let v_nprc = genid "nprc" in
  let idx = ref (-1) in
  let mexp,binds  = trans_rgxpat ty (P.Var v_mval) pt_ls in
    P.Fix([v_nprc,[],nxt],
         P.Let(v_mval,(trans_expr env exp),
              P.Let(v_temp,mexp, (* v_temp:(index,data) *)
                   P.Case(P.Select(P.Var v_temp,P.Const(P.Int 0)),
			 L.map2 (fun p -> function
                             None -> incr idx; Some(P.Int !idx),trans_proc env (P.Call(P.Var v_nprc,[])) p
                           | Some s -> incr idx; Some(P.Int !idx),P.Let(s,P.Select(P.Var v_temp,P.Const(P.Int 1)),
									    trans_proc env (P.Call(P.Var v_nprc,[])) p)
                         ) pr_ls binds))))
      
(* パタン式と束縛リストを返す *)
and trans_rgxpat ty v patns =
  (* 型のリストからDFAを作成する． *)
  let binds,rs = L.split (
    L.map (
      function 
          A.PatAny _          -> None,T.REXP(R.CLOS(R.oct))
        | A.PatRegex(_,s,r,t) -> Some s,!t
        | _ -> assert false
    ) patns )
  in
    match ty with
	T.STRING -> P.Prim(P.Match,[v;P.Const(P.Int (Dfa.generate rs))]),binds
      | T.REGEX r -> P.Prim(P.Match,[v;P.Const(P.Int (Dfa.generate2 rs r))]),binds
      | _ -> assert false
	  
(*
 * 型に対する初期化式を生成する
 * 
 *   引　数： ---: typ   --- 抽象構文の型式
 *   戻り値：初期化式
 * 
 *)
and trans_type = function
    T.UNIT   -> P.Const(P.Unit)
  | T.BOOL   -> P.Const(P.Bool false)
  | T.INT    -> P.Const(P.Int 0)
  | T.STRING -> P.Const(P.String "")
  | T.ARRAY(t,n) -> P.Record (make_list (trans_type t) n)
  | T.RECORD ts  -> P.Record (L.map (fun (_,t) -> trans_type t) ts)
  | T.TUPLE ts   -> P.Record (L.map trans_type ts)
  | T.VARIANT ((s,t)::_) -> P.Record ([P.Const(P.Int(Symbol.id s));trans_type t])
  | T.REGEX r    -> con_regex r
  | _ -> assert false

(*
 * π式へ変換する関数
 *
 *   引  数：env : Env.t    --- 名前環境
 *           defs: toplevel --- トップレベル定義のリスト
 *   戻り値：変換後のπ式
 * 
 *)
let trans env defs =
  (* トップレベル定義の変換を行う *)
  L.fold_right (
    fun def proc -> (
      match def with
          A.DefVar(_,s,A.DeclExpr ex) -> P.Let(s,trans_expr env ex,proc)
        | A.DefVar(_,s,A.DeclType(_,ty)) when T.is_chan !ty -> P.New(s,proc)
        | A.DefVar(_,s,A.DeclType(_,ty)) -> P.Let(s,trans_type !ty,proc)
        | A.DefType types -> proc
        | A.DefProc procs ->
            P.Fix(L.map (
                  fun (_,s,ds,_,p) ->
                    s,L.map fst ds,
                    trans_proc env P.Stop p
                ) procs,proc)
    )
  ) defs (P.Call(P.Var(Symbol.symbol "Main"),[]))


