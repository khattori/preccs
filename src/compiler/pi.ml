(**
   中間表現モジュール
   
   概要：π計算形式への変換を行う

   @author Hattori Kenta
   @version $Id: pi.ml,v 1.6 2006/07/27 00:07:18 hattori Exp $
*)
module A = Syntax
module T = Types
module R = Regex

type var = Symbol.t

let rec make_list a = function
    0 -> []
  | n when n < 0 -> assert false
  | n -> a::make_list a (n-1)

(* プロセス式定義 *)
type proc =
    End
  | Guard of guard
  | New   of var  * proc                   (* チャネル生成   *)
  | Let   of var  * exp * proc             (* 変数束縛       *)
  | Par   of proc * proc                   (* 並行合成       *)
  | Case  of exp  * proc list              (* 分岐実行       *)
  | Call  of var  * exp list               (* プロセス呼出し *)
  | Set   of exp  * exp * proc             (* 代入           *)
  | Cblk  of string list * exp list * proc (* Cコード        *)
  | Fix   of bind list * proc              (* プロセス定義   *)
and bind = var * var list * proc

(* ガード式定義 *)
and guard =
    Send of exp * exp * proc   (* 送信 *)
  | Recv of exp * var * proc   (* 受信 *)
  | Alt  of guard * guard      (* 選択実行 *)

(* 式定義 *)
and exp =
    Var    of var
  | Unit
  | Bool   of bool
  | Int    of int
  | Cint   of int
  | String of string
  | If     of exp * exp * exp
  | Record of exp list          (* レコード値         *)
  | Rexrcd of string * exp list (* 正規表現レコード   *)
  | Select of exp * exp         (* フィールド選択     *)
  | Offset of exp * exp         (* レコードオフセット *)
  | Prim   of prim * exp list

and prim =
    Add | Sub | Mul | Div | Mod
  | Eq  | Neq | Eqs | Lt  | Leq | Gt  | Geq
  | And | Or  | Cat
  | Neg | Not
  | Match      (* パターンマッチ *)

let v_next = Symbol.symbol("next")
let v_junk = Symbol.symbol("junk")
let v_temp = Symbol.symbol("temp")
let v_mval = Symbol.symbol("mval")

(** 識別子生成 *)
let counter = ref 0
let genid s =
  incr counter;
  Symbol.symbol (Printf.sprintf "%s%d" s !counter)

let trans_const = function
    A.ConUnit _    -> Unit
  | A.ConBool(_,b) -> Bool b
  | A.ConInt(_,n)  -> Int n
  | A.ConStr(_,s)  -> String s
let trans_bop = function
    A.BopAdd -> Add | A.BopSub -> Sub
  | A.BopMul -> Mul | A.BopDiv -> Div
  | A.BopMod -> Mod
  | A.BopEq  -> Eq  | A.BopNeq -> Neq
  | A.BopLt  -> Lt  | A.BopLeq -> Leq
  | A.BopGt  -> Gt  | A.BopGeq -> Geq
  | A.BopAnd -> And | A.BopOr  -> Or
  | A.BopCat -> Cat
let trans_mop = function A.MopNeg -> Neg | A.MopNot -> Not

(*
 * 正規表現型のデフォルト文字列を取得
 *)
let rec defstr_of_regex = function
    T.REXP re -> R.to_string re
  | T.RARR(r,0) -> ""
  | T.RARR(r,1) -> defstr_of_regex r
  | T.RARR(r,n) -> (defstr_of_regex r)^(defstr_of_regex (T.RARR(r,n-1)))
  | T.RITR(r,_) -> ""
  | T.RRCD fs -> List.fold_left (fun s (_,r) -> s ^ defstr_of_regex r) "" fs

let con_regex =
  let rec trav = function
      T.REXP re -> Cint (R.size re)
    | T.RARR(r,n) -> Cint ((R.size (T.regexify r)) * n)
    | T.RITR _ -> Cint 0
    | T.RRCD fs -> Record (List.map (fun (_,r) -> trav r)  fs)
  in
    function
        T.REXP re -> String (R.to_string re)
      | T.RARR(r,0) -> String ""
      | T.RARR(r,1) -> String (defstr_of_regex r)
      | T.RARR(r,n) -> String ((defstr_of_regex r)^(defstr_of_regex (T.RARR(r,n-1))))
      | T.RITR(r,_) -> String ""
      | T.RRCD fs ->
          let s = List.fold_left (fun s (_,r) -> s ^ defstr_of_regex r) "" fs in
            Rexrcd(s, List.map (fun (_,r) -> trav r) fs)

let is_regex_pattern patns =
  List.exists (
    function 
        A.PatAny _ | A.PatConst(A.ConStr _) | A.PatVar _ -> false
      | A.PatRegex _ -> true
      | _ -> assert false
  ) patns

(*
 * 値式の変換
 * 
 *   引　数：env: Env.t --- 名前環境
 *           ---: exp   --- 値式
 *   戻り値：変換後の値式
 * 
 *)
let rec trans_expr env = function
    A.ExpConst c   -> trans_const c
  | A.ExpVar v     -> trans_var env v
  | A.ExpRecord fs -> Record(List.map (fun (_,_,e) -> trans_expr env e) fs)
  | A.ExpTuple es  -> Record(List.map (fun e -> trans_expr env e) es)
  | A.ExpBinop(_,bop,e1,e2) ->
      Prim(trans_bop bop,[trans_expr env e1; trans_expr env e2])
  | A.ExpMonop(_,mop,e) -> Prim(trans_mop mop,[trans_expr env e])
  
(*
 * 変数参照式の変換
 * 
 *   引　数：env: Env.t --- 名前環境
 *           ---: var   --- 変数参照式
 *   戻り値：変換後の式
 * 
 *)
and trans_var env = function
    A.VarSimple(_,s) -> Var s
  | A.VarField(_,v,s,o,r) -> (
      match !r with
          T.RECORD _        -> Select(trans_var env v,Int !o)
        | T.REGEX(T.RRCD _) -> Offset(Select(trans_var env v,Int 2),Int !o)
        | _ -> assert false
    )
  | A.VarProj(_,v,n) -> Select(trans_var env v, Int n)
  | A.VarSubscr(_,v,e) -> Select(trans_var env v,trans_expr env e)

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
    A.ProcStop _ -> End
  | A.ProcSkip _ -> nxt
  | A.ProcSeq(_,ps) ->
      List.fold_right (
        fun p proc ->
          trans_proc env (New(v_next,Par(Guard(Send(Var v_next,Unit,End)),
                                         Guard(Recv(Var v_next,v_junk,proc))))) p
      ) ps nxt

  | A.ProcChoice(_,ps) ->
      let proc = List.hd ps in
      let g = fst proc in
      let p = snd proc in
      let v_nprc = genid "nprc" in
	Fix([v_nprc,[],nxt],
           Guard(
             List.fold_left (
               fun proc (g,p) ->
		 Alt(proc, trans_gproc env (trans_proc env (Call(v_nprc,[])) p) g)
             ) (trans_gproc env (trans_proc env (Call(v_nprc,[])) p) g) (List.tl ps)))
  | A.ProcMatch(i,e,ps,t) -> (
      try 
        trans_match env nxt (!t) e ps
      with
          Dfa.Not_exhaustive -> Error.errorAt i Error.ERR_NOT_EXHAUSTIVE )
  | A.ProcInput(_,c,s)  -> Guard(Recv(trans_var env c,s,nxt))
  | A.ProcOutput(_,c,e) -> Guard(Send(trans_var env c,trans_expr env e,nxt))
  | A.ProcRun(_,s,es) -> Par(Call(s,List.map (fun e -> trans_expr env e) es),nxt)
  | A.ProcVar(_,s,A.DeclExpr e)  -> Let(s,trans_expr env e,nxt)
  | A.ProcVar(_,s,A.DeclType(_,ty)) when T.is_chan !ty -> New(s,nxt)
  | A.ProcVar(_,s,A.DeclType(_,ty)) -> Let(s,trans_type !ty,nxt)
  | A.ProcAsign(_,v,e) -> Set(trans_var env v,trans_expr env e,nxt)
  | A.ProcCblock(_,cs,vs) -> Cblk(cs,List.map (trans_var env) vs,nxt)

(* ガード式変換 *)
and trans_gproc env nxt = function
    A.ProcInput(_,c,s)  -> Recv(trans_var env c,s,nxt)
  | A.ProcOutput(_,c,e) -> Send(trans_var env c,trans_expr env e,nxt)
  | _ -> assert false
          
(*
 * パタンマッチ式の変換
 * 
 *   引　数：env: Env.t     --- 名前環境
 *           nxt: proc      --- 逐次実行時の後続プロセス
 *           ty : Types.t   --- 値式の型
 *           exp: exp       --- 値式
 *           pts: pat list  --- パタン式のリスト
 *   戻り値：プロセス式（π式）
 *)
and trans_match env nxt ty exp pts =
  let pt_ls,pr_ls = List.split pts in
  let v_nprc = genid "nprc" in
    Fix([v_nprc,[],nxt],
        match ty with 
            T.STRING | T.REGEX _ when is_regex_pattern pt_ls ->
              let mexp,binds  = trans_patns ty (Var v_mval) pt_ls in
                Let(v_mval,(trans_expr env exp),
                    Let(v_temp,mexp, (* v_temp:(index,data) *)
                        Case(Select(Var v_temp,Int 0),
                             List.map2 (
                               fun p -> function
                                   None   -> trans_proc env (Call(v_nprc,[])) p
                                 | Some s -> Let(s,Select(Var v_temp,Int 1),
                                                 trans_proc env (Call(v_nprc,[])) p)
                             ) pr_ls binds)))
          | _ -> let mexp = trans_case ty (Var v_mval) pt_ls in
              Let(v_mval,(trans_expr env exp),
                  Let(v_temp,mexp, (* v_temp:index *)
                      Case(Var v_temp,
                           List.map (fun p ->
                                       trans_proc env (Call(v_nprc,[])) p) pr_ls)))
       )
      
(* パタン式と束縛リストを返す *)
and trans_patns ty v patns =
  (* 型のリストからDFAを作成する． *)
  let binds,rs = List.split (
    List.map (
      function 
          A.PatAny _                -> None,T.REXP(R.CLOS(R.oct))
        | A.PatConst(A.ConStr(_,s)) -> None,T.REXP(R.of_string s)
        | A.PatRegex(_,s,_,t)       -> Some s,!t
        | _ -> assert false
    ) patns ) in
    match ty with
        T.STRING  -> Prim(Match,[v;Int (Dfa.generate rs)]),binds
      | T.REGEX r -> Prim(Match,[v;Int (Dfa.generate2 rs r)]),binds
      | _ -> assert false

and trans_case ty v patns =
  (* ケース文処理 *)
  let len = List.length patns in
    fst (
      List.fold_right (
        fun pat (e,i) -> match pat with
            A.PatAny _ -> Int i,i-1
          | A.PatConst(A.ConBool(_,b)) ->
              If(Prim(Eq,[v;Bool b]),Int i,e),i-1
          | A.PatConst(A.ConInt(_,n)) ->
              If(Prim(Eq,[v;Int n]),Int i,e),i-1
          | A.PatConst(A.ConStr(_,s)) ->
              If(Prim(Eqs,[v;String s]),Int i,e),i-1
	  | A.PatVar(A.VarSimple(_,s)) when T.is_string ty ->
	      If(Prim(Eqs,[v;Var s]),Int i,e),i-1
	  | A.PatVar(A.VarSimple(_,s)) ->
	      If(Prim(Eq,[v;Var s]),Int i,e),i-1
          | _ -> assert false
      ) patns (Int len,len-1)
    )

(*
 * 型に対する初期化式を生成する
 * 
 *   引　数：env: Env.t --- 名前環境
 *           ---: typ   --- 抽象構文の型式
 *   戻り値：初期化式
 * 
 *)
and trans_type = function
    T.BOOL   -> Bool false
  | T.INT    -> Int 0
  | T.STRING -> String ""
  | T.ARRAY(t,n) -> Record (make_list (trans_type t) n)
  | T.RECORD ts  -> Record (List.map (fun (_,t) -> trans_type t) ts)
  | T.REGEX r    -> con_regex r
  | _ -> assert false

(*
 * π式へ変換する関数
 *
 *   引  数：defs: toplevel --- トップレベル定義のリスト
 *           env : Env.t    --- 名前環境
 *   戻り値：変換後のπ式
 * 
 *)
let trans env defs =
  (* トップレベル定義の変換を行う *)
  List.fold_right (
    fun def proc -> (
      match def with
          A.DefVar(_,s,A.DeclExpr ex) -> Let(s,trans_expr env ex,proc)
        | A.DefVar(_,s,A.DeclType(_,ty)) when T.is_chan !ty -> New(s,proc)
        | A.DefVar(_,s,A.DeclType(_,ty)) -> Let(s,trans_type !ty,proc)
        | A.DefType types -> proc
        | A.DefProc procs ->
            Fix(List.map (
                  fun (_,s,ds,p) ->
                    s,List.map fst ds,
                    trans_proc env End p
                ) procs,proc)
    )
  ) defs (Call(Symbol.symbol "Main",[]))

(*
 * 最適化処理：自明な通信処理を削除する
 * 
 *   引　数：--- : pexp --- π式
 *   戻り値：最適化後のπ式
 * 
 *   (ch!E.$|ch?x.P) ===> let x=E.P
 * 
 *)
let rec reducComm = function
    Guard g  -> Guard (reducCommG g)
  | New(v,p) -> New(v,reducComm p)
  | Let(v,e,p) -> Let(v,e,reducComm p)
  | Par(Guard(Send(Var(c1),e,End)),Guard(Recv(Var(c2),x,p)))
  | Par(Guard(Recv(Var(c2),x,p)),Guard(Send(Var(c1),e,End)))
      when Symbol.equal c1 c2
        -> Let(x,e,reducComm p)
  | Par(p1,p2) -> Par(reducComm p1,reducComm p2)
  | Case(e,ps) -> Case(e,List.map reducComm ps)
  | Cblk(cs,es,p) -> Cblk(cs,es,reducComm p)
  | Fix(bs,p) -> Fix(List.map (fun (v,vs,p) -> v,vs,reducComm p) bs,reducComm p)
  | _ as p -> p

and reducCommG = function
    Send(e1,e2,p) -> Send(e1,e2,reducComm p)
  | Recv(e,v,p)   -> Recv(e,v,reducComm p)
  | Alt(g1,g2)    -> Alt(reducCommG g1,reducCommG g2)

let rec unused v = function
    Guard g  -> unusedG v g
  | New(v',_) when Symbol.equal v v' -> true
  | New(_,p) -> unused v p
  | Let(v',_,_) when Symbol.equal v v' -> true
  | Let(_,e,p) -> unused v p && unusedE v e
  | Par(p1,p2) -> unused v p1 && unused v p2
  | Case(e,ps) -> unusedE v e && List.for_all (unused v) ps
  | Call(v',es) -> not (Symbol.equal v v') && List.for_all (unusedE v) es
  | Cblk(cs,es,p) -> unused v p && List.for_all (unusedE v) es
  | Set(e1,e2,p) -> unusedE v e1 && unusedE v e2 && unused v p
  | Fix(bs,p) ->
      unused v p &&
        List.for_all (
          fun (v',vs,p') ->
            (* Symbol.equal v v' || *)
            List.exists (Symbol.equal v) vs   (* 変数vが隠蔽される *)
            || unused v p'
        ) bs
  | End -> true

and unusedG v = function
    Send(e1,e2,p) -> unusedE v e1 && unusedE v e2 && unused v p
  | Recv(e,v',p) when Symbol.equal v v' -> unusedE v e
  | Recv(e,_,p) -> unusedE v e && unused v p
  | Alt(g1,g2)  -> unusedG v g1 && unusedG v g2
and unusedE v = function
    Var v' -> not (Symbol.equal v v')
  | Unit | Bool _ | Int _ | Cint _ | String _ -> true
  | If(e1,e2,e3) -> unusedE v e1 && unusedE v e2 && unusedE v e3
  | Record es | Rexrcd (_,es) -> List.for_all (unusedE v) es
  | Select(e1,e2) | Offset(e1,e2) -> unusedE v e1 && unusedE v e2
  | Prim(_,es) -> List.for_all (unusedE v) es


let isNoSideEffect e = true
(*
 * 最適化処理：不要な変数を削除
 * 
 *   引　数：--- : pexp --- π式
 *   戻り値：最適化後のπ式
 * 
 *   new junk.P ===> P
 *   let junk=E.P ===> P
 *            ^^^副作用が無いこと
 *   ※ 不要な引数の除去は行わない
 * 
 *)
let rec removeUnused = function
    Guard g  -> Guard (removeUnusedG g)
  | New(v,p) when unused v p   -> removeUnused p
  | New(v,p) -> New(v,removeUnused p)
  | Let(v,e,p) when unused v p && isNoSideEffect e -> removeUnused p
  | Let(v,e,p) -> Let(v,e,removeUnused p)
  | Par(p1,p2) -> Par(removeUnused p1,removeUnused p2)
  | Case(e,ps) -> Case(e,List.map removeUnused ps)
  | Cblk(cs,es,p) -> Cblk(cs,es,removeUnused p)
  | Fix(bs,p) ->
      Fix(List.map (fun (v,vs,p) -> v,vs,removeUnused p) bs,removeUnused p)
  | _ as p -> p

and removeUnusedG = function
    Send(e1,e2,p) -> Send(e1,e2,removeUnused p)
  | Recv(e,v,p)   -> Recv(e,v,removeUnused p)
  | Alt(g1,g2)    -> Alt(removeUnusedG g1,removeUnusedG g2)


(*
 * 最適化処理：自明な並行処理を削除する
 * 
 *   引　数：--- : pexp --- π式
 *   戻り値：最適化後のπ式
 * 
 *   ( P | $ ) ===> P
 * 
 *)
let rec reducPar = function
    Guard g  -> Guard(reducParG g)
  | New(v,p) -> New(v,reducPar p)
  | Par(p1,p2) ->
      let p1' = reducPar p1 in
      let p2' = reducPar p2 in
        if p1' == End then p2'
        else if p2' == End then p1'
        else Par(p1',p2')
  | Case(e,ps)    -> Case(e,List.map reducPar ps)
  | Cblk(cs,es,p) -> Cblk(cs,es,reducPar p)
  | Fix(bs,p)     -> Fix(List.map (fun (v,vs,p) -> v,vs,reducPar p) bs,reducPar p)
  | _ as p -> p
and reducParG = function
    Send(e1,e2,p) -> Send(e1,e2,reducPar p)
  | Recv(e,v,p)   -> Recv(e,v,reducPar p)
  | Alt(g1,g2)    -> Alt(reducParG g1,reducParG g2)

let string_of_prim = function
    Add -> "Add" | Sub -> "Sub" | Mul -> "Mul" | Div -> "Div" | Mod -> "Mod"
  | Eq  -> "Eq"  | Neq -> "Neq" | Eqs -> "Eqs"
  | Lt  -> "Lt"  | Leq -> "Leq" | Gt  -> "Gt"  | Geq -> "Geq"
  | And -> "And" | Or  -> "Or"  | Cat -> "Cat"
  | Neg -> "Neg" | Not -> "Not"
  | Match -> "Match"      (* パターンマッチ *)

let rec show_proc = function
    End        -> Printf.printf "End"
  | Guard g    ->
      Printf.printf "Guard(";
      show_guard g; Printf.printf ")"
  | New(v,p)   ->
      Printf.printf "New(%s," (Symbol.name v);
      show_proc p; Printf.printf ")"
  | Let(v,e,p) ->
      Printf.printf "Let(%s," (Symbol.name v);
      show_exp e;  Printf.printf ",";
      show_proc p; Printf.printf ")"
  | Par(p1,p2) ->
      Printf.printf "Par(";
      show_proc p1; Printf.printf ",";
      show_proc p2; Printf.printf ")"
  | Case(e,ps) ->
      Printf.printf "Case(";
      show_exp e; Printf.printf ",[";
      List.iter (fun p -> show_proc p;Printf.printf ";") ps;
      Printf.printf "])"
  | Call(v,es) ->
      Printf.printf "Call(%s,[" (Symbol.name v);
      List.iter (fun e -> show_exp e;Printf.printf ";") es;
      Printf.printf "])"
  | Cblk(cs,vs,p) ->
      Printf.printf "Cblk(";
      List.iter print_string cs;
      Printf.printf ",[";
      List.iter (fun v -> show_exp v; Printf.printf ";") vs;
      Printf.printf "],";
      show_proc p; Printf.printf ")"
  | Set(e1,e2,p) ->
      Printf.printf "Set(";
      show_exp e1; Printf.printf ",";
      show_exp e2; Printf.printf ",";
      show_proc p; Printf.printf ")"
  | Fix(bs,p) ->
      Printf.printf "Fix([";
      List.iter (fun (v,vs,p) ->
                   Printf.printf "(%s,[" (Symbol.name v);
                   List.iter (fun v -> Printf.printf "%s;" (Symbol.name v)) vs;
                   Printf.printf "],";
                   show_proc p; Printf.printf ");") bs;
      Printf.printf "],";
      show_proc p; Printf.printf ")"

and show_guard = function
    Send(e1,e2,p) ->
      Printf.printf "Send(";
      show_exp e1; Printf.printf ",";
      show_exp e2; Printf.printf ",";
      show_proc p; Printf.printf ")"
  | Recv(e,v,p) ->
      Printf.printf "Recv(";
      show_exp e; Printf.printf ",%s," (Symbol.name v);
      show_proc p; Printf.printf ")"
  | Alt(g1,g2) ->
      Printf.printf "Alt(";
      show_guard g1; Printf.printf ",";
      show_guard g2; Printf.printf ")"

and show_exp = function
    Var v    -> Printf.printf "Var(%s)" (Symbol.name v)
  | Unit     -> Printf.printf "Unit"
  | Bool b   -> Printf.printf "Bool(%b)" b
  | Int i    -> Printf.printf "Int(%d)" i
  | Cint i   -> Printf.printf "Cint(%d)" i
  | String s -> Printf.printf "String(%S)" s
  | If(b,e1,e2) ->
      Printf.printf "If(";
      show_exp b; Printf.printf ",";
      show_exp e1; Printf.printf ",";
      show_exp e2; Printf.printf ")"
  | Record es ->
      Printf.printf "Record([";
      List.iter (fun e -> show_exp e; Printf.printf ";") es;
      Printf.printf "])"
  | Rexrcd(s,es) ->
      Printf.printf "Rexrcd(%S,[" s;
      List.iter (fun e -> show_exp e; Printf.printf ";") es;
      Printf.printf "])"
  | Select(e1,e2) ->
      Printf.printf "Select(";
      show_exp e1; Printf.printf ",";
      show_exp e2; Printf.printf ")"
  | Offset(e1,e2) ->
      Printf.printf "Offset(";
      show_exp e1; Printf.printf ",";
      show_exp e2; Printf.printf ")"
  | Prim(p,es) ->
      Printf.printf "Prim(%s,[" (string_of_prim p);
      List.iter (fun e -> show_exp e; Printf.printf ";") es;
      Printf.printf "])"


