(**
   中間表現モジュール
   
   概要：π計算形式への変換を行う

   @author Hattori Kenta
   @version $Id: pi.ml,v 1.6 2006/07/27 00:07:18 hattori Exp $
*)
module A = Syntax
module T = Types
module R = Regex
module L = List

type var = Symbol.t


(* プロセス式定義 *)
type proc =
    Stop
  | Guard of guard
  | New   of var  * proc                       (* チャネル生成   *)
  | Let   of var  * exp * proc                 (* 変数束縛       *)
  | Asgn  of exp  * exp	* proc		       (* 代入           *)
  | Par   of proc * proc                       (* 並行合成       *)
  | Case  of exp  * (const option * proc) list (* 分岐実行       *)
  | Exit                                       (* 脱出           *)
  | Catch of proc * proc                       (* 捕捉           *)
  | Cblk  of string list * exp list * proc     (* Cコード        *)
  | Call  of exp  * exp list                   (* プロセス呼出し *)
  | Retn  of exp                               (* プロセス戻り   *)
  | Fix   of bind list * proc                  (* プロセス定義   *)
and bind = var * var list * proc

(* ガード式定義 *)
and guard =
    Send of exp * exp * proc   (* 送信 *)
  | Recv of exp * var * proc   (* 受信 *)
  | Alt  of guard * guard      (* 選択実行 *)

(* 式定義 *)
and exp =
    Var     of var
  | Const   of const
  | Read    of exp               (* チャネル値読込     *)
  | Apply   of exp * exp list    (* プロセス適用       *)
  | Record  of exp list          (* レコード値         *)
  | Rexrcd  of string * exp list (* 正規表現レコード   *)
  | Select  of exp * exp         (* フィールド選択     *)
  | Offset  of exp * exp         (* レコードオフセット *)
  | Prim    of prim * exp list

(* 定数定義 *)
and const =
    Unit
  | Bool    of bool
  | Int     of int
  | String  of string

and prim =
    Add | Sub | Mul | Div | Mod
  | Eq  | Neq | Eqs | Lt  | Leq | Gt  | Geq
  | And | Or  | Cat
  | Neg | Not
  | Match      (* パターンマッチ *)

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
  | Par(Guard(Send(Var(c1),e,Stop)),Guard(Recv(Var(c2),x,p)))
  | Par(Guard(Recv(Var(c2),x,p)),Guard(Send(Var(c1),e,Stop)))
      when Symbol.equal c1 c2
        -> Let(x,e,reducComm p)
  | Par(p1,p2) -> Par(reducComm p1,reducComm p2)
  | Case(e,cps) -> let cs,ps = L.split cps in Case(e,L.combine cs (L.map reducComm ps))
  | Cblk(cs,es,p) -> Cblk(cs,es,reducComm p)
  | Catch(p1,p2) -> Catch(reducComm p1,reducComm p2) 
  | Fix(bs,p) -> Fix(L.map (fun (v,vs,p) -> v,vs,reducComm p) bs,reducComm p)
  | p -> p

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
  | Asgn(e1,e2,p) -> unusedE v e1 && unusedE v e2 && unused v p
  | Par(p1,p2) -> unused v p1 && unused v p2
  | Case(e,cps) -> let _,ps = L.split cps in unusedE v e && L.for_all (unused v) ps
  | Call(e,es) -> unusedE v e && L.for_all (unusedE v) es
  | Cblk(cs,es,p) -> unused v p && L.for_all (unusedE v) es
  | Fix(bs,p) ->
      unused v p &&
        L.for_all (
          fun (v',vs,p') ->
            (* Symbol.equal v v' || *)
            L.exists (Symbol.equal v) vs   (* 変数vが隠蔽される *)
            || unused v p'
        ) bs
  | Catch(p1,p2) -> unused v p1 && unused v p2
  | Retn e -> unusedE v e
  | Stop | Exit -> true


and unusedG v = function
    Send(e1,e2,p) -> unusedE v e1 && unusedE v e2 && unused v p
  | Recv(e,v',p) when Symbol.equal v v' -> unusedE v e
  | Recv(e,_,p) -> unusedE v e && unused v p
  | Alt(g1,g2)  -> unusedG v g1 && unusedG v g2
and unusedE v = function
    Var v' -> not (Symbol.equal v v')
  | Const(Unit) | Const(Bool _) | Const(Int _) | Const(String _) -> true
  | Record es | Rexrcd (_,es) -> L.for_all (unusedE v) es
  | Select(e1,e2) | Offset(e1,e2) -> unusedE v e1 && unusedE v e2
  | Prim(_,es) -> L.for_all (unusedE v) es
  | Apply(e,es) -> unusedE v e && L.for_all (unusedE v) es
  | Read e -> unusedE v e

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
  | Case(e,cps) -> let cs,ps = L.split cps in Case(e,L.combine cs (L.map removeUnused ps))
  | Cblk(cs,es,p) -> Cblk(cs,es,removeUnused p)
  | Catch(p1,p2) -> Catch(removeUnused p1,removeUnused p2)
  | Fix(bs,p) -> Fix(L.map (fun (v,vs,p) -> v,vs,removeUnused p) bs,removeUnused p)
  | p -> p

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
        if p1' == Stop then p2'
        else if p2' == Stop then p1'
        else Par(p1',p2')
  | Case(e,cps) -> let cs,ps = L.split cps in Case(e,L.combine cs (L.map reducPar ps))
  | Cblk(cs,es,p) -> Cblk(cs,es,reducPar p)
  | Fix(bs,p)     -> Fix(L.map (fun (v,vs,p) -> v,vs,reducPar p) bs,reducPar p)
  | p -> p
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
    Stop        -> Printf.printf "Stop"
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
  | Asgn(e1,e2,p) ->
      Printf.printf "Asgn(";
      show_exp e1; Printf.printf ","; show_exp e2; Printf.printf ","; show_proc p;
      Printf.printf ")"
  | Par(p1,p2) ->
      Printf.printf "Par(";
      show_proc p1; Printf.printf ",";
      show_proc p2; Printf.printf ")"
  | Case(e,cps) ->
      Printf.printf "Case(";
      show_exp e; Printf.printf ",[";
      L.iter (fun (c,p) ->
	match c with
	    None -> Printf.printf "_ ->"; show_proc p;Printf.printf ";"
	  | Some c -> show_const c; Printf.printf "->"; show_proc p;Printf.printf ";") cps;
      Printf.printf "])"
  | Call(e,es) ->
      Printf.printf "Call(";
      show_exp e; Printf.printf ",[";
      L.iter (fun e -> show_exp e;Printf.printf ";") es;
      Printf.printf "])"
  | Cblk(cs,vs,p) ->
      Printf.printf "Cblk(";
      L.iter print_string cs;
      Printf.printf ",[";
      L.iter (fun v -> show_exp v; Printf.printf ";") vs;
      Printf.printf "],";
      show_proc p; Printf.printf ")"
  | Fix(bs,p) ->
      Printf.printf "Fix([";
      L.iter (fun (v,vs,p) ->
                   Printf.printf "(%s,[" (Symbol.name v);
                   L.iter (fun v -> Printf.printf "%s;" (Symbol.name v)) vs;
                   Printf.printf "],";
                   show_proc p; Printf.printf ");") bs;
      Printf.printf "],";
      show_proc p; Printf.printf ")"
  | Retn e ->
      Printf.printf "Retn("; show_exp e; Printf.printf ")"
  | Exit -> Printf.printf "Exit"
  | Catch(p1,p2) ->
      Printf.printf "Catch(";
      show_proc p1; Printf.printf ","; show_proc p2;
      Printf.printf ")"

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
  | Const c  -> show_const c
  | Record es ->
      Printf.printf "Record([";
      L.iter (fun e -> show_exp e; Printf.printf ";") es;
      Printf.printf "])"
  | Rexrcd(s,es) ->
      Printf.printf "Rexrcd(%S,[" s;
      L.iter (fun e -> show_exp e; Printf.printf ";") es;
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
      L.iter (fun e -> show_exp e; Printf.printf ";") es;
      Printf.printf "])"
  | Apply(e,es) ->
      Printf.printf "Apply(";
      show_exp e; Printf.printf ",[";
      L.iter (fun e -> show_exp e; Printf.printf ";") es;
      Printf.printf "])"
  | Read e ->
      Printf.printf "Read(";
      show_exp e;
      Printf.printf ")"
and show_const = function
    Unit     -> Printf.printf "Unit"
  | Bool b   -> Printf.printf "Bool(%b)" b
  | Int i    -> Printf.printf "Int(%d)" i
  | String s -> Printf.printf "String(%S)" s


