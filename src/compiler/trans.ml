(**
   中間言語変換モジュール
   
   概要：π中間言語からCPS形式への変換を行う

   @author Hattori Kenta
   @version $Id: trans.ml,v 1.6 2006/07/27 00:07:18 hattori Exp $
*)

module P = Pi
module T = Types
module C = Cps
module Sm = Map.Make(Symbol)


let genid = Symbol.genid

let trans_prim = function
    P.Add -> C.Add | P.Sub -> C.Sub
  | P.Mul -> C.Mul | P.Div -> C.Div
  | P.Mod -> C.Mod
  | P.Eq  -> C.Eq  | P.Neq -> C.Neq | P.Eqs -> C.Eqs
  | P.Lt  -> C.Lt  | P.Leq -> C.Leq
  | P.Gt  -> C.Gt  | P.Geq -> C.Geq
  | P.And -> C.And | P.Or  -> C.Or
  | P.Cat -> C.Cat
  | P.Neg -> C.Neg | P.Not -> C.Not
  | P.Match -> C.Match

let rec expand_exp o = function
    P.Record es ->
      let ls,tl = expand_exp_list o es in [P.Const(P.Int o);P.Const(P.Unit);P.Record(ls @ [P.Const(P.Int tl)])],tl
  | P.Const(P.Int i) -> [P.Const(P.Int o);P.Const(P.Unit);P.Const(P.Unit)],o+i
  | _ -> assert false
and expand_exp_list o =
  List.fold_left (fun (ls,j) e ->
                    let ls',j' = expand_exp j e
                    in ls@ls',j'
                 ) ([],o)
  
let trans_const = function
    P.Unit     -> C.Cint 0
  | P.Bool b   -> C.Bool b
  | P.Int i    -> C.Int i
  | P.String s -> C.String s

(*
 * 値式の変換
 * 
 *   引　数：env : Env.t         --- 名前環境
 *           ctxt: C.value->cexp --- 評価文脈
 *           --- : Pi.exp        --- 値式
 *   戻り値：変換後の値式
 * 
 *)
let rec trans_exp env ctxt = function
    P.Var v   -> ( try ctxt (Sm.find v env) with Not_found -> ctxt (C.Var v) )
  | P.Const c -> ctxt (C.Const(trans_const c))
  | P.Record [] -> ctxt (C.Const(C.Int 0))
  | P.Record es ->
      let t = genid "t" in
        trans_exp_list env (
          fun vs -> C.Prim(C.Record,vs,[t],[ctxt (C.Var t)]),ref []
        ) es
  | P.Rexrcd(s,es) ->
      let t1 = genid "t" in
      let t2 = genid "t" in
        C.Prim(C.Set,[C.Const(C.String s)],[t1],[
                 trans_exp env
                   (fun x ->
                      C.Prim(C.Rexrcd,[C.Var t1;x],[t2],[ctxt (C.Var t2)]),ref [])
                   (let ls,tl = expand_exp_list 0 es in
                      P.Record (ls @ [P.Const(P.Int tl)]))
               ]),ref []
  | P.Select(e1,e2) ->
      let t = genid "t" in
        trans_exp_list env (
          fun vs -> C.Prim(C.Select,vs,[t],[ctxt (C.Var t)]),ref []
        ) [e1;e2]
  | P.Offset(e1,e2) ->
      let t = genid "t" in
        trans_exp_list env (
          fun vs -> C.Prim(C.Offset,vs,[t],[ctxt (C.Var t)]),ref []
        ) [e1;e2]
  | P.Apply(e,es) ->
      let k = genid "k" in
      let r = genid "r" in
	trans_exp env (fun x ->
          trans_exp_list env (
            fun xs ->
              C.Fix([k,[r],ctxt (C.Var r)],
                   (C.App(x,C.Var k::xs),ref [])),ref []
          ) es) e
  | P.Read(e) ->
      let k = genid "k" in
      let r = genid "r" in
        trans_exp env (fun x ->
          C.Fix([k,[r],ctxt (C.Var r)],
               (C.App(C.Label(Symbol.symbol "recv"),
		     [C.Label(Symbol.symbol "disp");x;C.Var k;C.Const(C.Cint 0)]),ref [])),ref []) e
  | P.Prim(op,es) ->
      let t = genid "t" in
        trans_exp_list env (
          fun vs -> C.Prim(trans_prim op,vs,[t],[ctxt (C.Var t)]),ref []
        ) es

(* 値式の変換(リスト版) *)
and trans_exp_list env ctxt vs =
  let rec g xs = function
      []    -> ctxt (List.rev xs)
    | e::es -> trans_exp env (fun x -> g (x::xs) es) e
  in
    g [] vs

(*
 * プロセス式の変換
 * 
 *   引　数：env : Env.t           --- 名前環境
 *           ctxt: C.Value -> cexp --- 継続処理
 *           --- : Pi.exp          --- π式
 *   戻り値：変換後のCPS式
 * 
 *)
and trans_proc env ctxt cexp = function
    P.Stop -> C.disp
  | P.Exit -> cexp
  | P.Catch(p1,p2) ->
      let exn = genid "exn" in
        C.Fix([exn,[],trans_proc env ctxt cexp p2],
          trans_proc env ctxt (C.App(C.Var exn,[]),ref []) p1),ref []
  | P.Retn e ->
      trans_exp env (fun x -> ctxt x) e
  | P.Guard g    -> trans_guard env ctxt cexp g
  | P.New(v,p)   ->
      let c = genid "c" in
        C.Prim(C.New,[],[c],[trans_proc (Sm.add v (C.Var c) env) ctxt cexp p]),ref []
  | P.Let(v,e,p) ->
      trans_exp env (fun x -> trans_proc (Sm.add v x env) ctxt cexp p) e
  | P.Asgn(e1,e2,p) -> (
      match e1 with
        P.Var v -> 
          trans_exp env (
            fun x ->
              trans_exp env (
                fun y ->
                  match x with
                      C.Var _ ->
                        C.Prim(C.Asgn,[x;y],[],[trans_proc env ctxt cexp p]),ref []
                    | _ ->
                        let t = genid "t" in
                          C.Prim(C.Set,[y],[t],[trans_proc (Sm.add v (C.Var t) env) ctxt cexp p]),ref []
              ) e2 ) e1
        | P.Select(r,i) ->
            trans_exp env (
              fun x ->
                trans_exp_list env (
                  fun vs ->
                    C.Prim(C.Update,vs@[x],[],[trans_proc env ctxt cexp p]),ref []
                ) [r;i] ) e2
        | _ -> assert false )
  | P.Par(p1,p2) ->
      let q = genid "p" in
	C.Fix([q,[],trans_proc env ctxt cexp p2],
	     (C.Prim(C.Run,[C.Var q],[],[trans_proc env ctxt cexp p1]),ref [])),ref []
  | P.Case(e,ps) ->
      trans_exp env (
        fun x ->
          C.Switch(x,List.map (
            fun (c,p) -> 
	      (match c with
		  None   -> C.Unit
		| Some v -> trans_const v),
	      trans_proc env ctxt cexp p
          ) ps),ref []) e
  | P.Call(e,es) ->
      let k = genid "k" in
      let r = genid "r" in
	trans_exp env (
	  fun x ->
            trans_exp_list env (
              fun xs ->
		C.Fix([k,[r],ctxt (C.Var r)],
                     (C.App(x,C.Var k::xs),ref [])),ref []
            ) es) e
  | P.Cblk(cs,es,p) ->
      let rec g xs env' = function
          [] -> C.Cblk(cs,(List.rev xs),trans_proc env' ctxt cexp p),ref []
        | P.Var v as e::es ->
            let t = genid "t" in
              trans_exp env' (
                fun x -> match x with
                    C.Var _ -> g (x::xs) (Sm.add v x env') es
                  | _ -> C.Prim(C.Set,[x],[t],
                                [g (C.Var t::xs) (Sm.add v (C.Var t) env') es]),
                      ref []
              ) e
        | e::es -> trans_exp env' (fun x -> g (x::xs) env' es) e
      in
        g [] env es
  | P.Fix(bs,p) ->
      let k = genid "k" in
        C.Fix(List.map (
                fun (f,vs,p) ->
                  (f,k::vs,
                   trans_proc (
                     List.fold_left (fun e v -> Sm.add v (C.Var v) e) env vs
                   ) (fun x -> (C.App(C.Var k,[x]),ref [])) cexp p)
              ) bs,trans_proc env ctxt cexp p),ref []

(* プロセス式の変換(ガード式版) *)
and trans_guard env ctxt cexp = function
    P.Send(e1,e2,p) ->
      let k = genid "k" in
        trans_exp env (fun x ->
          trans_exp env (fun y ->
            C.Fix([k,[],trans_proc env ctxt cexp p],
                 (C.App(C.Label (Symbol.symbol "send"),
		       [C.Label (Symbol.symbol "disp");x;y;C.Var k;C.Const(C.Cint 0)]),ref [])),ref []) e2) e1
  | P.Recv(e,v,p) ->
      let k = genid "k" in
        trans_exp env (fun x ->
          C.Fix([k,[v],trans_proc (Sm.add v (C.Var v) env) ctxt cexp p],
               (C.App(C.Label(Symbol.symbol "recv"),
		     [C.Label(Symbol.symbol "disp");x;C.Var k;C.Const(C.Cint 0)]),ref [])),ref []) e
  | P.Alt(g1,g2) ->
      let t = genid "t" in (* トランザクション管理用 *)
      let k = genid "k" in
      let r = genid "r" in
        C.Fix([k,[r],ctxt (C.Var r)],
             (C.Prim(C.Record,[C.Const(C.Bool false)],[t],
		    [trans_alt env (C.Var t)
			(trans_alt env (C.Var t)
			    C.disp
			    (fun x -> C.App(C.Var k,[x]),ref []) cexp g2)
			(fun y -> C.App(C.Var k,[y]),ref []) cexp g1]),ref [])),ref []
	  
and trans_alt env t cont ctxt cexp = function
    P.Send(e1,e2,p) ->
      let k1 = genid "k" in
      let k2 = genid "k" in
	trans_exp env (fun x ->
	  trans_exp env (fun y ->
	    C.Fix([(k1,[],trans_proc env ctxt cexp p);(k2,[],cont)],
		 (C.App(C.Label (Symbol.symbol "send"),
		       [C.Var k2;x;y;C.Var k1;t]),ref [])),ref []
	  ) e2) e1
  | P.Recv(e,v,p) ->
      let k1 = genid "k" in
      let k2 = genid "k" in
	trans_exp env (fun x ->
	  C.Fix([(k1,[v],trans_proc (Sm.add v (C.Var v) env) ctxt cexp p);(k2,[],cont)],
	       (C.App(C.Label(Symbol.symbol "recv"),
		     [C.Var k2;x;C.Var k1;t]),ref [])),ref []
	) e
  | P.Alt(g1,g2) -> trans_alt env t (trans_alt env t cont ctxt cexp g2) ctxt cexp g1
      
let base_env = Env.fold (
  fun k ent e -> match ent with
      Env.VarEntry _ -> Sm.add k (C.Label k) e
    | _              -> e
) Env.base_env Sm.empty

let trans proc =
  trans_proc base_env (fun x -> C.disp) (C.disp) proc
