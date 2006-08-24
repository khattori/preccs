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
      let ls,tl = expand_exp_list o es in [P.Cint o; P.Cint 0;P.Record ls],tl
  | P.Cint i -> [P.Cint o; P.Cint 0;P.Cint 0],o+i
  | _ -> assert false
and expand_exp_list o =
  List.fold_left (fun (ls,j) e ->
                    let ls',j' = expand_exp j e
                    in ls@ls',j'
                 ) ([],o)
  
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
    P.Var v    -> (
      try ctxt (Sm.find v env) with
          Not_found -> Printf.printf "%s\n" (Symbol.name v);assert false
    )
  | P.Unit     -> ctxt (C.Int 0)
  | P.Bool b   -> ctxt (C.Bool b)
  | P.Int i    -> ctxt (C.Int i)
  | P.Cint i   -> ctxt (C.Cint i)
  | P.String s -> ctxt (C.String s)
  | P.If(b,e1,e2) ->
      let j = C.genid "j" in (* 分岐終了後の動作 *)
      let v = C.genid "v" in
        trans_exp env (
          fun x ->
            C.Fix([j,[v],ctxt (C.Var v)],
                  (C.Prim(C.If,[x],[],[
                            trans_exp env (fun x -> C.App(C.Var j,[x]),ref []) e1;
                            trans_exp env (fun x -> C.App(C.Var j,[x]),ref []) e2
                          ]),ref [])),ref []
        ) b
  | P.Record [] -> ctxt (C.Int 0)
  | P.Record es ->
      let t = C.genid "t" in
        trans_exp_list env (
          fun vs -> C.Prim(C.Record,vs,[t],[ctxt (C.Var t)]),ref []
        ) es
  | P.Rexrcd(s,es) ->
      let t1 = C.genid "t" in
      let t2 = C.genid "t" in
        C.Prim(C.Set,[C.String s],[t1],[
                 trans_exp env
                   (fun x ->
                      C.Prim(C.Rexrcd,[C.Var t1;x],[t2],[ctxt (C.Var t2)]),ref [])
                   (let ls,tl = expand_exp_list 0 es in
                      P.Record (ls @ [P.Cint tl]))
               ]),ref []
  | P.Select(e1,e2) ->
      let t = C.genid "t" in
        trans_exp_list env (
          fun vs -> C.Prim(C.Select,vs,[t],[ctxt (C.Var t)]),ref []
        ) [e1;e2]
  | P.Offset(e1,e2) ->
      let t = C.genid "t" in
        trans_exp_list env (
          fun vs -> C.Prim(C.Offset,vs,[t],[ctxt (C.Var t)]),ref []
        ) [e1;e2]
  | P.Prim(op,es) ->
      let t = C.genid "t" in
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
 *   引　数：env : Env.t         --- 名前環境
 *           cont: cexp          --- 継続処理
 *           --- : Pi.exp        --- π式
 *   戻り値：変換後のCPS式
 * 
 *)
and trans_proc env cont = function
    P.End        -> cont
  | P.Guard g    ->
      let t = C.genid "t" in (* トランザクション管理用 *)
        C.Prim(C.Record,[C.Bool false],[t],
               [trans_guard env (C.Var t) cont C.disp g]),ref []
  | P.New(v,p)   ->
      let c = C.genid "c" in
        C.Prim(C.New,[],[c],[trans_proc (Sm.add v (C.Var c) env) cont p]),ref []
  | P.Let(v,e,p) ->
      trans_exp env (fun x -> trans_proc (Sm.add v x env) cont p) e
  | P.Par(p1,p2) ->
      let p = C.genid "p" in
      let q = C.genid "p" in
        C.Fix([(p,[],trans_proc env cont p1);(q,[],trans_proc env C.disp p2)],
              (C.App(C.Label (Symbol.symbol "run"),[C.Var p;C.Var q]),ref [])),ref []
  | P.Case(e,ps) ->
      let n = C.genid "n" in
        C.Fix([n,[],cont],
              trans_exp env (
                fun x ->
                  C.Prim(C.Switch,[x],[],
                         List.map (
                           fun p -> trans_proc env (C.App(C.Var n,[]),ref []) p
                         ) ps),ref []
              ) e),ref []
  | P.Call(v,es) ->
      let k = C.genid "k" in
        trans_exp_list env (
          fun xs ->
            C.Fix([k,[],cont],
                  (C.App(C.Var v,C.Var k::xs),ref [])),ref []
        ) es
  | P.Cblk(cs,es,p) ->
      let rec g xs env' = function
          [] -> C.Cblk(cs,(List.rev xs),trans_proc env' cont p),ref []
        | P.Var v as e::es ->
            let t = C.genid "t" in
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
  | P.Set(e1,e2,p) -> (
      match e1 with
          P.Var v -> 
            trans_exp env (
              fun x ->
                trans_exp env (
                  fun y ->
                    match x with
                        C.Var _ ->
                          C.Prim(C.Asign,[x;y],[],[trans_proc env cont p]),ref []
                      | _ ->
                          let t = C.genid "t" in
                            C.Prim(C.Set,[y],[t],[trans_proc (Sm.add v (C.Var t) env) cont p]),ref []
                ) e2 ) e1
        | P.Select(r,i) ->
            trans_exp env (
              fun x ->
                let t = C.genid "t" in
                  trans_exp_list env (
                    fun vs ->
                      C.Prim(C.Update,vs@[x],[],[trans_proc env cont p]),ref []
                  ) [r;i] ) e2
        | _ -> assert false )

  | P.Fix(bs,p) ->
      let k = C.genid "k" in
        C.Fix(List.map (
                fun (f,vs,p) ->
                  (f,k::vs,
                   trans_proc (
                     List.fold_left (fun e v -> Sm.add v (C.Var v) e) env vs
                   ) (C.App(C.Var k,[]),ref []) p)
              ) bs,trans_proc env cont p),ref []

(* プロセス式の変換(ガード式版) *)
and trans_guard env t c1 c2 = function
    P.Send(e1,e2,p) ->
      let k1 = C.genid "k" in
      let k2 = C.genid "k" in
        trans_exp env (
          fun x ->
            trans_exp env (
              fun y ->
                C.Fix([(k1,[],trans_proc env c1 p);(k2,[],c2)],
                      (* (C.Prim(C.Send,[x;y;C.Var k;t],[],[c2]),ref [])),ref [] *)
                      (C.App(C.Label (Symbol.symbol "send"),[C.Var k2;x;y;C.Var k1;t]),ref [])),ref []
            ) e2) e1
  | P.Recv(e,v,p) ->
      let k1 = C.genid "k" in
      let k2 = C.genid "k" in
        trans_exp env (
          fun x ->
            C.Fix([(k1,[v],trans_proc (Sm.add v (C.Var v) env) c1 p);(k2,[],c2)],
                  (* (C.Prim(C.Recv,[x;C.Var k;t],[],[c2]),ref [])),ref [] *)
                  (C.App(C.Label(Symbol.symbol "recv"),[C.Var k2;x;C.Var k1;t]),ref [])),ref []
        ) e
  | P.Alt(g1,g2) ->
      trans_guard env t c1 (trans_guard env t c1 c2 g2) g1

let base_env = Env.fold (
  fun k ent e -> match ent with
      Env.VarEntry _ -> Sm.add k (C.Label k) e
    | _              -> e
) Env.base_env Sm.empty

let trans proc =
  trans_proc base_env C.disp proc
