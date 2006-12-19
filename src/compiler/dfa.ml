(**
   DFAモジュール

   @author Hattori Kenta
   @version $Id: dfa.ml,v 1.3 2006/07/06 04:15:36 hattori Exp $
*)
module Ht = Hashtbl

module T  = Types
module R  = Regex
module P  = Prop
module C  = Cond
module N  = Nfa

module PosSet    = Set.Make(Pos)
module LabelSet  = Set.Make(Label)
module LabelMap  = Map.Make(Label)
module DstateSet = Set.Make(Dstate)
module DstateMap = Map.Make(Dstate)

exception Not_exhaustive

(* スキップモード *)
let skipMode = ref false

(*
 * DFAをダンプする
 * 
 *   引　数：init   : Dstate.t    --- DFA初期状態
 *           st_map : DstateMap.t --- 状態遷移表
 * 
 *)
let show (init,st_map) =
  Dstate.show init; print_newline();
  DstateMap.iter (
    fun st dt ->
      Dstate.show st; print_string ":";
      Dtrans.show dt; print_newline()
  ) st_map

(*
 * 開始ラベルの計算
 *   ( ls2 - ls1を計算する )
 * 
 *   引　数：ls1 : LabelSet.t --- 遷移元ノードに対応するラベル集合
 *           ls2 : LabelSet.t --- 遷移先ノードに対応するラベル集合
 * 
 *   戻り値：開始ラベルの集合
 * 
 *)
let start_labels ls1 ls2 =
  LabelSet.fold (
    fun lbl ls -> if LabelSet.mem lbl ls1 then ls else LabelSet.add lbl ls
  ) ls2 LabelSet.empty


module TS = Set.Make(Tcond)
module TM = Map.Make(TS)
(*
 * 遷移条件のリストを直和分解する(disjointな集合に分解)
 *
 *   引　数：clps : (Cond.t * LabelSet.t * Pos.t) list --- 遷移条件と値の組のリスト
 * 
 *   戻り値：直和分解した結果のリスト：(TcondSet.t * LabelSet.t * Dstate.t) list
 * 
 *)
let decomp clps =
  let to_tcondset = function
      C.Prop(p) ->
        let rec trav ts = function
            P.Atom(C.Counter(l)) -> TS.add (Tcond.CntZero l) ts
          | P.Atom(C.Value(l))   -> TS.add (Tcond.ValZero l) ts
          | P.Neg(P.Atom(C.Counter(l))) -> TS.add (Tcond.CntNonz l) ts
          | P.Neg(P.Atom(C.Value(l)))   -> TS.add (Tcond.ValNonz l) ts
          | P.Conj(p1,p2) -> trav (trav ts p1) p2
          | _ -> assert false
        in
	  trav TS.empty p
    | C.Const(true) -> TS.empty
    | _             -> assert false
  in
  let vs = List.fold_left (
    fun vs (c,_,_) ->
      List.fold_left
	(fun vs' v -> if List.mem v vs' then vs' else v::vs')
	vs (C.get_vars c)
  ) [] clps in
  (* 遷移条件リストから命題論理式の組み合わせを抽出 *)
  let cs = C.gen_conds vs in
    (* 各遷移条件をチェック *)
    List.filter (fun (ts,ls,ps) -> not (PosSet.is_empty ps)) (
      List.fold_left (
        fun tlps c ->
          (List.fold_left (
            fun (ts,ls,ps) (c',ls',p) ->
              if C.is_true(C.impl c c') then
                ts,LabelSet.union ls ls',PosSet.add p ps
              else
                ts,ls,ps
	  ) (to_tcondset c,LabelSet.empty,PosSet.empty) clps)::tlps
      ) [] cs)


(*
 * 正規表現からDFAを構成する
 * 
 *     引  数： re : Regex.t -- 正規表現
 * 
 *     戻り値： DFA(初期状態，初期ラベル集合，遷移表)
 *) 
let gendfa re =
  let init = N.firstpos re in
  let follow  = N.followpos re in
  let init_ps = N.cp2ps init in
  let init_ls = N.cp2ls init in
  let init_st = Dstate.of_posset init_ps in
  let rec make marked unmarked =
    if DstateSet.is_empty unmarked then
      marked
    else
      let st = DstateSet.choose unmarked in
      let dtrans =
        Cset.fold (
          fun tm a ->
            let f flw p cp =
              if Cset.mem a (Pos.pos2cs p) then N.union cp (flw p) else cp in
            let nexts = PosSet.fold (f follow) (Dstate.to_posset st) []
            in
              List.fold_left (
                fun tm (ts,ls,ps) ->
                  Dtrans.add (Cset.create a) ts ls (Dstate.of_posset ps) tm
              ) tm (decomp nexts)
        ) Dtrans.empty (N.ps2cset (Dstate.to_posset st)) in
        make
          (DstateMap.add st dtrans marked)
          (DstateSet.fold (
             fun ns us ->
               if DstateMap.mem ns marked then
                 us                  (* 既にチェックずみ *)
               else 
                 DstateSet.add ns us (* 未チェックのものを追加 *)
           ) (Dtrans.next_all dtrans) (DstateSet.remove st unmarked))
  in
    init_st,init_ls,make DstateMap.empty (DstateSet.singleton init_st)

(*
 * 二つのDFAからスキップオートマトンを構成する
 *
 *   引  数： dfa1 : スキップ対象となるDFA
 *            dfa2 : 親元となるDFA
 * 
 *   戻り値：スキップオートマトン
 * 
 *)
let skipdfa (init_st1,init_ls1,st_map1) (init_st2,init_ls2,st_map2) =
  let init_lm = Nfa.add_lblmap (LabelSet.union init_ls1 init_ls2) LabelMap.empty in
  let rec make st_map lm chkd = function
      [] -> st_map
    | p::pairs when List.mem p chkd -> make st_map lm chkd pairs
    | (s1,s2)::pairs ->
        let dt1 = DstateMap.find s1 st_map1 in  (* 遷移テーブル *)
        let dt2 = DstateMap.find s2 st_map2 in
        let dt',lm',pairs' = Dtrans.skip dt1 dt2 lm in
          if Dtrans.is_empty dt' then
            make st_map lm ((s1,s2)::chkd) pairs
          else
            make (DstateMap.add s1 dt' st_map) lm' ((s1,s2)::chkd) (pairs'@pairs)
  in
    init_st1,init_ls1,make DstateMap.empty init_lm [] [init_st1,init_st2]

(** 正規表現を位置づけし，終了位置を追加する *)
let add_fin re =
  let p = Pos.create Cset.fin in R.SEQ(R.posify re,R.CHARS p),p

(*
 * 正規表現型のリストからDFAを生成する
 * 
 *   引　数：ts : Types.rgx list --- 正規表現型のリスト
 * 
 *   戻り値：int --- 生成したDFAのID
 * 
 *)
let generate ts =
  let rs, fs = List.split (List.map T.regexify2 ts) in
  let rs',ps = List.split (List.map add_fin rs) in
  let re = R.concat rs' in
  let init,init_ls,st_map = gendfa re
  in
    Dtable.Accept.register ps fs;
    Dtable.create init init_ls st_map

(*
 * 正規表現型のリストと親パタンからDFAを生成する
 * 
 *   引　数：ts : Types.rgx list --- 正規表現型のリスト
 *           t  : Types.rgx      --- 親パタン
 * 
 *   戻り値：int --- 生成したDFAのID
 * 
 *)
let generate2 ts t =
  let rs, fs = List.split (List.map T.regexify2 ts) in
  let rs',ps = List.split (List.map add_fin rs) in
  let re = R.concat rs' in
  let r  = T.regexify t in
  let re' = R.SEQ(R.posify r,R.CHARS(Pos.create Cset.fin)) in
  let init,init_ls,st_map =
    if !skipMode then
      skipdfa (gendfa re) (gendfa re')
    else
      gendfa re
  in
    if not (Subset.subset r (R.concat rs)) then raise Not_exhaustive;
    Dtable.Accept.register ps fs;
    Dtable.create init init_ls st_map
