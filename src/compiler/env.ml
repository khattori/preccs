(**
   環境操作モジュール

   @author Hattori Kenta
   @version $Id: env.ml,v 1.5 2006/06/21 00:14:14 hattori Exp $
*)

module S = Symbol
module T = Types
module Sm = Map.Make(Symbol)

(* 変数・プロセス・型環境の定義 *)
type entry =
    VarEntry  of Types.t
  | ProcEntry of Types.t list
  | TypeEntry of Types.t

let find = Sm.find
let add  = Sm.add
let fold = Sm.fold
let iter = Sm.iter

type t = entry Sm.t

let base_env =
  List.fold_left (fun sm (s,t) -> Sm.add (S.symbol s) t sm) Sm.empty
    [
      (* 予約済み変数（チャネル） *)
      ("stdin",  VarEntry(T.CHAN T.STRING));
      ("stdout", VarEntry(T.CHAN T.STRING));
      ("timer",  VarEntry(T.CHAN T.INT));
      ("utimer", VarEntry(T.CHAN T.INT));
      ("cond",   VarEntry(T.CHAN T.BOOL));
      (* 予約済み型名 *)
      ("int",    TypeEntry(T.INT));
      ("bool",   TypeEntry(T.BOOL));
      ("string", TypeEntry(T.STRING));
      ("octet",  TypeEntry(T.REGEX(T.REXP Regex.oct)));
    ]

