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
  | ProcEntry of Types.t list * Types.t option
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
      ("timer",  VarEntry(T.CHAN (T.TUPLE [T.INT;T.INT])));
(*      ("utimer", VarEntry(T.CHAN T.INT)); *)
      ("cond",   VarEntry(T.CHAN T.BOOL));
      ("null",   VarEntry(T.CHAN T.UNIT));

      (* 予約済み型名 *)
(*      ("void",   TypeEntry(T.VOID)); *)
      ("unit",   TypeEntry(T.UNIT));
      ("bool",   TypeEntry(T.BOOL));
(*      ("byte",   TypeEntry(T.BYTE)); *)
(*      ("ubyte",  TypeEntry(T.UBYTE)); *)
(*      ("short",  TypeEntry(T.SHORT)); *)
(*      ("ushort", TypeEntry(T.USHORT)); *)
      ("int",    TypeEntry(T.INT));
(*      ("uint",   TypeEntry(T.UINT)); *)
(*      ("long",   TypeEntry(T.LONG)); *)
(*      ("ulong",  TypeEntry(T.ULONG)); *)
(*      ("float",  TypeEntry(T.FLOAT)); *)
(*      ("double", TypeEntry(T.DOUBLE)); *)
      ("string", TypeEntry(T.STRING));
    ]

