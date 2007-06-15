(**
   抽象構文木の定義

   TODO:
    - infoの扱いについて整理する必要がある(不要なinfoを削除するなど)
    - 命名規則について再考の余地あり

   @author Hattori Kenta
   @version $Id: syntax.ml,v 1.10 2006/07/27 00:07:18 hattori Exp $
*)
open Error

(** 宣言 *)
type def =
    DefVar  of (info * Symbol.t * decl)
  | DefType of (info * Symbol.t * typ) list
  | DefProc of (info * Symbol.t * param list * proc) list
and decl  =
    DeclExpr of exp
  | DeclType of typ * Types.t ref
and param = Symbol.t * typ

(** 型式 *)
and typ =
    TypName   of info * Symbol.t
  | TypDeriv  of info * Symbol.t * (var * typ) list
  | TypChan   of info * typ
  | TypRegex  of info * rgx
  | TypArray  of info * typ * int
  | TypRecord of (info * Symbol.t * typ) list
  | TypTuple  of typ list

(** 正規表現式 *)
and rgx =
  | RgxString of info * string
  | RgxChrcls of info * string
  | RgxName   of info * Symbol.t
  | RgxCat    of info * rgx * rgx
  | RgxAlt    of info * rgx * rgx
  | RgxClos   of info * rgx
  | RgxPclos  of info * rgx
  | RgxOpt    of info * rgx
  | RgxArray  of info * rgx * int
  | RgxIter   of info * rgx * Symbol.t * Symbol.t option
  | RgxRecord of (info * Symbol.t * rgx) list

(** パターン式 *)
and pat =
    PatAny   of info
  | PatExp   of exp
  | PatRegex of info * Symbol.t * rgx * Types.rgx ref

(** プロセス式 *)
and proc =
    ProcStop   of info
  | ProcSkip   of info
  | ProcSeq    of info * proc list
  | ProcChoice of info * (proc * proc) list
  | ProcMatch  of info * exp * (pat * proc) list * Types.t ref
  | ProcInput  of info * var * Symbol.t
  | ProcOutput of info * var * exp
  | ProcRun    of info * Symbol.t * exp list
  | ProcVar    of info * Symbol.t * decl
  | ProcAsign  of info * var * exp
  | ProcCblock of info * string list * var list

(** 算術式 *)
and exp =
    ExpConst  of const
  | ExpVar    of var
  | ExpBinop  of info * binop * exp * exp
  | ExpMonop  of info * monop * exp
  | ExpRecord of record list
  | ExpTuple  of exp list
and record = info * Symbol.t * exp

(** 定数リテラル *)
and const =
    ConBool  of info * bool
  | ConInt   of info * int
  | ConStr   of info * string
  | ConUnit  of info

(** 変数 *)
and var =
  | VarSimple of info * Symbol.t      
  | VarField  of info * var * Symbol.t * int ref * Types.t ref (* ラベル参照 *)
  | VarSubscr of info * var * exp                (* 配列要素   *)
  | VarProj   of info * var * int

(** 二項演算子 *)
and binop =
    BopAdd | BopSub | BopMul | BopDiv | BopMod
  | BopEq  | BopNeq | BopLt  | BopLeq | BopGt  | BopGeq
  | BopAnd | BopOr  | BopCat
  
(** 単項演算子 *)
and monop = MopNeg | MopNot

let info_of_const = function ConUnit(i) | ConBool(i,_) | ConInt(i,_) | ConStr(i,_) -> i
let info_of_var = function VarSimple(i,_) | VarField(i,_,_,_,_) | VarSubscr(i,_,_) | VarProj(i,_,_) -> i
let rec info_of_expr = function
    ExpConst(c) -> info_of_const c
  | ExpVar(v)   -> info_of_var v
  | ExpBinop(i,_,_,_) | ExpMonop(i,_,_) -> i
  | ExpRecord(ls) ->let i,_,_ = List.hd ls in i
  | ExpTuple(ls) -> info_of_expr(List.hd ls)
    

type toplevel = def list

