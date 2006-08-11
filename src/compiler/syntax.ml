(**
   ���ۍ\���؂̒�`

   TODO:
    - info�̈����ɂ��Đ�������K�v������(�s�v��info���폜����Ȃ�)
    - �����K���ɂ��ččl�̗]�n����

   @author Hattori Kenta
   @version $Id: syntax.ml,v 1.10 2006/07/27 00:07:18 hattori Exp $
*)
open Error

(** �錾 *)
type def =
    DefVar  of (info * Symbol.t * decl)
  | DefType of (info * Symbol.t * typ) list
  | DefProc of (info * Symbol.t * param list * proc) list
and decl  =
    DeclExpr of exp
  | DeclType of typ * Types.t ref
and param = Symbol.t * typ

(** �^�� *)
and typ =
    TypName   of info * Symbol.t
  | TypDeriv  of info * Symbol.t * (var * typ) list
  | TypChan   of info * typ
  | TypRegex  of info * rgx
  | TypArray  of info * typ * int
  | TypRecord of (info * Symbol.t * typ) list
  | TypTuple  of typ list

(** ���K�\���� *)
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
  | RgxIter   of info * rgx * Symbol.t
  | RgxRecord of (info * Symbol.t * rgx) list

(** �p�^�[���� *)
and pat =
    PatAny   of info
  | PatConst of const
  | PatRegex of info * Symbol.t * rgx * Types.rgx ref

(** �v���Z�X�� *)
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

(** �Z�p�� *)
and exp =
    ExpConst  of const
  | ExpVar    of var
  | ExpBinop  of info * binop * exp * exp
  | ExpMonop  of info * monop * exp
  | ExpRecord of record list
  | ExpTuple  of exp list
and record = info * Symbol.t * exp

(** �萔���e���� *)
and const =
    ConBool  of info * bool
  | ConInt   of info * int
  | ConStr   of info * string

(** �ϐ� *)
and var =
  | VarSimple of info * Symbol.t      
  | VarField  of info * var * Symbol.t * int ref * Types.t ref (* ���x���Q�� *)
  | VarSubscr of info * var * exp                (* �z��v�f   *)
  | VarProj   of info * var * int

(** �񍀉��Z�q *)
and binop =
    BopAdd | BopSub | BopMul | BopDiv | BopMod
  | BopEq  | BopNeq | BopLt  | BopLeq | BopGt  | BopGeq
  | BopAnd | BopOr  | BopCat
  
(** �P�����Z�q *)
and monop = MopNeg | MopNot

let info_of_const = function  ConBool(i,_) | ConInt(i,_) | ConStr(i,_) -> i

type toplevel = def list

