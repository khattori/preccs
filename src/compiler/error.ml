(**
   エラーメッセージ表示ルーチン

   TODO:
    - エラーメッセージとエラー種別を分離して定義したい

   @author Hattori Kenta
   @version $Id: error.ml,v 1.5 2006/07/11 00:12:20 hattori Exp $
*)

exception Exit of int

(** エラー種別の定義 *)
type err =
    ERR_UNDEF_VAR       of Symbol.t
  | ERR_UNDEF_LABEL     of Symbol.t
  | ERR_UNDEF_PROC      of Symbol.t
  | ERR_UNDEF_TYPE      of Symbol.t
  | ERR_NOT_CHANNEL
  | ERR_NOT_EXHAUSTIVE
  | ERR_ILLEGAL_VAR     of Symbol.t
  | ERR_ILLEGAL_INDEX   of int
  | ERR_ILLEGAL_CHAR    of string
  | ERR_ILLEGAL_HEXCHAR of string
  | ERR_ILLEGAL_OCTCHAR of int
  | ERR_ILLEGAL_ESCAPE  of string
  | ERR_ILLEGAL_LABEL
  | ERR_ILLEGAL_ARRAY
  | ERR_ILLEGAL_ARRLEN  of int
  | ERR_ILLEGAL_TYPE
  | ERR_ILLEGAL_PATTERN
  | ERR_ILLEGAL_DERIV
  | ERR_INVALID_ARGNUM
  | ERR_TYPEMIS_ARG
  | ERR_TYPE_MISMATCH   of Types.t * Types.t
  | ERR_NONTERM_CBLOCK
  | ERR_NONTERM_STRING
  | ERR_NONTERM_COMMENT
  | ERR_UNMATCH_COMMENT
  | ERR_FILE_NOTFOUND	of string
  | ERR_NO_INPUT
  | ERR_PARSE_ERROR
  | ERR_UNSUPPORT
  | ERR_INTERNAL
  | ERR_UNKNOWN

let quote s = "'"^s^"'"
let doublequote s = "\""^s^"\""

let errmsg = function
    ERR_UNDEF_VAR(s)       -> "undefined variable " ^ (doublequote (Symbol.name s))
  | ERR_UNDEF_LABEL(s)     -> "undefined label " ^ (doublequote (Symbol.name s))
  | ERR_UNDEF_PROC(s)      -> "undefined process " ^ (doublequote (Symbol.name s))
  | ERR_UNDEF_TYPE(s)      -> "undefined type " ^ (doublequote (Symbol.name s))
  | ERR_NOT_CHANNEL        -> "channel is expected here"
  | ERR_NOT_EXHAUSTIVE     -> "pattern is not exahaustive"
  | ERR_ILLEGAL_VAR(s)     -> "illegal variable " ^ (doublequote (Symbol.name s))
  | ERR_ILLEGAL_INDEX(n)   -> "illegal index " ^ (string_of_int n)
  | ERR_ILLEGAL_CHAR(c)    -> "illegal character " ^ (quote c)
  | ERR_ILLEGAL_HEXCHAR(c) -> "illegal hex character " ^ (quote c)
  | ERR_ILLEGAL_OCTCHAR(n) -> "illegal octal character " ^ (quote (string_of_int n))
  | ERR_ILLEGAL_ESCAPE(c)  -> "illegal escape character " ^ (quote c)
  | ERR_ILLEGAL_LABEL      -> "illegal label reference"
  | ERR_ILLEGAL_ARRAY      -> "illegal array reference"
  | ERR_ILLEGAL_ARRLEN(n)  -> "illegal array length: " ^ string_of_int n
  | ERR_ILLEGAL_TYPE       -> "illegal type expression"
  | ERR_ILLEGAL_PATTERN    -> "illegal pattern match"
  | ERR_ILLEGAL_DERIV      -> "illegal type derivation"
  | ERR_INVALID_ARGNUM     -> "invalid arguments number"
  | ERR_TYPEMIS_ARG        -> "argument type mismatch"
  | ERR_TYPE_MISMATCH(t1,t2) -> "type mismatch"
  | ERR_NONTERM_CBLOCK     -> "c-block is not terminated"
  | ERR_NONTERM_STRING     -> "string is not terminated"
  | ERR_NONTERM_COMMENT    -> "comment is not terminated"
  | ERR_UNMATCH_COMMENT    -> "unmatched end comment"
  | ERR_FILE_NOTFOUND s	   -> (quote s) ^ " file not found"
  | ERR_NO_INPUT	   -> "no input files"
  | ERR_PARSE_ERROR        -> "parse error"
  | ERR_UNSUPPORT          -> "unsupported"
  | ERR_INTERNAL           -> "internal error"
  | _                      -> "unknown error"

(** 警告種別の定義 *)
type war =
    WAR_UNKNOWN

let warmsg = function
    WAR_UNKNOWN -> "unknown warning"

type info =
    FILEINF of string * int * int
  | NOFILE
  | UNKNOWN
type 'a withinfo = { i: info; v: 'a }

let unknownInfo = UNKNOWN
let nofileInfo = NOFILE
let createInfo f l c = FILEINF(f, l, c)

let printInfo =
  function
      FILEINF(f,l,c) ->
	print_string f; 
	print_string ":"; 
	print_int l; print_string "."; 
	print_int c; print_string ":"
    | UNKNOWN ->
	print_string "<Unknown file and line>: "
    | NOFILE -> ()

let perror s = print_string (Sys.executable_name ^ ": [error] "^s); print_newline()
let errorAt fi e = printInfo fi; perror(errmsg e); raise (Exit 1)
let error e = perror(errmsg e); raise (Exit 1)

let pwarning s = print_string (Sys.executable_name ^ ": [warning] "^ s); print_newline()
let warningAt fi w = printInfo fi; pwarning(warmsg w)
let warning e = pwarning(warmsg e)
