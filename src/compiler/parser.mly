/**
   Preccsコンパイラ構文解析器

   @author Hattori Kenta
   @version $Id: parser.mly,v 1.11 2006/07/27 00:07:17 hattori Exp $
*/
%{
open Error
open Syntax
module T = Types
module R = Regex
%}

/* キーワードのトークン */
%token <Error.info> TRUE
%token <Error.info> FALSE
%token <Error.info> VAR
%token <Error.info> PROC
%token <Error.info> TYPE
%token <Error.info> SKIP
%token <Error.info> STOP
%token <Error.info> RUN
/* %token <Error.info> NULL */
%token <Error.info> IMPORT

/* 識別子と定数値のトークン */
%token <Symbol.t Error.withinfo> IDENT
%token <int      Error.withinfo> INTV
%token <string   Error.withinfo> STRV

/* Cコードフラグメント */
%token <string>     CFRAG

/* 記号トークン */
%token <Error.info> ARROW
%token <Error.info> AS
%token <Error.info> EXCLM
%token <Error.info> QUEST
%token <Error.info> SHARP
%token <Error.info> COLON
%token <Error.info> COMMA
%token <Error.info> DOT
%token <Error.info> EQ
%token <Error.info> GT
%token <Error.info> LT
%token <Error.info> LCURLY
%token <Error.info> RCURLY
%token <Error.info> LCBLK
%token <Error.info> RCBLK
%token <Error.info> LPAREN
%token <Error.info> RPAREN
%token <Error.info> LSQUARE
%token <Error.info> RSQUARE
%token <Error.info> SEMI
%token <Error.info> PERCENT
%token <Error.info> STAR
%token <Error.info> PLUS
%token <Error.info> DASH
%token <Error.info> CARET
%token <Error.info> SLASH
%token <Error.info> AT
%token <Error.info> DOLLAR
%token <Error.info> USCORE
%token <Error.info> VBAR
%token <Error.info> EQEQ
%token <Error.info> NEQ
%token <Error.info> GEQ
%token <Error.info> LEQ
%token <Error.info> ANDAND
%token <Error.info> OROR
%token <Error.info> COLONEQ
%token <Error.info> EOF


/* 優先順位と結合度の定義 */
%right VBAR
%right SEMI

%left OROR
%left ANDAND
%left EQEQ NEQ
%left GT GEQ LT LEQ
%left CARET
%left PLUS DASH
%left STAR SLASH PERCENT
%nonassoc UMIN LNOT

%start toplevel
%type <string list * string * Syntax.toplevel * string> toplevel

%%

/* 開始ルール */
toplevel
  : cblockPart importList definitionList cblockPart EOF { 
    let vars,types,procs = $3 in
      $2,$1,vars@[DefType types]@[DefProc procs],$4
  }
;
definitionList
  : definition { 
    match $1 with
        DefVar _  -> [$1],[],[]
      | DefType t -> [],  t, []
      | DefProc p -> [],  [],p
  }
  | definitionList definition {
      let vars,types,procs = $1 in
        match $2 with
            DefVar _  -> vars@[$2],types,procs
          | DefType t -> vars,types@t,procs
          | DefProc p -> vars,types,procs@p
    }
;

/* ヘッダ/フッタ部 */
cblockPart
  : /* empty */       { "" }
  | LCBLK CFRAG RCBLK { $2 }
;

importList
  : /* empty */ { [] }
  | importList IMPORT STRV  { $1 @ [$3.v] }
;

/*****************************************************************
 * 各種定義
 */
definition
  : varDefinition     { DefVar $1   }
  | typeDefinition    { DefType[$1] }
  | procDefinition    { DefProc[$1] }
;
/* 変数定義 */
varDefinition
  : VAR IDENT declarator { $1,$2.v,$3 }
;
declarator
  : COLON typeExpression { DeclType($2,ref T.STRING) }
  | EQ expression        { DeclExpr($2)              }
;

/* 型定義 */
typeDefinition
  : TYPE IDENT EQ typeExpression { $1,$2.v,$4 }
  | TYPE IDENT EQ IDENT LCURLY derivList RCURLY { $1,$2.v,TypDeriv($4.i,$4.v,$6) }
;
/* 派生型の定義 */
derivList
  : derivPattern { [$1] }
  | derivList COMMA derivPattern { $1 @ [$3] }
;
derivPattern
  : varExpression EQ typeAtomicExpression { ($1,$3) }
;

/* プロセス定義 */
procDefinition
  : PROC IDENT LPAREN RPAREN EQ procExpression               { $1,$2.v,[],$6 }
  | PROC IDENT LPAREN parameterList RPAREN EQ procExpression { $1,$2.v,$4,$7 }
;
parameterList
  : parameter { [$1] }
  | parameterList COMMA parameter { $1 @ [$3] }
;
parameter
  : IDENT COLON typeAtomicExpression { ($1.v,$3) }

/*****************************************************************
 * 算術式に関する規則
 */
expression
  : arithExpression { $1 }
  | arithExpression COMMA expression { 
      match $3 with
          ExpTuple e -> ExpTuple($1::e)
        | _          -> ExpTuple($1::[$3])
    }
;

arithExpression
  : unaryExpression { $1 }
  | arithExpression PLUS    arithExpression { ExpBinop($2, BopAdd, $1, $3) }
  | arithExpression DASH    arithExpression { ExpBinop($2, BopSub, $1, $3) }
  | arithExpression STAR    arithExpression { ExpBinop($2, BopMul, $1, $3) }
  | arithExpression SLASH   arithExpression { ExpBinop($2, BopDiv, $1, $3) }
  | arithExpression PERCENT arithExpression { ExpBinop($2, BopMod, $1, $3) }
  | arithExpression EQEQ    arithExpression { ExpBinop($2, BopEq,  $1, $3) }
  | arithExpression NEQ     arithExpression { ExpBinop($2, BopNeq, $1, $3) }
  | arithExpression GT      arithExpression { ExpBinop($2, BopGt,  $1, $3) }
  | arithExpression GEQ     arithExpression { ExpBinop($2, BopGeq, $1, $3) }
  | arithExpression LT      arithExpression { ExpBinop($2, BopLt,  $1, $3) }
  | arithExpression LEQ     arithExpression { ExpBinop($2, BopLeq, $1, $3) }
  | arithExpression ANDAND  arithExpression { ExpBinop($2, BopAnd, $1, $3) }
  | arithExpression OROR    arithExpression { ExpBinop($2, BopOr,  $1, $3) }
  | arithExpression CARET   arithExpression { ExpBinop($2, BopCat, $1, $3) }
;

unaryExpression 
  : atomicExpression { $1 }
  | DASH   unaryExpression %prec UMIN { ExpMonop($1,MopNeg,$2) }
  | EXCLM  unaryExpression %prec LNOT { ExpMonop($1,MopNot,$2) }
;

atomicExpression
  : LPAREN expression RPAREN { $2 }
  | varExpression { ExpVar($1) }
  | LCURLY recFieldList RCURLY { ExpRecord($2) }
  | INTV  { ExpConst(ConInt($1.i,$1.v)) }
  | STRV  { ExpConst(ConStr($1.i,$1.v)) }
  | TRUE  { ExpConst(ConBool($1,true))  }
  | FALSE { ExpConst(ConBool($1,false)) }
  | LPAREN RPAREN { ExpConst(ConUnit($1)) }
;

varExpression
  : IDENT                                    { VarSimple($1.i,$1.v)         }
  | varExpression DOT IDENT                  { VarField($3.i,$1,$3.v,ref 0,ref Types.INT) }
  | varExpression INTV                       { VarProj($2.i, $1, $2.v)      }
  | varExpression LSQUARE expression RSQUARE { VarSubscr($2,$1,$3)          }

;
recFieldList
  : recField { [$1] }
  | recFieldList SEMI recField { $1 @ [$3] }
;
recField
  : IDENT EQ expression { ($1.i,$1.v,$3) }
;

/*****************************************************************
 * 型式に関する構文規則
 */
typeExpression
  : typeTupleExpression { $1 }
  | typeTupleExpression LSQUARE INTV RSQUARE { TypArray($2,$1,$3.v) }
;
typeTupleExpression
  : typeAtomicExpression            { $1 }
  | typeTupleExpression COMMA typeAtomicExpression  {
      match $3 with
          TypTuple t -> TypTuple($1::t)
        | _          -> TypTuple($1::[$3])
    }
;

typeAtomicExpression
  : IDENT                           { TypName($1.i,$1.v) }
  | LT typeExpression GT            { TypChan($1,$2)     } /* チャネル型 */
  | LCURLY rgxExpression RCURLY     { TypRegex($1,$2)    } /* 正規表現型 */
  | LCURLY fieldList RCURLY         { TypRecord($2)      } /* レコード型 */

;

/* レコード型のフィールドリスト */
fieldList
  : field { [$1] }
  | fieldList SEMI field { $1 @ [$3] }
;
field
  : IDENT COLON typeExpression  { ($1.i,$1.v,$3) }
;

/* 正規表現式 */
rgxExpression
  : rgxPostfixExpression { $1 }
  | rgxExpression SEMI rgxExpression { RgxCat($2,$1,$3) }
  | rgxExpression VBAR rgxExpression { RgxAlt($2,$1,$3) }
;
rgxPostfixExpression
  : rgxAtomicExpression { $1 }
  | rgxPostfixExpression STAR  { RgxClos($2,$1)  }
  | rgxPostfixExpression PLUS  { RgxPclos($2,$1) }
  | rgxPostfixExpression QUEST { RgxOpt($2,$1)   }
  | rgxPostfixExpression LSQUARE INTV RSQUARE  { RgxArray($2,$1,$3.v) }
  | rgxPostfixExpression LSQUARE IDENT RSQUARE { RgxIter($2,$1,$3.v)  }
;
rgxAtomicExpression
  : IDENT                       { RgxName($1.i,$1.v)   }
  | STRV                        { RgxString($1.i,$1.v) }
  | SLASH STRV                  { RgxChrcls($1,$2.v)   }
  | LCURLY rgxFieldList RCURLY  { RgxRecord($2)        }
  | LPAREN rgxExpression RPAREN { $2 }
;

/* 正規表現レコード型のフィールドリスト */
rgxFieldList
  : rgxField { [$1] }
  | rgxFieldList SEMI rgxField { $1 @ [$3] }
;
rgxField
  : IDENT COLON rgxPostfixExpression  { ($1.i,$1.v,$3) }
;


/*****************************************************************
 * パターン式に関する構文規則
 */
patExpression
  : USCORE { PatAny($1)                  }
  | INTV   { PatConst(ConInt($1.i,$1.v)) }
  | DASH INTV { PatConst(ConInt($1,-$2.v)) }
  | STRV   { PatConst(ConStr($1.i,$1.v)) }
  | TRUE   { PatConst(ConBool($1,true))  }
  | FALSE  { PatConst(ConBool($1,false)) }
  | IDENT COLON rgxExpression { PatRegex($1.i,$1.v,$3,ref (T.REXP R.EPS)) }
  | IDENT  { PatVar(VarSimple($1.i,$1.v)) }
;

/*****************************************************************
 * プロセス式に関する構文規則
 */
procExpression
  : procMatchExpression  { $1 }
  | procChoiceExpression { $1 }
;

/* 選択実行プロセス */
procChoiceExpression
  : procGuardAction VBAR procGuardAction { ProcChoice($2, [$1;$3]) }
  | procChoiceExpression VBAR procGuardAction 
      { match $1 with
	    ProcChoice(i, g) -> ProcChoice(i, g @ [$3])
	  | _                -> errorAt $2 ERR_INTERNAL
      }
;
/* ガード付き動作 */
procGuardAction
  : procGuardExpression ARROW procSeqExpression { $1,$3 }
;

/* パタンマッチプロセス */
procMatchExpression
  : procSeqExpression { $1 }
  | expression AT procMatchActionList { ProcMatch($2,$1,$3,ref T.STRING) }
;
procMatchActionList
  : procMatchAction { [$1] }
  | procMatchActionList VBAR procMatchAction { $1 @ [$3] }
;
procMatchAction
  : patExpression ARROW procSeqExpression { ($1, $3) }
;

/* 逐次実行プロセス */
procSeqExpression
  : procAtomExpression { $1 }
  | procSeqExpression SEMI procAtomExpression
      { match $1 with
	    ProcSeq(i, p) -> ProcSeq(i, p @ [$3])
	  | _             -> ProcSeq($2, [$1; $3])
      }
;

/* 基本プロセス式 */
procAtomExpression
  : LPAREN procExpression RPAREN     { $2 }
  | procGuardExpression              { $1 }
  | varExpression COLONEQ expression { ProcAsign($2,$1,$3)          }
  | VAR IDENT declarator             { ProcVar($1,$2.v,$3)          }
  | IDENT procInvocation             { ProcRun($1.i,$1.v,$2)        }
  | SKIP                             { ProcSkip($1)                 }
  | STOP                             { ProcStop($1)                 }
  | LCBLK codeFragList RCBLK         { ProcCblock($1,fst $2,snd $2) }
;

/*
 * [インラインブロック]
 * Cコード文字列,$変数参照式$,Cコード文字列, ...という形式で渡される
 */
codeFragList
  : CFRAG                            { [$1],[] }
  | codeFragList varExpression CFRAG { (fst $1)@[$3],(snd $1)@[$2] }
;

/* ガードプロセス */
procGuardExpression
  : varExpression EXCLM expression { ProcOutput($2,$1,$3)  } /* 出力 */
  | varExpression QUEST IDENT      { ProcInput($2,$1,$3.v) } /* 入力 */
;

/* プロセス呼出し式 */
procInvocation
  : LPAREN RPAREN { [] }
  | LPAREN argumentList RPAREN { $2 }
;
/* 引数リスト */
argumentList
  : arithExpression { [$1] }
  | argumentList COMMA arithExpression { $1 @ [$3] }
;

%%
