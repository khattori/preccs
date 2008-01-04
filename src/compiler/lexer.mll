(**
   Preccsコンパイラ字句解析器

   @author Hattori Kenta
   @version $Id: lexer.mll,v 1.5 2006/07/27 00:07:17 hattori Exp $
*)
{
open Error

(** 予約語の管理 *)
let rsvwords = Hashtbl.create 256
let _ =
  List.iter
    (fun (str, f) -> Hashtbl.add rsvwords str f)
    [ (* キーワード *)
(* IDとして処理される
      ( "octet",  fun i -> Parser.OCTET i   );
      ( "int",    fun i -> Parser.INT i     );
      ( "bool",   fun i -> Parser.BOOL i    );
      ( "unit",   fun i -> Parser.UNIT i    );
      ( "string", fun i -> Parser.STRING i  );
*)
      ( "true",   fun i -> Parser.TRUE i    );
      ( "false",  fun i -> Parser.FALSE i   );
      ( "var",    fun i -> Parser.VAR i     );
      ( "proc",   fun i -> Parser.PROC i    );
      ( "type",   fun i -> Parser.TYPE i    );
      ( "run",    fun i -> Parser.RUN i     );
      ( "skip",   fun i -> Parser.SKIP i    );
      ( "stop",   fun i -> Parser.STOP i    );
      ( "return", fun i -> Parser.RETURN i  );
(*      ( "null",   fun i -> Parser.NULL i    ); *)
      ( "import", fun i -> Parser.IMPORT i  );
      (* 特殊記号 *)
      ( "@",      fun i -> Parser.AT i      );
      ( "#",      fun i -> Parser.SHARP i   );
      ( "!",      fun i -> Parser.EXCLM i   );
      ( "?",      fun i -> Parser.QUEST i   );
      ( "%",      fun i -> Parser.PERCENT i );
      ( "*",      fun i -> Parser.STAR i    );
      ( "+",      fun i -> Parser.PLUS i    );
      ( "-",      fun i -> Parser.DASH i    );
      ( "^",      fun i -> Parser.CARET i   );
      ( "/",      fun i -> Parser.SLASH i   );
      ( "|",      fun i -> Parser.VBAR i    );
      ( ";",      fun i -> Parser.SEMI i    );
      ( ",",      fun i -> Parser.COMMA i   );
      ( ":",      fun i -> Parser.COLON i   );
      ( ".",      fun i -> Parser.DOT i     );
      ( "_",      fun i -> Parser.USCORE i  );
      ( "=",      fun i -> Parser.EQ i      );
      ( "[",      fun i -> Parser.LSQUARE i );
      ( "]",      fun i -> Parser.RSQUARE i );
      ( "<",      fun i -> Parser.LT i      );
      ( ">",      fun i -> Parser.GT i      );
      ( "{",      fun i -> Parser.LCURLY i  );
      ( "}",      fun i -> Parser.RCURLY i  );
      ( "(",      fun i -> Parser.LPAREN i  );
      ( ")",      fun i -> Parser.RPAREN i  );
      (* 複合記号 *)
      ( "&&",     fun i -> Parser.ANDAND i  );
      ( "||",     fun i -> Parser.OROR i    );
      ( ":=",     fun i -> Parser.COLONEQ i );
      ( "==",     fun i -> Parser.EQEQ i    );
      ( "!=",     fun i -> Parser.NEQ i     );
      ( ">=",     fun i -> Parser.GEQ i     );
      ( "<=",     fun i -> Parser.LEQ i     );
      ( "!=",     fun i -> Parser.NEQ i     );
      ( "->",     fun i -> Parser.ARROW i   );
      ( "<|",     fun i -> Parser.AS i      );
    ]
let createToken i str =
  try (Hashtbl.find rsvwords str) i
  with _ -> Parser.IDENT { i = i; v = Symbol.symbol(str) }

let lineno   = ref 1   (* 行番号 *)
and depth    = ref 0   (* コメントの入れ子深さ *)
and start    = ref 0   (* 行開始位置 *)

and filename = ref ""
and startLex = ref unknownInfo

let dummytoken lexbuf = Parser.EOF unknownInfo
let lexentry = ref dummytoken
let main lexbuf = !lexentry lexbuf

(* ファイルを準備 *)
let create file stream =
  filename := file;
  lineno   := 1;
  start    := 0;
  Lexing.from_channel stream

(** 改行処理 *)
let newline lexbuf =
  incr lineno; start := (Lexing.lexeme_start lexbuf)
(** ファイル情報生成 *)
let info lexbuf =
  createInfo (!filename) (!lineno) (Lexing.lexeme_start lexbuf - !start)
(** 文字列取出し *)
let text = Lexing.lexeme

(** 文字列リテラルの処理 *)
let strBuf = ref (String.create 2048)
let strEnd = ref 0
(** 文字列の終端処理 *)
let resetStr () = strEnd := 0
(** 文字列の追加 *)
let rec addStr ch =
  let buf = !strBuf in
    if !strEnd = String.length buf then
      let newBuf = String.create (!strEnd*2) (* Sys.max_string_lengthを越えると *)
      in                                     (* Invalid_argumentを投げる        *)
	String.blit buf 0 newBuf 0 !strEnd;
	strBuf := newBuf;
	addStr ch
    else (
      String.set buf !strEnd ch;
      incr strEnd
    )
(** 文字列の取得 *)
let getStr () = String.sub (!strBuf) 0 (!strEnd)

(** 16進文字列からバイト列へ変換 *)
let string_of_hex s = 
  let len = String.length s in
  let buf = Buffer.create (len/2) in
  let rec next_char s p =
    if p < len then
      let ch = String.sub s p 2 in
      let c = Char.chr (Scanf.sscanf ch "%x" (fun i -> i)) in
        Buffer.add_char buf c;
        next_char s (p+2)
  in
    if (len mod 2) <> 0 then
      raise (Invalid_argument "s");
    next_char s 0;
    Buffer.contents buf

}

let nondigit	= ['a'-'z' 'A'-'Z' '_']
let digit	= ['0'-'9']
let hex_digit	= ['0'-'9' 'a'-'f' 'A'-'F']
let separator = [' ' '\t' '\012']   (* スペース, タブ, 改ページ *)
let cr        = '\r'
let lf        = '\n'

(** 字句解析ルールの定義部 *)
rule token = parse
    separator+
      { token lexbuf }
  | separator* cr? lf
      { newline lexbuf; token lexbuf }
  | digit+
      { Parser.INTV { i = info lexbuf; v = int_of_string (text lexbuf) } }
  | "//" [^ '\n']* "\n"
      { newline lexbuf; token lexbuf }
  | "*/" { errorAt (info lexbuf) ERR_UNMATCH_COMMENT }
  | "/*" { depth := 1; startLex := info lexbuf; comment lexbuf; token lexbuf }

  | nondigit (nondigit|digit)*
      { createToken (info lexbuf) (text lexbuf) }

  | "&&" | "||" | ":=" | "==" | "!=" | ">=" | "<=" | "!=" | "->"
      { createToken (info lexbuf) (text lexbuf) }

  | ['@' '!' '?' '%' '*' '+' '-' '^' '/' '|' ';' ',' ':' '.' '_' '='
       '[' ']' '<' '>' '{' '}' '(' ')']
      { createToken (info lexbuf) (text lexbuf) }

  | "C{"	{ lexentry := cblock;
                resetStr(); startLex := info lexbuf; Parser.LCBLK !startLex }
  | '$'	{ resetStr(); startLex := info lexbuf; cblock lexbuf }
  | '"'	{ resetStr(); startLex := info lexbuf; string lexbuf }

  | eof	{ Parser.EOF (info lexbuf) }
  | _		{ errorAt (info lexbuf) (ERR_ILLEGAL_CHAR (text lexbuf)) }

(* Cインラインブロックの処理 *)
and cblock = parse
    "C}"	{ lexentry := ( fun lb -> lexentry := token;
                                        Parser.RCBLK (info lexbuf) );
                Parser.CFRAG(getStr())                                      }
  | '$'	{ lexentry := token; Parser.CFRAG (getStr())                  }
  | '\n'	{ addStr '\n'; newline lexbuf; cblock lexbuf                  }
  | eof	{ errorAt (!startLex) ERR_NONTERM_CBLOCK                      }
  | _		{ addStr (Lexing.lexeme_char lexbuf 0); cblock lexbuf         }

(* ブロックコメントの処理 *)
and comment = parse
    "/*"	{ depth := succ !depth; comment lexbuf                    }
  | "*/"	{ depth := pred !depth; if !depth > 0 then comment lexbuf }
  | eof	{ errorAt (!startLex) ERR_NONTERM_COMMENT                 }
  | [^ '\n']	{ comment lexbuf                                          }
  | "\n"	{ newline lexbuf; comment lexbuf                          }

(* 文字列処理 *)
and string = parse
    '"'	{ Parser.STRV { i = !startLex; v = getStr() }                 }
  | '"'('H'|'h')
		{ try
			Parser.STRV { i = !startLex; v = string_of_hex (getStr()) }
		  with
			Scanf.Scan_failure s ->
				errorAt (!startLex) (ERR_ILLEGAL_HEXCHAR s)
		}
  | '\\'	{ addStr (escaped lexbuf); string lexbuf                      }
  | '\n'	{ addStr '\n'; newline lexbuf; string lexbuf                  }
  | eof	{ errorAt (!startLex) ERR_NONTERM_STRING                      }
  | _		{ addStr (Lexing.lexeme_char lexbuf 0); string lexbuf         }

(* エスケープ文字の処理 *)
and escaped = parse
    'n'	{ '\n' }
  | 'r'	{ '\r' }
  | 't'	{ '\t' }
  | '\\'	{ '\\' }
  | '"'	{ '\034' }
  | '\''	{ '\'' }
  | digit digit digit
      {
	let x = int_of_string(text lexbuf) in
	  if x > 255 then
	    errorAt (info lexbuf) (ERR_ILLEGAL_OCTCHAR x)
	  else
	    Char.chr x
      }
  | [^ '"' '\\' 't' 'n' '\'']
      { errorAt (info lexbuf) (ERR_ILLEGAL_ESCAPE (text lexbuf)) }

{
let _ = lexentry := token
}
