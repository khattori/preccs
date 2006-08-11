(**
   Mainモジュール

   @author Hattori Kenta
   @version $Id: main.ml,v 1.9 2006/07/06 04:15:36 hattori Exp $
*)

open Error

let inputFiles = ref []      (* 入力ファイル名のリスト *)
let outputFile = ref None    (* 出力ファイル名の指定   *)
let debugMode  = ref false   (* デバッグ出力フラグ     *)
let poutMode   = ref false   (* π式出力フラグ         *)
let coutMode   = ref false   (* CPS式出力フラグ        *)

let error s =
  print_string (Sys.executable_name ^ ": " ^ s); raise (Exit 1)

(* バージョン情報の定義 *)
let version = "2.0.0"
let verMsg  = "Preccs Compiler Ver." ^ version ^ ".\n" ^
  "Copyright (C) 2006 Kenta Hattori\n"

(* 使用法 *)
let usageMsg = "Usage: prcc <options> <source-files>\n" ^
  "Options:"

(* コマンドライン引数の定義 *)
let argDefs = [
  ( (* バージョン情報を表示 *)
    "-v",
    Arg.Unit(fun () -> print_string verMsg; raise (Exit 0)),
    "Display version."
  );
  ( (* デバッグ情報を追加 *)
    "-d",
    Arg.Set(debugMode),
    "Add debug info."
  );
  ( (* π式出力モード *)
    "-p",
    Arg.Set(poutMode),
    "Pexp output."
  );
  ( (* CPS式出力モード *)
    "-c",
    Arg.Set(coutMode),
    "Cexp output."
  );
  ( (* CPS式出力モード *)
    "-s",
    Arg.Set(Dfa.skipMode),
    "Matching with skipping."
  );
  ( (* 出力ファイルを指定 *)
    "-o",
    Arg.String(fun s -> outputFile := Some s),
    "Place the output file into <file>"
  );]

(** コマンドライン引数の解析 *)
let parseArgs () =
  Arg.parse argDefs
    (fun s -> inputFiles := s::!inputFiles)
    usageMsg;
  match !inputFiles with
      [] -> error "no input files"
    | _  -> inputFiles := List.rev !inputFiles

(** 入力ファイルの解析 *)
let parseFiles () = 
  let parse file =
    let inp = open_in file in
    let lexbuf = Lexer.create file inp in
    let result =
      try 
        let (head,abs,foot) = Parser.toplevel Lexer.main lexbuf in
          if (!poutMode) then
            Pi.show_proc
              (Pi.reducPar
                 (Pi.removeUnused
                    (Pi.reducComm
                       (Pi.trans (Check.check abs) abs))))
          else if (!coutMode) then
            print_string (
              Cps.showCexp
                (Cps.etaReduc
                   (Trans.trans
                      (Pi.reducPar
                         (Pi.removeUnused
                            (Pi.reducComm
                               (Pi.trans (Check.check abs) abs)))))))
          else
            let cexp = Semant.translate abs in
              Closure.attachFv cexp;
              print_string head;
              Emit.emit Rmap.empty cexp;
              print_string foot

(*
  Printf.printf "%s\n"
  (Cps.showCexp (Trans.trans (Pi.trans (Check.check abs) abs)))
*)
      with
	  Parsing.Parse_error -> errorAt (Lexer.info lexbuf) ERR_PARSE_ERROR
    in
      Parsing.clear_parser(); close_in inp
  in
    List.iter parse !inputFiles

(** 処理本体 *)
let () =
  try 
    parseArgs();
    parseFiles()
  with Exit n -> exit n
