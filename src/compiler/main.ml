(**
   Mainモジュール

   @author Hattori Kenta
   @version $Id: main.ml,v 1.9 2006/07/06 04:15:36 hattori Exp $
*)

open Error

let inputFiles = ref []      (* 入力ファイル名のリスト *)
let outputFile = ref None    (* 出力ファイル名の指定   *)
let debugMode  = ref false   (* デバッグ出力フラグ     *)

let error s =
  print_string (Sys.executable_name ^ ": " ^ s); raise (Exit 1)

(* 使用法 *)
let usageMsg = "Usage: prcc <options> <source-files>\n" ^
  "Options:"

(* コマンドライン引数の定義 *)
let argDefs = [
  ( (* バージョン情報を表示 *)
    "-v",
    Arg.Unit(fun () -> print_string Version.message; raise (Exit 0)),
    "Display version."
  );
  ( (* デバッグ情報を追加 *)
    "-d",
    Arg.Set(debugMode),
    "Add debug info."
  );
  ( (* π式出力モード *)
    "-p",
    Arg.Set(Semant.poutMode),
    "Pexp output."
  );
  ( (* CPS式出力モード *)
    "-c",
    Arg.Set(Semant.coutMode),
    "Cexp output."
  );
  ( (* スキップモード *)
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
  let files = Stack.create() in
  let head = ref "" in
  let synt = ref [] in
  let foot = ref "" in
    begin
      List.iter (fun f -> Stack.push f files) !inputFiles;
      while not (Stack.is_empty files)
      do
        let file = Stack.pop files in
        let inp = open_in file in
        let lexbuf = Lexer.create file inp in
          try
            let (fs,hd,sy,ft) = Parser.toplevel Lexer.main lexbuf in
              begin
                List.iter (fun f -> Stack.push f files) fs;
                head := hd ^ !head;
                synt := sy @ !synt;
                foot := ft ^ !foot;
                Parsing.clear_parser();
                close_in inp
              end
          with
	      Parsing.Parse_error -> errorAt (Lexer.info lexbuf) ERR_PARSE_ERROR
      done;
      Semant.translate !head !synt !foot
    end


(** 処理本体 *)
let () =
  try 
    parseArgs();
    parseFiles()
  with Exit n -> exit n
