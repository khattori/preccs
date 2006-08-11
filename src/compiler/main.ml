(**
   Main���W���[��

   @author Hattori Kenta
   @version $Id: main.ml,v 1.9 2006/07/06 04:15:36 hattori Exp $
*)

open Error

let inputFiles = ref []      (* ���̓t�@�C�����̃��X�g *)
let outputFile = ref None    (* �o�̓t�@�C�����̎w��   *)
let debugMode  = ref false   (* �f�o�b�O�o�̓t���O     *)
let poutMode   = ref false   (* �Ύ��o�̓t���O         *)
let coutMode   = ref false   (* CPS���o�̓t���O        *)

let error s =
  print_string (Sys.executable_name ^ ": " ^ s); raise (Exit 1)

(* �o�[�W�������̒�` *)
let version = "2.0.0"
let verMsg  = "Preccs Compiler Ver." ^ version ^ ".\n" ^
  "Copyright (C) 2006 Kenta Hattori\n"

(* �g�p�@ *)
let usageMsg = "Usage: prcc <options> <source-files>\n" ^
  "Options:"

(* �R�}���h���C�������̒�` *)
let argDefs = [
  ( (* �o�[�W��������\�� *)
    "-v",
    Arg.Unit(fun () -> print_string verMsg; raise (Exit 0)),
    "Display version."
  );
  ( (* �f�o�b�O����ǉ� *)
    "-d",
    Arg.Set(debugMode),
    "Add debug info."
  );
  ( (* �Ύ��o�̓��[�h *)
    "-p",
    Arg.Set(poutMode),
    "Pexp output."
  );
  ( (* CPS���o�̓��[�h *)
    "-c",
    Arg.Set(coutMode),
    "Cexp output."
  );
  ( (* CPS���o�̓��[�h *)
    "-s",
    Arg.Set(Dfa.skipMode),
    "Matching with skipping."
  );
  ( (* �o�̓t�@�C�����w�� *)
    "-o",
    Arg.String(fun s -> outputFile := Some s),
    "Place the output file into <file>"
  );]

(** �R�}���h���C�������̉�� *)
let parseArgs () =
  Arg.parse argDefs
    (fun s -> inputFiles := s::!inputFiles)
    usageMsg;
  match !inputFiles with
      [] -> error "no input files"
    | _  -> inputFiles := List.rev !inputFiles

(** ���̓t�@�C���̉�� *)
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

(** �����{�� *)
let () =
  try 
    parseArgs();
    parseFiles()
  with Exit n -> exit n
