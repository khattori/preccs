(**
   意味解析モジュール
   
   概要：型検査、型式の変換を行う

   @author Hattori Kenta
   @version $Id: semant.ml,v 1.6 2006/07/06 04:15:36 hattori Exp $
*)

module E = Exception

let poutMode   = ref false   (* �式出力フラグ         *)
let coutMode   = ref false   (* CPS式出力フラグ        *)

(** 意味解析ルーチン *)
let translate head defs foot =
  let pexp = ref Pi.Stop in
  let cexp = ref Cps.disp in
    try begin
      pexp := Pi.reducPar
        (Pi.removeUnused
           (Pi.reducComm
              (Ptrans.trans (Check.check defs) defs)));
      if !poutMode then 
        begin
          Pi.show_proc !pexp;
          raise E.Return
        end;
      cexp := Closure.convert (Cps.etaReduc (Trans.trans !pexp));
      if !coutMode then
        begin 
          print_string (Cps.showCexp !cexp);
          raise E.Return
        end;
      Closure.attachFv !cexp;
      print_string head;
      Emit.emit Rmap.empty !cexp;
      print_string foot
    end
    with E.Return -> ()
      
      
