(**
   意味解析モジュール
   
   概要：型検査、型式の変換を行う

   @author Hattori Kenta
   @version $Id: semant.ml,v 1.6 2006/07/06 04:15:36 hattori Exp $
*)

(** 意味解析ルーチン *)
let translate defs =
  Closure.convert
    (Cps.etaReduc
       (Trans.trans
          (Pi.reducPar
             (Pi.removeUnused
                (Pi.reducComm
                   (Pi.trans (Check.check defs) defs))))))

