(**
   �Ӗ���̓��W���[��
   
   �T�v�F�^�����A�^���̕ϊ����s��

   @author Hattori Kenta
   @version $Id: semant.ml,v 1.6 2006/07/06 04:15:36 hattori Exp $
*)

(** �Ӗ���̓��[�`�� *)
let translate defs =
  Closure.convert
    (Cps.etaReduc
       (Trans.trans
          (Pi.reducPar
             (Pi.removeUnused
                (Pi.reducComm
                   (Pi.trans (Check.check defs) defs))))))

