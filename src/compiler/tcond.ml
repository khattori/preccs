(**
   モジュール

   @author Hattori Kenta
   @version $Id: tcond.ml,v 1.2 2006/06/21 00:14:14 hattori Exp $
*)
module P = Prop
module C = Cond
module Lm = Map.Make(Label)

(* 遷移条件 *)
type t =
    ValZero of Label.t
  | ValNonz of Label.t (* C[l] <- val_of_l - 1 *)
  | CntZero of Label.t
  | CntNonz of Label.t (* C[l]-- *)
let compare = Pervasives.compare
let show = function
    ValZero(l) -> Printf.printf "ValZero("; Label.show l; print_string ")"
  | ValNonz(l) -> Printf.printf "ValNonz("; Label.show l; print_string ")"
  | CntZero(l) -> Printf.printf "CntZero("; Label.show l; print_string ")"
  | CntNonz(l) -> Printf.printf "CntNonz("; Label.show l; print_string ")"

(* アルファ変換 *)
let alpha lm = function
    ValZero(l) when Lm.mem l lm -> ValZero(Lm.find l lm)
  | ValNonz(l) when Lm.mem l lm -> ValNonz(Lm.find l lm)
  | CntZero(l) when Lm.mem l lm -> CntZero(Lm.find l lm)
  | CntNonz(l) when Lm.mem l lm -> CntNonz(Lm.find l lm)
  | tc -> tc

