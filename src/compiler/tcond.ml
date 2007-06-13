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
    ValZero of Label.t * Symbol.t option
  | ValNonz of Label.t * Symbol.t option (* C[l] <- val_of_l - 1 *)
  | CntZero of Label.t
  | CntNonz of Label.t (* C[l]-- *)
let compare = Pervasives.compare
let show = function
    ValZero(l,_)-> Printf.printf "ValZero("; Label.show l; print_string ")"
  | ValNonz(l,_)-> Printf.printf "ValNonz("; Label.show l; print_string ")"
  | CntZero(l) -> Printf.printf "CntZero("; Label.show l; print_string ")"
  | CntNonz(l) -> Printf.printf "CntNonz("; Label.show l; print_string ")"

(* アルファ変換 *)
let alpha lm = function
    ValZero(l,f) when Lm.mem (Label.deref l) lm -> ValZero(Lm.find (Label.deref l) lm,f)
  | ValNonz(l,f) when Lm.mem (Label.deref l) lm -> ValNonz(Lm.find (Label.deref l) lm,f)
  | CntZero(l) when Lm.mem (Label.deref l) lm -> CntZero(Lm.find (Label.deref l) lm)
  | CntNonz(l) when Lm.mem (Label.deref l) lm -> CntNonz(Lm.find (Label.deref l) lm)
  | tc -> tc
