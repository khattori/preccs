(**
   文字集合の位置モジュール

   @author Hattori Kenta
   @version $Id: pos.ml,v 1.2 2006/06/06 04:39:15 hattori Exp $
*)
module Ht = Hashtbl

type t = int
let compare = Pervasives.compare

let pos = ref 0
let posmap : (t, Cset.t) Ht.t = Ht.create 13
let pos2cs p = Ht.find posmap p
let create c = let p = !pos in Ht.add posmap p c; incr pos; p

