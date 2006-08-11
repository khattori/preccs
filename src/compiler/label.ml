(**
   正規表現のラベル管理モジュール

   @author Hattori Kenta
   @version $Id: label.ml,v 1.2 2006/06/21 00:14:14 hattori Exp $
*)

type t = int
let compare = Pervasives.compare

let size_table:(t,int) Hashtbl.t = Hashtbl.create(13)

let lbl = ref 0
let create sz = let lid = !lbl in incr lbl; Hashtbl.add size_table lid sz; lid
let size l = Hashtbl.find size_table l
let show l = print_int l

(* ラベルID→ラベル番号への変換用 *)
type map = (t,int) Hashtbl.t * int ref
let map_create() = Hashtbl.create(13),ref 0
let map_find (tbl,num) lbl =
  if Hashtbl.mem tbl lbl then
    Hashtbl.find tbl lbl
  else
    let id = !num in
      Hashtbl.add tbl lbl id;
      incr num;
      id
let map_size (_,num) = !num
  

