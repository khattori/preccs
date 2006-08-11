(**
   �L���\���W���[��

   @author Hattori Kenta
   @version $Id: symbol.ml,v 1.5 2006/05/25 06:33:57 hattori Exp $
*)

module Ht = Hashtbl
   
type t = string * int
let compare = Pervasives.compare

let nextsym = ref 0    (* �V���{���ԍ� *)
let symtbl:(string,int) Ht.t = Ht.create 2048

(** �����񂩂�V���{���𐶐� *)
let symbol name =
  try 
    name,Ht.find symtbl name
  with Not_found ->
    let i = !nextsym in
      incr nextsym; Ht.add symtbl name i; name,i

(** �V���{�����當��������o�� *)
let name (s,n) = s

(** �V���{���̓������� *)
let equal (s1,n1) (s2,n2) = n1==n2
