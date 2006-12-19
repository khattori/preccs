(**
   ��ԑJ�ڕ\���W���[��(��ԊǗ�)

   @author Hattori Kenta
   @version $Id: dtable.ml,v 1.3 2006/07/06 04:15:36 hattori Exp $
*)
module Ht = Hashtbl

module PosSet    = Set.Make(Pos)
module PosSetMap = Map.Make(PosSet)

type t = int
let compare = Pervasives.compare

let state = ref (-1)
let state_map = ref (PosSetMap.empty : t PosSetMap.t)
let state_map_r = Ht.create(13)  (* �t�ϊ��p: Dstate.t -> PosSet.t *)

(* ��ԏW�������Ԕԍ����擾���� *)
let of_posset ps =
  try
    PosSetMap.find ps !state_map
  with Not_found ->
    incr state;
    state_map := PosSetMap.add ps !state !state_map;
    Ht.add state_map_r !state ps;
    !state

(* ��Ԕԍ������ԏW�����擾���� *)
let to_posset st = Ht.find state_map_r st

(* �V������Ԕԍ���Ԃ� *)
let create() = incr state; !state

(* ��ԕ\�� *)
let show st =
  Printf.printf "[%d:" st;
  PosSet.iter (fun p -> Printf.printf "%d;" p) (to_posset st);
  print_string "]"

