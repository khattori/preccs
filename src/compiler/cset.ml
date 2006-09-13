(**
   文字集合モジュール

   @author Hattori Kenta
   @version $Id: cset.ml,v 1.6 2006/07/27 00:07:17 hattori Exp $
*)

type t = (int * int) list

let compare = Pervasives.compare
let empty = []
let is_empty s = s == []
let isEmpty = function
    [] -> true
  | _  -> false

(** サブセットの判定 S1⊆S2 *)
let rec subset s1 s2 =
  match s1,s2 with
      [],_ -> true
    | _,[] -> false
    | (c1,d1)::r1,(c2,d2)::r2 ->
        if c1 > d2 then subset s1 r2
        else if c1 < c2 || d1 > d2 then false
        else subset r1 s2

(** 和集合の計算S1∪S2 *)
let rec union s1 s2 =
  match s1,s2 with
      [],_ -> s2
    | _,[] -> s1
    | (c1,d1) as p1::r1, (c2,d2)::r2 ->
        if c1 > c2 then union s2 s1
        else (
          if d1+1 < c2 then
            p1::union r1 s2
          else if d1 < d2 then
            union ((c1,d2)::r2) r1
          else
            union s1 r2
        )

(** 積集合の計算S1∩S2 *)
let rec inter s1 s2 =
  match s1,s2 with
      [],_ | _,[] -> []
    | (c1,_)::_, (c2,_)::_ when c1 > c2 -> inter s2 s1
    | (c1,d1)::r1, (c2,d2)::r2 -> (* c1 <= c2 *)
        if d1 < c2 then
          inter r1 s2
        else if d1 < d2 then
          (c2,d1)::inter r1 ((d1+1,d2)::r2)
        else
          (c2,d2)::inter r2 ((d2+1,d1)::r1)

(** 文字集合に文字が含まれているか *)
let rec mem c = function
    []         -> false
  | (c1,c2)::r ->
      if c < c1 then false
      else if c <= c2 then true
      else mem c r

(** 文字を追加 *)
let add c s = union [c,c] s

  
(** 文字集合の各要素に対して処理を行う *)
let rec iter f = function
    []       -> ()
  | (c,d)::r -> (f c); if c+1 <= d then iter f ((c+1,d)::r) else iter f r

(** 文字集合の各要素の畳み込み *)
let rec fold f a = function
    [] -> a
  | (c,d)::r ->
      let a' = (f a c) in
        if c+1 <= d then fold f a' ((c+1,d)::r) else fold f a' r

(** 補集合の計算 ^S *)
let cmpl s =
  let s' = ref empty in
    for i = 0 to 255 do
      if not (mem i s) then s' := add i !s'
    done;
    !s'

(** 文字集合のエンコード *)
let encode cs =
  let enc n =
    let num = ref Int32.zero in
      for i = 0 to 31 do
        if mem (n*32+i) cs then
          num := Int32.logor !num (Int32.shift_left Int32.one i)
      done;
      !num
  in
    Printf.sprintf
      "\n\t\t{0x%08lx,0x%08lx,0x%08lx,0x%08lx,\n\t\t 0x%08lx,0x%08lx,0x%08lx,0x%08lx}"
      (enc 0) (enc 1) (enc 2) (enc 3) (enc 4) (enc 5) (enc 6) (enc 7)


(** 一つ文字を取得 *)
let get_char = function
    [] -> assert false
  | (c,_)::_ -> char_of_int c
  
let singleton ch = let c = Char.code ch in [c,c]
let create c = [c,c]

let all = [0,255]
let fin = [-1,-1]

let show = List.iter (fun (c1,c2) -> Printf.printf "(%d-%d)" c1 c2)
