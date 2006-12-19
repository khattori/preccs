(**
   正規表現モジュール

   @author Hattori Kenta
   @version $Id: regex.ml,v 1.8 2006/07/27 00:07:18 hattori Exp $
*)

module Ht = Hashtbl

(** 正規表現の定義 *)
(** NOTE: アルファベットを型抽象するという手もある *)
type 'a t =
    EPS
  | CHARS of 'a
  | SEQ   of 'a t * 'a t
  | ALT   of 'a t * 'a t
  | CLOS  of 'a t
  | LBL   of 'a t * Label.t
  | REP   of 'a t * Label.t (* 参照先インデックス *)

(** 正規表現のサイズを取得 *)
let size re =
  let rec trav = function
      EPS      -> 0
    | CHARS _  -> 1
    | SEQ(r1,r2) -> (trav r1) + (trav r2)
    | ALT(r1,r2) ->
        let s1 = trav r1 in
        let s2 = trav r2 in
          if s1 == s2 then s1 else raise Exit
    | CLOS r   -> if trav r == 0 then 0 else raise Exit
    | LBL(r,_) -> trav r
    | REP(_,_) -> raise Exit
  in
    try trav re with Exit -> 0

(** 文字列を正規表現型に変換 *)
let of_string s =
  let result = ref EPS in
    for i = String.length s-1 downto 0 do
      result := SEQ(CHARS(Cset.singleton(String.get s i)),!result)
    done;
    !result

(** 文字クラスを正規表現型に変換 *)
let of_chrcls s =
  let l = String.length s in
    if l == 0 then EPS
    else
      let i = ref 0 in
      let cmpl = ref false in
      let prev = ref false in
      let cs = ref Cset.empty in
        if (String.get s 0) == '^' then ( incr i; cmpl := true ); (* 先頭が^ *)
        while !i < l do
          let c = String.get s !i in
            if !prev && c=='-' && !i+1 < l then
              ( for j = int_of_char (String.get s (!i-1)) to int_of_char (String.get s(!i+1)) do
                  cs := Cset.add j !cs 
                done;
                prev := false;
                i := !i + 2
              )
            else
              ( cs := Cset.add (int_of_char (String.get s !i)) !cs;
                prev := true;
                incr i;
              )
        done;
        if !cmpl then cs := Cset.cmpl !cs;
        if Cset.is_empty !cs then EPS else CHARS(!cs)
    
let oct = CHARS(Cset.all)
let any = CLOS(oct)

let pclos r = SEQ(r,CLOS r)
let opt   r = ALT(EPS,r)

let rec array r n =
  if      n<=0 then assert false
  else if n==1 then r
  else              SEQ(r,array r (n-1))

(** 正規表現リストの要素をALTで連結する *)
let concat rs =
  List.fold_left (fun r' r -> ALT(r',r)) (List.hd rs) (List.tl rs)

(** 位置情報を付加したものを取得する *)
let rec posify = function
    EPS        -> EPS
  | CHARS(c)   -> CHARS(Pos.create c)
  | SEQ(r1,r2) -> SEQ(posify r1,posify r2)
  | ALT(r1,r2) -> ALT(posify r1,posify r2)
  | CLOS(r)    -> CLOS(posify r)
  | LBL(r,l)   -> LBL(posify r,l)
  | REP(r,l)   -> REP(posify r,l)

(** 正規表現から文字列を取得 *)
let rec to_string = function
    CHARS cs -> Printf.sprintf "%c" (Cset.get_char cs)
  | SEQ(r1,r2) -> to_string(r1) ^ to_string(r2)
  | ALT(r,_) | LBL(r,_) -> to_string r
  | EPS | CLOS _ | REP(_,_) -> ""

let rec show = function
    EPS        -> print_string "EPS"
  | CHARS _    -> print_string "CHARS"
  | SEQ(r1,r2) ->
      print_string "SEQ(";
      show r1; print_string ","; show r2;
      print_string ")" 
  | ALT(r1,r2) ->
      print_string "ALT(";
      show r1; print_string ","; show r2;
      print_string ")" 
  | CLOS(r)  -> print_string "CLOS("; show r; print_string ")"
  | LBL(r,l) ->
      print_string "LBL(";
      show r; print_string ","; Label.show l;
      print_string ")"
  | REP(r,l) ->
      print_string "REP(";
      show r; print_string ","; Label.show l;
      print_string ")"
