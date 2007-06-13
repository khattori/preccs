(**
   型情報モジュール

   @author Hattori Kenta
   @version $Id: types.ml,v 1.10 2006/07/27 00:07:18 hattori Exp $
*)
module R  = Regex
module Ht = Hashtbl

exception Ill_field
exception Ill_proj
exception Ill_index
exception Ill_deriv

(** 型情報の定義 *)
type t =
    UNIT | BOOL | INT | STRING       (* 基本型     *)
  | CHAN   of t                      (* チャネル型 *)
  | ARRAY  of t * int                (* 配列型     *)
  | RECORD of (Symbol.t * t) list    (* レコード型 *)
  | TUPLE  of t list                 (* 組型       *)
  | REGEX  of rgx                    (* 正規表現型 *)
and rgx = 
    REXP of Cset.t Regex.t
  | RARR of rgx * int                (* R配列       *)
  | RITR of rgx * Symbol.t * Symbol.t option (* R可変長配列 *)
  | RRCD of (Symbol.t * rgx) list    (* Rレコード   *)

(** フィールド型定義 *)
type field = {
  label : Label.t;
  child : field list;
}

let rec show_field_list fs =
  print_string "{";
  List.iter (
    fun f ->
      Label.show f.label;
      if f.child == [] then () else show_field_list f.child;
      print_string ";"
  ) fs;
  print_string "}"

let rec encode_field_list lmap = function
    [] -> ""
  | fs ->
      let n = List.length fs in
        List.fold_left (
          fun s f -> 
            s 
            ^ (Printf.sprintf "S\\x%02x" (Label.map_find lmap f.label))
            ^ (encode_field_list lmap f.child)
        ) (Printf.sprintf "\\x43\\x%02x" n) fs

(** ラベルを検索 *)
let field typ sym = 
  match typ with 
      RECORD fs      -> snd (List.find (fun (s,_) -> Symbol.equal sym s) fs)
    | REGEX(RRCD fs) -> REGEX(snd (List.find (fun (s,r) -> Symbol.equal sym s) fs))
    | _ -> raise Ill_field

(** 組の要素を取得 *)
let proj typ i =
  match typ with
      TUPLE ts -> List.nth ts i
    | _ -> raise Ill_proj

(** チャネル型か判定する *)
let is_chan = function CHAN _ -> true | _ -> false

let is_string = function STRING | REGEX _ -> true | _ -> false

(** オフセットを取得 *)
let offset typ sym =
  (* ('a -> bool) -> 'a list -> int *)
  let off f ls =
    let rec loop i = function
        []     -> raise Not_found
      | hd::tl -> if f hd then i else loop (i+1) tl
    in
      loop 0 ls in

    match typ with 
        RECORD fs -> off (fun (s,_) -> Symbol.equal sym s) fs
      | REGEX(RRCD fs) -> (off (fun (s,_) -> Symbol.equal sym s) fs) * 3
      | _ -> raise Ill_field

(** 配列要素を検索 *)
let index typ =
  match typ with
      ARRAY(t,_)                          -> t
    | REGEX(RARR(r,_)) | REGEX(RITR(r,_,_)) -> REGEX r
    | _ -> raise Ill_index

(** 不要なラベルの削除 *)
let rec rmlabel =
  let ltbl = Ht.create(13) in
  let rec rm keep = function
      R.EPS        -> R.EPS
    | R.CHARS(c)   -> R.CHARS(c)
    | R.SEQ(r1,r2) ->
        let r2' = rm keep r2 in
        let r1' = rm keep r1 in
          R.SEQ(r1',r2')
    | R.ALT(r1,r2) -> R.ALT(rm false r1,rm false r2)
    | R.CLOS(r)    -> R.CLOS(rm false r)
    | R.LBL(r,l)   -> 
        if keep || Ht.mem ltbl l then R.LBL(rm keep r,l) else rm keep r
    | R.REP(r,l,f) -> Ht.add ltbl (Label.deref l) true; R.REP(rm false r,l,f)
  in
    rm true

(* 正規表現型に変換 *)
let regexify rt = 
  let rec trans tbl = function
    | REXP re   -> re
    | RARR(r,n) -> R.array (trans tbl r) n
    | RITR(r,s,f) -> (
        try
          let l = List.assoc s tbl in R.REP(trans tbl r,Label.mkref l,f)
        with Not_found -> R.CLOS(trans tbl r)
      )
    | RRCD rs   ->
        let re,_ =
          List.fold_left (
            fun (re',tbl') (s,r) ->
              let re'' = trans tbl' r in
              let lbl = Label.create (R.size re'') in
              let tbl'' = (s,lbl)::tbl' in R.SEQ(re',R.LBL(re'',lbl)),tbl''
          ) (R.EPS,[]) rs
        in re
  in
    rmlabel (trans [] rt)

(* 正規表現型への変換とラベルの変換を返す *)
let regexify2 rt = 
  let rec trans tbl = function
    | REXP re   -> re,[]
    | RARR(r,n) -> R.array (fst (trans tbl r)) n,[] (* 配列はキープせず *)
    | RITR(r,s,f) ->
        let l = List.assoc s tbl in R.REP(fst (trans tbl r),Label.mkref l,f),[]
    | RRCD rs   ->
        let re,_,fs =
          List.fold_left (
            fun (re',tbl',fs') (s,r) ->
              let re'',fs'' = trans tbl' r in
              let lbl = Label.create (R.size re'') in
              let tbl'' = (s,lbl)::tbl' in
              let fld = {label=lbl; child=fs'';} in
                R.SEQ(re',R.LBL(re'',lbl)),tbl'',(fld::fs')
          ) (R.EPS,[],[]) rs
        in re,List.rev fs
  in
  let re,fs = trans [] rt in rmlabel re,fs

(** 部分型判定
    t1 <: t2かどうかを判定
*)
let rec subtype ty1 ty2 =
  match ty1,ty2 with
      UNIT,UNIT | BOOL,BOOL | INT,INT | STRING,STRING -> true
    | ARRAY(t1,n),ARRAY(t2,m) when n==m -> subtype t1 t2
    | RECORD(fs1),RECORD(fs2) when List.length fs1 = List.length fs2 -> 
        List.fold_left2
          (fun b (s1,t1) (s2,t2) -> b && (Symbol.equal s1 s2) && (subtype t1 t2))
          true fs1 fs2
    | CHAN(t1),CHAN(t2)   -> eqtype t1 t2 (* invariantにする必要あり *)
    | REGEX(r1),REGEX(r2) -> rsubtype r1 r2
    | REGEX(_),STRING     -> true
    | _                   -> false

(** 正規表現型の部分型判定 *)
and rsubtype r1 r2 =
  match r1,r2 with
      RARR(r1',n),RARR(r2',m) when n==m -> rsubtype r1' r2'
    | RITR(r1',s1,None),RITR(r2',s2,None) when Symbol.equal s1 s2 -> rsubtype r1' r2'
    | RITR(r1',s1,Some f1),RITR(r2',s2,Some f2) when Symbol.equal s1 s2 && Symbol.equal f1 f2 -> rsubtype r1' r2'
    | RRCD(rs1),RRCD(rs2) when List.length rs1 = List.length rs2 ->
        List.fold_left2
          (fun b (s1,r1') (s2,r2') -> b&&(Symbol.equal s1 s2)&&(rsubtype r1' r2'))
          true rs1 rs2
    | REXP(re1),REXP(re2) -> Subset.subset re1 re2
    | _,REXP(re)          -> Subset.subset (regexify r1) re
    | _                   -> false

(** 型等価判定 *)
and eqtype x y = (subtype x y) && (subtype y x)

let get_rgx = function REGEX(re) -> re | _ -> assert false

(** 派生型の生成 *)
let deriv ty path ty2 =
  let rec trav ap t =
    if ap==[] then ty2
    else match t with
        UNIT | BOOL | INT | STRING | CHAN(_) | ARRAY(_,_) | TUPLE _ -> t
      | RECORD(sts) -> RECORD(List.map (fun (s,t') ->
                                          if Symbol.equal (List.hd ap) s then
                                            s,trav (List.tl ap) t'
                                          else
                                            s,t') sts)
      | REGEX(r1) ->
          let rec rtrav ap r =
            if ap==[] then (get_rgx ty2)
            else match r with
               RRCD rs -> RRCD(List.map (fun (s,rt') ->
                                           if Symbol.equal (List.hd ap) s then
                                             s,rtrav (List.tl ap) rt'
                                           else
                                             s,rt') rs)
              | _ -> r
          in REGEX(rtrav ap r1)
  in
    trav path ty 
