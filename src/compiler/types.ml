(**
   Œ^î•ñƒ‚ƒWƒ…[ƒ‹

   @author Hattori Kenta
   @version $Id: types.ml,v 1.10 2006/07/27 00:07:18 hattori Exp $
*)
module R  = Regex
module Ht = Hashtbl

exception Ill_field
exception Ill_proj
exception Ill_index
exception Ill_deriv

(** Œ^î•ñ‚Ì’è‹` *)
type t =
    BOOL | INT | STRING              (* Šî–{Œ^     *)
  | CHAN   of t                      (* ƒ`ƒƒƒlƒ‹Œ^ *)
  | ARRAY  of t * int                (* ”z—ñŒ^     *)
  | RECORD of (Symbol.t * t) list    (* ƒŒƒR[ƒhŒ^ *)
  | TUPLE  of t list                 (* ‘gŒ^       *)
  | REGEX  of rgx                    (* ³‹K•\Œ»Œ^ *)
and rgx = 
    REXP of Cset.t Regex.t
  | RARR of rgx * int                (* R”z—ñ       *)
  | RITR of rgx * Symbol.t           (* R‰Â•Ï’·”z—ñ *)
  | RRCD of (Symbol.t * rgx) list    (* RƒŒƒR[ƒh   *)

(** ƒtƒB[ƒ‹ƒhŒ^’è‹` *)
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

(** ƒ‰ƒxƒ‹‚ğŒŸõ *)
let field typ sym = 
  match typ with 
      RECORD fs      -> snd (List.find (fun (s,_) -> Symbol.equal sym s) fs)
    | REGEX(RRCD fs) -> REGEX(snd (List.find (fun (s,r) -> Symbol.equal sym s) fs))
    | _ -> raise Ill_field

(** ‘g‚Ì—v‘f‚ğæ“¾ *)
let proj typ i =
  match typ with
      TUPLE ts -> List.nth ts i
    | _ -> raise Ill_proj

(** ƒ`ƒƒƒlƒ‹Œ^‚©”»’è‚·‚é *)
let is_chan = function CHAN _ -> true | _ -> false

(** ƒIƒtƒZƒbƒg‚ğæ“¾ *)
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

(** ”z—ñ—v‘f‚ğŒŸõ *)
let index typ =
  match typ with
      ARRAY(t,_)                          -> t
    | REGEX(RARR(r,_)) | REGEX(RITR(r,_)) -> REGEX r
    | _ -> raise Ill_index

(** •s—v‚Èƒ‰ƒxƒ‹‚Ìíœ *)
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
    | R.REP(r,l)   -> Ht.add ltbl l true; R.REP(rm false r,l)
  in
    rm true

(* ³‹K•\Œ»Œ^‚É•ÏŠ· *)
let regexify rt = 
  let rec trans tbl = function
    | REXP re   -> re
    | RARR(r,n) -> R.array (trans tbl r) n
    | RITR(r,s) ->
        let l = List.assoc s tbl in R.REP(trans tbl r,l)
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

(* ³‹K•\Œ»Œ^‚Ö‚Ì•ÏŠ·‚Æƒ‰ƒxƒ‹‚Ì•ÏŠ·‚ğ•Ô‚· *)
let regexify2 rt = 
  let rec trans tbl = function
    | REXP re   -> re,[]
    | RARR(r,n) -> R.array (fst (trans tbl r)) n,[] (* ”z—ñ‚ÍƒL[ƒv‚¹‚¸ *)
    | RITR(r,s) ->
        let l = List.assoc s tbl in R.REP(fst (trans tbl r),l),[]
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

(** •”•ªŒ^”»’è
    t1 <: t2‚©‚Ç‚¤‚©‚ğ”»’è
*)
let rec subtype ty1 ty2 =
  match ty1,ty2 with
      BOOL,BOOL | INT,INT | STRING,STRING -> true
    | ARRAY(t1,n),ARRAY(t2,m) when n==m -> subtype t1 t2
    | RECORD(fs1),RECORD(fs2) when List.length fs1 = List.length fs2 -> 
        List.fold_left2
          (fun b (s1,t1) (s2,t2) -> b && (Symbol.equal s1 s2) && (subtype t1 t2))
          true fs1 fs2
    | CHAN(t1),CHAN(t2)   -> eqtype t1 t2 (* invariant‚É‚·‚é•K—v‚ ‚è *)
    | REGEX(r1),REGEX(r2) -> rsubtype r1 r2
    | REGEX(_),STRING     -> true
    | _                   -> false

(** ³‹K•\Œ»Œ^‚Ì•”•ªŒ^”»’è *)
and rsubtype r1 r2 =
  match r1,r2 with
      RARR(r1',n),RARR(r2',m) when n==m -> rsubtype r1' r2'
    | RITR(r1',s1),RITR(r2',s2) when Symbol.equal s1 s2 -> rsubtype r1' r2'
    | RRCD(rs1),RRCD(rs2) when List.length rs1 = List.length rs2 ->
        List.fold_left2
          (fun b (s1,r1') (s2,r2') -> b&&(Symbol.equal s1 s2)&&(rsubtype r1' r2'))
          true rs1 rs2
    | REXP(re1),REXP(re2) -> Subset.subset re1 re2
    | _,REXP(re)          -> Subset.subset (regexify r1) re
    | _                   -> false

(** Œ^“™‰¿”»’è *)
and eqtype x y = (subtype x y) && (subtype y x)

let get_rgx = function REGEX(re) -> re | _ -> assert false

(** ”h¶Œ^‚Ì¶¬ *)
let deriv ty path ty2 =
  let rec trav ap t =
    if ap==[] then ty2
    else match t with
        BOOL | INT | STRING | CHAN(_) | ARRAY(_,_) | TUPLE _ -> t
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

