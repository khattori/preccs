(**
   CPSモジュール

   @author Hattori Kenta
   @version $Id: cps.ml,v 1.7 2006/07/27 00:07:17 hattori Exp $
*)

module S = Symbol

(** 拡張CPS式 *)
type cexp' = cexp * Symbol.t list ref

(** CPS式の定義 *)
and cexp =
    Prim of prim * value list * lvar list * cexp' list
  | App  of value * value list
  | Fix  of bind list * cexp'
  | Cblk of string list * value list * cexp'

(* プリミティブ演算子 *)
and prim =
    Disp          (* ディスパッチ: ()->() *)
  | If            (* 条件分岐 *)
  | Switch        (* 値分岐   *)
  | Match         (* パターンマッチ: (DFA ID, String val)->lvar list*)
  | Set           (* 値セット: (lvar, value) *)
  | New           (* チャネル生成 *)
  | Alloc
  | Record | Rexrcd | Select | Offset
  | Add | Sub | Mul | Div | Mod
  | Eq  | Neq | Eqs | Lt  | Leq | Gt  | Geq
  | And | Or  | Cat
  | Neg | Not

and value =
    Var    of lvar
  | Label  of lvar
  | Bool   of bool
  | Int    of int
  | String of string
  | Cint   of int       (* Cの整数 *)

and bind = lvar * lvar list * cexp'

and lvar = Symbol.t

(** 識別子生成 *)
let counter = ref 0
let genid s =
  incr counter;
  S.symbol (Printf.sprintf "%s%d" s !counter)

(* let disp:cexp' = Prim(Disp,[],[],[]),ref [] *)
let disp:cexp' = App(Label(S.symbol "disp"),[]),ref []

(** CPS式→文字列：デバッグ表示用 *)
let shows f = function
    [] -> "[]"
  | [l] -> "["^ f l ^"]"
  | l::ls -> "["^ f l ^ (List.fold_left (fun s c -> s ^";"^ (f c)) "" ls) ^ "]"

let rec showCexp (cexp,fv) =
  match cexp with
      Prim(p,vs,lvs,cs) ->
        "Prim(" ^showPrim p ^","^ shows showValue vs ^","^ shows showLvar lvs ^
          "," ^ shows showCexp cs ^ ")"
    | App(v,vs) ->
        "App(" ^showValue v ^","^ shows showValue vs ^ ")"
    | Fix(bs,c) ->
        "Fix(" ^shows showBind bs ^","^ showCexp c ^ ")"
    | Cblk(cs,vs,c) ->
        "Cblk({" ^ ( List.fold_left2
                       (fun s c v -> s^showValue v^c)
                       (List.hd cs) (List.tl cs) vs ) ^ "}," ^ showCexp c^")"

        
and showPrim = function
    Disp   -> "Disp"
  | If     -> "If"
  | Switch -> "Switch"
  | Match  -> "Match"
  | Set    -> "Set"
  | New    -> "New"
  | Record -> "Record"
  | Rexrcd -> "Rexrcd"
  | Select -> "Select"
  | Offset -> "Offset"
  | Alloc  -> "Alloc"
  | Add -> "Add" | Sub -> "Sub" | Mul -> "Mul" | Div -> "Div" | Mod -> "Mod"
  | Eq -> "Eq" | Neq -> "Neq" | Eqs -> "Eqs"
  | Lt -> "Lt" | Leq -> "Leq" | Gt -> "Gt"  | Geq -> "Geq"
  | And -> "And" | Or -> "Or" | Cat -> "Cat"
  | Neg -> "Neg" | Not -> "Not"
and showValue = function
    Var l    -> "Var(" ^ showLvar l ^ ")"
  | Label l  -> "Label(" ^ showLvar l ^ ")"
  | Bool b   -> "Bool(" ^ string_of_bool b ^ ")"
  | Int i    -> "Int(" ^ string_of_int i ^ ")"
  | Cint i   -> "Cint(" ^ string_of_int i ^ ")"
  | String s -> "String(\"" ^ s ^ "\")"
and showLvar (s,_) = "\"" ^ s ^ "\""
and showBind (lv,lvs,c) =
  "(" ^ showLvar lv ^","^ shows showLvar lvs ^","^ showCexp c ^ ")"

(** η簡約 *)
let rec etaReduc = function
    Fix(bs,c),_          -> etaReducFbs [] (etaReduc c) bs
  | Prim(p,ops,rs,cs),_  -> Prim(p,ops,rs,List.map etaReduc cs),ref []
  | c                    -> c
and etaReducFbs bsr c = function
  | (f,xs,(App(g,ys),_))::r
      when (List.length xs)=(List.length ys)
        && (List.for_all2 ( fun x y -> (Var x) = y) xs ys)
        -> etaReducFbs bsr (subst f g c) r
  | (f,xs,c')::r -> etaReducFbs ((f,xs,etaReduc c')::bsr) c r
  | [] -> if bsr=[] then c else Fix(List.rev bsr,c),ref []

(** η簡約のための置換関数 *)
and subst a b = function
    Prim(p,ops,rs,cs),_ 
      -> Prim(p,(List.map (substOp a b) ops),rs,List.map (subst a b) cs), ref []
  | Fix(bs,c'),_ -> Fix(List.map (substBind a b) bs, subst a b c'), ref []
  | App(f,vs),_  -> App(substOp a b f, List.map (substOp a b) vs), ref []
  | Cblk(cs,vs,c'),_ -> Cblk(cs,vs,subst a b c'),ref []
and substOp a b = function
    Var s when s=a -> b
  | c -> c
and substBind a b (f,xs,c') = f,xs,subst a b c'
