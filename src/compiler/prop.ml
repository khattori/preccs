(**
   命題論理モジュール

   @author Hattori Kenta
   @version $Id: prop.ml,v 1.4 2006/06/06 04:39:15 hattori Exp $
*)

(*
 * 命題論理式の型定義（'aは命題変数の型）
 *)
type 'a t =
    Atom of 'a
  | Neg  of 'a t
  | Conj of 'a t * 'a t
  | Disj of 'a t * 'a t

(*
 * 含意を構成する：p→q ≡ ￢p∨q
 *)
let implies p q = Disj(Neg(p),q)

(*
 * トートロジーチェッカー：
 *     Wangのアルゴリズムにより，与えられた命題論理式の恒真判定を行う
 * 
 *   引　数：p : 'a Prop.t --- 命題論理式
 * 
 *   戻り値：恒真ならtrue，そうでなければfalseを返す
 * 
 *)
let taut p = 
  let rec common = function
      [], s      -> false
    | (hd::tl),s -> if List.exists ((=) hd) s then true else common(tl,s) in
  let rec wang = function
      ([], la, [], ra) -> common (la, ra)
   |  (lt, la, Atom(a)::tl, ra) -> wang(lt, la, tl, Atom(a)::ra)
   |  (Atom(a)::tl, la, rt, ra) -> wang(tl, Atom(a)::la, rt, ra)
   |  (lt, la, Neg(a)::tl, ra) -> wang(a::lt, la, tl, ra)         (* ￢右 *)
   |  (Neg(a)::tl, la, rt, ra) -> wang(tl, la, a::rt, ra)         (* ￢左 *)
   |  (lt, la, Conj(a,b)::tl, ra) -> wang(lt, la, a::tl, ra)
                                   & wang(lt, la, b::tl, ra)      (* ∧右 *)
   |  (Conj(a,b)::tl, la, rt, ra) -> wang(a::b::tl, la, rt, ra)   (* ∧左 *)
   |  (lt, la, Disj(a,b)::tl, ra) -> wang(lt, la, a::b::tl, ra)   (* ∨右 *)
   |  (Disj(a,b)::tl, la, rt, ra) -> wang(a::tl, la, rt, ra)
                                   & wang(b::tl, la, rt, ra)      (* ∨左 *)
  in
    wang ([],[],[p],[])

(*
 * 命題変数のリストから，すべての組合せの命題論理式を生成する
 * 
 *   引　数：---: 'a list --- 命題変数のリスト
 * 
 *   戻り値：生成した命題論理式のリスト
 * 
 *)
let rec gen_comb = function
    []    -> []
  | [x]   -> [Atom x;Neg(Atom x)]
  | x::xs ->
      let ps = gen_comb xs in
        List.map (fun p -> Conj(Atom x,p)) ps
        @ List.map (fun p -> Conj(Neg(Atom x),p)) ps

(*
 * 命題変数のリストを取得する
 * 
 *   引　数：p: Prop.t --- 命題論理式
 * 
 *   戻り値：命題変数のリスト
 * 
 *)
let get_vars p =
  let rec trav vs = function
      Atom v      -> v::vs
    | Neg p       -> trav vs p
    | Conj(p1,p2) -> trav (trav vs p1) p2
    | Disj(p1,p2) -> trav (trav vs p1) p2
  in
    trav [] p

