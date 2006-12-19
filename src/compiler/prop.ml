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
 * 命題論理式の命題変数をマッピングする
 *)
let rec map f = function
    Atom a -> Atom(f a)
  | Neg p  -> Neg(map f p)
  | Conj(p1,p2) -> Conj(map f p1, map f p2)
  | Disj(p1,p2) -> Disj(map f p1, map f p2)

(*
 * 含意を構成する：p→q ≡ ¬p∨q
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
   |  (lt, la, Neg(a)::tl, ra) -> wang(a::lt, la, tl, ra)         (* ¬右 *)
   |  (Neg(a)::tl, la, rt, ra) -> wang(tl, la, a::rt, ra)         (* ¬左 *)
   |  (lt, la, Conj(a,b)::tl, ra) -> wang(lt, la, a::tl, ra)
                                   & wang(lt, la, b::tl, ra)      (* ∧右 *)
   |  (Conj(a,b)::tl, la, rt, ra) -> wang(a::b::tl, la, rt, ra)   (* ∧左 *)
   |  (lt, la, Disj(a,b)::tl, ra) -> wang(lt, la, a::b::tl, ra)   (* ∨右 *)
   |  (Disj(a,b)::tl, la, rt, ra) -> wang(a::tl, la, rt, ra)
                                   & wang(b::tl, la, rt, ra)      (* ∨左 *)
  in
    wang ([],[],[p],[])


(*
 * Negation Normal Formに変換する
 *)
let rec nnf = function
    Atom a -> Atom a
  | Neg(Atom a) -> Neg(Atom a)
  | Neg(Neg p) -> nnf p
  | Neg(Conj(p,q)) -> nnf (Disj(Neg p,Neg q))
  | Neg(Disj(p,q)) -> nnf (Conj(Neg p,Neg q))
  | Conj(p,q) -> Conj(nnf p, nnf q)
  | Disj(p,q) -> Disj(nnf p, nnf q)
      
(*
 * 積和標準形(Disjunctive Normal Form)に変換する
 *)
let dnf p = 
  let rec distrib = function
      p,Disj(q,r)  -> Disj(distrib(p,q),distrib(p,r))
    | Disj(q,r),p  -> Disj(distrib(q,p),distrib(r,p))
    | p,q -> Conj(p,q) in
  let rec dnf_ = function
      Conj(p,q) -> distrib(dnf_ p,dnf_ q)
    | Disj(p,q) -> Disj(dnf_ p,dnf_ q)
    | p -> p
  in
    dnf_ (nnf p)
    
let rec show f = function
    Atom a -> f a
  | Neg p  -> print_string "~("; show f p; print_string ")"
  | Conj(p,q) ->
      print_string "(";
      show f p; print_string "^"; show f q;
      print_string ")"
  | Disj(p,q) ->
      print_string "(";
      show f p; print_string "|"; show f q;
      print_string ")"
