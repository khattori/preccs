(**
   レジスタマップ関連モジュール

   @author Hattori Kenta
   @version $Id: rmap.ml,v 1.5 2006/07/06 04:15:36 hattori Exp $
*)

module L = List

(* 
 * レジスタマップの型
 * 
 * - 次に割当て可能な空き番号
 * - 割当て済みリスト (レジスタ番号 × シンボル)
 * 　       ※ レジスタ番号で昇順になっている
 * 
 *)
type t = int * (int * Symbol.t) list

let treg = -1     (* 一時レジスタ番号 *)
let mreg = ref 0  (* 最大レジスタ番号 *)
let maxreg () = 1+(!mreg)

let empty:t = 0,[]

(* 初期レジスタマップの作成 *)
let make vs = L.fold_left (fun (i,rm) s -> i+1,rm@[i,s]) empty vs

(* 次の空きレジスタを探す *)
let next s (_,ls) = L.fold_left (fun i (j,_) -> if i=j then i+1 else i) s ls

(** レジスタ名取得 *)
let regname = function
    i when i=treg -> "__prc__treg"
  | i             -> (if i>(!mreg) then mreg:=i else ());Printf.sprintf "__prc__regs[%d]" i

(** 対応するシンボルを取得 *)
let get (_,ls) i = try L.assoc i ls with _ -> assert false

(** シンボルの保持するレジスタ番号を検索 *)
let find (_,ls) s =
  try
    let i,_ = L.find (fun (i,s') -> s=s') ls in i
  with
      Not_found -> Printf.printf "Rmap.find: not found %s\n" (Symbol.name s); assert false

let add (n,ls) i s =
  let nl = L.sort (fun (j,_) (k,_) -> Pervasives.compare j k) ((i,s)::ls) in
    next 0 (n,nl),nl
let delete (n,ls) i =
  let nl = L.remove_assoc i ls in
    next 0 (n,nl),nl

(** レジスタ間の移動 i -> j *)
let move rmap i j =
  delete (add (delete rmap j) j (get rmap i)) i

(** レジスタマップの解放(vs以外を解放する) *)
let release (n,ls) vs =
  L.fold_left (fun rm (i,s) -> if L.mem s vs then add rm i s else rm) empty ls

(** レジスタの割当て *)
let assign (n,ls) s = add (n,ls) n s

let isFree (_,ls) i = not (L.mem_assoc i ls)

(** 表示 *)
let show (_,rs) =
  List.iter (fun (i,s) -> Printf.printf "__prc__regs[%d]=%s\n" i (Symbol.name s)) rs
