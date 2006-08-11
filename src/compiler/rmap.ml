(**
   ���W�X�^�}�b�v�֘A���W���[��

   @author Hattori Kenta
   @version $Id: rmap.ml,v 1.5 2006/07/06 04:15:36 hattori Exp $
*)

module L = List

(* 
 * ���W�X�^�}�b�v�̌^
 * 
 * - ���Ɋ����ĉ\�ȋ󂫔ԍ�
 * - �����čς݃��X�g (���W�X�^�ԍ� �~ �V���{��)
 * �@       �� ���W�X�^�ԍ��ŏ����ɂȂ��Ă���
 * 
 *)
type t = int * (int * Symbol.t) list

let treg = -1     (* �ꎞ���W�X�^�ԍ� *)
let mreg = ref 0  (* �ő僌�W�X�^�ԍ� *)
let maxreg () = 1+(!mreg)

let empty:t = 0,[]

(* �������W�X�^�}�b�v�̍쐬 *)
let make vs = L.fold_left (fun (i,rm) s -> i+1,rm@[i,s]) empty vs

(* ���̋󂫃��W�X�^��T�� *)
let next s (_,ls) = L.fold_left (fun i (j,_) -> if i=j then i+1 else i) s ls

(** ���W�X�^���擾 *)
let regname = function
    i when i=treg -> "__prc__treg"
  | i             -> (if i>(!mreg) then mreg:=i else ());Printf.sprintf "__prc__regs[%d]" i

(** �Ή�����V���{�����擾 *)
let get (_,ls) i = try L.assoc i ls with _ -> assert false

(** �V���{���̕ێ����郌�W�X�^�ԍ������� *)
let find (_,ls) s = let i,_ = L.find (fun (i,s') -> s=s') ls in i

let add (n,ls) i s =
  let nl = L.sort (fun (j,_) (k,_) -> Pervasives.compare j k) ((i,s)::ls) in
    next 0 (n,nl),nl
let delete (n,ls) i =
  let nl = L.remove_assoc i ls in
    next 0 (n,nl),nl

(** ���W�X�^�Ԃ̈ړ� i -> j *)
let move rmap i j =
  delete (add (delete rmap j) j (get rmap i)) i

(** ���W�X�^�}�b�v�̉��(vs�ȊO���������) *)
let release (n,ls) vs =
  L.fold_left (fun rm (i,s) -> if L.mem s vs then add rm i s else rm) empty ls

(** ���W�X�^�̊����� *)
let assign (n,ls) s = add (n,ls) n s

let isFree (_,ls) i = not (L.mem_assoc i ls)

(** �\�� *)
let show (_,rs) =
  List.iter (fun (i,s) -> Printf.printf "__prc__regs[%d]=%s\n" i (Symbol.name s)) rs
