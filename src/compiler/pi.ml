(**
   ���ԕ\�����W���[��
   
   �T�v�F�Όv�Z�`���ւ̕ϊ����s��

   @author Hattori Kenta
   @version $Id: pi.ml,v 1.6 2006/07/27 00:07:18 hattori Exp $
*)
module A = Syntax
module T = Types
module R = Regex

type var = Symbol.t

let rec make_list a = function
    0 -> []
  | n when n < 0 -> assert false
  | n -> a::make_list a (n-1)

(* �v���Z�X����` *)
type proc =
    End
  | Guard of guard
  | New   of var  * proc                   (* �`���l������   *)
  | Let   of var  * exp * proc             (* �ϐ�����       *)
  | Par   of proc * proc                   (* ���s����       *)
  | Case  of exp  * proc list              (* ������s       *)
  | Call  of var  * exp list               (* �v���Z�X�ďo�� *)
  | Cblk  of string list * exp list * proc (* C�R�[�h        *)
  | Fix   of bind list * proc              (* �v���Z�X��`   *)
and bind = var * var list * proc

(* �K�[�h����` *)
and guard =
    Send of exp * exp * proc   (* ���M *)
  | Recv of exp * var * proc   (* ��M *)
  | Alt  of guard * guard      (* �I�����s *)

(* ����` *)
and exp =
    Var    of var
  | Unit
  | Bool   of bool
  | Int    of int
  | Cint   of int
  | String of string
  | If     of exp * exp * exp
  | Record of exp list          (* ���R�[�h�l     *)
  | Rexrcd of string * exp list (* ���K�\�����R�[�h *)
  | Select of exp * exp         (* �t�B�[���h�I�� *)
  | Offset of exp * exp         (* ���R�[�h�I�t�Z�b�g *)
  | Prim   of prim * exp list

and prim =
    Add | Sub | Mul | Div | Mod
  | Eq  | Neq | Eqs | Lt  | Leq | Gt  | Geq
  | And | Or  | Cat
  | Neg | Not
  | Match      (* �p�^�[���}�b�` *)

let v_next = Symbol.symbol("next")
let v_junk = Symbol.symbol("junk")
let v_temp = Symbol.symbol("temp")
let v_mval = Symbol.symbol("mval")

let trans_const = function
    A.ConBool(_,b) -> Bool b
  | A.ConInt(_,n)  -> Int n
  | A.ConStr(_,s)  -> String s
let trans_bop = function
    A.BopAdd -> Add | A.BopSub -> Sub
  | A.BopMul -> Mul | A.BopDiv -> Div
  | A.BopMod -> Mod
  | A.BopEq  -> Eq  | A.BopNeq -> Neq
  | A.BopLt  -> Lt  | A.BopLeq -> Leq
  | A.BopGt  -> Gt  | A.BopGeq -> Geq
  | A.BopAnd -> And | A.BopOr  -> Or
  | A.BopCat -> Cat
let trans_mop = function A.MopNeg -> Neg | A.MopNot -> Not

(*
 * ���K�\���^�̃f�t�H���g��������擾
 *)
let rec defstr_of_regex = function
    T.REXP re -> R.to_string re
  | T.RARR(r,0) -> ""
  | T.RARR(r,1) -> defstr_of_regex r
  | T.RARR(r,n) -> (defstr_of_regex r)^(defstr_of_regex (T.RARR(r,n-1)))
  | T.RITR(r,_) -> ""
  | T.RRCD fs -> List.fold_left (fun s (_,r) -> s ^ defstr_of_regex r) "" fs

let con_regex =
  let rec trav = function
      T.REXP re -> Cint (R.size re)
    | T.RARR(r,n) -> Cint ((R.size (T.regexify r)) * n)
    | T.RITR _ -> Cint 0
    | T.RRCD fs -> Record (List.map (fun (_,r) -> trav r)  fs)
  in
    function
        T.REXP re -> String (R.to_string re)
      | T.RARR(r,0) -> String ""
      | T.RARR(r,1) -> String (defstr_of_regex r)
      | T.RARR(r,n) -> String ((defstr_of_regex r)^(defstr_of_regex (T.RARR(r,n-1))))
      | T.RITR(r,_) -> String ""
      | T.RRCD fs ->
          let s = List.fold_left (fun s (_,r) -> s ^ defstr_of_regex r) "" fs in
            Rexrcd(s, List.map (fun (_,r) -> trav r) fs)

(*
 * �l���̕ϊ�
 * 
 *   ���@���Fenv: Env.t --- ���O��
 *           ---: exp   --- �l��
 *   �߂�l�F�ϊ���̒l��
 * 
 *)
let rec trans_expr env = function
    A.ExpConst c   -> trans_const c
  | A.ExpVar v     -> trans_var env v
  | A.ExpRecord fs -> Record(List.map (fun (_,_,e) -> trans_expr env e) fs)
  | A.ExpTuple es  -> Record(List.map (fun e -> trans_expr env e) es)
  | A.ExpBinop(_,bop,e1,e2) ->
      Prim(trans_bop bop,[trans_expr env e1; trans_expr env e2])
  | A.ExpMonop(_,mop,e) -> Prim(trans_mop mop,[trans_expr env e])
  
(*
 * �ϐ��Q�Ǝ��̕ϊ�
 * 
 *   ���@���Fenv: Env.t --- ���O��
 *           ---: var   --- �ϐ��Q�Ǝ�
 *   �߂�l�F�ϊ���̎�
 * 
 *)
and trans_var env = function
    A.VarSimple(_,s) -> Var s
  | A.VarField(_,v,s,o,r) -> (
      match !r with
          T.RECORD _        -> Select(trans_var env v,Int !o)
        | T.REGEX(T.RRCD _) -> Offset(Select(trans_var env v,Int 2),Int !o)
        | _ -> assert false
    )
  | A.VarProj(_,v,n) -> Select(trans_var env v, Int n)
  | A.VarSubscr(_,v,e) -> Select(trans_var env v,trans_expr env e)

(*
 * �v���Z�X���̕ϊ�
 * 
 *   ���@���Fenv: Env.t --- ���O��
 *           nxt: proc  --- �������s���̌㑱�v���Z�X
 *           ---: proc  --- �v���Z�X���i���ۍ\���j
 *   �߂�l�F�v���Z�X���i�Ύ��j
 * 
 *)
and trans_proc env nxt = function
    A.ProcStop _ -> End
  | A.ProcSkip _ -> nxt
  | A.ProcSeq(_,ps) ->
      List.fold_right (
        fun p proc ->
          trans_proc env (New(v_next,Par(Guard(Send(Var v_next,Unit,End)),
                                         Guard(Recv(Var v_next,v_junk,proc))))) p
      ) ps nxt

  | A.ProcChoice(_,ps) ->
      let proc = List.hd ps in
      let g = fst proc in
      let p = snd proc in
        Guard(
          List.fold_left (
            fun proc (g,p) ->
              Alt(trans_gproc env (trans_proc env nxt p) g,proc)
          ) (trans_gproc env (trans_proc env nxt p) g) ps)
  | A.ProcMatch(_,e,ps,t) -> trans_match env nxt (!t) e ps
  | A.ProcInput(_,c,s)  -> Guard(Recv(trans_var env c,s,nxt))
  | A.ProcOutput(_,c,e) -> Guard(Send(trans_var env c,trans_expr env e,nxt))
  | A.ProcRun(_,s,es) -> Par(Call(s,List.map (fun e -> trans_expr env e) es),nxt)
  | A.ProcVar(_,s,A.DeclExpr e)  -> Let(s,trans_expr env e,nxt)
  | A.ProcVar(_,s,A.DeclType(t,ty)) when T.is_chan !ty -> New(s,nxt)
  | A.ProcVar(_,s,A.DeclType(t,ty)) -> Let(s,trans_type !ty,nxt)
  | A.ProcAsign(_,v,e) -> assert false (* not yet implement *)
  | A.ProcCblock(_,cs,vs) -> Cblk(cs,List.map (trans_var env) vs,nxt)

(* �K�[�h���ϊ� *)
and trans_gproc env nxt = function
    A.ProcInput(_,c,s)  -> Recv(trans_var env c,s,nxt)
  | A.ProcOutput(_,c,e) -> Send(trans_var env c,trans_expr env e,nxt)
  | _ -> assert false
          
(*
 * �p�^���}�b�`���̕ϊ�
 * 
 *   ���@���Fenv: Env.t     --- ���O��
 *           nxt: proc      --- �������s���̌㑱�v���Z�X
 *           ty : Types.t   --- �l���̌^
 *           exp: exp       --- �l��
 *           pts: pat list  --- �p�^�����̃��X�g
 *   �߂�l�F�v���Z�X���i�Ύ��j
 *)
and trans_match env nxt ty exp pts =
  let pt_ls,pr_ls = List.split pts in match ty with 
      T.STRING | T.REGEX _ ->
        let mexp,binds  = trans_patns ty (Var v_mval) pt_ls in
          Let(v_mval,(trans_expr env exp),
              Let(v_temp,mexp, (* v_temp:(index,data) *)
                  Case(Select(Var v_temp,Int 0),
                       List.map2 (
                         fun p -> function
                             None   -> trans_proc env nxt p
                           | Some s -> Let(s,Select(Var v_temp,Int 1),
                                           trans_proc env nxt p)
                       ) pr_ls binds)))
    | _ -> let mexp = trans_case (Var v_mval) pt_ls in
        Let(v_mval,(trans_expr env exp),
            Let(v_temp,mexp, (* v_temp:index *)
                Case(Var v_temp,
                     List.map (fun p -> trans_proc env nxt p) pr_ls)))

(* �p�^�����Ƒ������X�g��Ԃ� *)
and trans_patns ty v patns =
  (* �^�̃��X�g����DFA���쐬����D *)
  let binds,rs = List.split (
    List.map (
      function 
          A.PatAny _ ->
            ( match ty with
                  T.STRING  -> None,T.REXP(R.CLOS(R.oct))
                | T.REGEX r -> None,r
                | _ -> assert false )
        | A.PatConst(A.ConStr(_,s)) -> None,T.REXP(R.of_string s)
        | A.PatRegex(_,s,_,t)       -> Some s,!t
        | _ -> assert false
    ) patns ) in
    match ty with
        T.STRING  -> Prim(Match,[v;Int (Dfa.generate rs)]),binds
      | T.REGEX r -> Prim(Match,[v;Int (Dfa.generate2 r rs)]),binds
      | _ -> assert false

and trans_case v patns =
  (* �P�[�X������ *)
  let len = List.length patns in
    fst (
      List.fold_right (
        fun pat (e,i) -> match pat with
            A.PatAny _ -> Int i,i-1
          | A.PatConst(A.ConBool(_,b)) ->
              If(Prim(Eq,[v;Bool b]),Int i,e),i-1
          | A.PatConst(A.ConInt(_,n)) ->
              If(Prim(Eq,[v;Int n]),Int i,e),i-1
          | _ -> assert false
      ) patns (Int len,len-1)
    )

(*
 * �^�ɑ΂��鏉�������𐶐�����
 * 
 *   ���@���Fenv: Env.t --- ���O��
 *           ---: typ   --- ���ۍ\���̌^��
 *   �߂�l�F��������
 * 
 *)
and trans_type = function
    T.BOOL   -> Bool false
  | T.INT    -> Int 0
  | T.STRING -> String ""
  | T.ARRAY(t,n) -> Record (make_list (trans_type t) n)
  | T.RECORD ts  -> Record (List.map (fun (_,t) -> trans_type t) ts)
  | T.REGEX r    -> con_regex r
  | _ -> assert false

(*
 * �Ύ��֕ϊ�����֐�
 *
 *   ��  ���Fdefs: toplevel --- �g�b�v���x����`�̃��X�g
 *           env : Env.t    --- ���O��
 *   �߂�l�F�ϊ���̃Ύ�
 * 
 *)
let trans env defs =
  (* �g�b�v���x����`�̕ϊ����s�� *)
  List.fold_right (
    fun def proc -> (
      match def with
          A.DefVar(_,s,A.DeclExpr ex) -> Let(s,trans_expr env ex,proc)
        | A.DefVar(_,s,A.DeclType(_,ty)) -> Let(s,trans_type !ty,proc)
        | A.DefType types -> proc
        | A.DefProc procs ->
            Fix(List.map (
                  fun (_,s,ds,p) ->
                    s,List.map fst ds,
                    trans_proc env End p
                ) procs,proc)
    )
  ) defs (Call(Symbol.symbol "Main",[]))

(*
 * �œK�������F�����ȒʐM�������폜����
 * 
 *   ���@���F--- : pexp --- �Ύ�
 *   �߂�l�F�œK����̃Ύ�
 * 
 *   (ch!E.$|ch?x.P) ===> let x=E.P
 * 
 *)
let rec reducComm = function
    Guard g  -> Guard (reducCommG g)
  | New(v,p) -> New(v,reducComm p)
  | Let(v,e,p) -> Let(v,e,reducComm p)
  | Par(Guard(Send(Var(c1),e,End)),Guard(Recv(Var(c2),x,p)))
  | Par(Guard(Recv(Var(c2),x,p)),Guard(Send(Var(c1),e,End)))
      when Symbol.equal c1 c2
        -> Let(x,e,reducComm p)
  | Par(p1,p2) -> Par(reducComm p1,reducComm p2)
  | Case(e,ps) -> Case(e,List.map reducComm ps)
  | Cblk(cs,es,p) -> Cblk(cs,es,reducComm p)
  | Fix(bs,p) -> Fix(List.map (fun (v,vs,p) -> v,vs,reducComm p) bs,reducComm p)
  | _ as p -> p

and reducCommG = function
    Send(e1,e2,p) -> Send(e1,e2,reducComm p)
  | Recv(e,v,p)   -> Recv(e,v,reducComm p)
  | Alt(g1,g2)    -> Alt(reducCommG g1,reducCommG g2)

let rec unused v = function
    Guard g  -> unusedG v g
  | New(v',_) when Symbol.equal v v' -> true
  | New(_,p) -> unused v p
  | Let(v',_,_) when Symbol.equal v v' -> true
  | Let(_,e,p) -> unused v p && unusedE v e
  | Par(p1,p2) -> unused v p1 && unused v p2
  | Case(e,ps) -> unusedE v e && List.for_all (unused v) ps
  | Call(v',es) -> not (Symbol.equal v v') && List.for_all (unusedE v) es
  | Cblk(cs,es,p) -> unused v p && List.for_all (unusedE v) es
  | Fix(bs,p) ->
      unused v p &&
        List.for_all (
          fun (v',vs,p') ->
            (* Symbol.equal v v' || *)
            List.exists (Symbol.equal v) vs   (* �ϐ�v���B������� *)
            || unused v p'
        ) bs
  | End -> true

and unusedG v = function
    Send(e1,e2,p) -> unusedE v e1 && unusedE v e2 && unused v p
  | Recv(e,v',p) when Symbol.equal v v' -> unusedE v e
  | Recv(e,_,p) -> unusedE v e && unused v p
  | Alt(g1,g2)  -> unusedG v g1 && unusedG v g2
and unusedE v = function
    Var v' -> not (Symbol.equal v v')
  | Unit | Bool _ | Int _ | Cint _ | String _ -> true
  | If(e1,e2,e3) -> unusedE v e1 && unusedE v e2 && unusedE v e3
  | Record es | Rexrcd (_,es) -> List.for_all (unusedE v) es
  | Select(e1,e2) | Offset(e1,e2) -> unusedE v e1 && unusedE v e2
  | Prim(_,es) -> List.for_all (unusedE v) es


let isNoSideEffect e = true
(*
 * �œK�������F�s�v�ȕϐ����폜
 * 
 *   ���@���F--- : pexp --- �Ύ�
 *   �߂�l�F�œK����̃Ύ�
 * 
 *   new junk.P ===> P
 *   let junk=E.P ===> P
 *            ^^^����p����������
 *   �� �s�v�Ȉ����̏����͍s��Ȃ�
 * 
 *)
let rec removeUnused = function
    Guard g  -> Guard (removeUnusedG g)
  | New(v,p) when unused v p   -> removeUnused p
  | New(v,p) -> New(v,removeUnused p)
  | Let(v,e,p) when unused v p && isNoSideEffect e -> removeUnused p
  | Let(v,e,p) -> Let(v,e,removeUnused p)
  | Par(p1,p2) -> Par(removeUnused p1,removeUnused p2)
  | Case(e,ps) -> Case(e,List.map removeUnused ps)
  | Cblk(cs,es,p) -> Cblk(cs,es,removeUnused p)
  | Fix(bs,p) ->
      Fix(List.map (fun (v,vs,p) -> v,vs,removeUnused p) bs,removeUnused p)
  | _ as p -> p

and removeUnusedG = function
    Send(e1,e2,p) -> Send(e1,e2,removeUnused p)
  | Recv(e,v,p)   -> Recv(e,v,removeUnused p)
  | Alt(g1,g2)    -> Alt(removeUnusedG g1,removeUnusedG g2)


(*
 * �œK�������F�����ȕ��s�������폜����
 * 
 *   ���@���F--- : pexp --- �Ύ�
 *   �߂�l�F�œK����̃Ύ�
 * 
 *   ( P | $ ) ===> P
 * 
 *)
let rec reducPar = function
    Guard g  -> Guard(reducParG g)
  | New(v,p) -> New(v,reducPar p)
  | Par(p1,p2) ->
      let p1' = reducPar p1 in
      let p2' = reducPar p2 in
        if p1' == End then p2'
        else if p2' == End then p1'
        else Par(p1',p2')
  | Case(e,ps)    -> Case(e,List.map reducPar ps)
  | Cblk(cs,es,p) -> Cblk(cs,es,reducPar p)
  | Fix(bs,p)     -> Fix(List.map (fun (v,vs,p) -> v,vs,reducPar p) bs,reducPar p)
  | _ as p -> p
and reducParG = function
    Send(e1,e2,p) -> Send(e1,e2,reducPar p)
  | Recv(e,v,p)   -> Recv(e,v,reducPar p)
  | Alt(g1,g2)    -> Alt(reducParG g1,reducParG g2)

let string_of_prim = function
    Add -> "Add" | Sub -> "Sub" | Mul -> "Mul" | Div -> "Div" | Mod -> "Mod"
  | Eq  -> "Eq"  | Neq -> "Neq" | Eqs -> "Eqs"
  | Lt  -> "Lt"  | Leq -> "Leq" | Gt  -> "Gt"  | Geq -> "Geq"
  | And -> "And" | Or  -> "Or"  | Cat -> "Cat"
  | Neg -> "Neg" | Not -> "Not"
  | Match -> "Match"      (* �p�^�[���}�b�` *)

let rec show_proc = function
    End        -> Printf.printf "End"
  | Guard g    ->
      Printf.printf "Guard(";
      show_guard g; Printf.printf ")"
  | New(v,p)   ->
      Printf.printf "New(%s," (Symbol.name v);
      show_proc p; Printf.printf ")"
  | Let(v,e,p) ->
      Printf.printf "Let(%s," (Symbol.name v);
      show_exp e;  Printf.printf ",";
      show_proc p; Printf.printf ")"
  | Par(p1,p2) ->
      Printf.printf "Par(";
      show_proc p1; Printf.printf ",";
      show_proc p2; Printf.printf ")"
  | Case(e,ps) ->
      Printf.printf "Case(";
      show_exp e; Printf.printf ",[";
      List.iter (fun p -> show_proc p;Printf.printf ";") ps;
      Printf.printf "])"
  | Call(v,es) ->
      Printf.printf "Call(%s,[" (Symbol.name v);
      List.iter (fun e -> show_exp e;Printf.printf ";") es;
      Printf.printf "])"
  | Cblk(cs,vs,p) ->
      Printf.printf "Cblk(";
      List.iter print_string cs;
      Printf.printf ",[";
      List.iter (fun v -> show_exp v; Printf.printf ";") vs;
      Printf.printf "],";
      show_proc p; Printf.printf ")"
  | Fix(bs,p) ->
      Printf.printf "Fix([";
      List.iter (fun (v,vs,p) ->
                   Printf.printf "(%s,[" (Symbol.name v);
                   List.iter (fun v -> Printf.printf "%s;" (Symbol.name v)) vs;
                   Printf.printf "],";
                   show_proc p; Printf.printf ");") bs;
      Printf.printf "],";
      show_proc p; Printf.printf ")"

and show_guard = function
    Send(e1,e2,p) ->
      Printf.printf "Send(";
      show_exp e1; Printf.printf ",";
      show_exp e2; Printf.printf ",";
      show_proc p; Printf.printf ")"
  | Recv(e,v,p) ->
      Printf.printf "Recv(";
      show_exp e; Printf.printf ",%s," (Symbol.name v);
      show_proc p; Printf.printf ")"
  | Alt(g1,g2) ->
      Printf.printf "Alt(";
      show_guard g1; Printf.printf ",";
      show_guard g2; Printf.printf ")"

and show_exp = function
    Var v    -> Printf.printf "Var(%s)" (Symbol.name v)
  | Unit     -> Printf.printf "Unit"
  | Bool b   -> Printf.printf "Bool(%b)" b
  | Int i    -> Printf.printf "Int(%d)" i
  | Cint i   -> Printf.printf "Cint(%d)" i
  | String s -> Printf.printf "String(%S)" s
  | If(b,e1,e2) ->
      Printf.printf "If(";
      show_exp b; Printf.printf ",";
      show_exp e1; Printf.printf ",";
      show_exp e2; Printf.printf ")"
  | Record es ->
      Printf.printf "Record([";
      List.iter (fun e -> show_exp e; Printf.printf ";") es;
      Printf.printf "])"
  | Rexrcd(s,es) ->
      Printf.printf "Rexrcd(%S,[" s;
      List.iter (fun e -> show_exp e; Printf.printf ";") es;
      Printf.printf "])"
  | Select(e1,e2) ->
      Printf.printf "Select(";
      show_exp e1; Printf.printf ",";
      show_exp e2; Printf.printf ")"
  | Offset(e1,e2) ->
      Printf.printf "Offset(";
      show_exp e1; Printf.printf ",";
      show_exp e2; Printf.printf ")"
  | Prim(p,es) ->
      Printf.printf "Prim(%s,[" (string_of_prim p);
      List.iter (fun e -> show_exp e; Printf.printf ";") es;
      Printf.printf "])"


