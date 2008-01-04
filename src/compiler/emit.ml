(**
   Cコード生成モジュール

   @author Hattori Kenta
   @version $Id: emit.ml,v 1.8 2006/07/27 00:07:17 hattori Exp $
*)

module C = Cps
module E = Exception

let string_hexstr s =
  let h = ref "" in 
    String.iter (
      fun c -> h := (!h)^Printf.sprintf "\\x%02x" (int_of_char c)
    ) s;
    !h

let emitHeader ()    = Printf.printf "#include <preccs/prcrt.h>\n"
let emitFndecl fname = Printf.printf "static int __prc__%s(void);\n" fname
let emitProlog fname = Printf.printf "static int __prc__%s(void) {\n" fname
let emitPrologInit ()= Printf.printf "int __prc__init(void) {\n"
let emitEpilog ()    = Printf.printf "}\n"
let emitMoveReg i j  = Printf.printf "%s=%s;\n" (Rmap.regname j) (Rmap.regname i)
let emitDtable ()    = Printf.printf "prc_dtable_t __prc__dtbl = {\n";
                       Printf.printf "__prc__state_table,\n";
                       Printf.printf "__prc__fact_table,\n";
                       Printf.printf "__prc__mact_table,\n";
                       Printf.printf "__prc__cset_table,\n";
                       Printf.printf "__prc__ract_table,\n";
                       Printf.printf "__prc__cond_table,\n";
                       Printf.printf "__prc__cact_table,\n";
                       Printf.printf "MAX_LABEL\n";
                       Printf.printf "};\n"
let emitPrcMain ()   = Printf.printf "int prc_main(void) {\n";
                       Printf.printf "return __prc__main__(%d, __prc__init, &__prc__dtbl);\n" (Rmap.maxreg());
                       Printf.printf "}\n"

let const2str = function
    C.Unit     -> "0"
  | C.Bool b   -> if b then "~0" else "1"
  | C.Int i    -> string_of_int ((i lsl 1) lxor 1)
  | C.Cint i   -> string_of_int i
  | C.String s -> Printf.sprintf "(int)__string__(%d,\"%s\")"
      (String.length s) (string_hexstr s)

let val2str rmap = function
    C.Var l    -> Rmap.regname (Rmap.find rmap l)
  | C.Label l  -> Printf.sprintf "(int)__prc__%s" (Symbol.name l)
  | C.Const c  -> const2str c

let binop2str = function
    C.Add -> "IADD"
  | C.Sub -> "ISUB"
  | C.Mul -> "IMUL"
  | C.Div -> "IDIV"
  | C.Mod -> "IMOD"
  | C.Eq  -> "EQ"
  | C.Neq -> "NEQ"
  | C.Lt  -> "LT"
  | C.Leq -> "LEQ"
  | C.Gt  -> "GT"
  | C.Geq -> "GEQ"
  | C.And -> "AND"
  | C.Or  -> "OR"
  | C.Eqs -> "EQS"
  | C.Cat   -> "CONCAT"
  | C.Match -> "__dmatch__"
  | _     -> assert false

let monop2str = function
    C.Neg -> "INEG"
  | C.Not -> "NOT"
  | C.Set -> "SET"
  | _     -> assert false

(** Cコードの出力 *)
let rec emit rmap (cexp,fv) =
  (* レジスタマップを更新 *)
  let rm = Rmap.release rmap !fv in
  match cexp with
    C.Prim(p,ops,rs,cs) ->
        emitPrim rm (p,ops,rs,cs)
  | C.Fix(bs,c) ->
      emitHeader();
      Dtable.emit();
      (* 関数宣言出力 *)
      List.iter (fun (f,_,_) -> emitFndecl (Symbol.name f)) bs;
      List.iter emitFbind bs;  (* 各関数定義の出力 *)
      emitPrologInit();        (* 初期化コードの出力 *)
      emit Rmap.empty c;
      emitEpilog();
      emitDtable();
      emitPrcMain()
  | C.App(op,ops) ->
      emitApp rmap (op,ops)
  | C.Switch(v,((C.String _,_)::_ as bcs)) ->
      (
	try
	  List.iter (
	    fun (b,c) ->
    	      match b with
		  C.Unit ->
		    Printf.printf "if (1) {\n";
		    emit rm c;
		    Printf.printf "} else ";
		    raise E.Break
		| C.String s ->
		    Printf.printf "if (EQS(%s,%s)==~0) {\n" (val2str rm v) (const2str b) ;
		    emit rm c;
		    Printf.printf "} else "
		| _ -> assert false
	  ) bcs
	with
	    E.Break -> ()
      );
      Printf.printf "{ assert(0); }\n"
  | C.Switch(v,bcs) ->
      Printf.printf "switch (%s) {\n" (val2str rm v); (
	try
	  List.iter (
          fun (b,c) ->
	    match b with
		C.Unit ->
		  Printf.printf "default:\n";
		  emit rm c;
		  Printf.printf "break;\n";
		  raise E.Break
	      | _ ->
		  Printf.printf "case %s:\n" (const2str b);
		  emit rm c;
		  Printf.printf "break;\n";
	  ) bcs
	with
	    E.Break -> ()
      );
      Printf.printf "}\nreturn 0;\n"
  | C.Cblk(cs,vs,c) ->
      Printf.printf "{%s}\n"
        ( List.fold_left2
            ( fun s c v -> s^(val2str rm v)^c ) (List.hd cs) (List.tl cs) vs );
      emit rm c

and emitFbind (f,ps,c) =
  emitProlog(Symbol.name f);
  emit (Rmap.make ps) c; (* 関数本体の出力 *)
  emitEpilog()

and emitApp rmap (op,ops) = 
  (* オペレータの退避 *)
  let saveop rm =
    match op with
        C.Var l ->
          let i = List.length ops in (* オペランドの数 *)
          let j = Rmap.find rm l in  (* オペレータのレジスタ *)
            if j < i then
              (* オペランドの数以降で空いているレジスタに退避 *)
              let n = Rmap.next i rm in (emitMoveReg j n; Rmap.move rm j n)
            else rm
      | _ -> rm in

  (* レジスタの置換 *)
  let perm rm = List.fold_left (
    fun (i,rm',rtry,movd) v -> match v with
        C.Var l ->
          let j = Rmap.find rm' l in
            if i=j then i+1,rm',rtry,movd
            else if Rmap.isFree rm' i then (emitMoveReg j i;
                                            i+1,Rmap.move rm' j i,rtry,movd+1)
            else i+1,rm',(j,i),movd
      | _       -> i+1,rm',rtry,movd )
    (0,rm,(-1,-1),0) in
  let rec permvar rm vs =
      match perm rm vs with
        _,rm',(-1,-1),_ -> rm'                          (* 完了状態 *)
      | _,rm',( i, j),0 -> permvar (save rm' i j vs) vs (* 循環状態 *)
      | _,rm',      _,_ -> permvar rm' vs               (* 続行状態 *)

  and save rm i j vs = 
    emitMoveReg i Rmap.treg;
    loop (Rmap.move rm i Rmap.treg) vs
  and loop rm vs =
    match perm rm vs with
        _,rm',_,0 -> rm'
      | _,rm',_,_ -> loop rm' vs in

  (* 定数パラメータのセット *)
  let setval =
    List.fold_left (
      fun i v -> match v with
          C.Var _ -> i+1
        | C.Label l ->
	    Printf.printf "__prc__regs[%d]=(int)__prc__%s;\n" i (Symbol.name l); i+1
        | C.Const c -> Printf.printf "__prc__regs[%d]=%s;\n" i (const2str c); i+1
    ) 0 in
  let rm  = saveop rmap in
  let rm' = permvar rm ops in
    ignore (setval ops);
    Printf.printf "return (int)%s;\n" (val2str rm' op)

(* (p,ops,rs,cs) *)
and emitPrim rm = function
    C.Disp,[],[],[] -> print_string "return (int)__disp__;\n"
  | C.New,[],[r],[c,fv] ->
      let rm'  = Rmap.release rm !fv in
      let rm'' = Rmap.assign rm' r in
        Printf.printf "%s=(int)__chan__();\n" (val2str rm'' (C.Var r));
        emit rm'' (c,fv)
  | C.Record,vs,[r],[c,fv] ->
      let rm'  = Rmap.assign rm r in
      let rm'' = Rmap.release rm' !fv in
      let rs   = val2str rm' (C.Var r) in
      let l    = List.length vs in
	Printf.printf "%s=__record__(%d);\n" rs l;
	for i = 0 to (List.length vs) - 1 do
	    Printf.printf "__prc__temp=%s;\n" (val2str rm (List.nth vs i));
	    Printf.printf "((int*)%s)[%d]=__prc__temp;\n" rs i
	   (* Printf.printf "((int*)%s)[%d]=%s;\n" rs i (val2str rm (List.nth vs i)) *)
	done;
        emit rm'' (c,fv)
  | C.Rexrcd,[s;x],[r],[c,fv] ->
      let rm'  = Rmap.release rm !fv in
      let rm'' = Rmap.assign rm' r in
      let rs   = val2str rm'' (C.Var r) in
        Printf.printf "%s=__rexrcd__(%s,%s);\n" rs (val2str rm s) (val2str rm x);
        emit rm'' (c,fv)

  | C.Select,[v;o],[r],[c,fv] ->
      let rm'  = Rmap.release rm !fv in
      let rm'' = Rmap.assign rm' r in
      let rs   = val2str rm'' (C.Var r) in
        Printf.printf "%s=((int *)%s)[TOCINT(%s)];\n"
          rs (val2str rm v) (val2str rm o);
        emit rm'' (c,fv)
  | C.Offset,[v;C.Const(C.Int o)],[r],[c,fv] ->
      let rm'  = Rmap.release rm !fv in
      let rm'' = Rmap.assign rm' r in
      let rs   = val2str rm'' (C.Var r) in
        Printf.printf "%s=(int)&((int *)%s)[%d];\n" rs (val2str rm v) o;
        emit rm'' (c,fv)
  | C.Asgn,[v1;v2],[],[c,fv] ->
      let rm'  = Rmap.release rm !fv in
        Printf.printf "%s=%s;\n" (val2str rm v1) (val2str rm v2);
        emit rm' (c,fv)
  | C.Update,[v1;o;v2],[],[c,fv] ->
      let rm'  = Rmap.release rm !fv in
        Printf.printf "__prc__temp=%s;\n" (val2str rm v2);
        Printf.printf "((int *)%s)[TOCINT(%s)]=__prc__temp;\n"
          (val2str rm v1) (val2str rm o);
        emit rm' (c,fv)
  | C.Run,[v],[],[c,fv] ->
      let rm'  = Rmap.release rm !fv in
        Printf.printf "__run__(%s);\n" (val2str rm v);
        emit rm' (c,fv)
  | binop,[v1;v2],[r],[c,fv] ->
      let rm'  = Rmap.release rm !fv in
      let rm'' = Rmap.assign rm' r in
      let rs   = val2str rm'' (C.Var r) in
        Printf.printf "%s=%s(%s,%s);\n"
          rs (binop2str binop) (val2str rm v1) (val2str rm v2);
        emit rm'' (c,fv)
  | monop,[v],[r],[c,fv] ->
      let rm'  = Rmap.release rm !fv in
      let rm'' = Rmap.assign rm' r in
      let rs   = val2str rm'' (C.Var r) in
        Printf.printf "%s=%s(%s);\n"
          rs (monop2str monop) (val2str rm v);
        emit rm'' (c,fv)
  | x,_,_,_ -> print_string (C.showPrim x); assert false

