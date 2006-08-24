(**
   Cコード生成モジュール

   @author Hattori Kenta
   @version $Id: emit.ml,v 1.8 2006/07/27 00:07:17 hattori Exp $
*)

module C = Cps

let string_hexstr s =
  let h = ref "" in 
    String.iter (
      fun c -> h := (!h)^Printf.sprintf "\\x%02x" (int_of_char c)
    ) s;
    !h

let emitHeader ()    = Printf.printf "#include \"prcrt.h\"\n"
let emitFndecl fname = Printf.printf "static int __prc__%s(void);\n" fname
let emitProlog fname = Printf.printf "static int __prc__%s(void) {\n" fname
let emitPrologInit ()= Printf.printf "int __prc__init(void) {\n"
let emitEpilog ()    = Printf.printf "}\n"
let emitMoveReg i j  = Printf.printf "%s=%s;\n" (Rmap.regname j) (Rmap.regname i)
let emitRegdecl ()   = Printf.printf "int __prc__treg;\n";
                       Printf.printf "int __prc__regs[%d];\n" (Rmap.maxreg());
                       Printf.printf "int __prc__rnum = %d;\n" (Rmap.maxreg())
                       
let val2str rmap = function
    C.Var l    -> Rmap.regname (Rmap.find rmap l)
  | C.Label l  -> Printf.sprintf "__prc__%s" (Symbol.name l)
  | C.Bool b   -> if b then "~0" else "1"
  | C.Int i    -> string_of_int ((i lsl 1) lxor 1)
  | C.Cint i   -> string_of_int i
  | C.String s -> Printf.sprintf "(int)__string__(%d,\"%s\")"
      (String.length s) (string_hexstr s)


let const2str = function
    C.Var _    -> assert false
  | C.Label l  -> Printf.sprintf "__prc__%s" (Symbol.name l)
  | C.Bool b   -> if b then "~0" else "1"
  | C.Int i    -> string_of_int ((i lsl 1) lxor 1)
  | C.Cint i   -> string_of_int i
  | C.String s -> Printf.sprintf "(int)__string__(%d,\"%s\")"
      (String.length s) (string_hexstr s)

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
  | C.Cat   -> "__concat__"
  | C.Eqs   -> "__equals__"
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
      Dfa.emit();
      (* 関数宣言出力 *)
      List.iter (fun (f,_,_) -> emitFndecl (Symbol.name f)) bs;
      List.iter emitFbind bs;  (* 各関数定義の出力 *)
      emitPrologInit();        (* 初期化コードの出力 *)
      emit Rmap.empty c;
      emitEpilog();
      emitRegdecl()
  | C.App(op,ops) ->
      emitApp rmap (op,ops)
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
        | _       -> Printf.printf "__prc__regs[%d]=%s;\n" i (const2str v); i+1
    ) 0 in
  let rm  = saveop rmap in
  let rm' = permvar rm ops in
    ignore (setval ops);
    Printf.printf "return (int)%s;\n" (val2str rm' op)

(* (p,ops,rs,cs) *)
and emitPrim rm = function
    C.Disp,[],[],[] -> print_string "return (int)__disp__;\n"
  | C.If,[b],[],[c1;c2] ->
      Printf.printf "if (%s==~0) {\n" (val2str rm b);
      emit rm c1;
      Printf.printf "}\n";
      emit rm c2
  | C.Switch,[n],[],cs ->
      Printf.printf "switch (TOCINT(%s)) {\n" (val2str rm n);
      ignore (
        List.fold_left (
          fun i c ->
            Printf.printf "case %d:\n" i;
            emit rm c;
            Printf.printf "break;\n";
            i+1
        ) 0 cs
      );
      Printf.printf "default: assert(0);\n";
      Printf.printf "}\n"
  | C.New,[],[r],[c,fv] ->
      let rm'  = Rmap.release rm !fv in
      let rm'' = Rmap.assign rm' r in
        Printf.printf "%s=(int)__chan__();\n" (val2str rm'' (C.Var r));
        emit rm'' (c,fv)
  | C.Record,vs,[r],[c,fv] ->
      let rm'  = Rmap.release rm !fv in
      let rm'' = Rmap.assign rm' r in
      let rs   = val2str rm'' (C.Var r) in
      let l    = List.length vs in
        Printf.printf "%s=__record__(%d" rs l;
        List.iter (fun v -> Printf.printf ",%s" (val2str rm v)) vs;
        print_string ");\n";
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
  | C.Offset,[v;C.Int o],[r],[c,fv] ->
      let rm'  = Rmap.release rm !fv in
      let rm'' = Rmap.assign rm' r in
      let rs   = val2str rm'' (C.Var r) in
        Printf.printf "%s=(int)&((int *)%s)[%d];\n" rs (val2str rm v) o;
        emit rm'' (c,fv)
  | C.Asign,[v1;v2],[],[c,fv] ->
      let rm'  = Rmap.release rm !fv in
        Printf.printf "%s=%s;\n" (val2str rm v1) (val2str rm v2);
        emit rm' (c,fv)
  | C.Update,[v1;o;v2],[],[c,fv] ->
      let rm'  = Rmap.release rm !fv in
        Printf.printf "((int *)%s)[TOCINT(%s)]=%s;\n"
          (val2str rm v1) (val2str rm o) (val2str rm v2);
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

