//
// Catコマンド
//    --- 自分自身のファイル（cat.prc）を標準出力に表示する
//
type FileIn = {ok:bool;in:<string>}

proc Main() =
    Cat("cat.prc")

proc Cat(fname:string) =
    var ret:<FileIn>;
    PrcFileOpenR(ret, fname);
    ret?fr;
    { fr.ok @ true  -> FileCopy(fr.in, stdout)
            | false -> stdout!"file not found.\n" }

proc FileCopy(in:<string>, out:<string>) =
    in?buf;
    { buf @ "" -> stop
          | _  -> out!buf; FileCopy(in,out) }

proc PrcFileOpenR(ret:<FileIn>, fname:string) =
    var h:int;
    var fin:<string>;
    C{ $h$ = TOPINT(prc_FileOpenR($fin$,STRPTR($fname$))); C};
    { h @ -1 -> ret!{ok=false;in=fin}
        | _  -> ret!{ok=true;in=fin} }
