//
// Memoコマンド
//   --- tempファイルに標準入力からの入力を書き出す
//   --- qかQで終了する
//
type FileOut = {ok:bool;out:<string>}

proc Main() =
    Memo("temp")

proc Memo(fname:string) =
    var ret:<FileOut>;
    PrcFileCreate(ret, fname);
    ret?fr;
    { fr.ok @ true  -> FileCopy(stdin, fr.out)
            | false -> stdout!"file not found.\n" }

proc FileCopy(in:<string>, out:<string>) =
    in?buf;
    { buf @ c:("q"|"Q");octet* -> out!""; stdout!"FileCopy: stop\n"
          | _  -> out!buf; FileCopy(in,out) }

proc PrcFileCreate(ret:<FileOut>, fname:string) =
    var h:int;
    var fout:<string>;
    C{ $h$ = TOPINT(prc_FileCreate($fout$,STRPTR($fname$))); C};
    { h @ -1 -> ret!{ok=false;out=fout}
        | _  -> ret!{ok=true;out=fout} }
