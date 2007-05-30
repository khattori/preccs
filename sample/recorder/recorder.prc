type FileIn  = {ok:bool;in:<string>}
type FileOut = {ok:bool;out:<string>}

type CMD_RECD = {"R"|"r"}
type CMD_PLAY = {"P"|"p"}
type CMD_QUIT = {"Q"|"q"}
type CMD_STOP = {"S"|"s"}

proc Main() = Ready()

proc Ready() =
    stdout!"Menu:\n- (R)ecord\n- (Q)uit\n> ";
    stdin?msg;
    ( msg @ c:CMD_RECD;octet* -> Recording()
          | c:CMD_QUIT;octet* -> stdout!"Quit\n"; stop
          | _                 -> stdout!"Invalid Command\n"; Ready() )
proc Ready2() =
    stdout!"Menu:\n- (P)lay\n- (R)ecord\n- (Q)uit\n> ";
    stdin?msg;
    ( msg @ c:CMD_PLAY;octet* -> Playing()
          | c:CMD_RECD;octet* -> Recording()
          | c:CMD_QUIT;octet* -> stdout!"Quit\n"; stop
          | _                 -> stdout!"Invalid Command\n"; Ready2() )

proc Playing() =
    var ret:<FileIn>;
    PrcFileOpenR(ret, "temp");
    ret?fr;
    ( fr.ok @ false -> stdout!"file open failed\n"; stop 
            | true  -> var wout:<string>;
                       C{ prc_WaveOutOpen($wout$, 8000); C};
                       var m:<string>; WaveOut(m, fr.in, wout);
                       stdout!"Menu:\n- (S)top\n- (Q)uit\n> ";
                       ( stdin?msg ->
                         ( msg @ c: CMD_STOP;octet* -> m!"stop"; Ready2()
                               | c: CMD_QUIT;octet* -> stop
                               | _ -> stdout!"Invalid Command\n"; Playing() )
                       | m?msg -> Ready2() ) )

proc Recording() =
    var ret:<FileOut>;
    PrcFileCreate(ret, "temp");
    ret?fr;
    ( fr.ok @ false -> stop
            | true  -> var win:<string>;
                       C{ prc_WaveInOpen($win$, 8000); C};
                       var m:<string>; WaveIn(m, win, fr.out);
                       stdout!"Menu:\n- (S)top\n- (Q)uit\n> ";
                       stdin?msg;
                       ( msg @ c: CMD_STOP;octet* -> m!"stop"; Ready2()
                             | c: CMD_QUIT;octet* -> stop
                             | _ -> stdout!"Invalid Command\n"; Recording() ) )

proc WaveOut(m:<string>, in:<string>, out:<string>) =
    m?msg   -> out?x; stop
  | in?data -> ( data @ "" -> out!""; m!"end"
                      | _  -> out!data; WaveOut(m, in, out) )

proc WaveIn(m:<string>, in:<string>, out:<string>) =
    m?msg   -> in!""; out!""; stop
  | in?data -> out!data; WaveIn(m, in, out)

proc PrcFileOpenR(ret:<FileIn>, fname:string) =
    var h:int;
    var fin:<string>;
    C{ $h$ = TOPINT(prc_FileOpenR($fin$,STRPTR($fname$))); C};
    ( h @ -1 -> ret!{ok=false;in=fin}
        | _  -> ret!{ok=true;in=fin} )

proc PrcFileCreate(ret:<FileOut>, fname:string) =
    var h:int;
    var fout:<string>;
    C{ $h$ = TOPINT(prc_FileCreate($fout$,STRPTR($fname$))); C};
    ( h @ -1 -> ret!{ok=false;out=fout}
        | _  -> ret!{ok=true;out=fout} )
