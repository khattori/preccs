/*
 * 無限ループ
 */
C{
#include <stdio.h>
#include <string.h>
C}
proc Main() =
    var ch:<bool>;
    Loop0(0, ch); ch?ret; stdout!"Loop0 finished\n";
    Loop1(0, ch); ch?ret; stdout!"Loop1 finished\n";
    Loop2(0, ch); ch?ret; stdout!"Loop2 finished\n";
    Loop3(0, ch); stdin?msg; ch!true;
    Loop4(0, ch); stdin?msg; ch!true

proc Loop0(n:int, ch:<bool>) =
    var ret:<string>;
    Int2Str(n,ret);
    ret?msg;
    { stdin?msg  -> ch!true
    | stdout!msg -> stdout!"OK\n"; Loop0(n+1, ch) }

proc Loop1(n:int, ch:<bool>) =
    var ret:<string>;
    Int2Str(n,ret);
    ret?msg;
    { stdin?msg  -> ch!true
    | stdout!msg -> Loop1(n+1, ch) }

proc Loop2(n:int, ch:<bool>) =
    var ret:<string>;
    Int2Str(n,ret);
    ret?msg;
    { stdout!msg -> Loop2(n+1, ch)
    | stdin?msg  -> ch!true }

proc Loop3(n:int, ch:<bool>) =
    var ret:<string>;
    Int2Str(n,ret);
    ret?msg;
    { ch?msg  -> stdout!"Loop3 finished\n" 
    | stdout!msg -> Loop3(n+1, ch) }

proc Loop4(n:int, ch:<bool>) =
    var ret:<string>;
    Int2Str(n,ret);
    ret?msg;
    { stdout!msg -> Loop4(n+1, ch)
    | ch?msg  -> stdout!"Loop4 finished\n" }

proc Int2Str(n:int,out:<string>) =
    var s:string;
    C{ static char buf[BUFSIZ];
       sprintf(buf,"%d\n",TOCINT($n$));
       $s$=__string__(strlen(buf),buf); C};
    out!s
