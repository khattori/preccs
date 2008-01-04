/*
 * Fibonacci数を求める
 */
C{
#include <stdio.h>
#include <string.h>
C}
proc Main() = 
    var n = Str2Int(stdin);
    Int2Str(FibProc(n),stdout)

proc FibProc(n:int):int =
    n @ 0 -> return 0
      | 1 -> return 1
      | _ -> return FibProc(n-1)+FibProc(n-2)

proc Int2Str(n:int,out:<string>) =
    var s:string;
    C{ static char buf[BUFSIZ];
       sprintf(buf,"%d",$n$/2);
       $s$=__string__(strlen(buf),buf); C};
    out!s^"\n"

proc Str2Int(in:<string>):int =
    var n:int;
    in?s;
    C{ $n$ = atoi(STRPTR($s$)); $n$=($n$<<1)^1; C};
    return n

C{
/* trailer part */
C}
