/*
 * Fibonacci数を求める
 */
C{
#include <stdio.h>
C}
proc Main() = 
    var ch:<int>;
    stdin?s; Str2Int(s,ch);
    ch?n;
    FibProc(n,ch);
    ch?r;Int2Str(r,stdout)

proc FibProc(n:int, out:<int>) =
    n @ 0 -> out!0
      | 1 -> out!1
      | _ -> var ch:<int>;
             FibProc(n-1,ch);
             FibProc(n-2,ch);
	      ch?r1;ch?r2;out!r1+r2

proc Int2Str(n:int,out:<string>) =
    var s:string;
    C{ static char buf[BUFSIZ];
       sprintf(buf,"%d",$n$/2);
       $s$=__string__(strlen(buf),buf); C};
    out!s

proc Str2Int(s:string,out:<int>) =
    var n:int;
    C{ $n$ = atoi(STRPTR($s$)); $n$=($n$<<1)^1; C};
    out!n

C{
/* trailer part */
C}