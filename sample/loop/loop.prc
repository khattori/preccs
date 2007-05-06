/*
 * 無限ループ
 */
C{
#include <stdio.h>
#include <string.h>
C}
proc Main() = var ch:<bool>; Stop(ch); Loop(0,ch)
proc Loop(n:int,ch:<bool>) =
    ch?msg  -> stop
  | null!() -> var ret:<string>;
               Int2Str(n,ret);
               ret?msg; stdout!msg;
               Loop(n+1,ch)
proc Stop(ch:<bool>) = stdin?msg; ch!true
/*
proc PrintInt(n:int) =
    C{
        printf("%d\n", TOCINT($n$));
        fflush(stdout);
    C}
*/
proc Int2Str(n:int,out:<string>) =
    var s:string;
    C{ static char buf[BUFSIZ];
       sprintf(buf,"%d\n",TOCINT($n$));
       $s$=__string__(strlen(buf),buf); C};
    out!s
