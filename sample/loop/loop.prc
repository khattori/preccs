/*
 * 無限ループ
 */
C{
#include <stdio.h>
C}
proc Main() = var ch:<bool>; Stop(ch); Loop(0,ch)
proc Loop(n:int,ch:<bool>) =
    ch?msg  -> stop
  | null!() -> PrintInt(n); Loop(n+1,ch)
proc Stop(ch:<bool>) = stdin?msg; ch!true
proc PrintInt(n:int) = skip
/*
    C{
        printf("%d\n", $n$/2);
        fflush(stdout);
    C}
*/
/*
proc Int2Str(n:int,out:<string>) =
    var s:string;
    C{ static char buf[BUFSIZ];
       sprintf(buf,"%d\n",$n$/2);
       $s$=__string__(strlen(buf),buf); C};
    out!s
*/


