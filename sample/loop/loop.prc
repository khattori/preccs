/*
 * ñ≥å¿ÉãÅ[Év
 */
C{
#include <stdio.h>
C}
proc Main() = Loop(0)
proc Loop(n:int) = PrintInt(n); Loop(n+1)
proc PrintInt(n:int) =
    C{
        printf("%d\n", $n$/2);
        fflush(stdout);
    C}
/*
proc Int2Str(n:int,out:<string>) =
    var s:string;
    C{ static char buf[BUFSIZ];
       sprintf(buf,"%d\n",$n$/2);
       $s$=__string__(strlen(buf),buf); C};
    out!s
*/


