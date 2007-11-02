C{
#include <stdio.h>
C}
proc Main() =
   var nch:<int>;
   PrimeNumbers(nch); PrintInt(nch)

proc PrimeNumbers(out:<int>) =
   var ch:<int>;
   Nums(2,ch); Sieve(ch,out)

proc Nums(n:int,out:<int>) =
   out!n; Nums(n+1,out)

proc Sieve(in:<int>,out:<int>) =
    var ch:<int>;
    in?n; out!n;
    Filter(n,in,ch); Sieve(ch,out)

proc Filter(n:int,in:<int>,out:<int>) =
    in?x;
    ( x%n @
      0 -> Filter(n,in,out)
    | _ -> out!x; Filter(n,in,out) )

proc PrintInt(nch:<int>) =
    nch?n;
    C{ printf("%d\n", TOCINT($n$)); C};
    PrintInt(nch)  