C{
#include <stdio.h>
C}

type FooBar = { ("foo"|"hoge")* }

proc Main() =
       var msg = "foohogefoohogefoohogefoohogefoohoge";
	Match(msg^msg);
       stdout!msg^msg

proc Match(msg:string) =
	msg @ x:FooBar -> Skip(x,100000)
	    | _ -> stdout!"Match:NG\n"

proc Skip(msg:FooBar,n:int) =
	n @ 0 -> stop
	  | _ -> ( msg @ x:"foohoge"* -> Skip(msg, n-1)
			 | _ -> stdout!"Skip:NG\n" )
