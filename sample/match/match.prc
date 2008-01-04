C{
#include <stdio.h>
C}

proc Main() =
//	Match("00"h);
//	Match("0101"h);
//	Match("0102"h);
//	Match("020102"h);
	Match("020201"h);
	Match2("ab");
	Match2("a" ^ "00"h ^ "b");
	Match2("a" ^ "01"h^"bb");
	Match2("a" ^ "01"h^"b" ^ "02"h^"bbb")
proc Match(msg:string) =
	msg @ x : { l: octet; m: ("01"h|"02"h)[l] } -> stdout!"OK\n"
	    | _ -> stdout!"NG\n"
proc Match2(msg:string) =
	msg @ x : "a"; { l: octet; m: "b"[l] }*; "b" -> stdout!"OK\n"
	    | _ -> stdout!"NG\n"

