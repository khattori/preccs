C{
#include <stdio.h>
C}

proc Main() =
  var in:<int>;
  var out:<int>;
  Buffer(in,out);
  Sender(in, 0);
  Receiver(out,"id1");
  Receiver(out,"id2");
  Receiver(out,"id3")
proc Sender(in:<int>,n:int) =
  in!n; timer!(0,100); Sender(in, n+1)
proc Receiver(out:<int>, id:string) =
  out?n;
  C{
     printf("%s: %d\n", STRPTR($id$), TOCINT($n$));
     fflush(stdout);
  C};
  Receiver(out,id)

proc Buffer(in:<int>,out:<int>) =
  in?n; var ch:<unit>;
  Buf_tl(in,out,ch); Buf_hd(n,out,ch)

proc Buf_hd(n:int,out:<int>,t:<unit>) =
  out!n; t!()

proc Buf_tl(in:<int>,out:<int>,t:<unit>) =
    in?n -> var ch:<unit>;
            Buf_tl(in,out,ch);
            Buf_(n,out,ch,t)
  | t?x  -> Buffer(in,out)

proc Buf_(n:int,out:<int>,t1:<unit>,t2:<unit>) =
  t2?x; Buf_hd(n,out,t1)

