C{
#include <stdio.h>
C}

import "test1.prc"
import "test2.prc"
import "test3.prc"
import "test4.prc"

var ok:<int>
var ng:<int>
var res:<bool>

proc Main() =
    Total();
    var end:<bool>;
    Test1(end,ok,ng); end?x;
    Test2(end,ok,ng); end?x;
    Test3(end,ok,ng); end?x;
    Test4(end,ok,ng); end?x;
    res!true;
    stop

proc Total() = Total_(0,0)
proc Total_(okn:int,ngn:int) =
    ok?n  -> Total_(okn+n,ngn)
  | ng?n  -> Total_(okn,ngn+n)
  | res?x -> var total = okn + ngn;
             C{
             printf("Result: %d/%d passed.\n", TOCINT($okn$), TOCINT($total$));
             C}
