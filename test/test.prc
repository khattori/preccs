C{
#include <stdio.h>
C}
import "test1.prc"
import "test2.prc"
import "test3.prc"
import "test4.prc"
import "test5.prc"

var ok:<int>
var ng:<int>
var res:<bool>

proc Main() =
    Total();
    var end:<bool>;

    Test1(end,ok,ng); end?x;
    C{ printf("TEST1 completed\n"); C};
    Test2(end,ok,ng); end?x;
    C{ printf("TEST2 completed\n"); C};
    Test3(end,ok,ng); end?x;
    C{ printf("TEST3 completed\n"); C};
    Test4(end,ok,ng); end?x;
    C{ printf("TEST4 completed\n"); C};
    Test5(end,ok,ng); end?x;
    C{ printf("TEST5 completed\n"); C};
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
