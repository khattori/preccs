//
// Timer$B!'%?%$%^!<%A%c%M%k$HA*Br<B9T$N%5%s%W%k(B
//
// $B0lDj;~4VF~NO$,L5$$>l9g$K$O%?%$%`%"%&%H$9$k(B
//
proc Timer() = 
    stdin?msg -> stdout!("message: " ^ msg)
  | timer!3  -> stdout!"timeout\n"

proc Main() = Timer()