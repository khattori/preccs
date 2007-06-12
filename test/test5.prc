type lvv = {{ l:octet; v1:octet[l]; v2:octet[l] }}

type foo = {{ ll:octet[1]; mm:octet[2]; nn:octet[3] }}
type bar = {{ l :octet[3]; m :octet[2]; n :foo }}

proc Test5(end:<bool>,ok:<int>,ng:<int>) =
    var x1 = "00"h;
    ( x1 @ x:lvv -> ok!1;
                    ( x.l @ "00"h -> ok!1
                          | _     -> ng!1 );
                    ( x.v1 @ "" -> ok!1
                           | _  -> ng!1 );
                    ( x.v2 @ "" -> ok!1
                           | _  -> ng!1 )
         | _     -> ng!1 );
    var x2 = "010000"h;
    ( x2 @ x:lvv -> ok!1;
                    ( x.v1 @ "00"h -> ok!1
                           | _  -> ng!1 );
                    ( x.v2 @ "00"h -> ok!1
                           | _  -> ng!1 )
         | _     -> ng!1 );
    var x3 = "0201020304"h;
    ( x3 @ x:lvv -> ok!1;
                    ( x.v1 @ "0102"h -> ok!1
                           | _  -> ng!1 );
                    ( x.v2 @ "0304"h -> ok!1
                           | _  -> ng!1 )
         | _     -> ng!1 );
    var x4 = "03000102000102"h;
    ( x4 @ x:lvv -> ok!1
         | _     -> ng!1 );
    var x5 = "040001020300010203"h;
    ( x5 @ x:lvv -> ok!1
         | _     -> ng!1 );
    var x6 = "0500010203040001020304"h;
    ( x6 @ x:lvv -> ok!1
         | _     -> ng!1 );

    var x:bar;
    ( x.n.nn @ "000000"h -> ok!1
             | _         -> ng!1 );

    end!true
