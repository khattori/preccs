type TLVP = {{
    tag : octet[2];
    len : octet[2];
    val : octet[len];
    pad : "00"h[padding(len)]
}}
type Mesg = {{
    version: "01"h;
    flags  : octet;
    rsrvd  : "0000"h;
    tlvs   : TLVP*
}}

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

    var msg = "01ff0000"h;
    var tlv0  = "01010000"h;
    var tlv1  = "01010001"h^"a"^"000000"h;
    var tlv2  = "01010002"h^"ab"^"0000"h;
    var tlv3  = "01010003"h^"abc"^"00"h;
    var tlv4  = "01010004"h^"abcd";
    var tlv5  = "01010005"h^"abcde"^"000000"h;
    var tlvx  = "01010008"h^"abcd"^"000000"h;

    ( msg @ m:Mesg -> ok!1;
                      ( m.version @ "01"h -> ok!1
                                  | _     -> ng!1 );
                      ( m.tlvs @ "" -> ok!1
                               | _  -> ng!1 )
          | _ -> ng!1 );
    ( msg^tlv0 @ m:Mesg -> ok!1;
                           ( m.tlvs @ t:TLVP -> ok!1;
                                              ( t.tag @ "0101"h -> ok!1
                                                      | _ -> ng!1 );
                                              ( t.len @ "0000"h -> ok!1
                                                      | _ -> ng!1 );
                                              ( t.val @ "" -> ok!1
                                                      | _  -> ng!1 );
                                              ( t.pad @ "" -> ok!1
                                                      | _  -> ng!1 )
                                   | _ -> ng!1 ) 
            | _ -> ng!1 );
    ( msg^tlv1 @ m:Mesg -> ok!1;
                           ( m.tlvs @ t:TLVP -> ok!1;
                                              ( t.len @ "0001"h -> ok!1
                                                      | _ -> ng!1 );
                                              ( t.val @ "a" -> ok!1
                                                      | _  -> ng!1 );
                                              ( t.pad @ "000000"h -> ok!1
                                                      | _  -> ng!1 )
                                   | _ -> ng!1 ) 
            | _ -> ng!1 );
    ( msg^tlv2 @ m:Mesg -> ok!1;
                           ( m.tlvs @ t:TLVP -> ok!1;
                                              ( t.val @ "ab" -> ok!1
                                                      | _  -> ng!1 );
                                              ( t.pad @ "0000"h -> ok!1
                                                      | _  -> ng!1 )
                                   | _ -> ng!1 ) 
            | _ -> ng!1 );
    ( msg^tlv3^tlv4^tlv5 @ m:Mesg -> ok!1;
                           ( m.tlvs @ t:{t3:TLVP;t4:TLVP;t5:TLVP} -> ok!1;
                                              ( t.t3.val @ "abc" -> ok!1
                                                      | _  -> ng!1 );
                                              ( t.t3.pad @ "00"h -> ok!1
                                                      | _  -> ng!1 );
                                              ( t.t4.val @ "abcd" -> ok!1
                                                      | _  -> ng!1 );
                                              ( t.t4.pad @ "" -> ok!1
                                                      | _  -> ng!1 );
                                              ( t.t5.val @ "abcde" -> ok!1
                                                      | _  -> ng!1 );
                                              ( t.t5.pad @ "000000"h -> ok!1
                                                      | _  -> ng!1 )
                                   | _ -> ng!1 ) 
            | _ -> ng!1 );
    ( msg^tlvx^tlv3 @ m:Mesg -> ng!1
                    | _      -> ok!1 );

    end!true
