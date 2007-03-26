type BootpOption = {{
    len	: octet;
    tag	: octet;
    data	: octet[len]
}}
type Bootp = {{
    opt	: BootpOption;
    data	: octet*
}}

type Bopt1 = BootpOption{tag={"a"}}
type Bopt2 = BootpOption{tag={"b"}}
type Bopt3 = BootpOption{tag={"a"},len={"a"}}
type Bopt4 = BootpOption{len={"04"h},tag={"00"h}}

type BootpFoo = Bootp{opt.tag={"01"h}}
type BootpBar = Bootp{opt.tag={"02"h}}

type TEXT = {/"^\r\n"*}

proc Test2(end:<bool>,ok:<int>,ng:<int>) =

    var x = "hogehogehogehogehogehoge";
    ( "00"h^"a" @ x: Bopt1 -> ok!1
                | x: Bopt2 -> ng!1
                | _        -> ng!1 );
    ( "00"h^"b" @ x: Bopt1 -> ng!1
                | x: Bopt2 -> ok!1
                | _        -> ng!1 );
    ( "0001"h @ x: BootpFoo -> ok!1
              | x: BootpBar -> ng!1
              | _           -> ng!1 );
    ( "0002"h @ x: BootpFoo -> ng!1
              | x: BootpBar -> ok!1
              | _           -> ng!1 );
    ( "0003"h @ x: BootpFoo -> ng!1
              | x: BootpBar -> ng!1
              | _           -> ok!1 );

    var x:{octet|"hoge"};
    ( x @ y:octet -> ok!1
        | y:"hoge" -> ng!1 );

    var x:{{l:octet;m:(octet;octet)}|octet};
    ( x @ y:octet[3] -> ok!1
        | y:octet -> ng!1);

    var x:{octet+};
    ( x @ y:{l:octet;m:octet[l]} -> ok!1
        | _ -> ng!1 );

    var x:{{l:octet;m:octet}};
    ( x @ y:{l:octet;m:octet[l]} -> ng!1
	 | _ -> ok!1 );

    var x:{{l:octet;m:octet*}};
    ( x @ y:{l:octet;m:octet[l]} -> ok!1
	 | _ -> ng!1 );

    var x:{{l:octet;m:octet[l]}};
    ( x @ y:{l:octet;m:octet*} -> ok!1
	 | _ -> ng!1 );

    var x:{{l:octet;m:octet[l]}};
    ( x @ y:octet* -> ok!1 );

    var x:{{l:octet;m:octet[l]}*};
    ( x @ y:{l:octet;m:octet[l]} -> ng!1
        | y:"" -> ok!1
	 | _ -> ng!1 );

    var x:{{l:octet;m:octet[l]}};
    ( x @ y:{l:octet;m:octet[l]}* -> ok!1
	 | _ -> ng!1 );

    var x:{{l:octet;m:octet[l]}|octet};
    ( x @ y:{l:octet;m:octet[l]} -> ok!1
        | y:octet -> ng!1 );

    var x:{{l:octet;m:octet[l]}};
    ( x @ _ -> ok!1 );

    var x:{{l:octet;m:octet[l]}};
    ( x @ y:{l:octet;m:octet[l]} -> ok!1
	 | _ -> ng!1 );

    var x:BootpOption;
    ( x @ y:Bopt1 -> ng!1
        | y:Bopt2 -> ng!1
        | y:Bopt3 -> ng!1
        | y:Bopt4 -> ng!1
        | _ -> ok!1 );

    var x:{{l:octet;m:octet[l]}|octet};
    ( x @ y:{l:octet;m:octet[l]}|octet -> ok!1
        | _ -> ng!1 );

    var x = "hogehogehoge";
    ( x @ y:{l:octet;m:(octet;octet)*} -> ng!1
        | _ -> ok!1 );

    var x = "hhogehogehoge";
    ( x @ "h" -> ng!1 
	 | _ -> ok!1 );

    var x="hhogehogehoge";
    ( x @ y:{l:octet;m:octet*} ->
          ( y.l @ "h" -> ok!1
                | _   -> ng!1 )
        | _ -> ng!1 );

    var x:{("foo"|"hoge")*};
    ( x @ y:"foohoge"*      -> ok!1
        | _ -> ng!1 );

    var x:octet;
    ( x @ "h" -> ng!1
        | _ -> ok!1 );

    var x="";
    ( x @ y:"a"* -> ok!1
        | _ -> ng!1 );
    ( x @ y:{a:"a"*} -> ok!1;
          ( y.a @ "" -> ok!1
                | _  -> ng!1 )
        | _ -> ng!1);

    var x="aaaabbb";
    ( x @ y:{a:"a"*;b:"b"*} ->
          ( y.a @ "aaaa" -> ok!1
                | _ -> ng!1 );
          ( y.b @ "bbb" -> ok!1
                | _ -> ng!1 )
        | _ -> ng!1 );

    var x="";
    ( x @ y:{a:"a"*;b:"b"*} ->
          ( y.a @ "" -> ok!1
                | _ -> ng!1 );
          ( y.b @ "" -> ok!1
                | _ -> ng!1 )
        | _ -> ng!1 );

    var x="0105FF"h;
    ( x @ y:{l:octet;m:octet;n:(octet[l]|octet[m])} -> ok!1
        | _ -> ng!1 );

    var x="hogehoge\r\n";
    ( x @ y:{text:TEXT; cr:("\r"|"\n")*} ->
	    ok!1; 
            ( y.text @ "hogehoge" -> ok!1
                     | _ -> ng!1 
	    )
        | _ -> ng!1 );

    end!true

