proc Test3(end:<bool>,ok:<int>,ng:<int>) =
    ( 1+2*3==7 @ true -> ok!1 | false -> ng!1 );
    ( 9/4-1==1 @ true -> ok!1 | false -> ng!1 );
    ( -4==0-4  @ true -> ok!1 | false -> ng!1 );
    ( -13==0-13@ true -> ok!1 | false -> ng!1 );
    ( -(-33)==33@ true -> ok!1 | false -> ng!1 );
    ( 3+5==8   @ true -> ok!1 | false -> ng!1 );
    ( 13%5==3  @ true -> ok!1 | false -> ng!1 );
    // ( "foo"=="foo" @ true -> ok!1 | false -> ng!1 );
    ( "foo"=="hoge" @ true -> ng!1 | false -> ok!1 );
    var x = "foo"; var y = "bar"; var z = "baz";
    ( x @ z -> ng!1 | y -> ng!1 | x -> ok!1 );
    var x = 1; var y = 2; var z = 3;
    ( x @ 4 -> ng!1 | z -> ng!1 | y -> ng!1 | x -> ok!1 );
    end!true
