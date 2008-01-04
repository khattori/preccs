/*
 * パターンマッチのテスト
 */
proc Test3(end:<bool>,ok:<int>,ng:<int>) =
    { 1+2*3==7 @ true -> ok!1 | false -> ng!1 };
    { 9/4-1==1 @ true -> ok!1 | false -> ng!1 };
    { -4==0-4  @ true -> ok!1 | false -> ng!1 };
    { -13==0-13@ true -> ok!1 | false -> ng!1 };
    { -(-33)==33@ true -> ok!1 | false -> ng!1 };
    { 3+5==8   @ true -> ok!1 | false -> ng!1 };
    { 13%5==3  @ true -> ok!1 | false -> ng!1 };
//    { "foo"=="foo" @ true -> ok!1 | false -> ng!1 };
    { "foo"=="hoge" @ true -> ng!1 | false -> ok!1 };
    var x = "foo";
    { x @ "baz" -> ng!1 | "bar" -> ng!1 | _ -> ok!1 };
    var x = 1;
    { x @ 4 -> ng!1 | 3 -> ng!1 | 2 -> ng!1 | 1 -> ok!1 };
    var x = {f1="foo";f2="bar";f3="baz"}; var y = "bar";
    { x.f1 @ "foo" -> ok!1 | "bar" -> ng!1 | "baz" -> ng!1 };
    { x.f2 @ "foo" -> ng!1 | "bar" -> ok!1 | "baz" -> ng!1 };
    { x.f3 @ "foo" -> ng!1 | "bar" -> ng!1 | "baz" -> ok!1 };

    end!true
