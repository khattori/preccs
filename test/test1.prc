//
// 代入のテスト
//
var x = "hello"
var n = 0
var r = {l="hoge";m="foo";n="bar"}
proc Test1(end:<bool>,ok:<int>,ng:<int>) =
    ( x @ "hello" -> ok!1
        | _       -> ng!1 ); x := "foo";
    ( x @  "foo"  -> ok!1
        | _       -> ng!1 );

    ( n==0 @ true  -> ok!1
           | false -> ng!1 ); n := n+1;
    ( n==1 @ true  -> ok!1
           | false -> ng!1 ); n := n+n;
    ( n @ 2 -> ok!1
        | _ -> ng!1 );

    ( r.m @ "foo"  -> ok!1
          | _      -> ng!1 ); r.m := "bake";
    ( r.m @ "bake" -> ok!1
          | _      -> ng!1 );

    end!true
