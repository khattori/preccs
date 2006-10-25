proc Test3(end:<bool>,ok:<int>,ng:<int>) =
    ( 1+2*3==7 @ true -> ok!1 | false -> ng!1 );
    ( 9/4-1==1 @ true -> ok!1 | false -> ng!1 );
    ( -4==0-4  @ true -> ok!1 | false -> ng!1 );
    ( -13==0-13@ true -> ok!1 | false -> ng!1 );
    ( -(-33)==33@ true -> ok!1 | false -> ng!1 );
    ( 3+5==8   @ true -> ok!1 | false -> ng!1 );
    ( 13%5==3  @ true -> ok!1 | false -> ng!1 );
    end!true