type Week = | Mon | Tue | Wed |Thu| Fri | Sat|Sun|
type Type = | INT:int|STR:string|POS:(int,int) |

proc Test6(end:<bool>,ok:<int>,ng:<int>) =
    var tue:Week;
    tue := |Tue|;
    { tue @ |Mon| -> ng!1
          | |Tue| -> ok!1
          | |Wed| -> ng!1
          | _ -> ng!1 }; 
    var x:(int,bool,Week);
    x := (2,true,|Sun|);
    { x @ (3,true,|Sat|) -> ng!1
        | (_,_,|Wed|) -> ng!1
        | (2,true,_) -> ok!1
        | (3,true,|Sun|) -> ng!1
        | (_,_,_) -> ng!1};
    var y=(1,2);
    { y @ (1,1) -> ng!1
        | (1,2) -> ok!1
        | (3,4) -> ng!1
        | _ -> ng!1 };
    { y @ (1,1) -> ng!1
        | (3,4) -> ng!1
        | (1,2) -> ok!1
        | _ -> ng!1 };
    var t:Type;
    t := |POS=(3,6)|;
    { t @ |INT=_| -> ng!1
        | |STR=x| -> stdout!"NG:"^x; ng!1
        | |POS=(3,3)| -> ng!1
        | |POS=(3,6)| -> ok!1 };
    var t:Type;
    t := |INT=9|;
    { t @ |INT=3| -> ng!1
        | |INT=9| -> ok!1
        | |STR=x| -> stdout!"NG:"^x; ng!1
        | |POS=(3,3)| -> ng!1
        | |POS=(3,6)| -> ng!1 };
    var ich:<int>;
    { IntProc(ich) @ 0 -> ng!1
                   | 1 -> ok!1
                   | _ -> ng!1 };
    ich?x;
    { x @ 0 -> ng!1
        | 1 -> ng!1
        | 2 -> ok!1
        | _ -> ng!1 };
    ich?x;
    { x @ 0 -> ng!1
        | 3 -> ok!1
        | 2 -> ng!1
        | _ -> ng!1 };
    end!true

proc IntProc(ret:<int>):int =
   return 1;
   ret!2;
   ret!3
   
