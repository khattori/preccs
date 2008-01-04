type KeyVal = { key:string; val:string }
type Table = {
    add:<KeyVal>;
    get:<{ key:string; ret:<string> }>;
    del:<{ key:string; ret:<bool> }>;
    ___:<unit>
}
proc TableProc() = MakeTable(); CreateTable()

var mktbl:<Table>
proc MakeTable() =
    var addCh:<KeyVal>;
    var getCh:<{ key:string; ret:<string> }>;
    var delCh:<{ key:string; ret:<bool> }>;
    var ch:<unit>;
    mktbl!{ add=addCh; get=getCh; del=delCh; ___=ch };
    MakeTable()

var create_table:<Table>
proc CreateTable() =
    mktbl?tbl;
    create_table!tbl;
    NoDataHd(tbl);
    CreateTable()

proc NoDataHd(tbl:Table) =
     tbl.add?req -> mktbl?ntbl;
                    HasDataHd(tbl, ntbl, req);
                    NoDataTl(tbl,ntbl)
   | tbl.get?req -> req.ret!""; NoDataHd(tbl)
   | tbl.del?req -> req.ret!false; NoDataHd(tbl)
proc NoData__(pre:Table,nxt:Table) =
     pre.___?req -> nxt.___!()
   | pre.get?req -> nxt.get!req; NoData__(pre,nxt)
   | pre.del?req -> nxt.del!req; NoData__(pre,nxt)
proc NoDataTl(tbl:Table,pre:Table) =
     pre.___?req -> NoDataHd(tbl)
   | pre.get?req -> req.ret!""; NoDataTl(tbl,pre)
   | pre.del?req -> req.ret!false; NoDataTl(tbl,pre)

proc HasDataHd(tbl:Table, nxt:Table, kv:KeyVal) =
     tbl.add?req -> mktbl?ntbl;
                    HasDataHd(tbl, ntbl, req);
                    HasData__(tbl, ntbl, nxt, kv)
   | tbl.get?req ->
       ( (req.key == kv.key) @ true -> req.ret!kv.val
                             | _    -> nxt.get!req );
       HasDataHd(tbl, nxt, kv)
   | tbl.del?req ->
       ( (req.key == kv.key) @ true -> nxt.___!()
                             | _    -> nxt.del!req;
                                       HasDataHd(tbl, nxt, kv) )
proc HasData__(tbl:Table, pre:Table, nxt:Table, kv:KeyVal) =
     pre.___?req -> HasDataHd(tbl, nxt, kv)
   | pre.get?req ->
       ( req.key == kv.key @ true -> req.ret!kv.val
                           | _    -> nxt.get!req );
       HasData__(tbl, pre, nxt, kv)
   | pre.del?req ->     
       ( req.key == kv.key @ true -> NoData__(pre,nxt)
                           | _    -> nxt.del!req;
                                    HasData__(tbl, pre, nxt, kv) )

proc Main() =
    TableProc();
    create_table?tbl1;
    var ret:<string>; 
    var retb:<bool>;

    tbl1.get!{key="k1";ret=ret}; ret?val; stdout!"ret1:"^val^"\n";

    tbl1.add!{key="k1";val="hogehoge"};
    tbl1.add!{key="k2";val="foobar"};
    tbl1.add!{key="k3";val="hattori"};

    tbl1.get!{key="k1";ret=ret}; ret?val; stdout!"ret1:"^val^"\n";
    tbl1.get!{key="k2";ret=ret}; ret?val; stdout!"ret2:"^val^"\n";
    tbl1.get!{key="k3";ret=ret}; ret?val; stdout!"ret3:"^val^"\n";

    tbl1.del!{key="k2";ret=retb}; stdout!"delete k2\n";

    tbl1.get!{key="k1";ret=ret}; ret?val; stdout!"ret1:"^val^"\n";
    tbl1.get!{key="k2";ret=ret}; ret?val; stdout!"ret2:"^val^"\n";
    tbl1.get!{key="k3";ret=ret}; ret?val; stdout!"ret3:"^val^"\n";

    tbl1.del!{key="k1";ret=retb}; stdout!"delete k1\n";
    tbl1.get!{key="k1";ret=ret}; ret?val; stdout!"ret1:"^val^"\n";
    tbl1.get!{key="k3";ret=ret}; ret?val; stdout!"ret3:"^val^"\n";

    tbl1.del!{key="k3";ret=retb}; stdout!"delete k3\n";
    tbl1.get!{key="k3";ret=ret}; ret?val; stdout!"ret3:"^val^"\n"
