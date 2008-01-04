C{
#include "iotest.h"
C}

var sink:<unit>
var source:<unit>

proc SinkProc() = sink?x; SinkProc()
proc SourceProc() = source!(); SourceProc()

proc Main() =
    var mode:int;
    SinkProc(); SourceProc();
    C{ $mode$ = TOPINT(g_test_mode); C};
    { mode @ 1 -> StdinProc()
           | 2 -> StdoutProc()
           | 3 -> TimerProc()
	   | 4 -> ServerProc()
	   | 5 -> ClientProc()
	   | 6 -> UdpProc()
           | _ -> stop }

proc StdinProc() = stdout!"start StdinProc()\n";
    stdin?msg; stdout!msg;
    { stdin?msg -> stdout!msg
    | stdin?msg -> stdout!"unexpected\n" };

    { stdin?msg -> stdout!"unexpected\n"
    | null!()   -> stdout!"1st message\n" };
    { null!()   -> stdout!"2nd message\n"
    | stdin?msg -> stdout!"unexpected\n" };

    { stdin?msg -> stdout!msg
    | timer!(10,0) -> stdout!"unexpected\n" };
    { timer!(10,0) -> stdout!"unexpected\n"
    | stdin?msg -> stdout!msg };

    { sink!()	-> stdout!"3rd message\n"
    | sink!()	-> stdout!"unexpected\n" };
    { sink!()	-> stdout!"4th message\n"
    | stdin?msg -> stdout!"unexpected\n" };
    { stdin?msg -> stdout!"unexpected\n"
    | sink!()	-> stdout!"5th message\n" };

    { source?x	-> stdout!"6th message\n"
    | source?x	-> stdout!"unexpected\n" };
    { source?x	-> stdout!"7th message\n"
    | stdin?msg -> stdout!"unexpected\n" };
    { stdin?msg -> stdout!"unexpected\n"
    | source?x	-> stdout!"8th message\n" }

proc StdoutProc() = stdout!"start StdoutProc()\n";
    stdout!"1st message\n";
    stdout!"2nd message\n";
    { stdout!"3rd message\n" -> skip
    | stdout!"unexpected\n" -> skip};
    { stdout!"unexpected\n" -> skip
    | null!() -> stdout!"4th message\n" };
    { null!() -> stdout!"5th message\n"
    | stdout!"unexpected\n" -> skip };
    { stdout!"6th message\n" -> skip
    | timer!(1,0) -> stdout!"unexpected\n" };
    { timer!(1,0) -> stdout!"unexpected\n"
    | stdout!"7th message\n" -> skip };
    { stdout!"8th message\n" -> skip
    | stdin?msg -> stdout!"unexpected\n" };
    { stdin?msg -> stdout!"unexpected\n"
    | stdout!"9th message\n" -> skip };
    { stdout!"unexpected\n" -> skip
    | sink!() -> stdout!"10th message\n" };
    { sink!() -> stdout!"11th message\n"
    | stdout!"unexpected\n" -> skip };
    OutMessage("12th message\n"); OutMessage("13th message\n")

proc OutMessage(msg:string) = stdout!msg

    
proc TimerProc() = stdout!"start TimerProc()\n";
    timer!(2,0);
    stdout!"1st message\n";
    { timer!(0,0) -> stdout!"2nd message\n"
    | stdin?x -> stdout!"unexpected\n" };
    { stdin?x -> stdout!"unexpected\n"
    | timer!(0,0) -> stdout!"3rd message\n" };
    { timer!(1,0) -> stdout!"4th message\n"
    | stdin?x -> stdout!"unexpected\n" };
    { stdin?x -> stdout!"unexpected\n"
    | timer!(1,0) -> stdout!"5th message\n" };
    { timer!(3,0) -> stdout!"unexpected\n"
    | timer!(2,0) -> stdout!"unexpected\n"
    | timer!(1,0) -> stdout!"6th message\n" };
    DelayOut((1,0), "7th message\n"); DelayOut((2,0), "8th message\n")

proc DelayOut(t:(int,int),msg:string) = timer!t; stdout!msg

type SockPair = { in:<string>; out:<string> }
proc ServerProc() = stdout!"start ServerProc()\n";
    var lsock:<SockPair>;
    C{ prc_SockTcpServer($lsock$, 10001); C};
    lsock?csock; stdout!"Server:1st message\n";
    csock.in?msg; stdout!"Server:2nd message\n";
    { msg @ "0123456789ABCDEF" -> stdout!"Server:3rd message\n"
          | _ -> stdout!"Server:unexpected\n" };
    csock.out!msg^msg; stdout!"Server:4th message\n";
    csock.in?x; stdout!"Server:5th message\n";
    { x @ "" -> stdout!"Server:6th message\n"
        | _ -> stdout!"Server:unexpected\n" };
    csock.out!""; stdout!"Server:7th message\n";
// 接続待ちのキャンセル 
    { lsock?csock -> stdout!"Server:unexpected\n"
    | null!()  -> stdout!"Server:8th message\n" };
// 複数クライアントの同時接続
    var ret1:<unit>; var ret2:<unit>;
    lsock?csock; Server(csock,"ID1", ret1);
    lsock?csock; Server(csock,"ID2", ret2);
    ret1?x; stdout!"Server:9th message\n";
    ret2?x; stdout!"Server:10th message\n";
// 読み込みと書き込みの同時処理
    var ret:<unit>;
    lsock?csock; ParaServer(csock, ret);
    ret?x; stdout!"Server:11th message\n"

proc Server(sp:SockPair,id:string,ret:<unit>) =
    sp.out!id;
    sp.in?msg; { msg @ id -> skip | _  -> stdout!"Server:unexpected\n" };
    sp.in?msg; { msg @ id -> skip | _  -> stdout!"Server:unexpected\n" };
    sp.out!id;
    sp.out!""; sp.in?x;
    ret!()
    
proc ClientProc() = stdout!"start ClientProc()\n";
    var sin:<string>; var sout:<string>; var sp={in=sin;out=sout};
    C{ prc_SockTcpClient($sp.in$,$sp.out$,"127.0.0.1",10001); C};
    sp.out!"0123456789ABCDEF"; stdout!"Client:1st message\n";
    sp.in?msg; stdout!"Client:2nd message\n";
    { msg @ "0123456789ABCDEF0123456789ABCDEF" -> stdout!"Client:3rd message\n"
	  | _ -> stdout!"Client:unexpected\n" };
    sp.out!""; stdout!"Client:4th message\n";
    sp.in?x; stdout!"Client:5th message\n";
    { x @ "" -> stdout!"Client:6th message\n"
        | _ -> stdout!"Client:unexpected\n" };
// 複数クライアントの同時接続
    var ret1:<unit>; var ret2:<unit>;
    Client("ID1",ret1); Client("ID2",ret2);
    ret1?x; stdout!"Client:7th message\n";
    ret2?x; stdout!"Client:8th message\n";
// 読み込みと書き込みの同時処理
    var ret:<unit>;
    ParaClient(ret);
    ret?x; stdout!"Client:9th message\n"

proc Client(id:string,ret:<unit>) =
    var sin:<string>; var sout:<string>; var sp={in=sin;out=sout};
    C{ prc_SockTcpClient($sp.in$,$sp.out$,"127.0.0.1",10001); C};
    sp.in?msg; { msg @ id -> skip | _ -> stdout!"Client:unexpected\n" };
    sp.out!id;
    timer!(1,0);
    sp.out!id;
    sp.in?msg; { msg @ id -> skip | _ -> stdout!"Client:unexpected\n" };
    sp.out!""; sp.in?x;
    ret!() 

proc ParaServer(sp:SockPair,ret:<unit>) =
    sp.out!"Hello";
    var rch:<unit>;
    SrvReceiver(sp.in,rch);
    SrvSender(sp.out,rch);
    rch?x;rch?x;
    ret!()

proc SrvReceiver(sin:<string>,rch:<unit>) =
    sin?msg; 
    { msg @ "from Client" -> skip | _ -> stdout!"SrvReceiver:unexpected\n" };
    rch!()
proc SrvSender(sout:<string>,rch:<unit>) =
    sout!"from Server"; rch!()

proc ParaClient(ret:<unit>) =
    var sin:<string>; var sout:<string>; var sp={in=sin;out=sout};
    C{ prc_SockTcpClient($sp.in$,$sp.out$,"127.0.0.1",10001); C};
    sp.in?msg; { msg @ "Hello" -> skip | _ -> stdout!"ParaCli:unexpected\n" };
    var rch:<unit>;
    CliReceiver(sp.in,rch);
    CliSender(sp.out,rch);
    rch?x;rch?x;
    ret!()
proc CliReceiver(sin:<string>,rch:<unit>) =
    sin?msg;
    { msg @ "from Server" -> skip | _ -> stdout!"CliReceiver:unexpected\n" };
    rch!()
proc CliSender(sout:<string>,rch:<unit>) =
    sout!"from Client"; rch!()

type ipaddr = { octet[4] }
type sockaddr = { addr	: ipaddr; port	: int }
type mesgaddr = { data	: string; saddr	: sockaddr }
type UdpSock = { in:<mesgaddr>; out:<mesgaddr> }

proc UdpProc() = stdout!"start UdpProc()\n";
    var sin:<string>; var sout:<string>; var sp1={in=sin;out=sout};
    C{ prc_SockUdpOpen($sin$, $sout$, "127.0.0.1", 10001, 10002); C};
    var sin:<string>; var sout:<string>; var sp2={in=sin;out=sout};
    C{ prc_SockUdpOpen($sin$, $sout$, "127.0.0.1", 10002, 10001); C};
    var ret1:<unit>; var ret2:<unit>;
    UdpPeer1(sp1,ret1); UdpPeer2(sp2,ret2);
    ret1?x; stdout!"1st message\n";
    ret2?x; stdout!"2nd message\n";
    var sin:<string>; var sout:<string>; var sp1={in=sin;out=sout};
    C{ prc_SockUdpClient($sin$, $sout$, "127.0.0.1", 10000); C};
    var sin:<mesgaddr>; var sout:<mesgaddr>; var sp2={in=sin;out=sout};
    C{ prc_SockUdpServer($sin$, $sout$, 10000); C};
    UdpClient(sp1,ret1); UdpServer(sp2,ret2);
    ret1?x; stdout!"3rd message\n";
    ret2?x; stdout!"4th message\n"
    
proc UdpPeer1(sp:SockPair,ret:<unit>) =
    sp.out!"HELLO";
    sp.in?msg; { msg @ "WORLD" -> skip | _ -> stdout!"unexpected\n" };
    sp.in?msg; { msg @ "HELLO" -> skip | _ -> stdout!"unexpected\n" };
    sp.out!"WORLD";
    sp.out!""; ret!()

proc UdpPeer2(sp:SockPair,ret:<unit>) =
    sp.in?msg; { msg @ "HELLO" -> skip | _ -> stdout!"unexpected\n" };
    sp.out!"WORLD";
    sp.out!"HELLO";
    sp.in?msg; { msg @ "WORLD" -> skip | _ -> stdout!"unexpected\n" };
    sp.out!""; ret!()

proc UdpClient(sp:SockPair, ret:<unit>) =
    sp.out!"HELLO";
    sp.in?msg; { msg @ "WORLD" -> skip | _ -> stdout!"unexpected\n" };
    sp.out!"WORLD";
    sp.in?msg; { msg @ "HELLO" -> skip | _ -> stdout!"unexpected\n" };
    sp.out!""; ret!()

proc UdpServer(sp:UdpSock, ret:<unit>) =
    sp.in?msg; { msg.data @ "HELLO" -> skip | _ -> stdout!"unexpected\n" };
    sp.out!{data="WORLD";saddr=msg.saddr};
    sp.in?msg; { msg.data @ "WORLD" -> skip | _ -> stdout!"unexpected\n" };
    sp.out!{data="HELLO";saddr=msg.saddr};
    ret!()
