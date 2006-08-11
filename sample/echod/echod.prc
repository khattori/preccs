type Sockets = {h:int;in:<string>;out:<string>}

//
// Echoサーバー
//
proc Main() =
    var lsock:<Sockets>;
    C{	prc_SockTcpServer($lsock$, 10007); C};
    EchoSrv(lsock)

proc EchoSrv(lsock:<Sockets>) =
      stdin?msg   -> stop
    | lsock?csock -> ( stdout!"conneted\n";		// 接続待ち
			  EchoProc(csock);
		         EchoSrv(lsock) )

proc EchoProc(s:Sockets) =
    s.in?msg;
    ( msg @ "" -> stdout!"closed\n"; stop
          | _  -> s.out!msg; EchoProc(s) )
