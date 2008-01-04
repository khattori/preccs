type Sockets = {in:<string>;out:<string>}

//
// Echoサーバ
//   --- 10007番ポートを使用したechoサーバ
//   --- 標準入力から入力があると終了
//       ※ただし，クライアントからの接続がある場合には，
//         接続が切れるまで終了しない
//
proc Main() =
    var lsock:<Sockets>;
    C{	prc_SockTcpServer($lsock$, 10007); C};
    EchoSrv(lsock)

proc EchoSrv(lsock:<Sockets>) =
      stdin?msg   -> stdout!"Echo Server: stop\n"
    | lsock?csock -> { stdout!"Echo Server: conneted\n"; // 接続待ち
		       EchoProc(csock, "ID=1: ");
		       EchoProc(csock, "ID=2: ");
		       EchoSrv(lsock) }

proc EchoProc(s:Sockets, id:string) =
    s.in?msg  -> stdout!"EchoProc:"^id^"recvd: "^msg^"\n";
                 { msg @ "" -> stdout!"EchoProc: "^id^"closed\n"; s.out!"BYE\n"; s.out!""
	               | _  -> s.out!msg; EchoProc(s, id) }
  | stdin?msg -> stdout!"EchoProc: "^id^"stop\n"

