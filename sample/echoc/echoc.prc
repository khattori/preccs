type Sockets = {in:<string>;out:<string>}

//
// Echoクライアント
//   --- 10007番ポートに接続し，標準入力からのデータを送信する．
//   --- 受信データを標準出力に表示する．
//
proc Main() =
    var so1:<string>;
    var so2:<string>;
    var sock = {in=so1;out=so2};
    stdout!"host: "; stdin?host;
    C{
	prc_SockTcpClient($sock.in$, $sock.out$, STRPTR($host$), 10007);
    C};
    EchoProc(sock)

proc EchoProc(so:Sockets) =
    ( so.in?msg   -> stdout!"Message Received: "^msg
    | stdin?msg -> ( msg @ x:"q";octet* -> stop
                         | _ -> so.out!msg ) );
    EchoProc(so)
