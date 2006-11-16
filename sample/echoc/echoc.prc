type Sockets = {in:<string>;out:<string>}

//
// Echoクライアント
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
