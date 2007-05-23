C{
#include <stdio.h>
#include <string.h>
C}
type Sockets = {in:<string>;out:<string>}

//
// Echoクライアント
//   --- 10007番ポートに接続し，標準入力からのデータを送信する．
//   --- 受信データを標準出力に表示する．
//
proc Main() =
    var so1:<string>;
    var so2:<string>;
    var ch:<string>;
    var sock = {in=so1;out=so2};
    stdout!"host: "; stdin?host;
    Chop(host,ch); ch?host;  // ホスト名の改行文字を削除
    C{
        static char buf[BUFSIZ];
        memcpy(buf, STRPTR($host$), STRLEN($host$));
        buf[STRLEN($host$)] = 0;
	prc_SockTcpClient($sock.in$, $sock.out$, buf, 10007);
    C};
    EchoProc(sock)

proc EchoProc(so:Sockets) =
    ( so.in?msg -> stdout!"EchoProc: recvd: "^msg
    | stdin?msg -> ( msg @ x:"q";octet* -> stdout!"EchoProc: stop\n"; so.out!""
                         | _ -> so.out!msg ) );
    EchoProc(so)

proc Chop(host:string, ret:<string>) =
    host @ x:{name:/"^\r\n"*; crlf:("\r"|"\n")*} -> ret!x.name
         | _ -> ret!host
