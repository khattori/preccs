C{
#include <stdio.h>
#include "global.h"
extern unsigned char linear2ulaw(short pcm_val);
extern short ulaw2linear(unsigned char u_val);
C}

// メニュコマンド
type CMD_INVITE = {"I"|"i"}
type CMD_CANCEL = {"C"|"c"}
type CMD_TAKE   = {"T"|"t"}
type CMD_BYE    = {"B"|"b"}
type CMD_QUIT   = {"Q"|"q"}

type DG    = { /"0-9" }
type ALPHA = { /"a-zA-Z" }
type CRLF  = { "\r\n" } 
type SP    = { " " }
type HT    = { "\t" }
type LWS   = { "\r\n"?;(SP|HT)+ }
type TEXT  = { /"^\r\n"* }
type Unreserved = { ALPHA|DG|"-"|"."|"_"|"~" }
type Host = { Unreserved* }
type URI = { "sip:";Unreserved*;"@";Unreserved* }

type SipVersion = { "SIP/2.0" }
// SIPメソッド定義
type SipMethod = {
	  "INVITE"
	| "ACK"
	| "BYE"
	| "CANCEL"
	| "REGISTER"
	| "OPTIONS"
	| "INFO"
}
type SipRequestLine = {{
	method	: SipMethod;
	sp1	: SP;
	path	: URI;
	sp2	: (SP; SipVersion; CRLF) }}

type SipRequest = {{
	start	: SipRequestLine;
	head	: TEXT;
	crlf	: CRLF;
	body	: octet*
}}
type SipReqInvite = SipRequest{start.method={"INVITE"}}
type SipReqCancel = SipRequest{start.method={"CANCEL"}}
type SipReqBye    = SipRequest{start.method={"BYE"}}
type SipReqAck    = SipRequest{start.method={"ACK"}}

// SIP応答定義
/*
"SIP/2.0 180 Ringing\r\n\r\n"
"SIP/2.0 100 Tring\r\n\r\n"
"SIP/2.0 200 OK\r\n\r\n"
"SIP/2.0 404 Not Found\r\n\r\n"
*/
type SipStatusLine = {{
	ver	: SipVersion;
	sp1	: SP;
	code	: (DG;DG;DG);
	sp2	: SP;
	phrase	: TEXT;
	end	: CRLF
}}
type SipResponse = {{
	status	: SipStatusLine;
	head	: TEXT;
	crlf	: CRLF;
	body	: octet*
}}

type SipRespRinging = SipResponse{status.code={"180"}}
type SipRespOK      = SipResponse{status.code={"200"}}

//
// RTPパケット定義
//
type RtpHeader = {{
	vpxcc	: octet;	// V(2),P(1),X(1),CC(4)
	mpt	: octet;	// M(1),PT(7)
	seq	: octet[2];
	ts	: octet[4];
	ssrc	: octet[4]
}}
type RtpPacket = {{
	header	: RtpHeader;
	data	: octet+
}}

type SocketPair = {in:<string>;out:<string>}
proc Main() =
    var lsock:<SocketPair>;
    var ret:int;
    C{	$ret$ = TOPINT(prc_SockTcpServer($lsock$, g_sport)); C};
    ( ret @ -1 -> stop
          | _  -> VoipReady(lsock) )

// 準備完了
proc VoipReady(ls:<SocketPair>) =
    stdout!"Menu:\n- (I)nvite\n- (Q)uit\n";
    ( stdin?msg   -> ( msg @ c:CMD_INVITE;octet* -> VoipInvite(ls)
                           | c:CMD_QUIT;octet*   -> stop
                           | _ -> stdout!"unknown command\n"; VoipReady(ls) )
    | ls?csock -> VoipRinging(ls,csock) )

// 呼び出しを行う
proc VoipInvite(ls:<SocketPair>) =
    var ret:<{ok:bool;sp:SocketPair}>;
    var host:string;
    var cport:int;
    C{
        $host$  = __string__(strlen(g_host), g_host);
        $cport$ = TOPINT(g_cport);
    C};
    PrcTcpClientSock(ret, host, cport);
    ret?cs;
    ( cs.ok @ true  -> cs.sp.out!"INVITE sip:user@com SIP/2.0\r\n\r\n";
                       VoipWaiting(ls, cs.sp)
            | false -> stdout!"failure to invite\n"; VoipReady(ls) )

// 応答待ち
proc VoipWaiting(ls:<SocketPair>,sp:SocketPair) =
    stdout!"Waiting...\n";
    stdout!"Menu:\n- (C)ancel\n";
    ( stdin?cmd ->
      ( cmd @ c:CMD_CANCEL;octet* -> sp.out!"CANCEL sip:user@com SIP/2.0\r\n\r\n";
                                     VoipReady(ls)
            | _ -> stdout!"unknown command\n"; VoipWaiting(ls,sp) )
    | sp.in?msg ->
      ( msg @ r:SipRespRinging -> stdout!"Ringing...\n"; VoipWaiting(ls,sp)
            | r:SipRespOK      -> sp.out!"ACK sip:user@com SIP/2.0\r\n\r\n";
                                  VoipStartSession(ls,sp)
            | _ -> stdout!"illegal response\n";
                   stdout!msg; sp.out!""; VoipReady(ls) )
    )

// 呼び出し中
proc VoipRinging(ls:<SocketPair>,sp:SocketPair) =
    sp.in?msg;
    ( msg @ r:SipReqInvite -> sp.out!"SIP/2.0 180 Ringing\r\n\r\n"
          | _ -> stdout!"illegal request\n"; sp.out!""; VoipReady(ls) );
    stdout!"Menu:\n- (T)ake\n";
    ( stdin?cmd ->
      ( cmd @ c:CMD_TAKE;octet* -> sp.out!"SIP/2.0 200 OK\r\n\r\n"; VoipWaitAck(ls, sp)
            | _ -> stdout!"unknown command\n"; VoipRinging(ls,sp) )
    | sp.in?msg ->
      ( msg @ r:SipReqCancel -> sp.out!""; stdout!"Canceled\n"; VoipReady(ls)
            | _ -> stdout!"illegal request\n"; sp.out!""; VoipReady(ls) )
    )

// ACK待ち
proc VoipWaitAck(ls:<SocketPair>,sp:SocketPair) =
    sp.in?msg;
    ( msg @ r:SipReqAck -> VoipStartSession(ls,sp)
          | _ -> stdout!"illegal request\n"; sp.out!""; VoipReady(ls) )    

proc VoipStartSession(ls:<SocketPair>, sp:SocketPair) =
    var h:int;
    var sin:<string>;
    var sout:<string>;
    C{  $h$ =
	 TOPINT(prc_SockUdpOpen($sin$, $sout$, g_host, g_rpport, g_rmport));
    C};
    ( h @ -1 -> stop
        | _  -> var min:<string>; var mout:<string>;
                VoipSession(ls,sp,h,min,mout);
                var wout:<string>; C{ prc_WaveOutOpen($wout$, 8000); C};
                var win:<string>; C{ prc_WaveInOpen($win$, 8000); C};
                WaveIn(min,win,sout);WaveOut(mout,sin,wout) )

proc VoipSession(ls:<SocketPair>, sp:SocketPair, h:int, min:<string>, mout:<string>) =
    stdout!"Session Started\n";
    stdout!"Menu:\n- (B)ye\n";
    ( stdin?cmd ->
      ( cmd @ c:CMD_BYE;octet* -> sp.out!"BYE sip:user@com SIP/2.0\r\n\r\n";
                                  VoipEndSession(ls,min,mout,h)
            | _ -> stdout!"illegal command\n"; VoipSession(ls,sp,h,min,mout) )
    | sp.in?msg ->
      ( msg @ r:SipReqBye -> sp.out!"SIP/2.0 200 OK\r\n\r\n"; VoipEndSession(ls,min,mout,h)
            | r:SipReqCancel -> sp.out!""; VoipEndSession(ls,min,mout,h)
            | _ -> stdout!"Illegal request\n"; sp.out!""; VoipEndSession(ls,min,mout,h) )
    )

proc WaveIn(m:<string>, win:<string>, sout:<string>) =
      m?cmd -> win!""; m!"ok"
    | win?data -> var ulaw:string;
                  var hd:RtpHeader;
                  C{
                  static char buf[BUFSIZ];
                  static short seq;
                  DWORD *tm;
                  int i;
                  short *p = (short *)STRPTR($data$);
                  int len = STRLEN($data$) / 2;

                  for (i = 0; i < len; i++) {
                      buf[i] = linear2ulaw(*p++);
                  }
                  $ulaw$ = __string__(len, buf);
		    *STRPTR($hd.vpxcc$) = 0x80;    // バージョン番号設定
                  p = (short *)STRPTR($hd.seq$); // シーケンス設定
                  *p = htons(seq++);
                  tm = (DWORD *)STRPTR($hd.ts$); // タイムスタンプ設定
                  *tm = htonl(GetTickCount());
                  C};
                  sout!hd^ulaw; WaveIn(m,win,sout)

proc WaveOut(m:<string>, sin:<string>, wout:<string>) =
      m?cmd -> wout?x; m!"ok"
    | sin?data ->
        ( data @ pkt:RtpPacket ->
                   var linear:string;
                   C{
                   static short buf[BUFSIZ];
                   int i;
                   char *p = STRPTR($pkt.data$);
                   int len = STRLEN($pkt.data$);
                   for (i = 0; i < len; i++) {
                       buf[i] = ulaw2linear(*p++);
                   }
                   $linear$ = __string__(len*2, (char *)buf);
                   C};
                   wout!linear; WaveOut(m,sin,wout)
               | _ -> stdout!"recv: Illegal RTP packet\n"; WaveOut(m,sin,wout) )

proc VoipEndSession(ls:<SocketPair>, min:<string>, mout:<string>, h:int) =
    min!"stop";mout!"stop";min?x;mout?x;
    C{ prc_SockClose(TOCINT($h$)); C};
    VoipReady(ls)

proc PrcTcpClientSock(ret:<{ok:bool;sp:SocketPair}>, host:string, port:int) =
    var h:int;
    var so1:<string>;
    var so2:<string>;
    C{ $h$ =
       TOPINT(prc_SockTcpClient($so1$, $so2$, STRPTR($host$), TOCINT($port$)));
    C};
    ( h @ -1 -> ret!{ok=false;sp={in=so1;out=so2}}
        | _  -> ret!{ok=true;sp={in=so1;out=so2}} )
