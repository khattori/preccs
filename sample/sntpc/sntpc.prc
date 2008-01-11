//
// SNTPクライアント
//
C{
extern void print_ntp_time(char *msg);
C}

//
// タイムスタンプ形式の定義
//
type TimeStamp = {{
    second  : octet[4];
    decimal : octet[4]
}}
//
// SNTPパケット形式の定義
//
type SntpPacket = {{
    mode	: octet;
    stratum	: octet;
    poll	: octet;
    precs	: octet;
    rtdel	: octet[4];
    rtdisp	: octet[4];
    refid	: octet[4];
    refts	: TimeStamp;
    orgts	: TimeStamp;
    rcvts	: TimeStamp;
    trsts	: TimeStamp;
    opt	: {
	keyid	: octet[32];
	msgdgs	: octet[128]
    }?
}}
type TEXT =  { /"^\r\n"* }

type SntpSndPkt = SntpPacket{mode={"0B"h}}
type SntpRcvPkt = SntpPacket{mode={"CC"h|"0C"h}}

type Socket = {in:<string>;out:<string>}

proc Main() =
    stdout!"Host> ";
    stdin?host;
    { host @ h:{ name: TEXT; x: ("\r"|"\n")* } ->
              var sp = SockUdpCreate(h.name,123);
              SntpProcInit(sp)
           | _ -> skip }

proc SntpProcInit(sp:Socket) =
    stdout!"Enter> ";
    stdin?cmnd;
    { cmnd @ msg : ("q"|"Q");octet* -> stop
           | _ -> var pkt:SntpSndPkt;
                  sp.out!pkt;
                  SntpProcWait(sp) }

proc SntpProcWait(sp:Socket) =
    timer!(10,0) -> stdout!"Timeout\n"
  | stdin?msg    -> stdout!"Bye\n"
  | sp.in?msg    -> { msg @ pkt:SntpRcvPkt ->
                              C{ print_ntp_time(STRPTR($pkt.trsts.second$)); C}
                          | _ -> stdout!"Error packet recvd\n" };
                    SntpProcInit(sp)
  
proc SockUdpCreate(host:string,port:int):Socket =
    var sin:<string>;
    var sout:<string>;
    C{
	char *hname = (char *)__string__(STRLEN($host$),STRPTR($host$));
	prc_SockUdpClient($sin$, $sout$, STRPTR((int)hname), TOCINT($port$));
    C};
    return {in=sin;out=sout}
