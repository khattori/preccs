//
// SNTP�N���C�A���g
//
C{
extern void print_ntp_time(char *msg);
C}

//
// �^�C���X�^���v�`���̒�`
//
type TimeStamp = {{
    second  : octet[4];
    decimal : octet[4]
}}
//
// SNTP�p�P�b�g�`���̒�`
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

type SntpSndPkt = SntpPacket{mode={"0B"h}}
type SntpRcvPkt = SntpPacket{mode={"CC"h|"0C"h}}

type SocketPair = {in:<string>;out:<string>}

proc Main() =
    var ret:<SocketPair>;
    stdout!"Host> ";
    stdin?host;
    SockUdpCreate(ret,host,123);
    ret?sp;
    SntpProcInit(sp)

proc SntpProcInit(sp:SocketPair) =
    stdout!"Enter> ";
    stdin?cmnd;
    ( cmnd @ msg : ("q"|"Q");octet* -> stop
           | _ -> var pkt:SntpSndPkt;
                  sp.out!pkt;
                  SntpProcWait(sp) )

proc SntpProcWait(sp:SocketPair) =
    timer!10  -> stdout!"Timeout\n"
  | stdin?msg -> stdout!"Bye\n"
  | sp.in?msg -> ( msg @ pkt:SntpRcvPkt ->
                              C{ print_ntp_time(STRPTR($pkt.trsts.second$)); C}
                       | _ -> stdout!"Error packet recvd\n" );
                 SntpProcInit(sp)
  
proc SockUdpCreate(ret:<SocketPair>,host:string,port:int) =
    var sin:<string>;
    var sout:<string>;
    C{
	 prc_SockUdpClient($sin$, $sout$, STRPTR($host$), TOCINT($port$));
    C};
    ret!{in=sin;out=sout}
