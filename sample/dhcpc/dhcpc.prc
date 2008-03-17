C{
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>
extern int create_sock(void);
extern int set_ifconf(void);

extern char g_subnet[];
extern char g_gwaddr[];
extern char g_dnssrv[];
C}

////////////////////////////////////////////////////////////////
//
// DHCPメッセージ定義
//
type BOOT_REQUEST   = {"01"h}
type BOOT_REPLY     = {"02"h}
type BOOT_BROADCAST = {"8000"h}
type MAGIC_CODE     = {"63825363"h}
type PAD = {"00"h}
type END = {"FF"h}

// BOOTPオプション
type BootpOption = {{
    tag		: octet;
    len		: octet;
    data	: octet[len]
}}
type DhcpOption = {{
    magic	: MAGIC_CODE;
    opts	: BootpOption+;
    end		: END;
    pad		: PAD*
}}
// 基本DHCPメッセージ
type DhcpMessage = {{
    op		: ( BOOT_REQUEST | BOOT_REPLY );
    htype	: octet;	// ハードウェア番号
    hlen	: octet;	// ハードウェアアドレス長
    hops	: octet;	// ホップ数
    xid		: octet[4];	// トランザクションID
    secs	: octet[2];	// 経過時間
    flags	: octet[2];	// フラグ
    ciaddr	: octet[4];	// クライアントIP
    yiaddr	: octet[4];	// 割当てIPアドレス
    siaddr	: octet[4];	// サーバIPアドレス
    giaddr	: octet[4];	// リレーエージェント
    chaddr	: octet[16];	// ハードウェアアドレス
    sname	: octet[64];	// サーバ名
    file	: octet[128];	// ブートファイル名
    option	: DhcpOption
}}

type BoptSubnet    = BootpOption{tag={"01"h},len={"04"h}}
type BoptRouter    = BootpOption{tag={"03"h},len={"04"h}}
type BoptDnsServer = BootpOption{tag={"06"h},len={"04"h}}
type BoptDomain    = BootpOption{tag={"0F"h}}
type BoptAddrReq   = BootpOption{tag={"32"h},len={"04"h},data={octet[4]}}
type BoptAddrTime  = BootpOption{tag={"33"h},len={"04"h}}
type BoptMsgType   = BootpOption{tag={"35"h},len={"01"h}}
type BoptServerID  = BootpOption{tag={"36"h},len={"04"h},data={octet[4]}}
type CliAddr       = {{t:"01"h;addr:octet[6]}}
type BoptClientID  = BootpOption{tag={"3D"h},len={"07"h},data=CliAddr}

// 基本オプションを定義
type BoptMsgDiscover = BoptMsgType{data={"01"h}}
type BoptMsgOffer    = BoptMsgType{data={"02"h}}
type BoptMsgRequest  = BoptMsgType{data={"03"h}}
type BoptMsgAck      = BoptMsgType{data={"05"h}}
type BoptMsgNak      = BoptMsgType{data={"06"h}}

type DhcpDiscoverOpt = DhcpOption{opts={{bmt:BoptMsgDiscover;
					 bcid:BoptClientID}},
				  pad={PAD[47]}}
type DhcpOfferOpt    = DhcpOption{opts={{bmt:BoptMsgOffer;
				 	 x1:BootpOption*;
					 bsid:BoptServerID;
					 x2:BootpOption*}}}
type DhcpRequestOpt  = DhcpOption{opts={{bmt:BoptMsgRequest;
					 bcid:BoptClientID;
					 brip:BoptAddrReq;
					 bsid:BoptServerID}},
				  pad={PAD[35]}}
type DhcpAckOpt	     = DhcpOption{opts={BoptMsgAck;BootpOption*}}
type DhcpNakOpt	     = DhcpOption{opts={BoptMsgNak;BootpOption*}}

type DhcpDiscoverMsg = DhcpMessage{op=BOOT_REQUEST,
				   htype={"01"h},
				   hlen={"06"h},
				   flags=BOOT_BROADCAST,
				   option=DhcpDiscoverOpt}
type DhcpOfferMsg    = DhcpMessage{op=BOOT_REPLY,
				   option=DhcpOfferOpt}
type DhcpRequestMsg  = DhcpMessage{op=BOOT_REQUEST,
				   htype={"01"h},
				   hlen={"06"h},
				   flags=BOOT_BROADCAST,
				   option=DhcpRequestOpt}
type DhcpAckMsg	     = DhcpMessage{op=BOOT_REPLY,
				   option=DhcpAckOpt}
type DhcpNakMsg	     = DhcpMessage{op=BOOT_REPLY,
				   option=DhcpNakOpt}

type Address = { iaddr:string; port:int }
type MsgAddr = { msg:string; addr:Address }

type UdpSocket = {in:<MsgAddr>;out:<MsgAddr>}

var DHCP_SERVER_PORT = 67
var DHCP_BROADCAST_ADDR = { iaddr = "FFFFFFFF"h; port = DHCP_SERVER_PORT }

////////////////////////////////////////////////////////////////
//
// DHCPクライアントプロセス定義
//
// メインプロセス
proc Main() = 
    var so = SockUdpCreate();  
    DhcpInit(so)
// 初期状態
proc DhcpInit(so:UdpSocket) =
    so.out!{ msg  = CreateDhcpDiscover();
             addr = DHCP_BROADCAST_ADDR };
    DhcpSelecting(so)
// 選択中
proc DhcpSelecting(so:UdpSocket) =
    so.in?msgaddr -> {
        msgaddr.msg @ m:DhcpOfferMsg -> SetOptions(m.option);
					var smsg = CreateDhcpRequest(m.yiaddr,m.option.opts.bsid.data);
                                        so.out!{ msg  = smsg;
						 addr = DHCP_BROADCAST_ADDR };
                                        DhcpRequesting(so,smsg)
                    | _ -> stdout!"[Selecting] Illegal message recvd.\n";
                           DhcpInit(so) }
  | timer!(10,0) -> stdout!"[Selecting] timeout.\n";
                    DhcpInit(so)
// 応答待ち
proc DhcpRequesting(so:UdpSocket,smsg:string) = 
    so.in?msgaddr -> {
	msgaddr.msg @ m:DhcpAckMsg   -> DhcpBound(so,smsg)
		    | m:DhcpNakMsg   -> stdout!"[Requesting] Nak recvd.\n";
			 	        DhcpInit(so)
		    | m:DhcpOfferMsg -> DhcpRequesting(so,smsg)
		    | _ -> stdout!"[Requesting] Illegal message recvd.\n";
			   DhcpInit(so) }
  | timer!(10,0) -> stdout!"[Requesting] timeout.\n";
                    DhcpInit(so)
// リース
proc DhcpBound(so:UdpSocket,smsg:string) =
    C{ set_ifconf(); C};
    { so.in?msgaddr -> {
	msgaddr.msg @ x:(DhcpOfferMsg|DhcpNakMsg|DhcpAckMsg) -> DhcpBound(so,smsg)
		    | _ -> stdout!"[Bound] Illegal message recvd.\n";
			   DhcpInit(so) }
    | timer!(3600,0) -> so.out!{ msg  = smsg;
				 addr = DHCP_BROADCAST_ADDR };
			DhcpRenewing(so, smsg) }
// リース延長
proc DhcpRenewing(so:UdpSocket,smsg:string) =
    so.in?msgaddr -> {
	msgaddr.msg @ m:DhcpNakMsg -> DhcpInit(so)
		    | m:DhcpAckMsg -> DhcpBound(so,smsg)
		    | _ -> stdout!"[Renewing] Illegal message recvd.\n";
			   DhcpInit(so) }
  | timer!(10,0) -> so.out!{ msg  = smsg;
		 	     addr = DHCP_BROADCAST_ADDR };
		    DhcpRebinding(so,smsg)
// 再割り当て
proc DhcpRebinding(so:UdpSocket,smsg:string) =
    so.in?msgaddr -> {
	msgaddr.msg @ m:DhcpNakMsg -> DhcpInit(so)
		    | m:DhcpAckMsg -> DhcpBound(so,smsg)
		    | _ -> stdout!"[Rebinding] Illegal message recvd.\n";
			   DhcpInit(so) }
  | timer!(10,0) -> stdout!"[Rebinding] timeout.\n"; DhcpInit(so)

proc SetOptions(o:DhcpOfferOpt) = SetOpt(o.opts.x1); SetOpt(o.opts.x2)
proc SetOpt(opts:{BootpOption*}) =
    opts @ x:{hd:BoptSubnet;tl:BootpOption*} ->
		C{ memcpy(g_subnet, STRPTR($x.hd.data$), 4); C};
		SetOpt(x.tl)
         | x:{hd:BoptRouter;tl:BootpOption*} ->
		C{ memcpy(g_gwaddr, STRPTR($x.hd.data$), 4); C};
		SetOpt(x.tl)
         | x:{hd:BoptDnsServer;tl:BootpOption*} ->
		C{ memcpy(g_dnssrv, STRPTR($x.hd.data$), 4); C};
		SetOpt(x.tl)
         | x:{hd:BootpOption;tl:BootpOption*} ->
		SetOpt(x.tl)
         | _ -> skip

// DHCP REQUESTメッセージ生成
proc CreateDhcpDiscover():string =
    var msg:DhcpDiscoverMsg;
    C{
        extern char g_ifaddr[6];
	memcpy(STRPTR($msg.xid$),"\x00\x00\x00\x00\x00\x01", 6);
        memcpy(STRPTR($msg.chaddr$), g_ifaddr, sizeof g_ifaddr);
        memcpy(STRPTR($msg.option.opts.bcid.data.addr$), g_ifaddr, sizeof g_ifaddr);
    C};
    return msg
// DHCP REQUESTメッセージ生成
proc CreateDhcpRequest(yiaddr:{octet[4]},sid:{octet[4]}):string =
    var msg:DhcpRequestMsg;
    C{
	extern char g_ifaddr[6];
	extern char g_ipaddr[4];

	memcpy(g_ipaddr, STRPTR($yiaddr$), sizeof g_ipaddr);
	printf("Get IP: %s\n", inet_ntoa(*(struct in_addr *)g_ipaddr));
 	memcpy(STRPTR($msg.yiaddr$), g_ipaddr, sizeof g_ipaddr);

	memcpy(STRPTR($msg.xid$),"\x00\x00\x00\x00\x00\x01", 6);
        memcpy(STRPTR($msg.chaddr$), g_ifaddr, sizeof g_ifaddr);
        memcpy(STRPTR($msg.option.opts.bcid.data.addr$), g_ifaddr, sizeof g_ifaddr);
        memcpy(STRPTR($msg.option.opts.brip.data$), g_ipaddr, sizeof g_ipaddr);
	memcpy(STRPTR($msg.option.opts.bsid.data$), STRPTR($sid$), STRLEN($sid$));
    C};
    return msg

// UDPソケット作成
proc SockUdpCreate():UdpSocket =
    var so:UdpSocket;
    C{ prc_SockSetUnconnChan($so.in$, $so.out$, create_sock()); C};
    return so
