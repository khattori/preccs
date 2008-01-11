C{
extern int create_sock(void);
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

type DhcpBody = {{
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
    file	: octet[128]	// ブートファイル名
}}
// BOOTPオプション
type BootpOption = {{
    tag		: octet;
    len		: octet;
    data	: octet[len]
}}
// 基本DHCPパケット
type DhcpPacket = {{
    body	: DhcpBody;
    magic	: MAGIC_CODE;
    opts	: BootpOption+;
    end		: END;
    pad		: PAD*
}}

type BoptSubnet    = BootpOption{tag={"01"h},len={"04"h}}
type BoptRouter    = BootpOption{tag={"03"h},len={"04"h}}
type BoptDnsServer = BootpOption{tag={"06"h},len={"04"h}}
type BoptDomain    = BootpOption{tag={"0F"h}}
type BoptMsgType   = BootpOption{tag={"35"h},len={"01"h}}
type CliAddr       = {{t:"01"h;addr:octet[6]}}
type BoptClientID  = BootpOption{tag={"3D"h},len={"07"h},data=CliAddr}

// 基本オプションを定義
type DiscoverOption = BoptMsgType{data={"01"h}}
type OfferOption    = BoptMsgType{data={"02"h}}
type RequestOption  = BoptMsgType{data={"03"h}}
type AckOption      = BoptMsgType{data={"05"h}}
type NakOption      = BoptMsgType{data={"06"h}}

type DhcpDiscoverBody = DhcpBody{op=BOOT_REQUEST,htype={"01"h},hlen={"06"h},flags=BOOT_BROADCAST}

type Socket = {in:<string>;out:<string>}
////////////////////////////////////////////////////////////////
//
// DHCPクライアントプロセス定義
//
// メインプロセス
proc Main() = 
    var sp = SockUdpCreate();  
    DhcpInit(sp)

// 初期状態
proc DhcpInit(sp:Socket) =
    sp.out!CreateDhcpDiscover();
    stop

proc CreateDhcpDiscover():string =
    var body:DhcpDiscoverBody;
    var magic:MAGIC_CODE;
    var opt1:DiscoverOption;
    var opt2:BoptClientID;
    var end:END;
    var pad:{PAD[47]};
    C{
        extern char g_ifaddr[6];
	memcpy(STRPTR($body.xid$),"\x00\x00\x00\x00\x00\x01", 6);
        memcpy(STRPTR($body.chaddr$), g_ifaddr, sizeof g_ifaddr);
        memcpy(STRPTR($opt2.data.addr$), g_ifaddr, sizeof g_ifaddr);
    C};
    return body^magic^opt1^opt2^end^pad

// 
proc SockUdpCreate():Socket =
    var sin:<string>;
    var sout:<string>;
    C{
	prc_SockSetConnChan($sin$, $sout$, create_sock());
    C};
    return {in=sin;out=sout}
