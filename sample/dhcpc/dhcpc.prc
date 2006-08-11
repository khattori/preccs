type BOOT_REQUEST   = {"01"h}
type BOOT_REPLY     = {"02"h}
type BOOT_BROADCAST = {"8000"h}
type MAGIC_CODE     = {"63825363"h}
type PAD = {"00"h}
type END = {"FF"h}
// BOOTPオプション
type BootpOption = {{
    tag	: octet;
    len	: octet;
    data	: octet[len]
}}

// DHCPオプション
type DhcpOption = {{
    magic	: MAGIC_CODE;
    opts	: BootpOption+;
    end	: END;
    pad	: PAD*
}}

// 基本DHCPパケット
type DhcpPacket = {{
    op	: ( BOOT_REQUEST | BOOT_REPLY );
    htype	: octet;	// ハードウェア番号
    hlen	: octet;	// ハードウェアアドレス長
    hops	: octet;	// ホップ数
    xid	: octet[4];	// トランザクションID
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
/*
type BoptSubnet    = BootpOption{tag={"01"h},len={"04"h}}
type BoptRouter    = BootpOption{tag={"03"h},len={"04"h}}
type BoptDnsServer = BootpOption{tag={"06"h},len={"04"h}}
type BoptDomain    = BootpOption{tag={"0F"h}}
type BoptMsgType   = BootpOption{tag={"35"h},len={"01"h}}
type CliAddr       = {{t:"01"h;addr:octet[6]}}
type BoptClientID  = BootpOption{tag={"3D"h},len={"07"h},data=CliAddr}
*/
// メインプロセス
proc Main() =
    var msg =
	"01010600"h^
	"19bfa3dc"h^ // xid
	"00000000"h^ // secs , flags
 	"00000000"h^ // ciaddr
	"00000000"h^ // yiaddr
	"00000000"h^ // siaddr
	"00000000"h^ // giaddr
	"000b9729a64000000000000000000000"h^
	"0000000000000000000000000000000000000000000000000000000000000000"h^
	"0000000000000000000000000000000000000000000000000000000000000000"h^
	"0000000000000000000000000000000000000000000000000000000000000000"h^
	"0000000000000000000000000000000000000000000000000000000000000000"h^
	"0000000000000000000000000000000000000000000000000000000000000000"h^
	"0000000000000000000000000000000000000000000000000000000000000000"h^
	"63825363"h^ // magic
	"350101"h^
	"740101"h^
	"3d0701000b9729a640"h^
	"0c06627261686d73"h^
	"3c084d53465429252e30"h^
	"370b010f03062c2e2f1f21f92b"h^
	"ff00000000000000000000000000"h;
    Match(msg,100000)

proc Match(msg:string,n:int) =
    n @ 0 -> stop
      | _ -> ( msg @ x:DhcpPacket -> Match(msg,n-1)
                   | _            -> stdout!"unknown\n" )


