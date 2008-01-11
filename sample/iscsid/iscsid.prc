C{
#define _FILE_OFFSET_BITS 64

#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>
extern void print_hex( char *title, char *msg, int size );
extern int cast_to_int( char *msg, int size );
extern int init_fileIO();
extern int read_data( off_t addr, size_t len, char* buf );
extern int write_data( off_t addr, size_t len, char* buf );
int g_ssn = 0;
C}

////////////////////////////////////////////////////////////////////
//                          型宣言                                //
////////////////////////////////////////////////////////////////////
type Sockets = {in:<string>;out:<string>}

//Page Code for SCSI MODE PAGE
type PageCodeSupportVitalProductData = { "00"h }
type PageCodeUnitSerrialNumnber      = { "80"h }
type PageCodeDeviceIdentification    = { "83"h }

type PageCodeInformationalExceptionsControl = { "1c"h }
type PageCodeAllPages                       = { "3f"h }

//-----------------------------------------------//
//                CDBs Definitions               //
//-----------------------------------------------//
type ReportLUNs = {{
    res1: octet;        //Reserved
    selr: octet;        //Select Report
    res2: octet[3];     //Reserved
    alen: octet[4];     //Allocation Length
    res3: octet;        //Reserved
    ctl : octet;        //Control
    pad : octet[4]      //16byte長にする為のパディング
}}
type Inquiry = {{
    evpd: octet;        //Bit field: 7-2:Reserved 1:Obsolate 0:Enable Vital Product Data(EVPD)
    pcd : octet;        //Page Code
    alen: octet[2];     //Allocation Length
    clt : octet;        //Control
    pad : octet[10]     //16byte長にする為のパディング
}}
type ReadCapacity = {{
    res : octet;        //Reserved
    lba : octet[4];     //Logical Block Address
    res2: octet[2];     //Reserved
    pmi : octet;        //Partial Medium Indicator
    ctl : octet;        //Control
    pad : octet[6]      //16byte長にする為のパディング
}}
type Read10 = {{
    bit : octet;        //bit field: 7-5:RDPROTECT 4:DPO 3:FUA 2:Reserved 1:FUA_NV 0:Obsolate
    lba : octet[4];     //Logical Block Address
    grpn: octet;        //bit field: 7-5:Reserved 4-0:GROUP NUMBER
    tlen: octet[2];     //Transfer Length
    ctl : octet;        //Control
    pad : octet[6]      //16byte長にする為のパディング
}}
type ModeSense6 = {{ 
    bit : octet;        //bit field: 3:DBD others:Reserved
    pcd : octet;        //7-6:PC 5-0:PAGE CODE
    spcd: octet;        //Sub PageCode
    alen: octet;        //Allocation Length
    ctl : octet;        //Control
    pad : octet[10]     //16byte長にする為のパディング
}}
type TestUnitReady = {{ 
    res : octet[4];     //Reserved
    ctl : octet;        //Control
    pad : octet[10]     //16byte長にする為のパディング
}}
type Write10 = {{
    bit : octet;        //bit field: 7-5:RDPROTECT 4:DPO 3:FUA 2:Reserved 1:FUA_NV 0:Obsolate
    lba : octet[4];     //Logical Block Address
    grpn: octet;        //bit field: 7-5:Reserved 4-0:GROUP NUMBER
    tlen: octet[2];     //Transfer Length
    ctl : octet;        //Control
    pad : octet[6]      //16byte長にする為のパディング
}}
type Verify = {{
    data: octet[15]     //ImmediateData
}}
type CDB = {{
    opc : octet;
    csp : (ReportLUNs|ReadCapacity|Inquiry|ModeSense6|Read10|
                                           Write10|TestUnitReady|Verify)
}}
type TestUnitReadyCDB = CDB{opc={"00"h},csp=TestUnitReady}
type InquiryCDB       = CDB{opc={"12"h},csp=Inquiry}
type ModeSense6CDB    = CDB{opc={"1a"h},csp=ModeSense6}
type ReadCapacityCDB  = CDB{opc={"25"h},csp=ReadCapacity}
type Read10CDB	      = CDB{opc={"28"h},csp=Read10}
type Write10CDB       = CDB{opc={"2a"h},csp=Write10}
type VerifyCDB        = CDB{opc={"2f"h},csp=Verify}
type ReportLUNsCDB    = CDB{opc={"a0"h},csp=ReportLUNs}

type CdbData = {cdb:CDB;data:string}

//-----------------------------------------------//
//                PDUs Definitions               //
//-----------------------------------------------//
type DG    = { /"0-9" }
type ALPHA = { /"a-zA-Z" }
type VALUE = { (DG|ALPHA|","|"."|"-"|":") }

type KeyValuePair = {{
    name : (ALPHA|DG)+;
    eq   : "=";
    value: VALUE+;
    tm   : "00"h
}}

//Operation Code of PDU
type OpeCodeNopOut        = { "00"h | "40"h }
type OpeCodeNopIn         = { "20"h }
type OpeCodeLoginRequest  = { "43"h | "c3"h }
type OpeCodeLoginResponse = { "23"h | "63"h | "a3"h | "e3"h }
type OpeCodeSCSICommand   = { "01"h | "41"h | "81"h | "c1"h }
type OpeCodeSCSIResponse  = { "21"h | "61"h | "a1"h | "e1"h }
type OpeCodeSCSIDataIn    = { "25"h | "65"h | "a5"h | "e5"h }
type OpeCodeR2T           = { "31"h | "71"h | "b1"h | "f1"h }
type OpeCodeSCSIDataOut   = { "05"h | "45"h | "85"h | "c5"h }
type OpeCodeLogoutRequest  = { "06"h | "46"h | "86"h | "c6"h }
type OpeCodeLogoutResponse = { "26"h | "66"h | "a6"h | "e6"h }

//-------------------------
//          PDU
//-------------------------
type BHS = {{
    opc : octet;          // opcode
    osf1: octet[3];       // opcode specific fields TDB: 最新のコンパイラで確認
    tal : octet;          // total AHS Length
    dsl : octet[3];       // data segment length
    osf2lun : octet[8];   // LUN or opcode specific fields
    itt : octet[4];       // initiator task tag
    osf3: octet[28]       // opcode specific field
}}
type PDU = {{
    bhs : BHS;
    data: octet*
}}
//-------------------------
//    Nop Out
//-------------------------
type NopOut3 = {{
    ttt  : octet[4];          //Target Transfer Tag
    cmdsn: octet[4];          //Command Sequence Number
    expsn: octet[4];          //Expected Status Number
    res  : octet[16]          //Reserved
}}
type NopOutPDU = PDU{
                      bhs.opc=OpeCodeNopOut,
                      bhs.osf1={"800000"h},
                      bhs.osf3=NopOut3
                    }
//-------------------------
//    Nop In
//-------------------------
type NopIn3 = {{
    ttt  : "ff"h[4];          //Target Transfer Tag
    ssn  : octet[4];          //Status Sequence Number
    expsn: octet[4];          //Next Expected CmdSN
    maxsn: octet[4];          //Maximum CmdSN
    res  : octet[12]          //Reserved
}}
type NopInPDU = PDU{
                     bhs.opc=OpeCodeNopIn,
                     bhs.osf1={"800000"h},
                     bhs.osf3=NopIn3
                   }

//-------------------------
//    LoginRequest PDU
//-------------------------
type LoginRequest1 = {{
    bit  : octet;               //bit field |Transit|Countinue|･|･|CurrentStage|NextStage|
    vmax : octet;               //Version-Max
    vmin : octet                //Version-Min
}}
type LoginRequest2 = {{
    isid : octet[6];            //Initiator-defined component of the session identifier
    tsih : octet[2]             //Target Session Identifying Handle
}}
type LoginRequest3 = {{
    cid  : octet[2];            //Connection ID
    res1 : octet[2];            //Reserved
    cmdsn: octet[4];            //Command Sequence Number
    expsn: octet[4];            //Expected Status Number
    res2 : octet[16]           //Reserved
}}
type LoginRequestData = {{
    kvp  : KeyValuePair+;       //パラメータリスト(必ず１個以上のパラメータが提示されなければならないので＋で表現）
    pad  : octet*
}}
type LoginRequestPDU = PDU{
                            bhs.opc=OpeCodeLoginRequest,
                            bhs.osf1=LoginRequest1,
                            bhs.osf2lun=LoginRequest2,
                            bhs.osf3=LoginRequest3,
                            data=LoginRequestData
                          }
//-------------------------
//    LoginResponse PDU
//-------------------------
type LoginResponse1 = {{
    bit  : octet;               //bit field |Transit|Countinue|･|･|CurrentStage|NextStage|
    vmax : octet;               //Version-Max
    vact : octet                //Version-Active
}}
type LoginResponse2 = {{
    isid : octet[6];            //Initiator-defined component of the session identifier
    tsih : octet[2]             //Target Session Identifying Handle
}}
type LoginResponse3 = {{
    res1 : octet[4];            //Reserved
    ssn  : octet[4];            //Status Sequence Number
    expsn: octet[4];            //Next Expected CmdSN
    maxsn: octet[4];            //Maximum CmdSN
    scls : octet;               //Status-Class
    sdtl : octet;               //Status-Detail
    res2 : octet[10]            //Reserved
}}
type LoginResponseData = {{
    kvp  : KeyValuePair*        //パラメータリスト（プロトコル上は１つ以上のパラメータが入るが、実装の都合上このパラメータの領域サイズは０でいいので、＊で表現）
}}
type LoginResponsePDU = PDU{
                            bhs.opc=OpeCodeLoginResponse,
                            bhs.osf1=LoginResponse1,
                            bhs.osf2lun=LoginResponse2,
                            bhs.osf3=LoginResponse3,
                            data=LoginResponseData
                          }
//-------------------------
//    SCSICommand PDU
//-------------------------
type SCSICommand1 = {{
    bit  : octet;               //bit field |Transit|Countinue|･|･|CurrentStage|NextStage|
    res1 : octet[2]             //Reserved
}}
type SCSICommand3 = {{
    edtl : octet[4];            //Expected Data Transfer Length
    cmdsn: octet[4];            //Command Sequence Number
    expsn: octet[4];            //Expected Status Number
    cdb  : CDB                  //SCSI CDB
}}
type SCSICommandPDU = PDU{
                           bhs.opc=OpeCodeSCSICommand,
                           bhs.osf1=SCSICommand1,
                           bhs.osf3=SCSICommand3
                         }
//-------------------------
//    SCSIResponse PDU
//-------------------------
type SCSIResponse1 = {{
    bit  : octet;               //bit field
    resp : octet;               //Response
    sts  : octet                //Status
}}
type SCSIResponse3 = {{
    snak : octet[4];            //SNACK Tag or Reserved
    ssn  : octet[4];            //Status Sequence Number
    expsn: octet[4];            //Next Expected CmdSN
    maxsn: octet[4];            //Maximum CmdSN
    expds: octet[4];            //
    brrc : octet[4];            //Bidirectional Read Residual Count or Reserved
    rdc  : octet[4]             //Residual Count or Reserved
}}
type SCSIResponsePDU =PDU{
                           bhs.opc=OpeCodeSCSIResponse,
                           bhs.osf1=SCSIResponse1,
                           bhs.osf3=SCSIResponse3
                         }
//-------------------------
//    SCSIDataIn PDU
//-------------------------
type SCSIDataIn1 = {{
    bit  : octet;               //bit field : F(Final) A(Acknowledge) 0 0 O(Overflow) U(Unduerflow) S(Status contain)
    res1 : octet;               //Reserved
    sts  : octet                //Status or Reserved
}}
type SCSIDataIn3 = {{
    ttt  : "ff"h[4];         //Target Transfer Tag or 0xffffffff
    ssn  : octet[4];            //Status Sequence Number or Reserved
    expsn: octet[4];            //Next Expected CmdSN
    maxsn: octet[4];            //Maximum CmdSN
    dsn  : octet[4];            //Data Sequence Number
    boff : octet[4];            //Buffer Offset
    rdc  : octet[4]             //Residual Count
}}
type SCSIDataInPDU =PDU{
                            bhs.opc=OpeCodeSCSIDataIn,
                            bhs.osf1=SCSIDataIn1,
                            bhs.osf3=SCSIDataIn3
                          }
//-------------------------
//    R2T PDU
//-------------------------
type R2T1 = {{
    res  : "800000"h            //Reserved
}}
type R2T3 = {{
    ttt  : octet[4];            //Target Transfer Tag
    ssn  : octet[4];            //Status Sequence Number
    expsn: octet[4];            //Next Expected CmdSN
    maxsn: octet[4];            //Maximum CmdSN
    r2tsn: octet[4];            //Data Sequence Number
    boff : octet[4];            //Buffer Offset
    ddtl : octet[4]             //Desired Data Transfer Length
}}
type R2TPDU = PDU{
                   bhs.opc=OpeCodeR2T,
                   bhs.osf1=R2T1,
                   bhs.osf3=R2T3
                  }
//-------------------------
//    SCSIDataOut PDU
//-------------------------
type SCSIDataOut1 = {{
    bit  : octet;               //bit field : F(Final) A(Acknowledge) 0 0 O(Overflow) U(Unduerflow) S(Status contain)
    res1 : octet[2]             //Reserved
}}
type SCSIDataOut3  = {{
    ttt  : octet[4];            //Target Transfer Tag or 0xffffffff
    res2 : octet[4];            //Reserved
    expsn: octet[4];            //Expected Status Number
    res3 : octet[4];            //Reserved
    dsn  : octet[4];            //Data Sequence Number
    boff : octet[4];            //Buffer Offset
    res4 : octet[4]             //Reserved
}}
type SCSIDataOutPDU =PDU{
                         bhs.opc=OpeCodeSCSIDataOut,
                         bhs.osf1=SCSIDataOut1,
                         bhs.osf3=SCSIDataOut3
                        }
//-------------------------
//    LogoutRequest PDU
//-------------------------
type LogoutRequest1 = {{ 
    bit  : octet;               //bit field |Transit|Countinue|･|･|CurrentStage|NextStage|
    res1 : octet[2]             //Reserved
}}
type LogoutRequest2 = {{ 
    res2 : octet[8]             //Reserved
}}
type LogoutRequest3 = {{ 
    cid  : octet[2];            //Connection ID or reserved
    res3 : octet[2];            //Reserved
    cmdsn: octet[4];            //Command Sequence Number
    expsn: octet[4];            //Expected Status Number
    res4 : octet[16]            //Reserved
}}
type LogoutRequestPDU =PDU{
                            bhs.opc=OpeCodeLogoutRequest,
                            bhs.osf1=LogoutRequest1,
                            bhs.osf2lun=LogoutRequest2,
                            bhs.osf3=LogoutRequest3
                          }
//-------------------------
//    LogoutResponse PDU
//-------------------------
type LogoutResponse1 = {{
    bit  : octet;               //bit field |Transit|Countinue|･|･|CurrentStage|NextStage|
    resp : octet;               //
    res1 : octet                //
}}
type LogoutResponse2 = {{
    res2 : octet[8]
}}
type LogoutResponse3 = {{
    res3 : octet[4];            //Reserved
    ssn  : octet[4];            //Status Sequence Number
    expsn: octet[4];            //Next Expected CmdSN
    maxsn: octet[4];            //Maximum CmdSN
    res4 : octet[4];
    t2w  : octet[2];            //Time2Wati
    t2r  : octet[2];            //Time2Retain
    res5 : octet[4]             //Reserved
}}
type LogoutResponsePDU =PDU{
                            bhs.opc=OpeCodeLogoutResponse,
                            bhs.osf1=LogoutResponse1,
                            bhs.osf2lun=LogoutResponse2,
                            bhs.osf3=LogoutResponse3
                           }
////////////////////////////////////////////////////////////////////
//                         大域変数                               //
////////////////////////////////////////////////////////////////////
var chMaxDataSeg:<int>
////////////////////////////////////////////////////////////////////
//                  ユーティリティプロセス                        //
////////////////////////////////////////////////////////////////////
proc MakePadding  ( ret:<string>, data:string ) =
    var ch:<int>;
    StrLen(data, ch);
    ch?len;
    ( len % 4 @ 3 -> ret!"00"h
              | 2 -> ret!"0000"h
              | 1 -> ret!"000000"h
              | 0 -> ret!"" )
proc MaxDataSegProc(max:int) = chMaxDataSeg!max; MaxDataSegProc(max) 
proc Str2Int( str:string, ret:<int> ) =
    var n:int;
    C{
        int i;
        int _n = 0;
        int _s = STRLEN($str$);
        for (i = 0; i < _s; i++) {
            _n *= 256;
            _n += (unsigned char)*(STRPTR($str$)+i);
        }
        $n$ = TOPINT(_n);
    C};
    ret!n
proc StrLen( str:string, ret:<int> ) =
    var n:int;
    C{
        int _n = STRLEN($str$);
        $n$ = TOPINT(_n);
    C};
    ret!n
proc SplitData( data:string, len:int, ret:<{data:string;remain:string}> ) =
    var head:string;
    var tail:string;
    C{
        int *hd, *tl, *dt; 
        int _hlen;
        int _tlen;
        $data$ = __pullup__($data$);
        $head$ = __record__(4);
        $tail$ = __record__(4);
        _hlen = TOCINT($len$);
        _tlen = STRLEN($data$) - TOCINT($len$);
        hd = (int*)$head$;
        tl = (int*)$tail$;
        dt = (int*)$data$;
        hd[1]=tl[1]=dt[1];
        hd[2]=tl[2]=0;
        hd[0]=dt[0];
        hd[3]=tl[0]=TOPINT(TOCINT(dt[0])+_hlen);
        tl[3]=TOPINT(TOCINT(tl[0])+_tlen);
    C};
    ret!{data=head;remain=tail}

////////////////////////////////////////////////////////////////////
//                      メインプロセス                            //
////////////////////////////////////////////////////////////////////
//
// iSCSI Targetサーバ
//   --- 3260番ポートを使用したiSCSI Target
//   --- 標準入力から入力があると終了
//       ※ただし，イニシエータからの接続がある場合には，
//         接続が切れるまで終了しない
proc Main() =
    var lsock:<Sockets>;
    C{
        prc_SockTcpServer($lsock$, 3260);
        if (init_fileIO() < 0) exit(1);
    C};
    WaitConnection( lsock )

proc WaitConnection( lsock:<Sockets> ) =
      stdin?msg   -> stop
    | lsock?csock -> stdout!"conneted\n";        // 接続待ち
                     LoginPhase( csock )

////////////////////////////////////////////////////////////////////
//               ログインフェーズトッププロセス                   //
////////////////////////////////////////////////////////////////////
proc LoginPhase( s: Sockets ) =
    s.in?msg;
    ( msg @ lpdu:LoginRequestPDU ->
                ( lpdu.bhs.osf1.bit @ "81"h -> stdout!"SecurityNegotiation Start\n";
                                               SecurityNegotiationMakeResponse( s.out, lpdu );
					       LoginPhase( s )
                                    | "87"h -> stdout!"OperationalNegotiation Start\n";
                                               SplitParameter( lpdu.data.kvp ); 
                                               OperationalNegotiationMakeResponse( s.out, lpdu );
                                               FullFeaturePhase( s )

                          | _ -> stdout!"Unknown bit field\n" )
          | _                    -> stdout!"Unknown PDU Type ++\n"; stop          )

proc MakeLoginResponse( sout:<string>, lpdu:LoginRequestPDU, chKvp:<string>, bit:string ) =
    var rpdu:LoginResponsePDU;
    var ch:<int>;
    var sch:<string>;
    Str2Int( lpdu.bhs.osf3.cmdsn, ch );
    ch?cmdsn;
    chKvp?kvp;
    StrLen(kvp, ch);
    ch?dsl;
    C{
        int _ssn   = htonl(g_ssn);
        int _dsl   = TOCINT($dsl$);
        int _expsn = TOCINT($cmdsn$)+1;
        _dsl   = htonl(_dsl);
        _expsn = htonl(_expsn);
        g_ssn++;
        memcpy( STRPTR($rpdu.bhs.osf1.bit$), STRPTR($bit$), 1 ), 
        memcpy( STRPTR($rpdu.bhs.dsl$) , ((char *)&_dsl)+1, 3 );
        memcpy( STRPTR($rpdu.bhs.itt$) ,STRPTR($lpdu.bhs.itt $), 4 );
        memcpy( STRPTR($rpdu.bhs.osf2lun.isid$), STRPTR($lpdu.bhs.osf2lun.isid$), 6 );
        memcpy( STRPTR($rpdu.bhs.osf3.ssn$), &_ssn, 4 );
        memcpy( STRPTR($rpdu.bhs.osf3.expsn$), &_expsn, 4 );
        memcpy( STRPTR($rpdu.bhs.osf3.maxsn$), &_expsn, 4 );
    C};
    MakePadding( sch, kvp );
    sch?pad;
    sout!rpdu^kvp^pad

proc SplitParameter( kvps:{KeyValuePair*} ) = 
    kvps @ x:{hd:KeyValuePair;tl:KeyValuePair*} -> SelectParameter( x.hd );
                                                   SplitParameter(x.tl)
         | _ -> stop
////////////////////////////////////////////////////////////////////
//               セキュリティーネゴシエーション                   //
////////////////////////////////////////////////////////////////////
proc SecurityNegotiationMakeResponse( sout:<string>, lpdu:LoginRequestPDU ) =
    var sch:<string>;
    SecurityNegotiationMakeResponse_kvp( sch, lpdu );
    MakeLoginResponse( sout, lpdu, sch, "81"h )

proc SecurityNegotiationMakeResponse_kvp  ( ret:<string>, lpdu:LoginRequestPDU ) =
    ret!"TargetPortalGroupTag=1"^"00"h^"AuthMethod=None"^"00"h

////////////////////////////////////////////////////////////////////
//              オペレーショナルネゴシエーション                  //
////////////////////////////////////////////////////////////////////
proc SelectParameter( kvp:KeyValuePair ) =
    stdout!"KVP:key="^kvp.name^"val="^kvp.value^"\n";
    ( kvp.name @ "MaxRecvDataSegmentLength" -> ProcessMaxRecvDataSegmentLength( kvp )
               | _                          -> skip 
    )
proc ProcessMaxRecvDataSegmentLength ( kvp:KeyValuePair ) =
    var max:int;
    C{
        int m = atoi( STRPTR($kvp.value$) );
        $max$ = TOPINT(m/8);
    C};
    MaxDataSegProc(max)

proc OperationalNegotiationMakeResponse( sout:<string>, lpdu:LoginRequestPDU ) =
    var sch:<string>;
    OperationalNegotiatinMakeResponse_kvp( sch, lpdu );
    MakeLoginResponse( sout, lpdu, sch, "87"h )

proc OperationalNegotiatinMakeResponse_kvp  ( ret:<string>, lpdu:LoginRequestPDU ) =
      ret!"HeaderDigest=None"^"00"h^
          "DataDigest=None"^"00"h^
          "ErrorRecoveryLevel=0"^"00"h^
          "InitialR2T=Yes"^"00"h^
          "ImmediateData=Yes"^"00"h^
          "MaxRecvDataSegmentLength=65536"^"00"h^
          "MaxBurstLength=262144"^"00"h^
          "FirstBurstLength=65536"^"00"h^
          "MaxConnections=1"^"00"h^
          "DataPDUInOrder=Yes"^"00"h^
          "DataSequenceInOrder=Yes"^"00"h^
          "DefaultTime2Wait=5"^"00"h^
          "DefaultTime2Retain=5"^"00"h^
          "MaxOutstandingR2T=1"^"00"h
////////////////////////////////////////////////////////////////////
//                     FullFeaturePhase                           //
////////////////////////////////////////////////////////////////////
proc FullFeaturePhase( s:Sockets ) =
    stdout!"FullFeaturePhase Start\n";
    var chPdu:<PDU>;
    var chCdbData:<CdbData>;
    var chData:<string>;
    iSCSI_GetPDU( s.in, "", chPdu);
    iSCSI_stack( chPdu, chCdbData, chData, s.out );
    SCSI_stack( chCdbData, chData )

proc iSCSI_GetPDU( sin:<string>, remain:string, pduOut:<PDU> ) =
    remain @ pdu:PDU ->  var ret:<int>;
                         Str2Int( pdu.bhs.dsl, ret );
                         ret?dsl;
                         var chPduData:<{data:string; remain:string}>;
                         iSCSI_GetPDUData(dsl, pdu.data, sin, chPduData);
                         chPduData?pduData;
                         iSCSI_MakePDU(pdu.bhs^pduData.data, pduOut );
                         iSCSI_GetPDU( sin, pduData.remain, pduOut )
           | _ -> var ret: <int>; 
                  StrLen( remain, ret );
                  ret?tlen;
                  ( cond!(tlen < 48) -> sin?msg; iSCSI_GetPDU( sin, remain^msg, pduOut )
                  | cond!true        -> stdout!"Unknown PDU Type %%\n"  )

proc iSCSI_MakePDU( msg:string, pduOut:<PDU> ) =
    ( msg @ pdu:PDU -> pduOut!pdu
        | _       -> stdout!"Unknown PDU Type //\n" )

proc iSCSI_GetPDUData(dsl:int, data:string, sin:<string>, chOut:<{data:string;remain:string}>) =
    var ret:<int>;
    StrLen( data, ret );
    ret?tlen;
    ( cond!(dsl==tlen) -> chOut!{data=data;remain=""}
    | cond!(dsl<=tlen) -> SplitData(data, dsl, chOut)
    | cond!(dsl>tlen)  -> sin?msg; iSCSI_GetPDUData(dsl, data^msg, sin, chOut)
    )
////////////////////////////////////////////////////////////////////
//             iSCSI Protocol Stack Processes                     //
////////////////////////////////////////////////////////////////////
//-------------------------------
// Stack Top Level Processes
//-------------------------------
proc iSCSI_stack( pduIn:<PDU>, cdbDataOut:<CdbData>, dataIn:<string>, sout:<string> ) =
    var chScpdu:<SCSICommandPDU>;
    var chSdpdu:<SCSIDataOutPDU>;
    iSCSI_stackLoop( pduIn, chScpdu, chSdpdu, sout );
    iSCSI_ProcessCommand( chScpdu, chSdpdu, cdbDataOut, dataIn, sout )
proc iSCSI_stackLoop( pduIn:<PDU>, scpduOut:<SCSICommandPDU>, sdpduOut:<SCSIDataOutPDU>, sout:<string> ) =
    pduIn?pdu;
    ( pdu @ p:SCSICommandPDU   -> scpduOut!p;
                                  iSCSI_stackLoop( pduIn, scpduOut, sdpduOut, sout )
          | p:SCSIDataOutPDU   -> sdpduOut!p;
                                  iSCSI_stackLoop( pduIn, scpduOut, sdpduOut, sout )
          | p:LogoutRequestPDU -> iSCSI_ProcessLogout( p, sout )
          | p:NopOutPDU        -> iSCSI_ProcessNopOut( p, sout );
                                  iSCSI_stackLoop( pduIn, scpduOut, sdpduOut, sout )
          | _                  -> stdout!"Unknown PDU Type --\n"
    )
proc iSCSI_ProcessCommand( scpduIn:<SCSICommandPDU>, sdpduIn:<SCSIDataOutPDU>, cdbDataOut:<CdbData>, dataIn:<string>, sout:<string> ) =
    scpduIn?scpdu;
    ( scpdu.bhs.osf3.cdb @
            c:Read10CDB       -> cdbDataOut!{cdb=c;data=""};
                                 dataIn?d;
                                 iSCSI_MakeReadResponse( scpdu, d, sout )
          | c:Write10CDB      -> iSCSI_ProcessWriteCommand( scpdu, c, sdpduIn, cdbDataOut, dataIn, sout )
          | c:(ReportLUNsCDB|InquiryCDB|ReadCapacityCDB|ModeSense6CDB)
                              -> cdbDataOut!{cdb=scpdu.bhs.osf3.cdb;data=""};
                                 dataIn?d;
                                 iSCSI_MakeDataIn( scpdu, d, 0, 0, true, sout )
          | c:(TestUnitReadyCDB|VerifyCDB)
                              -> cdbDataOut!{cdb=scpdu.bhs.osf3.cdb;data=""};
                                 dataIn?d;
                                 iSCSI_MakeSCSIResponse( scpdu, sout )
          | _                 -> stdout!"Unknown CDB Type\n"
    );
    iSCSI_ProcessCommand( scpduIn, sdpduIn, cdbDataOut, dataIn, sout )

//-------------------------------
//Process Logout Command
//-------------------------------
proc iSCSI_ProcessLogout( lrpdu:LogoutRequestPDU, sout:<string> ) =
    var rpdu:LogoutResponsePDU;
    var ch:<int>;
    Str2Int( lrpdu.bhs.osf3.cmdsn, ch );
    ch?cmdsn;
    C{
        char c = 0x80;
        int _ssn = htonl(g_ssn);
        int _expsn = TOCINT($cmdsn$)+1;
        g_ssn++;
        _expsn = htonl(_expsn);
        memcpy( STRPTR($rpdu.bhs.osf1.bit$), &c, 1 );
        memcpy( STRPTR($rpdu.bhs.itt$), STRPTR($lrpdu.bhs.itt$), 4 );
        memcpy( STRPTR($rpdu.bhs.osf3.ssn$), &_ssn, 4 );
        memcpy( STRPTR($rpdu.bhs.osf3.expsn$), &_expsn, 4 );
        memcpy( STRPTR($rpdu.bhs.osf3.maxsn$), &_expsn, 4 );
    C};
    sout!rpdu
//-------------------------------
//Process Nop Out Command
//-------------------------------
proc iSCSI_ProcessNopOut( nopdu:NopOutPDU, sout:<string> ) =
    var nipdu:NopInPDU;
    var ch:<int>;
    Str2Int( nopdu.bhs.osf3.cmdsn, ch );
    ch?cmdsn;
    C{
	int _ssn = htonl(g_ssn);
        int _expsn = TOCINT($cmdsn$)+1;
        g_ssn++;
        _expsn = htonl(_expsn);
        memcpy( STRPTR($nipdu.bhs.itt$), STRPTR($nopdu.bhs.itt$), 4 );
        memcpy( STRPTR($nipdu.bhs.osf3.ssn$), &_ssn, 4 );
        memcpy( STRPTR($nipdu.bhs.osf3.expsn$), &_expsn, 4 );
        memcpy( STRPTR($nipdu.bhs.osf3.maxsn$), &_expsn, 4 );
    C};
    sout!nipdu

//-------------------------------
//Process Write Command
//-------------------------------
proc iSCSI_ProcessWriteCommand( scpdu:SCSICommandPDU, wcdb:Write10CDB, sdpduIn:<SCSIDataOutPDU>, cdbDataOut:<CdbData>, dataIn:<string>, sout:<string> ) =
    var ret:<int>;
    Str2Int(scpdu.bhs.osf3.edtl,ret);
    ret?edtl;
    Str2Int(scpdu.bhs.dsl,ret);
    ret?dsl;
    ( cond!(edtl > dsl)  -> var chData:<string>;
                            iSCSI_MakeR2T( scpdu, edtl, sout ); //TBD FirstBurstLengtとMaxBurstLengthを考慮する必要があるかも
                            iSCSI_DataOutLoop( scpdu.data, scpdu.bhs.osf3.expsn, edtl, sdpduIn, chData );
                            chData?data;
                            cdbDataOut!{cdb=wcdb;data=data};
                            dataIn?d;
                            iSCSI_MakeSCSIResponse( scpdu, sout )
    | cond!(edtl == dsl) -> cdbDataOut!{cdb=wcdb;data=scpdu.data};
                            dataIn?d;
                            iSCSI_MakeSCSIResponse( scpdu, sout )
    | cond!true          -> stdout!"arienai\n";skip
    )
proc iSCSI_DataOutLoop( data:string, expsn:string, tlen:int, sdpduIn:<SCSIDataOutPDU>, ret:<string> ) =
    var ch:<int>;
    StrLen( data, ch );
    ch?dlen;
    ( cond!( tlen == dlen ) -> ret!data
    | cond!( tlen > dlen  ) -> sdpduIn?sdpdu;
                               ( sdpdu.bhs.osf3.expsn @ expsn -> iSCSI_DataOutLoop( data^sdpdu.data, expsn, tlen, sdpduIn, ret )
                                                      | _ -> sdpduIn!sdpdu; iSCSI_DataOutLoop( data, expsn, tlen, sdpduIn, ret ) )
    | cond!true             -> stdout!"okoranai\n";stop
    )
//-------------------------------
//Process Read Command
//-------------------------------
proc iSCSI_MakeReadResponse( scpdu:SCSICommandPDU, data:string, sout:<string> ) =
    chMaxDataSeg?max;
    iSCSI_MakeReadResponseLoop( scpdu, data, max, 0, 0, sout )

proc iSCSI_MakeReadResponseLoop( scpdu:SCSICommandPDU, data:string, max:int, offset:int, cnt:int, sout:<string> ) =
    var ret:<int>;
    StrLen(data, ret);
    ret?len;
    ( cond!(len <= max) -> iSCSI_MakeDataIn( scpdu, data, offset, cnt, true, sout )
    | cond!true         -> var ch:<{data:string;remain:string}>;
                           SplitData(data, max, ch);
                           ch?dr;
                           iSCSI_MakeDataIn( scpdu, dr.data, offset, cnt, false, sout );
                           iSCSI_MakeReadResponseLoop( scpdu, dr.remain, max, offset+max, cnt+1, sout ) 
    )
//-------------------------------
//Create R2T
//-------------------------------
proc iSCSI_MakeR2T( scpdu:SCSICommandPDU, edtl:int, sout:<string> ) =
    var rpdu:R2TPDU;
    var ret:<int>;
    Str2Int( scpdu.bhs.osf3.cmdsn, ret );
    ret?cmdsn;
    Str2Int( scpdu.bhs.dsl, ret );
    ret?dsl;
    var ddtl = edtl - dsl;
    C{
        int _ddtl  = TOCINT($ddtl$);
        int _ssn   = htonl(g_ssn);
        int _expsn = TOCINT($cmdsn$)+1;
        _ddtl  = htonl(_ddtl);
        _expsn = htonl(_expsn);
        memcpy( STRPTR($rpdu.bhs.itt$)        ,STRPTR($scpdu.bhs.itt$), 4 );
        memcpy( STRPTR($rpdu.bhs.osf3.ssn$)   ,&_ssn, 4 );
        memcpy( STRPTR($rpdu.bhs.osf3.expsn$) ,&_expsn, 4 );
        memcpy( STRPTR($rpdu.bhs.osf3.maxsn$) ,&_expsn, 4 );
        memcpy( STRPTR($rpdu.bhs.osf3.boff$)+1,STRPTR($scpdu.bhs.dsl$), 3 );
        memcpy( STRPTR($rpdu.bhs.osf3.ddtl$)  ,&_ddtl, 4 );
    C};
    sout!rpdu

//-------------------------------
//Create DataIn
//-------------------------------
proc iSCSI_MakeSCSIDataIn( scpdu:SCSICommandPDU, data:string, offset:int, dsn:int, rdc:int, bit:string, isFinal:bool, sout:<string> ) =
    var rpdu:SCSIDataInPDU;
    var ret:<int>;
    Str2Int( scpdu.bhs.osf3.cmdsn, ret );
    ret?cmdsn;
    C{
        char _bit   = 0x00;
        int _rdc    = 0;
        int _offset = TOCINT($offset$);
        int _dsl    = STRLEN($data$); //このDataInPDUで搬送されるデータの長さ
        int _dsn    = TOCINT($dsn$);
        int _ssn    = htonl(g_ssn);
        int _expsn  = TOCINT($cmdsn$)+1;
        _expsn = htonl(_expsn);

        if (TOCINT($isFinal$)) {
            memcpy( &_bit, STRPTR($bit$), 1 );
            g_ssn++;
            _bit |= 0x81;
            _rdc    = TOCINT($rdc$);
        }
        memcpy( STRPTR($rpdu.bhs.itt$)        ,STRPTR($scpdu.bhs.itt$), 4 );
        memcpy( STRPTR($rpdu.bhs.osf3.ssn$)   ,&_ssn, 4);
        memcpy( STRPTR($rpdu.bhs.osf3.expsn$) ,&_expsn, 4);
        memcpy( STRPTR($rpdu.bhs.osf3.maxsn$) ,&_expsn, 4);

        //set A bit : Error Recovery Level 0では使わないので常に0で決め打ち。
        // Nothing to do.
        _dsl    = htonl(_dsl);
        _rdc    = htonl(_rdc);
        _dsn    = htonl(_dsn);
        _offset = htonl(_offset);
        memcpy( STRPTR($rpdu.bhs.osf1.bit$) , &_bit, 1 );
        memcpy( STRPTR($rpdu.bhs.dsl$) , ((char*)&_dsl)+1, 3 );
        memcpy( STRPTR($rpdu.bhs.osf3.rdc$) , &_rdc, 4 );
        memcpy( STRPTR($rpdu.bhs.osf3.dsn$) , &_dsn, 4 );
        memcpy( STRPTR($rpdu.bhs.osf3.boff$) , &_offset, 4 );
    C};
    sout!rpdu^data

proc iSCSI_MakeDataIn( scpdu:SCSICommandPDU, data:string, offset:int, dsn:int, isFinal:bool, sout:<string> ) =
    var ch:<int>;
    Str2Int( scpdu.bhs.osf3.edtl, ch );
    ch?edtl;
    StrLen( data, ch );
    ch?dsl;
    ( cond!( edtl > dsl+offset )  -> iSCSI_MakeSCSIDataIn( scpdu, data, offset, dsn, edtl-(dsl+offset), "02"h, isFinal, sout )
    | cond!( edtl < dsl+offset )  -> var retData:<{data:string;remain:string}>;
                                     SplitData( data, edtl, retData );
                                     retData?d;
                                     iSCSI_MakeSCSIDataIn( scpdu, d.data, offset, dsn, (dsl+offset)-edtl, "04"h, isFinal, sout )
    | cond!( edtl == dsl+offset ) -> iSCSI_MakeSCSIDataIn( scpdu, data, offset, dsn, 0, "00"h, isFinal, sout )
    )

//-------------------------------
//Create SCSI Response
//-------------------------------
proc iSCSI_MakeSCSIResponse( scpdu:SCSICommandPDU, sout:<string> ) =
    var rpdu:SCSIResponsePDU;
    var ret:<int>;
    Str2Int( scpdu.bhs.osf3.cmdsn, ret );
    ret?cmdsn;
    C{
        char _bit = 0x80;
        int  _ssn = htonl(g_ssn);
        int  _expsn = TOCINT($cmdsn$)+1;
        g_ssn++;
        _expsn = htonl(_expsn);
        memcpy( STRPTR($rpdu.bhs.osf1.bit$) , &_bit, 1 );

        memcpy( STRPTR($rpdu.bhs.itt$)        ,STRPTR($scpdu.bhs.itt$), 4 );
        memcpy( STRPTR($rpdu.bhs.osf3.ssn$)   ,&_ssn, 4);
        memcpy( STRPTR($rpdu.bhs.osf3.expsn$) ,&_expsn, 4);
        memcpy( STRPTR($rpdu.bhs.osf3.maxsn$) ,&_expsn, 4);
    C};
    //resp, sts, tal, snack, expds, brrcは0のままでOKなので、とくに処理しない。
    sout!rpdu

////////////////////////////////////////////////////////////////////
//                   SCSI Protocol Stack                         //
////////////////////////////////////////////////////////////////////
//-------------------------------
// Stack Top Level Processes
//-------------------------------
proc SCSI_stack( in:<CdbData>, out:<string>  ) = 
    in?msg;
    SCSI_MakeResponse( out, msg );
    SCSI_stack( in, out )

proc SCSI_MakeResponse( ret:<string>, dat:CdbData  ) =
    (dat.cdb @ rlns:ReportLUNsCDB   -> SCSI_MakeReportLUNsResponse( ret, rlns.csp )
             | inqu:InquiryCDB      -> SCSI_MakeInquiryResponse( ret,inqu.csp )
             | redc:ReadCapacityCDB -> SCSI_MakeReadCapacityResponse( ret,redc.csp )
             | read:Read10CDB       -> SCSI_MakeReadResponse( ret,read.csp )
             | mods:ModeSense6CDB   -> SCSI_MakeModeSenseResponse( ret,mods.csp )
             | turd:TestUnitReadyCDB-> ret!""
             | writ:Write10CDB      -> SCSI_MakeWriteResponse( ret, writ.csp, dat.data )
             | very:VerifyCDB       -> ret!""
         | _                    -> stdout!"Unknown SCSI CDB Opcode \n"; stop 
    )
//-------------------------------
//Create ReportLUNs response
//-------------------------------
proc SCSI_MakeReportLUNsResponse( ret:<string>, rlns:ReportLUNs ) =
    ret!"00000008000000000000000000000000"h //決め打ち
//-------------------------------
//Create Inquiry response
//-------------------------------
proc SCSI_MakeInquiryResponse( ret:<string>, inqu:Inquiry ) =
    var ch:<int>;
    Str2Int( inqu.evpd, ch );
    ch?evpd;
    ( evpd%2 @ 0 -> SCSI_MakeInquiryResponseData( ret, inqu ) //EVPD bit off
             | 1 -> SCSI_MakeInquiryResponsePage( ret, inqu ) //EVPD bit on
    )

type InquiryData = {{
    periphType  : "00"h;
    rmb         : "00"h;
    version     : "04"h;
    aerc        : "42"h;
    addLen      : "3b"h;
    sccs        : "00"h;
    bque        : "00"h;
    reladr      : "02"h;
    vendorId    : "ISP/OGK ";
    productId   : "Preccs/iSCSI-TGT";
    prodRevLev  : "0.00"
}}

proc SCSI_MakeInquiryResponseData( ret:<string>, inqu:Inquiry ) = 
    var inqRespData:InquiryData;
    ret!inqRespData

type SupportedVitalProductDataPages = {{
    periphType  : "00"h;
    pageCode    : "00"h; // SVP
    reserved    : "00"h;
    pageLen     : "02"h;
    pageList    : "0083"h;
    padding     : "0000"h
}}

type IdentifierDescriptor = {{
    codeSet     : "01"h;
    identType   : "01"h;
    reserved    : "00"h;
    identLen    : "18"h;
    identifier  : "ISP/iSCSI/DiskImage.0000"
}}
type DeviceIdentificationPage = {{
    periphType  : "00"h;
    pageCode    : "83"h; // SVP
    reserved    : "00"h;
    pageLen     : "1c"h;
    idDesc      : IdentifierDescriptor
}}

proc SCSI_MakeInquiryResponsePage( ret:<string>, inqu:Inquiry ) =
    (inqu.pcd  @  a:PageCodeSupportVitalProductData -> SCSI_MakeInquiryResponsePage_SVP( ret, inqu )
               |  a:PageCodeDeviceIdentification    -> SCSI_MakeInquiryResponsePage_DID( ret, inqu )
               | _  -> stdout!"Unknown SCSI Inquiry PageType \n"; stop 
    )
proc SCSI_MakeInquiryResponsePage_SVP( ret:<string>, inqu:Inquiry ) =
    var svpDataPages:SupportedVitalProductDataPages;
    ret!svpDataPages

proc SCSI_MakeInquiryResponsePage_DID( ret:<string>, inqu:Inquiry ) =
    var devIdPage:DeviceIdentificationPage;
    ret!devIdPage

//-------------------------------
//Create ReadCapacity response 
//-------------------------------
proc SCSI_MakeReadCapacityResponse( ret:<string>, redc:ReadCapacity ) = 
    var resp:{{lastlba:octet[4]; blksz:"00000200"h}};
    C{
        int _lastlba;
        extern int g_block_num;
        _lastlba = htonl(g_block_num-1);
        memcpy(STRPTR($resp.lastlba$), &_lastlba, sizeof _lastlba);
    C};
    ret!resp

//-------------------------------
//Create Read response 
//-------------------------------
proc SCSI_MakeReadResponse( ret:<string>, read:Read10 ) =
    var databuf:string;
    C{
        uint32_t _lba;
        uint16_t _tlen;
        off_t _addr;
        size_t _size;

        memcpy( &_tlen, STRPTR($read.tlen$), 2);
        _tlen = ntohs(_tlen);

        memcpy( &_lba, STRPTR($read.lba$), 4);
        _lba = ntohl(_lba);

        _size = _tlen * 512;
        _addr = _lba  * 512;

        $databuf$ = __salloc__( _size ); //とりあえず、Block長は512で決め打ち
        read_data( _addr, _size, (char*)STRPTR($databuf$) );
    C};
    ret!databuf

//-------------------------------
//Create ModeSense response 
//-------------------------------
type ModePatameterHeader6 = {{
    modeDataLen     : octet;
    mediumType      : "00"h;
    deviceSpec      : "00"h;
    blockDescLen    : "08"h
}}
type DirectAccesDevBlockDescriptor = {{
    numBlocks   : octet[4];
    destinyCode : "00"h;
    blockLen    : "000200"h // 512B
}}
type InformationalExceptionsModePage = {{
    pageCode    : "1c"h;
    pageLen     : "0a"h;
    bitField    : "08"h;
    MRIE        : "00"h;
    intervalTm  : "00000000"h;
    reposrtCnt  : "00000000"h
}}
proc SCSI_MakeModeSenseResponse( ret:<string>, mods:ModeSense6 ) =
    var mh:ModePatameterHeader6;
    var bd:DirectAccesDevBlockDescriptor;
    var iec:InformationalExceptionsModePage;
    var ch:<int>;

    StrLen( iec, ch );
    ch?len;
    C{
        extern int g_block_num;
        char c = TOCINT($len$);
        int _block_num = htonl(g_block_num);
        memcpy( STRPTR($mh.modeDataLen$), &c, 1 );
        memcpy( STRPTR($bd.numBlocks$), &_block_num, sizeof(_block_num) );
    C};

    ret!mh^bd^iec
//ModeSenseコマンドを実装するサーバはModeSelectも実装しなければならない、SPC2-r20では規定されている。
//実装していないページコード要求された場合の挙動、SPC2-r20のp.101
//-------------------------------
//Process write
//-------------------------------
proc SCSI_MakeWriteResponse( ret:<string>, writ:Write10, data:string ) =
    C{
        size_t _dlen = STRLEN($data$);
        uint32_t _addr;
        memcpy( &_addr, STRPTR($writ.lba$), 4 );
        write_data( ntohl(_addr)*512, _dlen, (char*)STRPTR($data$) );
    C};
    ret!""
