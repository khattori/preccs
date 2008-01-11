type DG    = { /"0-9" }
type ALPHA = { /"a-zA-Z" }
type CRLF  = { "\r\n" } 
type SP    = { " " }
type HT    = { "\t" }
type LWS   = { "\r\n"?;(SP|HT)+ }
type TEXT  = { /"^\r\n"* }
type Unreserved = { ALPHA|DG|"-"|"."|"_"|"~" }
type Host = { Unreserved* }
type Path = { ("/";Unreserved*)* }
type HttpVersion = { "HTTP/1";DG*;".";("0"|"1");DG* }
type Method = {
	  "OPTIONS"
	| "GET"
	| "HEAD"
	| "POST"
	| "PUT"
	| "DELETE"
	| "TRACE"
}
type RequestLine = {{
	method	: Method;
	sp1	: SP;
	path	: Path;
	sp2	: (SP; HttpVersion; CRLF)
}}
type RequestLineGet = RequestLine{method={"GET"}}
type HttpRequest = {{
	start	: RequestLine;
	head	: TEXT;
	crlf	: CRLF;
	body	: octet*
}}
type HttpRequestGet = HttpRequest{start=RequestLineGet}
type Socket = {in:<string>;out:<string>}
type FileIn = {ok:bool;in:<string>}

proc Main() =
    var lsock:<Socket>;
    C{ prc_SockTcpServer($lsock$, 8080); C};
    HttpSrv(lsock)

proc HttpSrv(lsock:<Socket>) =
    lsock?csock;
    HttpOnAcc(csock);
    HttpSrv(lsock)

proc HttpOnAcc(sp:Socket) =
    sp.in?msg;
    { msg @ "" -> stdout!"CLOSED\n"
          | _  -> { msg @ x:HttpRequestGet -> HttpOnRead(sp, x.start.path)
                  | _ -> stdout!"unknown request: "^msg;
                         sp.out!"" } }
proc HttpOnRead(sp:Socket, path:string) =
    var ret = PrcFileOpenR("."^path);
    { ret.ok @ true  -> sp.out!"HTTP/1.1 200 OK\r\n\r\n";
                        ReadFile(ret.in, sp.in, sp.out)
             | false -> sp.out!"HTTP/1.1 404 Not found\r\n";
                        sp.out!""; sp.in?msg }
proc ReadFile(in:<string>,sin:<string>,out:<string>) =
    in?buf; out!buf;
    { buf @ "" -> sin?msg
          | _  -> ReadFile(in,sin,out) }
proc PrcFileOpenR(fname:string):FileIn=
    var h:int;
    var fin:<string>;
    C{ $h$ = TOPINT(prc_FileOpenR($fin$,STRPTR($fname$))); C};
    { h @ -1 -> return {ok=false;in=fin}
        | _  -> return {ok=true;in=fin} }
