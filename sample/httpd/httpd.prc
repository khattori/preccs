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
	sp2	: (SP; HttpVersion; CRLF) }}
type RequestLineGet = RequestLine{method={"GET"}}
type HttpRequest = {{
	start	: RequestLine;
	head	: TEXT;
	crlf	: CRLF;
	body	: octet*
}}
type HttpRequestGet = HttpRequest{start=RequestLineGet}
type SocketPair = {in:<string>;out:<string>}
type FileIn = {ok:bool;in:<string>}
proc Main() =
    var lsock:<SocketPair>;
    C{	prc_SockTcpServer($lsock$, 80); C};
    HttpSrv(lsock)
proc HttpSrv(lsock:<SocketPair>) =
      stdin?msg   -> stop
    | lsock?csock -> HttpOnAcc(csock); HttpSrv(lsock)
proc HttpOnAcc(sp:SocketPair) =
    sp.in?msg;
    ( msg @ x:HttpRequestGet -> HttpOnRead(sp, x.start.path)
          | _ -> stdout!"unknown request: "^msg; sp.out!"" )
proc HttpOnRead(sp:SocketPair, path:string) =
    var ret:<FileIn>;
    PrcFileOpenR(ret, "."^path);
    ret?fr;
    ( fr.ok @ true  -> sp.out!"HTTP/1.1 200 OK\r\n\r\n"; ReadFile(fr.in, sp.out)
            | false -> sp.out!"HTTP/1.1 404 Not found\r\n"; sp.out!"" )
proc ReadFile(in:<string>,out:<string>) =
    in?buf; out!buf; ReadFile(in,out)
proc PrcFileOpenR(ret:<FileIn>, fname:string) =
    var h:int;
    var fin:<string>;
    C{ $h$ = TOPINT(prc_FileOpenR($fin$,STRPTR($fname$))); C};
    ( h @ -1 -> ret!{ok=false;in=fin}
        | _  -> ret!{ok=true;in=fin} )
