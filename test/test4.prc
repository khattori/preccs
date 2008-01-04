/* HTTPリクエストの処理 */
type DG    = { /"0-9" }
type ALPHA = { /"a-zA-Z" }
type CRLF  = { "\r\n" } 
type SP    = { " " }
type Unreserved = { ALPHA|DG|"-"|"."|"_"|"~" }
type Path = { ("/";Unreserved*)* }
type HttpVersion = { "HTTP/1";DG*;".";("0"|"1");DG* }
type RequestLine = {{
	method	: "GET";
	sp1	: SP;
	path	: Path;
	sp2	: (SP; HttpVersion; CRLF) }}

proc Test4(end:<bool>,ok:<int>,ng:<int>) =
    var msg = "GET /httpd.prc HTTP/1.1\r\n";
    { msg @ x:RequestLine -> ok!1;
            { x.path @ "/httpd.prc" -> ok!1
                     | _            -> ng!1 }
          | _ -> ng!1 };
    end!true
