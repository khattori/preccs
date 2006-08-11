type BootpOption = {{
    len	: octet;
    tag	: octet;
    data	: octet[len]
}}
type Bootp = {{
    opt	: BootpOption;
    data	: octet*
}}
type Foo = {{
    field1	: (octet;octet;octet;octet);
    field2	: {f1:octet;f2:octet};
    field3	: octet*
}}

type Bopt1 = BootpOption{tag={"a"}}
type Bopt2 = BootpOption{tag={"b"}}
type Bopt3 = BootpOption{tag={"a"},len={"a"}}
type Bopt4 = BootpOption{len={"04"h},tag={"00"h}}

type BootpFoo = Bootp{opt.tag={"01"h}}
type BootpBar = Bootp{opt.tag={"02"h}}
//var msg = "Hello,world!\n"
type Bar = Foo{field1={"abcd"},field2.f1={"e"},field3={"hogehoge\n"}}
proc Main() =
    var bar:Bar;
    stdout!bar.field1^"\n";
    stdout!bar.field2.f1^"\n";
    stdout!bar.field3;
    var x = "hogehogehogehogehogehoge";
    ( x @ f:Foo -> stdout!"OK\n";
                   stdout!f.field1^"\n";
                   stdout!f.field2^"\n";
                   stdout!f.field3^"\n";
                   C{
                       printf("len:%d\n", STRLEN($f.field1$));
                   C}
        | _ -> stdout!"NG\n" )

/*
    ( "00"h^"a" @ x: Bopt1 -> stdout!"OK\n"
                | x: Bopt2 -> stdout!"NG1\n"
                | _        -> stdout!"NG2\n" );
    ( "00"h^"b" @ x: Bopt1 -> stdout!"NG1\n"
                | x: Bopt2 -> stdout!"OK\n"
                | _        -> stdout!"NG2\n" );
    ( "0001"h @ x: BootpFoo -> stdout!"OK\n"
              | x: BootpBar -> stdout!"NG1\n"
              | _           -> stdout!"NG2\n" );
    ( "0002"h @ x: BootpFoo -> stdout!"NG1\n"
              | x: BootpBar -> stdout!"OK\n"
              | _           -> stdout!"NG2\n" );
    ( "0003"h @ x: BootpFoo -> stdout!"NG1\n"
              | x: BootpBar -> stdout!"NG2\n"
              | _           -> stdout!"OK\n" );
    var ch1:<string>; var ch2:<string>;
    P(ch1,ch2);Q(ch1,ch2);P(ch1,ch2);Q(ch1,ch2)

proc P(ch1:<string>,ch2:<string>) =
    ch1?x  -> stdout!"P_ch1_in\n"
  | ch2!"" -> stdout!"P_ch2_out\n"

proc Q(ch1:<string>,ch2:<string>) =
    ch1!"" -> stdout!"Q_ch1_out\n"
  | ch2?y  -> stdout!"Q_ch2_in\n"
*/