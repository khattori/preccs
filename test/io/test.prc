var ch:<unit>

proc Main() = Sender(); Sender(); Receiver()

proc Sender() = ch!() -> stdout!"OK3\n" | ch!() -> stdout!"OK4\n"
proc Receiver() =
    ch?x -> stdout!"OK1\n"
  | ch?x -> stdout!"OK2\n"

proc Test1() =
    stdin?msg;
    stdout!"Mesg:"^msg;
    timer!1;
    stdout!"timeout\n";
    (
      timer!10 -> stdout!"TO.\n"
    | stdin?msg -> stdout!"MSG: "^msg )

