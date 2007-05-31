C{
#include "iotest.h"
C}

proc Main() =
    var mode:int;
    C{ $mode$ = TOPINT(g_test_mode); C};
    ( mode @ 1 -> StdinProc()
           | 2 -> StdoutProc()
           | 3 -> TimerProc()
	   | 4 -> ServerProc()
	   | 5 -> ClientProc()
           | _ -> stop )

proc StdinProc() = stdout!"start StdinProc()\n"
proc StdoutProc() = stdout!"start StdoutProc()\n"
proc TimerProc() = stdout!"start TimerProc()\n"
proc ServerProc() = stdout!"start ServerProc()\n"
proc ClientProc() = stdout!"start ClientProc()\n"


var ch:<unit>

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

