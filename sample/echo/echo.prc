/*
 * Echoプロセス
 *    --- 標準入力からの入力を標準出力に出力する(Quitかquitで終了する)
 */

type NewLine = { "\r"?; "\n" }

proc Main() = Echo()

proc Echo() = 
    stdin?msg;
    { msg @ x:{l:("q"|"Q");m:"uit"?;n:NewLine} -> stdout!"Bye!";stop 
          | _ -> stdout!msg; Echo() }
