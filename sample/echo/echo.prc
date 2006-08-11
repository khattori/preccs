/*
 * Echoプロセス
 *    Qかqで終了する
 */

type NewLine = { "\r"?; "\n" }

proc Main() = Echo()

proc Echo() = 
    stdin?msg;
    ( msg @ x:{l:("q"|"Q");m:"uit";n:"\r"?;o:"\n"} -> stdout!(x.m^"Bye!");stop
          | x:{l:octet[2];m:octet[4]} -> stdout!msg; Echo()
          | _ -> stdout!msg; Echo() )
