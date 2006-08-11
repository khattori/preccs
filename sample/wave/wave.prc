var data = "00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF"h

proc Main() =
    stdout!"Menu:\n- (Q)uit\n- (S)tart\n";
    stdin?cmd;
    ( cmd @ x:("Q"|"q");octet* -> stop
          | x:("S"|"s");octet* -> WaveStart()
          | _ -> Main() )

proc WaveStart() =
    var wout:<string>;
    C{ prc_WaveOutOpen($wout$, 8000); C};
    Wave(wout)

proc Wave(wout:<string>) =
      stdin?cmd -> wout?msg; Main()
    | wout!data^data^data^data -> Wave(wout)

    