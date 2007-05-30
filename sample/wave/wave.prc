var data = "80A7CBE7F9FFF9E7CBA78050341806000618345080A7CBE7F9FFF9E7CBA780503418060006183450"h

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
      stdin?cmd -> wout!""; Main()
    | wout!data^data^data^data -> Wave(wout)

    