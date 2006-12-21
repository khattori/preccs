// ignore match
proc Main() =
    var x:{"a"+};
    ( x @ y:"abc" -> stop
        | _       -> stop )
