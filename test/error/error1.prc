// is not exhaustive
proc Main() =
    var x : {("foo"|"bar")*};
    ( x @ "foo" -> stop
        | "bar" -> stop
        | y:"foo"|"bar" -> stop )
