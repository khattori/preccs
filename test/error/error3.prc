// is not exhaustive
proc Main() =
    var x : {("foo"|"bar"|"baz")};
    ( x @ "foo" -> stop
        | "bar" -> stop
        | y:"foo"|"bar" -> stop )
