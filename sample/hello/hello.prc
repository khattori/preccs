proc HelloWorld() =
    stdout!"Hello,world1\n" -> stop
  | stdout!"Hello,world2\n" -> stop
  | stdout!"Hello,world3\n" -> stop

proc Main() = HelloWorld(); stdout!"Finished\n"
