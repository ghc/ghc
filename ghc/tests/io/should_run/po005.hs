import Posix

main =
    getEnvVar "TERM" >>= \ term ->
    putStr term >>
    putChar '\n' >>
    setEnvironment [("one","1"),("two","2")] >>
    getEnvironment >>= \ env ->
    print env >>
    putChar '\n' >>
    setEnvVar "foo" "bar" >>
    getEnvironment >>= \ env ->
    print env >>
    putChar '\n' >>
    setEnvVar "foo" "baz" >>
    getEnvironment >>= \ env ->
    print env >>
    putChar '\n' >>
    setEnvVar "fu" "bar" >>
    getEnvironment >>= \ env ->
    print env >>
    putChar '\n' >>
    removeEnvVar "foo" >>
    getEnvironment >>= \ env ->
    print env >>
    putChar '\n' >>
    setEnvironment [] >>
    getEnvironment >>= \ env ->
    print env >>
    putChar '\n'
