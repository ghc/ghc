import LibPosix

main =
    getEnvVar "TERM" >>= \ term ->
    putStr term >>
    putChar '\n' >>
    setEnvironment [("one","1"),("two","2")] >>
    getEnvironment >>= \ env ->
    putText env >>
    putChar '\n' >>
    setEnvVar "foo" "bar" >>
    getEnvironment >>= \ env ->
    putText env >>
    putChar '\n' >>
    setEnvVar "foo" "baz" >>
    getEnvironment >>= \ env ->
    putText env >>
    putChar '\n' >>
    setEnvVar "fu" "bar" >>
    getEnvironment >>= \ env ->
    putText env >>
    putChar '\n' >>
    removeEnvVar "foo" >>
    getEnvironment >>= \ env ->
    putText env >>
    putChar '\n' >>
    setEnvironment [] >>
    getEnvironment >>= \ env ->
    putText env >>
    putChar '\n'
