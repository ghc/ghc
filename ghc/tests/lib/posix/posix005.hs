import Posix
import IO
main =
    hSetBuffering stdout NoBuffering >>
    getEnvVar "TERM" >>= \ term ->
    putStrLn term >>
    setEnvironment [("one","1"),("two","2")] >>
    getEnvironment >>= \ env ->
    print env >>
    setEnvVar "foo" "bar" >>
    getEnvironment >>= \ env ->
    print env >>
    setEnvVar "foo" "baz" >>
    getEnvironment >>= \ env ->
    print env >>
    setEnvVar "fu" "bar" >>
    getEnvironment >>= \ env ->
    print env >>
    removeEnvVar "foo" >>
    getEnvironment >>= \ env ->
    print env >>
    setEnvironment [] >>
    getEnvironment >>= \ env ->
    print env

