import System (getEnv)

main = 
    getEnv "TERM" >>= \ term -> 
    putStr "Got $TERM" >>
    putChar '\n' >>
    getEnv "One fish, two fish, red fish, blue fish" >>= \ fish -> 
    putStr fish >> 
    putChar '\n'
