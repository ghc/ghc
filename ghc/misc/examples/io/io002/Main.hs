import System (getEnv)

main = 
    getEnv "TERM" >>= \ term -> 
    putStr term >>
    putChar '\n' >>
    getEnv "One fish, two fish, red fish, blue fish" >>= \ fish -> 
    putStr fish >>
    putChar '\n'



