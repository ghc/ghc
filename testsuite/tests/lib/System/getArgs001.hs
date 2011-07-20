import System.Environment (getProgName, getArgs)

main = 
    getProgName >>= \ argv0 ->
    putStr argv0 >>
    getArgs >>= \ argv ->
    sequence (map (\ x -> putChar ' ' >> putStr x) argv) >>
    putChar '\n'

