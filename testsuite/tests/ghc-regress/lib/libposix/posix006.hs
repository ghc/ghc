import Posix

main = 
    epochTime >>= \ start ->
    sleep 5 >>
    let timeleft = 0 in
    epochTime >>= \ finish ->
    putStr "Started: " >>
    print start >>
    putStr "\nSlept: " >>
    print (5 - timeleft) >>
    putStr "\nFinished: " >>
    print finish >>
    putChar '\n'
