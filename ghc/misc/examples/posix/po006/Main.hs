import LibPosix

main = 
    epochTime >>= \ start ->
    sleep 5 >>
    let timeleft = 0 in
    epochTime >>= \ finish ->
    putStr "Started: " >>
    putText start >>
    putStr "\nSlept: " >>
    putText (5 - timeleft) >>
    putStr "\nFinished: " >>
    putText finish >>
    putChar '\n'
