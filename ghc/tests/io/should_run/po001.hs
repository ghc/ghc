import Posix

main =
    getParentProcessID >>= \ ppid ->
    getProcessID >>= \ pid ->
    putStr "Parent Process ID: " >>
    print ppid >>
    putStr "\nProcess ID: " >>
    print pid >>
    putStr "\nforking ps uxww" >>
    print ppid >>
    putChar '\n' >>
    forkProcess >>= \ child ->
    case child of
	Nothing -> executeFile "ps" True ["uxww" ++ show ppid] Nothing
	Just x -> doParent x pid

doParent cpid pid =
    getProcessStatus True False cpid >>
    putStr "\nChild finished.  Now exec'ing ps uxww" >>
    print pid >>
    putChar '\n' >>
    executeFile "ps" True ["uxww" ++ show pid] Nothing
