module Main(main) where

import Posix

main :: IO ()
main = do
    ppid <- getParentProcessID
    pid  <- getProcessID
    putStr "Parent Process ID: "
    print ppid
    putStr "Process ID: "
    print pid
    putStr "forking ps ux"
    print ppid
    child <- forkProcess
    case child of
	Nothing -> executeFile "ps" True ["ux" ++ show ppid] Nothing
	Just x -> doParent x pid

doParent cpid pid = do
    getProcessStatus True False cpid
    putStr "\nChild finished.  Now exec'ing ps ux\n"
    print pid
    executeFile "ps" True ["ux" ++ show pid] Nothing
