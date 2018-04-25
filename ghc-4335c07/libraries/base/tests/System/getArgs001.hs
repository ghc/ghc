import System.Environment (getProgName, getArgs)

main = do argv0 <- getProgName
          putStr argv0
          argv <- getArgs
          mapM_ (\ x -> putChar ' ' >> putStr x) argv
          putChar '\n'

