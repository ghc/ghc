import Control.Concurrent
import System.Posix.IO

main = do
  putStrLn "hello"
  fd <- openFd "/dev/random" ReadOnly defaultFileFlags
  threadWaitRead fd
  putStrLn "goodbye"

