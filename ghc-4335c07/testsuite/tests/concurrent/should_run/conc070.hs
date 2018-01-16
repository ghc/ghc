import Control.Concurrent
import GHC.Conc
import Data.List
import Data.Maybe

main = do
  t1 <- forkIO (threadDelay 100000000)
  m <- newEmptyMVar
  t2 <- forkIO (takeMVar m)
  t3 <- forkIO (let loop = do r <- tryTakeMVar m; 
                              _ <- newEmptyMVar -- do some allocation :(
                              if isNothing r then loop else return ()
                in loop)
  t4 <- forkIO (return ())
  yield
  threadDelay 10000
  print =<< mapM threadStatus [t1,t2,t3,t4]
  putMVar m ()
