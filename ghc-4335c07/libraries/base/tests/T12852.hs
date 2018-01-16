import GHC.Conc
import GHC.IO
import GHC.IO.FD as FD
import System.Posix.IO
import System.Posix.Types

main = do
  (rfd,wfd) <- createPipe
  (waitread, unregister) <- threadWaitReadSTM rfd
  unregister
  result0 <- atomically $ (fmap (const False) waitread) `orElse` return True
  print result0
  fdWrite wfd "test"
  threadDelay 20000
  result1 <- atomically $ (fmap (const False) waitread) `orElse` return True
  print result1
  (waitread1, _) <- threadWaitReadSTM rfd
  threadDelay 20000
  result2 <- atomically $ (fmap (const True) waitread1) `orElse` return False
  print result2
