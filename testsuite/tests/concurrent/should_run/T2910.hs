import Control.Exception
import GHC.Conc
  
main = do
    t1 <- mask $ \_ -> forkIO yield
    t2 <- forkIO $ killThread t1
    threadDelay 100000
    threadStatus t1 >>= print
    threadStatus t2 >>= print
