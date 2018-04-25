import Control.Exception
import Control.Concurrent
import GHC.Conc
import Control.Monad

nfib n = if n < 2 then 1 else nfib (n-2) + nfib (n-1)

main = do
   t <- mask_ $ forkIO $ forM_ [1..] $ \n -> nfib n `seq` allowInterrupt
   killThread t
   let loop = do r <- threadStatus t
                 when (r /= ThreadFinished) $ do yield; loop
   loop
