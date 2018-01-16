-- Program from Neil Brown on haskell-cafe.
--
-- It exposes the O(n^2) behaviour in removing threads from the queue
-- on an MVar during shutdown - in GHC 6.12.1 the program takes 25s in
-- the EXIT phase deleting threads.

import Control.Concurrent
import Control.Concurrent.CML
import Control.Monad

main :: IO ()
main = do 
  let numChoices = 2
  cs <- replicateM numChoices channel
  mapM_ forkIO [replicateM_ (100000 `div` numChoices) $ sync $ transmit c () | c <- cs]
  replicateM_ 100000 $ sync $ choose [receive c (const True) | c <- cs] 
