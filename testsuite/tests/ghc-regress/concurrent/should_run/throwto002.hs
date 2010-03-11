{-# LANGUAGE DoRec, ScopedTypeVariables #-}
import Control.Concurrent
import Control.Exception
import Data.Array
import System.Random
import System.Environment
import Control.Monad
import GHC.Conc
import Data.IORef
import Prelude hiding (catch)

main = do
  r <- newIORef 0
  rec
    t1 <- block $ forkIO (thread r t2)
    t2 <- block $ forkIO (thread r t1)
  threadDelay 1000000
  readIORef r >>= print

thread r t = run
  where 
    run = (unblock $ forever $ do killThread t
                                  i <- atomicModifyIORef r (\i -> (i + 1, i))
                                  evaluate i)
             `catch` \(e::SomeException) -> run
