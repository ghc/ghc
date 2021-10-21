{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}
import Control.Concurrent
import Control.Exception
import Data.Array
import System.Environment
import Control.Monad
import GHC.Conc
import Data.IORef

main = do
  r <- newIORef 0
  rec
    t1 <- mask $ \restore -> forkIO (thread restore r t2)
    t2 <- mask $ \restore -> forkIO (thread restore r t1)
  threadDelay 1000000
  readIORef r >>= print . (/= 0)

thread restore r t = run
  where
    run = (restore $ forever $ do killThread t
                                  i <- atomicModifyIORef r (\i -> (i + 1, i))
                                  evaluate i)
             `catch` \(e::SomeExceptionWithLocation) -> run
