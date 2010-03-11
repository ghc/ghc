{-# LANGUAGE DoRec, ScopedTypeVariables #-}
import Control.Concurrent
import Control.Exception
import Control.Monad
import Prelude hiding (catch)

main = do
  m <- newMVar 1
  t1 <- forkIO $ thread m
  t2 <- forkIO $ forever $ killThread t1
  threadDelay 1000000
  takeMVar m

thread m = run
  where 
    run = (unblock $ forever $ modifyMVar_ m $ \v -> if v `mod` 2 == 1 then return (v*2) else return (v-1))
             `catch` \(e::SomeException) -> run
