-- $Id: message-ghc-2.code,v 1.3 2005/09/17 04:36:26 bfulgham Exp $
-- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org/
-- Contributed by Einar Karttunen
-- Modified by Simon Marlow

-- This is the shootout "cheap concurrency" benchmark, modified
-- slightly.  Modification noted below (***) to add more concurrency
-- and make a speedup on multiple processors available.

-- Creates 500 threads arranged in a sequence where each takes a value
-- from the left, adds 1, and passes it to the right (via MVars).
-- N more threads pump zeros in at the left.  A sub-thread
-- takes N values from the right and sums them.
-- 

import Control.Concurrent
import Control.Monad
import System.Environment

thread :: MVar Int -> MVar Int -> IO ()
thread inp out = do x <- takeMVar inp; putMVar out $! x+1; thread inp out

spawn cur n = do next <- newEmptyMVar
                 forkIO $ thread cur next
                 return next

main = do n <- getArgs >>= readIO.head
          s <- newEmptyMVar
          e <- foldM spawn s [1..500]
          f <- newEmptyMVar
          forkIO $ replicateM n (takeMVar e) >>= putMVar f . sum
          replicateM n (forkIO $ putMVar s 0)
-- ***    replicateM n (putMVar s 0)
          takeMVar f

-- vim: ts=4 ft=haskell
