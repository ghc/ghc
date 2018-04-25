-- $Id: message-ghc-2.code,v 1.3 2005/09/17 04:36:26 bfulgham Exp $
-- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org/
-- Contributed by Einar Karttunen
-- Modified by Simon Marlow

-- A Modification of threads003, using explicit assigning of threads to CPUs
-- (assumes 2 CPUs).  This version can go faster with -N2 than -N1.
-- 
-- NB. don't forget to run it with +RTS -qm, to disable automatic migration.

import Control.Concurrent
import Control.Monad
import System.Environment
import GHC.Conc (forkOnIO)

thread :: MVar Int -> MVar Int -> IO ()
thread inp out = do x <- takeMVar inp; putMVar out $! x+1; thread inp out

spawn cur n = do next <- newEmptyMVar
                 forkOnIO (if (n <= 1000) then 0 else 1) $ thread cur next
                 return next

main = do n <- getArgs >>= readIO.head
          s <- newEmptyMVar
          e <- foldM spawn s [1..2000]
          f <- newEmptyMVar
          forkOnIO 1 $ replicateM n (takeMVar e) >>= putMVar f . sum
          replicateM n (putMVar s 0)
          takeMVar f

-- vim: ts=4 ft=haskell
