{-# LANGUAGE ScopedTypeVariables #-}
-- 'closeFdWith' must not run the fd's callbacks inside 'uninterruptibleMask_'.
--
-- The callbacks are arbitrary code supplied through 'GHC.Event.registerFd'.
-- Running them while asynchronous exceptions are uninterruptibly masked means
-- that a callback which blocks makes the thread calling 'closeFdWith' -- in
-- practice the thread calling 'Network.Socket.close' or 'hClose' --
-- permanently unkillable.
module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad (void)
import GHC.Conc.IO (closeFdWith)
import GHC.Event hiding (closeFd)
import System.IO
import System.Posix.IO
import System.Posix.Types (Fd)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    -- 'blocker' is deliberately reachable from main until the very end.  Were
    -- it only reachable from the blocked thread, the RTS deadlock detector
    -- would resurrect that thread with BlockedIndefinitelyOnMVar -- which
    -- forkIO's default handler discards silently -- and the test would pass
    -- without exercising anything.
    blocker <- newEmptyMVar :: IO (MVar ())
    (r, w) <- createPipe
    Just mgr <- getSystemEventManager
    _ <- registerFd mgr (\_ _ -> takeMVar blocker) r evtRead OneShot

    victim <- forkIO $ closeFdWith closeFd r
    -- See Note [Waiting for a registration to land] in T21969a.hs
    threadDelay 200000

    killed <- newEmptyMVar
    _ <- forkIO $ killThread victim >> void (tryPutMVar killed True)
    timer <- forkIO $ threadDelay 5000000 >> void (tryPutMVar killed False)
    ok <- takeMVar killed
    killThread timer
    putStrLn ("killThread on the closing thread: " ++ if ok then "ok" else "STUCK")

    closeFd w
    void (tryTakeMVar blocker)
