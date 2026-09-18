{-# LANGUAGE ScopedTypeVariables #-}
-- An asynchronous exception delivered to a thread blocked in 'threadWait' must
-- reach that thread unchanged.
--
-- 'threadWait' cleans up with
--
--     evt' <- takeMVar m `onException` unregisterFd_ mgr reg
--
-- and 'unregisterFd_' talks to the polling backend, which throws on errors such
-- as EBADF or ENOENT.  That cleanup runs inside the handler for the
-- asynchronous exception, so its own exception replaces the one being
-- delivered: 'killThread' returns, but the thread dies from an unrelated
-- IOError and handlers for 'AsyncException' never run.  'timeout' is affected
-- the same way, since it recognises its own exception by value.
--
-- The backend error is forced here by closing the fd behind the IO manager's
-- back.
module Main (main) where

import Control.Concurrent
import Control.Exception
import System.IO
import System.Posix.IO
import System.Posix.Types (Fd)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    (r, w) <- createPipe
    got <- newEmptyMVar
    tid <- forkIO $ do
        res <- (threadWaitRead r >> return "returned normally")
                 `catch` (\(e :: AsyncException) -> return (show e))
                 `catch` (\(e :: SomeException)  -> return ("not an async exception: " ++ show e))
        putMVar got res
    -- See Note [Waiting for a registration to land] in T21969a.hs
    threadDelay 200000

    closeFd r
    killThread tid
    takeMVar got >>= putStrLn
    closeFd w
