{-# LANGUAGE ScopedTypeVariables #-}
-- An error reported by the polling backend while an fd is being closed must
-- not abandon the close.
--
-- 'closeFd_' removes the registrations from the callback table and only then
-- tells the backend to forget the fd.  When that backend call threw, the
-- exception aborted 'closeFdWith' before it had closed the fd, and the
-- callbacks -- which 'closeFd_' returns rather than runs -- were dropped on the
-- floor, so every thread waiting on the fd was left blocked forever with its
-- registration already gone.
--
-- The backend error is forced here by closing the fd without going through
-- 'closeFdWith'.
module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad (void)
import Data.IORef
import GHC.Conc.IO (closeFdWith)
import System.IO
import System.Posix.IO
import System.Posix.Types (Fd)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    (r, w) <- createPipe
    woken <- newEmptyMVar
    _ <- forkIO $ do
        res <- try (threadWaitRead r) :: IO (Either SomeException ())
        putMVar woken (either (const "exception") (const "returned") res)
    -- See Note [Waiting for a registration to land] in T21969a.hs
    threadDelay 200000

    closeFd r
    closeRan <- newIORef False
    res <- try (closeFdWith (\_ -> writeIORef closeRan True) r)
             :: IO (Either SomeException ())
    putStrLn ("closeFdWith threw: " ++ either (const "yes") (const "no") res)
    readIORef closeRan >>= \b -> putStrLn ("close action ran: " ++ show b)

    ok <- completesWithin 5000000 (void (readMVar woken))
    putStrLn ("waiter woken: " ++ show ok)
    closeFd w

completesWithin :: Int -> IO () -> IO Bool
completesWithin us act = do
    res <- newEmptyMVar
    _ <- forkIO $ (act `catch` \(_ :: SomeException) -> return ())
                    `finally` void (tryPutMVar res True)
    timer <- forkIO $ threadDelay us >> void (tryPutMVar res False)
    ok <- takeMVar res
    killThread timer
    return ok
