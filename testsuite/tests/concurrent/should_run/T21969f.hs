{-# LANGUAGE ScopedTypeVariables #-}
-- The 'threadWaitReadSTM' action initiates an I/O operation and returns an STM
-- action to wait on completion, and an IO action to cancel the I/O.  The use
-- of the cancellation action is inherently racy: there is a race between the
-- I/O completing and the I/O being cancelled.  It is vital therefore for the
-- cancellation to be safe when it loses the race.  It should have no effect in
-- this case.
--
-- Cancelling twice is the same question asked twice, so it is checked here as
-- well: the second one has nothing left to cancel, which is the state the
-- first one was trying to reach.
--
-- Issue #21969 describes a bug in the MIO I/O manager where the cancellation
-- action would (incorrectly) throw exceptions if it lost the race.  It was
--
--     return (waitAction, unregisterFd_ mgr reg >> return ())
--
-- and 'unregisterFd_' talks to the polling backend, which throws on errors
-- such as EBADF and ENOENT.  Thrown from a cleanup -- which is where a
-- cancellation is usually run -- that exception replaces whatever was being
-- propagated, exactly as it did for 'threadWait' before this series (see
-- T21969d).
--
-- The race is lost here by closing the fd behind the IO manager's back, which
-- is what leaves the backend without the registration that the callback table
-- still records.
module Main (main) where

import Control.Concurrent (threadWaitReadSTM)
import Control.Exception
import System.IO
import System.Posix.IO

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    (r, w) <- createPipe
    (_wait, unregister) <- threadWaitReadSTM r
    closeFd r                 -- not closeFdWith: that is the point
    closeFd w
    res <- try unregister
    putStrLn $ case res of
      Right ()                  -> "deregistered"
      Left (e :: SomeException) -> "threw: " ++ show e

    -- And again: deregistering is idempotent, so a second one has nothing
    -- left to do and must say so the same way.
    res2 <- try unregister
    putStrLn $ case res2 of
      Right ()                  -> "deregistered again"
      Left (e :: SomeException) -> "threw again: " ++ show e
