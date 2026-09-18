{-# LANGUAGE ScopedTypeVariables #-}
-- An exception escaping 'closeFdWith' must not leave the IO manager's callback
-- tables locked.
--
-- 'closeFdWith' takes every 'EventManager''s 'callbackTableVar' inside
-- 'uninterruptibleMask_' and only puts them back on the paths that do not
-- throw.  A throw from 'closeFd_' -- for example EBADF or ENOENT from
-- epoll_ctl/kevent when the backend no longer knows the fd -- therefore leaves
-- those MVars empty forever.  From then on every 'threadWaitRead' and
-- 'closeFdWith' on an fd in the same callback-table bucket blocks, and since
-- 'closeFdWith' blocks *inside* 'uninterruptibleMask_' such a thread can no
-- longer be killed at all.
--
-- The backend error is forced here by closing the fd without going through
-- 'closeFdWith'.  How the error arises does not matter; what matters is that
-- 'closeFdWith' stays exception safe when it does.
-- Note [Waiting for a registration to land]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The 'threadDelay' before the descriptor is closed is waiting for something
-- to have happened, not giving the scheduler a turn: the forked thread has to
-- reach 'threadWaitRead' and its registration has to reach the polling
-- backend.  The close is what the test is about, and a close that overtakes
-- the registration leaves nothing to find.
--
-- 'yield' is not enough for that.  It hands over once; it does not wait for an
-- IO manager on another capability to have taken the registration.  Measured
-- on aarch64 macOS (kqueue) with -N2, twenty runs of each of these four tests
-- against a tree with the bug still in -- where every run must fail:
--
--     test      threadDelay 200000      yield
--     T21969a   20/20 fail              2/20 fail
--     T21969b   20/20 fail             18/20 fail
--     T21969c   20/20 fail              2/20 fail
--     T21969d   20/20 fail             16/20 fail, 4 hang
--
-- Two of them stop catching the bug nine times out of ten.
--
-- The counts in the yield column move from run to run, as a timing
-- measurement does: a second set of twenty gave 2, 20, 0, and 11 with seven
-- hangs -- T21969c caught nothing at all that time.  What does not move is
-- the column beside it.
--
-- This is also why the wait in T21969e's 'raceRound' is a 'yield' and the
-- ones around it are not: that one is there to *not* wait, so that the close
-- lands while the manager is waking up.  See the comments there.
--
module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad (void)
import GHC.Conc.IO (closeFdWith)
import System.IO
import System.Posix.IO
import System.Posix.Types (Fd)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    (r, w) <- createPipe

    -- Park a thread in threadWaitRead so that the fd really is registered.
    _ <- forkIO $ threadWaitRead r `catch` \(_ :: SomeException) -> return ()
    threadDelay 200000  -- See Note [Waiting for a registration to land]

    closeFd r
    _ <- try (closeFdWith (\_ -> return ()) r) :: IO (Either SomeException ())
    closeFd w

    -- The IO manager must still be usable.  createPipe hands the same fd
    -- numbers back, so this exercises the same callback-table bucket.
    (r', w') <- createPipe
    _ <- fdWrite w' "x"          -- r' is readable, so threadWaitRead returns at once
    ok1 <- completesWithin 5000000 (threadWaitRead r')
    putStrLn ("threadWaitRead: " ++ verdict ok1)
    ok2 <- completesWithin 5000000 (closeFdWith closeFd r')
    putStrLn ("closeFdWith: " ++ verdict ok2)
    closeFd w'

verdict :: Bool -> String
verdict True  = "ok"
verdict False = "STUCK"

-- Note that the worker is never killed: when the bug is present it is stuck
-- inside uninterruptibleMask_, so killing it would block us too.
completesWithin :: Int -> IO () -> IO Bool
completesWithin us act = do
    res <- newEmptyMVar
    _ <- forkIO $ (act `catch` \(_ :: SomeException) -> return ())
                    `finally` void (tryPutMVar res True)
    timer <- forkIO $ threadDelay us >> void (tryPutMVar res False)
    ok <- takeMVar res
    killThread timer
    return ok
