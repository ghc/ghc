{-# LANGUAGE ScopedTypeVariables #-}
-- 'registerFd_' must not conclude from the callback table alone that a
-- descriptor is already registered with the polling backend (#27821).  What
-- is asked here is only that the wait finish, not that the close be made to
-- work.
module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad (void)
import GHC.Conc (BlockReason (..), ThreadStatus (..), forkOn, threadStatus)
import System.IO
import qualified System.Posix.IO as P

rounds :: Int
rounds = 3

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    verdicts <- mapM (const oneRound) [1 .. rounds]
    putStrLn (if and verdicts then "completed" else "BLOCKED")

oneRound :: IO Bool
oneRound = do
    (r, w) <- P.createPipe
    -- Parked and left parked: that is what keeps the registration on record.
    t <- forkOn 0 $ ignoring (threadWaitRead r)
    waitUntilParked t
    ignoring (P.closeFd r)            -- not closeFdWith: that is the point
    ignoring (P.closeFd w)

    -- The numbers just freed come straight back.
    (r', w') <- P.createPipe
    _ <- P.fdWrite w' "x"               -- readable, so this cannot wait for data
    ok <- finishes 2000000 (threadWaitRead r')
    ignoring (P.closeFd r')
    ignoring (P.closeFd w')
    return ok

-- Wait until the thread is really blocked in 'threadWait', so that the close
-- below happens while a registration is on record.  A plain delay instead
-- makes the test miss the bug about half the time.
waitUntilParked :: ThreadId -> IO ()
waitUntilParked t = go (2000 :: Int)
  where
    go 0 = return ()
    go n = do
      st <- threadStatus t
      case st of
        ThreadBlocked BlockedOnMVar -> return ()
        ThreadFinished              -> return ()
        _                           -> yield >> go (n - 1)

-- Did the action finish at all?  Returning and throwing both count; only
-- waiting for ever does not.  The worker is not killed: with the bug it is
-- stuck in 'takeMVar' with nothing to wake it.
finishes :: Int -> IO () -> IO Bool
finishes us act = do
    res <- newEmptyMVar
    -- One capability: it is one 'EventManager''s callback table that drifts.
    _ <- forkOn 0 $ do
           _ <- try act :: IO (Either SomeException ())
           void (tryPutMVar res True)
    _ <- forkOn 1 $ threadDelay us >> void (tryPutMVar res False)
    takeMVar res

ignoring :: IO a -> IO ()
ignoring act = void act `catch` \(_ :: SomeException) -> return ()
