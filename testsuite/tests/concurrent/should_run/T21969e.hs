{-# LANGUAGE ScopedTypeVariables #-}
-- An error reported by the polling backend for one file descriptor must not
-- take down the IO manager of the whole capability.
--
-- After dispatching an event, 'selectCallbacks' re-arms the fd for the
-- registrations that were not triggered.  When that backend call threw -- for
-- example EBADF once the fd had been closed without going through
-- 'closeFdWith' -- the exception travelled out of 'onFdEvent', out of the
-- callback given to 'I.poll' and out of 'step', where 'loop' turned it into
-- 'cleanup mgr'.  The 'EventManager' of that capability was shut down and
-- every thread using it was left blocked forever.
--
-- The IO manager must swallow whatever the backend reports and keep going, so
-- nothing may reach stderr: before the fix this printed the uncaught backend
-- exception and then 'ioManagerDie: write: Bad file descriptor' as the runtime
-- tried to talk to the manager it had just lost.
--
-- Reaching the re-arm
--
-- Both parts below park one thread in 'threadWaitRead' and another in
-- 'threadWaitWrite' on the same socket, whose send buffer has been filled so
-- that it is not writable.  Waking only the reader leaves the writer's
-- registration behind, which is what makes 'selectCallbacks' call the backend
-- at all: with a single reader the saved events are empty, and where the
-- backend has one-shot registrations -- epoll does -- the re-arm is skipped
-- and the bug is never reached.
--
-- Two ways in
--
-- 'dupRound' is deterministic but only bites on backends that keep watching a
-- descriptor whose number has been closed.  epoll does: it drops an entry only
-- once every descriptor for that open file description is gone, so the 'dup'
-- keeps it alive and the event still arrives naming a number that 'epoll_ctl'
-- will now reject.  kqueue drops its knote when the descriptor itself closes,
-- so no event arrives and the round is inert.
--
-- 'raceRound' covers those backends instead, by closing the socket while the
-- manager is waking up for it.  One round in a few dozen wins the race, which
-- is why there are many.
--
-- Keeping the race alive
--
-- Each round kills its waiters once the socket is gone.  That is not
-- tidiness: it puts the registrations back out of the callback table.  Left
-- in, those stale entries pile up on the handful of fd numbers the rounds keep
-- reusing, and 'registerFd_' then decides the fd is already armed and never
-- calls the backend, so no event fires and no re-arm is attempted.  Without
-- the kill the race dries up after a few dozen rounds.
--
-- Note that the test deliberately does not check that the file descriptors
-- involved still work afterwards.  Closing a registered fd without
-- 'closeFdWith' leaves the callback table out of step with the backend, and
-- fd numbers get reused; that is a documented hazard of doing so and not what
-- this test is about.
module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad (forM_, void)
import Foreign.C.Types
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr
import Foreign.Storable (peekElemOff)
import System.IO
import qualified System.Posix.IO as P
import System.Posix.Types (Fd (..), CSsize (..))

foreign import ccall unsafe "socketpair"
    c_socketpair :: CInt -> CInt -> CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "write"
    c_write :: CInt -> Ptr a -> CSize -> IO CSsize
foreign import ccall unsafe "close"
    c_close :: CInt -> IO CInt
foreign import ccall unsafe "dup"
    c_dup :: CInt -> IO CInt

-- AF_UNIX and SOCK_STREAM are both 1 on Linux and on the BSDs.
afUnix, sockStream :: CInt
afUnix = 1
sockStream = 1

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    forM_ [1 .. 20 :: Int]   $ \_ -> dupRound    -- See "Two ways in" above
    forM_ [1 .. 2000 :: Int] $ \_ -> raceRound
    -- Give the IO managers a moment to trip over the descriptors just closed.
    threadDelay 500000
    putStrLn "done"

-- Close the descriptor but keep the open file description, so that the backend
-- is still watching a number it can no longer accept.
dupRound :: IO ()
dupRound = do
    (a, b) <- socketPair
    keep <- c_dup (unFd a)
    (rt, wt) <- park a
    closeRaw a
    poke1 b                             -- the event names the closed number
    threadDelay 20000
    killThread rt >> killThread wt      -- See "Keeping the race alive" above
    void (c_close keep)
    closeRaw b

-- Close the descriptor just as the manager wakes up for it.
raceRound :: IO ()
raceRound = do
    (a, b) <- socketPair
    (rt, wt) <- park a
    poke1 b
    yield                               -- ... let the manager get that far ...
    closeRaw a                          -- ... while a is going away
    killThread rt >> killThread wt
    closeRaw b

-- Park a reader and a writer on the socket.  See "Reaching the re-arm" above.
park :: Fd -> IO (ThreadId, ThreadId)
park a = do
    P.setFdOption a P.NonBlockingRead True
    fillSendBuffer a
    rt <- forkIO $ ignoring (threadWaitRead a)
    wt <- forkIO $ ignoring (threadWaitWrite a)
    threadDelay 2000                    -- let both registrations land
    return (rt, wt)

socketPair :: IO (Fd, Fd)
socketPair = allocaArray 2 $ \sv -> do
    r <- c_socketpair afUnix sockStream 0 sv
    if r /= 0
      then ioError (userError "socketpair failed")
      else do x <- peekElemOff sv 0
              y <- peekElemOff sv 1
              return (Fd x, Fd y)

-- Write until the socket stops accepting data, so a writer parked on it really
-- does have to wait.
fillSendBuffer :: Fd -> IO ()
fillSendBuffer (Fd fd) = allocaBytes chunk $ \p -> go p (64 :: Int)
  where
    chunk = 65536
    go _ 0 = return ()
    go p n = do
      written <- c_write fd p (fromIntegral chunk)
      if written < 0 then return () else go p (n - 1)

poke1 :: Fd -> IO ()
poke1 (Fd fd) = allocaBytes 1 $ \p -> void (c_write fd p 1)

closeRaw :: Fd -> IO ()
closeRaw = void . c_close . unFd

unFd :: Fd -> CInt
unFd (Fd x) = x

ignoring :: IO a -> IO ()
ignoring act = void act `catch` \(_ :: SomeException) -> return ()
