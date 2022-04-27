{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Event.IoUring
  (
    new
  , available
  ) where

import qualified GHC.Event.Internal as EI

#include "HsBaseConfig.h"
#if !defined(HAVE_IO_URING)
import GHC.Base

new :: IO EI.Backend
new = errorWithoutStackTrace "IoUring back end not implemented for this platform"

available :: Bool
available = False
{-# INLINE available #-}
#else

import GHC.Base
import GHC.Enum
import qualified GHC.Event.Internal as EI ()
import GHC.Num
import GHC.Real
import GHC.Word (Word64)
import System.Linux.IO.URing
import System.Linux.IO.URing.Cqe
import System.Linux.IO.URing.PollEvent as PollEvent
import System.Posix.Types (Fd(..))
import Control.Concurrent.MVar
import Data.Bits ((.|.), (.&.), shiftR, shiftL)
import Data.Functor
import Data.Int
import Foreign.Marshal.Utils (with)

data UringState = UringState { ringLock :: MVar URing }

-- public interface:
available :: Bool
available = True

new :: IO EI.Backend
new = do
  ring <- newURing 512 -- why 512? Ideally we'd allow people to override this with a compilation flag and/or envvar
  lock <- newMVar ring
  let !be = EI.backend poll modifyFd modifyFdOnce delete (UringState lock)
  return be

-- The runtime will only ever request infinite or zero length timeout AFAICT. Only if this is used as a timer manager
-- backend will the timeout be used.
poll :: UringState -> Maybe EI.Timeout -> (Fd -> EI.Event -> IO ()) -> IO Int
poll state maybeTimeout callback = do
  cqesProcessed <- popAndCallbackAllCQEs state callback 0
  case cqesProcessed of
    0 -> case maybeTimeout of
      Nothing -> return 0
      Just EI.Forever -> do
        -- blocking wait in "safe" call for at least 1 event
        void $ EI.throwErrnoIfMinus1NoRetry "io_uring_enter submitAndWait" $ readMVar (ringLock state) >>= \ring -> submitAndWait ring (0 :: Int) (1 :: Int)
        popAndCallbackAllCQEs state callback 0
      -- As far as I can tell, the event manager will only ever request infinite or zero length timeouts, but
      -- the data constructor for finite timeouts exists so we might as well implement it.
      Just (EI.Timeout nanoseconds) -> do
        -- The io_uring_enter call does not support a timeout, so we fake it by submitting a timeout request without callback
        -- first and then wait for at least one request to complete. Maybe we get lucky and it's a "real" CQ event; then that gets
        -- returned and eventually the timeout will return without consequence. If nothing else happens, eventually the timeout will
        -- expire and this call will return.
        with (makeTimespec nanoseconds) $ \tsPtr -> withMVar (ringLock state) $ \ring -> do
          maybeSqeIndexTuple <- postSqe ring (timeout tsPtr (constructUserdata 0 (EI.evtNothing) False False))
          case maybeSqeIndexTuple of
            Nothing -> return () -- SQE ring is full. Should never happen as we always submit immediately whenever we push an event onto the ring
            Just (sqeIdx, _) -> do
              void $ EI.throwErrnoIfMinus1NoRetry "io_uring_enter submit" $ submit ring 1 Nothing
              freeSqe ring sqeIdx
        void $ EI.throwErrnoIfMinus1NoRetry "io_uring_enter submitAndWait" $ readMVar (ringLock state) >>= \ring -> submitAndWait ring (0 :: Int) (1 :: Int)
        popAndCallbackAllCQEs state callback 0
    n -> return n -- Blocking is never required in this case

modifyFdOnce :: UringState -> Fd -> EI.Event -> IO Bool
modifyFdOnce state fd evt = registerAndPostPollRequest state fd evt False

modifyFd :: UringState -> Fd -> EI.Event -> EI.Event -> IO Bool
modifyFd state fd oldevts nextevts
  | oldevts == mempty  = registerAndPostPollRequest state fd nextevts True
  | nextevts == mempty = cancelPollRequest state fd oldevts
  | otherwise = do
      void $ cancelPollRequest state fd oldevts
      registerAndPostPollRequest state fd nextevts True

-- Clean up and terminate the backend
-- It's not necessary to manually free the Uring structures in C land, they will be cleaned up by
-- the GC when the Uring is collected.
delete :: UringState -> IO ()
delete _ = return ()

--internal calls:

-- The workhorse of this backend. Pops CQEs off the ringbuffer and issues callbacks to waiting threads
popAndCallbackAllCQEs :: UringState -> (Fd -> EI.Event -> IO ()) -> Int -> IO Int
popAndCallbackAllCQEs state callback !poppedSoFar = do
  maybeCQE <- withMVar (ringLock state) $ \ring -> popCq ring
  case maybeCQE of
    Nothing  -> return poppedSoFar
    Just cqe -> do
      let (fd,originalEvents,isMultishot,hasCallback) = (deconstructUserdata $ cqeUserData cqe)
      when (hasCallback) $ callback fd (convertEventBack $ cqeRes cqe)
      -- if it was a multishot poll, resubmit. This happens with the  for example. If it was an error,
      -- don't resubmit to prevent getting into an infinite loop of errors
      when (isMultishot && cqeRes cqe >= 0) $ void $ registerAndPostPollRequest state fd originalEvents True
      popAndCallbackAllCQEs state callback $ if hasCallback then (poppedSoFar + 1) else poppedSoFar

-- An Event from the event manager is an (Event Int) and can be:
-- evtNothing = Event 0
-- | Data is available to be read.
-- evtRead = Event 1
-- | The file descriptor is ready to accept a write.
-- evtWrite = Event 2
-- | Another thread closed the file descriptor.
-- evtClose = Event 4
-- Events can be combined as binary flags, so eg evtRead <> evtClose would be Event 5
-- as 0x1 .&. 0x4 == 0b001 .&. 0b100 == 0b101 == 0x5

-- Meanwhile, io_uring poll event follow the event numberings from the poll(2) syscall
-- which go as follows:
-- POLLIN: 1
-- POLLPRI: 2
-- POLLOUT: 4
-- POLLERR: 8
-- POLLHUP: 16
-- POLLNVAL: 32
-- POLLRDNORM: 64
-- POLLRDBAND: 128
-- POLLWRNORM: 256
-- POLLWRBAND: 512
-- This means that we need to convert more fancily than just casting between numbers

convertEvent ::  EI.Event -> PollEvent.Event --naming is hard yo
convertEvent e = remap EI.evtRead  PollEvent.pollIn .|.
                 remap EI.evtWrite PollEvent.pollOut
  where remap evt to
            | e `EI.eventIs` evt = to
            | otherwise          = 0

-- The io_uring library wraps Event in a newtype, but the return value from popCqe
-- is just an Int32 since it could be anything. We map pollErr and pollHup into evtRead and evtWrite as well
-- because that is what the manager infrastructure expects. Without it you get weird EBADF errors
convertEventBack :: Int32 -> EI.Event
convertEventBack e
  | e < 0     = EI.evtRead `mappend` EI.evtWrite -- since polling errors get mapped onto everything
  | otherwise = remap (PollEvent.pollIn  .|. PollEvent.pollErr .|. PollEvent.pollHup) EI.evtRead `mappend`
                remap (PollEvent.pollOut .|. PollEvent.pollErr .|. PollEvent.pollHup) EI.evtWrite
  where remap evt to
            | e' .&. evt /= 0 = to
            | otherwise       = mempty
        e' = PollEvent.Event $ fromIntegral e

makeTimespec :: Word64 -> Timespec
makeTimespec ns = Timespec (fromIntegral $ ns `div` 1000000000) (fromIntegral $ ns `mod` 1000000000)

-- will handle the registering and posting of a SQE. Returns a Bool indicating
-- whether posting the SQE was successful
registerAndPostPollRequest :: UringState -> Fd -> EI.Event -> Bool -> IO Bool
registerAndPostPollRequest state fd evt isMultishot = withMVar (ringLock state) $ \ring -> do
  let userdata = constructUserdata fd evt isMultishot True
  maybeSqeIndexTuple <- postSqe ring (pollAdd fd (convertEvent evt) (fromIntegral userdata))
  case maybeSqeIndexTuple of
    Just (sqeIdx, _) -> do
      _ <- EI.throwErrnoIfMinus1NoRetry "io_uring_enter submit" $ submit ring 1 Nothing
      freeSqe ring sqeIdx
      return True
    Nothing -> do
      -- SQE ring is full. Should never happen as we always submit immediately whenever we push an event onto the ring
      -- We need to remove the entries from the callback and cancel maps though, otherwise they'll
      -- be in there forever.
      return False

cancelPollRequest :: UringState -> Fd -> EI.Event -> IO Bool
cancelPollRequest state fd evt = withMVar (ringLock state) $ \ring -> do
  -- we can't know if it was a multishot or not since that information only exists in the callback table of the
  -- manager itself. We nuke both just in case, io_uring won't mind
  maybeSqeIndexTuple1 <- postSqe ring (pollRemove (constructUserdata fd evt False True) (constructUserdata 0 (EI.evtNothing) False False))
  maybeSqeIndexTuple2 <- postSqe ring (pollRemove (constructUserdata fd evt True True) (constructUserdata 0 (EI.evtNothing) False False))
  case (maybeSqeIndexTuple1, maybeSqeIndexTuple2) of
    (Just (sqeIdx1, _), Just (sqeIdx2, _)) -> do
      void $ EI.throwErrnoIfMinus1NoRetry "io_uring_enter submit" $ submit ring 2 Nothing
      freeSqe ring sqeIdx1
      freeSqe ring sqeIdx2
      return True
    _ -> return False -- SQE ring is full. Should never happen as we always submit immediately whenever we push an event onto the ring

-- To perform the callbacks, we need to have the Fd and EI.Event values that were originally used
-- in the modifyFd call, as well as if the request is single- or multishot. We use a clever hack
-- for this by bitpacking these values into the mandatory 64-bit "userdata" value of the SQE that
-- is used to connect SQEs with CQEs. A Fd is 32 bits, the flags fit in another three bits and the
-- multishot flag fits in one bit.
-- NOTE 1: we don't strictly need the events in there for the result,
-- since the poll results will also include the events. However, we keep the events so that read-only
-- and write-only poll requests will get different userdata values and won't interfere with each other
-- NOTE 2: In some cases we don't want to trigger a callback, but the entire 32 bit space for Fd can be
-- used according to the posix spec (even 0, which is usually STDIN, can be reused if you manually close
-- STDIN) so we have another 1-bit flag that specifies whether the callback should be issued. SQEs for
-- which no callback should be issued are mostly the ones used for internal purposes like timeouts and
-- cancelation requests.
constructUserdata :: Fd -> EI.Event -> Bool -> Bool -> Word64
constructUserdata fd evt isMultishot hasCallback = hasCb .|. isMs .|. evt' .|. fd'
  where fd'   = fromIntegral fd
        evt'  = shiftL (fromIntegral $ convertEvent evt) 32
        isMs  = shiftL (fromIntegral . fromEnum $ isMultishot) 35
        hasCb = shiftL (fromIntegral . fromEnum $ hasCallback) 36


deconstructUserdata :: Word64 -> (Fd,EI.Event,Bool,Bool)
deconstructUserdata ud = (fd, evt, isMultishot, hasCallback)
  where fd = fromIntegral $ ud .&. 0xFFFF
        evt = convertEventBack . fromIntegral $ (shiftR ud 32) .&. 0x8
        isMultishot = toEnum . fromIntegral $ (shiftR ud 35) .&. 1
        hasCallback = toEnum . fromIntegral $ (shiftR ud 36) .&. 1

#endif /* defined(HAVE_IO_URING) */
