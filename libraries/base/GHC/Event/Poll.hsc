{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UnboxedTuples #-}

#include "EventConfig.h"

module GHC.Event.Poll
    (
#if defined(HAVE_POLL_H)
      BackendState
    , new
    , poll
    , modifyFd
    , modifyFdOnce
    , delete
#endif
    ) where

#if defined(HAVE_POLL_H)

#include <poll.h>

import Control.Concurrent.MVar (MVar, newMVar, swapMVar)
import Data.Bits (Bits, FiniteBits, (.|.), (.&.))
import Data.Word
import Foreign.C.Types (CInt(..), CShort(..))
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import GHC.Base
import GHC.Conc.Sync (withMVar)
import GHC.Enum (maxBound)
import GHC.Num (Num(..))
import GHC.Real (fromIntegral, div)
import GHC.Show (Show)
import System.Posix.Types (Fd(..))
import GHC.Primitive.Monad (Prim(..))

import qualified GHC.Primitive.Monad as P
import qualified GHC.Event.Array as A
import qualified GHC.Event.Internal as E

data BackendState = BackendState {
      pollChanges :: {-# UNPACK #-} !(MVar (A.Array PollFd))
    , pollFd      :: {-# UNPACK #-} !(A.Array PollFd)
    }

new :: IO BackendState
new = liftA2 BackendState (newMVar =<< A.empty) A.empty

delete :: BackendState -> IO ()
delete _ = pure ()

modifyFd :: BackendState -> Fd -> E.Event -> E.Event -> IO Bool
modifyFd p fd oevt nevt =
  withMVar (pollChanges p) $ \ary -> do
    A.snoc ary $ PollFd fd (fromEvent nevt) (fromEvent oevt)
    return True

modifyFdOnce :: BackendState -> Fd -> E.Event -> IO Bool
modifyFdOnce = errorWithoutStackTrace "modifyFdOnce not supported in Poll backend"

reworkFd :: BackendState -> PollFd -> IO ()
reworkFd p (PollFd fd npevt opevt) = do
  let ary = pollFd p
  if opevt == 0
    then A.snoc ary $ PollFd fd npevt 0
    else do
      found <- A.findIndex ((== fd) . pfdFd) ary
      case found of
        Nothing        -> errorWithoutStackTrace "reworkFd: event not found"
        Just (i,_)
          | npevt /= 0 -> A.unsafeWrite ary i $ PollFd fd npevt 0
          | otherwise  -> A.removeAt ary i

poll :: BackendState
     -> Maybe E.Timeout
     -> (Fd -> E.Event -> IO ())
     -> IO Int
poll p mtout f = do
  let a = pollFd p
  mods <- swapMVar (pollChanges p) =<< A.empty
  A.forM_ mods (reworkFd p)
  n <- A.useAsPtr a $ \ptr len ->
    E.throwErrnoIfMinus1NoRetry "c_poll" $
    case mtout of
      Just tout ->
        c_pollLoop ptr (fromIntegral len) (fromTimeout tout)
      Nothing   ->
        c_poll_unsafe ptr (fromIntegral len) 0
  when (n /= 0) $ do
    A.loop a 0 $ \i e -> do
      let r = pfdRevents e
      if r /= 0
        then do f (pfdFd e) (toEvent r)
                let i' = i + 1
                return (i', i' == n)
        else return (i, True)
  return (fromIntegral n)
  where
    -- The poll timeout is specified as an Int, but c_poll takes a CInt. These
    -- can't be safely coerced as on many systems (e.g. x86_64) CInt has a
    -- maxBound of (2^32 - 1), even though Int may have a significantly higher
    -- bound.
    --
    -- This function deals with timeouts greater than maxBound :: CInt, by
    -- looping until c_poll returns a non-zero value (0 indicates timeout
    -- expired) OR the full timeout has passed.
    c_pollLoop :: Ptr PollFd -> (#type nfds_t) -> Int -> IO CInt
    c_pollLoop ptr len tout
        | isShortTimeout = c_poll ptr len (fromIntegral tout)
        | otherwise = do
            result <- c_poll ptr len (fromIntegral maxPollTimeout)
            if result == 0
               then c_pollLoop ptr len (fromIntegral (tout - maxPollTimeout))
               else return result
        where
          -- maxPollTimeout is smaller than 0 IFF Int is smaller than CInt.
          -- This means any possible Int input to poll can be safely directly
          -- converted to CInt.
          isShortTimeout = tout <= maxPollTimeout || maxPollTimeout < 0

    -- We need to account for 3 cases:
    --     1. Int and CInt are of equal size.
    --     2. Int is larger than CInt
    --     3. Int is smaller than CInt
    --
    -- In case 1, the value of maxPollTimeout will be the maxBound of Int.
    --
    -- In case 2, the value of maxPollTimeout will be the maxBound of CInt,
    -- which is the largest value accepted by c_poll. This will result in
    -- c_pollLoop recursing if the provided timeout is larger.
    --
    -- In case 3, "fromIntegral (maxBound :: CInt) :: Int" will result in a
    -- negative Int. This will cause isShortTimeout to be true and result in
    -- the timeout being directly converted to a CInt.
    maxPollTimeout :: Int
    maxPollTimeout = fromIntegral (maxBound :: CInt)

fromTimeout :: E.Timeout -> Int
fromTimeout E.Forever     = -1
fromTimeout (E.Timeout s) = fromIntegral $ s `divRoundUp` 1000000
  where
    divRoundUp num denom = (num + denom - 1) `div` denom

data PollFd = PollFd {
      pfdFd      :: {-# UNPACK #-} !Fd
    , pfdEvents  :: {-# UNPACK #-} !Event
    , pfdRevents :: {-# UNPACK #-} !Event
    } deriving Show -- ^ @since 4.4.0.0

newtype Event = Event CShort
    deriving stock
      ( Eq         -- ^ @since 4.4.0.0
      , Show       -- ^ @since 4.4.0.0
      )
    deriving newtype
      ( Num        -- ^ @since 4.4.0.0
      , Storable   -- ^ @since 4.4.0.0
      , Bits       -- ^ @since 4.4.0.0
      , FiniteBits -- ^ @since 4.7.0.0
      , Prim       -- ^ @since 4.14.0.0
      )

-- We have to duplicate the whole enum like this in order for the
-- hsc2hs cross-compilation mode to work
#if defined(POLLRDHUP)
#{enum Event, Event
 , pollIn    = POLLIN
 , pollOut   = POLLOUT
 , pollRdHup = POLLRDHUP
 , pollErr   = POLLERR
 , pollHup   = POLLHUP
 }
#else
#{enum Event, Event
 , pollIn    = POLLIN
 , pollOut   = POLLOUT
 , pollErr   = POLLERR
 , pollHup   = POLLHUP
 }
#endif

fromEvent :: E.Event -> Event
fromEvent e = remap E.evtRead  pollIn .|.
              remap E.evtWrite pollOut
  where remap evt to
            | e `E.eventIs` evt = to
            | otherwise         = 0

toEvent :: Event -> E.Event
toEvent e = remap (pollIn .|. pollErr .|. pollHup)  E.evtRead `mappend`
            remap (pollOut .|. pollErr .|. pollHup) E.evtWrite
  where remap evt to
            | e .&. evt /= 0 = to
            | otherwise      = mempty

unI :: Int -> Int##
unI (I## i) = i

-- | @since 4.14.0.0
instance Prim PollFd where
  sizeOf## _ = unI #{size struct pollfd}
  alignment## _ = alignment## (undefined :: CInt)
  indexByteArray## arr i = PollFd
    (#{indexByteArrayHash struct pollfd, fd} arr i)
    (#{indexByteArrayHash struct pollfd, events} arr i)
    (#{indexByteArrayHash struct pollfd, revents} arr i)
  writeByteArray## arr i p s0 = case #{writeByteArrayHash struct pollfd, fd} arr i (pfdFd p) s0 of
    s1 -> case #{writeByteArrayHash struct pollfd, events} arr i (pfdEvents p) s1 of
      s2 -> #{writeByteArrayHash struct pollfd, revents} arr i (pfdRevents p) s2
  readByteArray## arr i s0 = case #{readByteArrayHash struct pollfd, fd} arr i s0 of
    (## s1, fdVal ##) -> case #{readByteArrayHash struct pollfd, events} arr i s1 of
      (## s2, eventsVal ##) -> case #{readByteArrayHash struct pollfd, revents} arr i s2 of
        (## s3, reventsVal ##) -> (## s3, PollFd fdVal eventsVal reventsVal ##)
  setByteArray## = P.defaultSetByteArray##
  indexOffAddr## arr i = PollFd
    (#{indexOffAddrHash struct pollfd, fd} arr i)
    (#{indexOffAddrHash struct pollfd, events} arr i)
    (#{indexOffAddrHash struct pollfd, revents} arr i)
  writeOffAddr## arr i p s0 = case #{writeOffAddrHash struct pollfd, fd} arr i (pfdFd p) s0 of
    s1 -> case #{writeOffAddrHash struct pollfd, events} arr i (pfdEvents p) s1 of
      s2 -> #{writeOffAddrHash struct pollfd, revents} arr i (pfdRevents p) s2
  readOffAddr## arr i s0 = case #{readOffAddrHash struct pollfd, fd} arr i s0 of
    (## s1, fdVal ##) -> case #{readOffAddrHash struct pollfd, events} arr i s1 of
      (## s2, eventsVal ##) -> case #{readOffAddrHash struct pollfd, revents} arr i s2 of
        (## s3, reventsVal ##) -> (## s3, PollFd fdVal eventsVal reventsVal ##)
  setOffAddr## = P.defaultSetOffAddr##
  

-- | @since 4.3.1.0
instance Storable PollFd where
    sizeOf _    = #size struct pollfd
    alignment _ = alignment (undefined :: CInt)

    peek ptr = do
      fd <- #{peek struct pollfd, fd} ptr
      events <- #{peek struct pollfd, events} ptr
      revents <- #{peek struct pollfd, revents} ptr
      let !pollFd' = PollFd fd events revents
      return pollFd'

    poke ptr p = do
      #{poke struct pollfd, fd} ptr (pfdFd p)
      #{poke struct pollfd, events} ptr (pfdEvents p)
      #{poke struct pollfd, revents} ptr (pfdRevents p)

foreign import ccall safe "poll.h poll"
    c_poll :: Ptr PollFd -> (#type nfds_t) -> CInt -> IO CInt

foreign import ccall unsafe "poll.h poll"
    c_poll_unsafe :: Ptr PollFd -> (#type nfds_t) -> CInt -> IO CInt
#endif /* defined(HAVE_POLL_H) */
