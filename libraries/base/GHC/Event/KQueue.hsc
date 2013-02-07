{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , ForeignFunctionInterface
           , CApiFFI
           , GeneralizedNewtypeDeriving
           , NoImplicitPrelude
           , RecordWildCards
           , BangPatterns
  #-}

module GHC.Event.KQueue
    (
      new
    , available
    ) where

import qualified GHC.Event.Internal as E

#include "EventConfig.h"
#if !defined(HAVE_KQUEUE)
import GHC.Base

new :: IO E.Backend
new = error "KQueue back end not implemented for this platform"

available :: Bool
available = False
{-# INLINE available #-}
#else

import Control.Monad (when, void)
import Data.Bits (Bits(..))
import Data.Maybe (Maybe(..))
import Data.Monoid (Monoid(..))
import Data.Word (Word16, Word32)
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..))
import GHC.Base
import GHC.Enum (toEnum)
import GHC.Err (undefined)
import GHC.Num (Num(..))
import GHC.Real (ceiling, floor, fromIntegral)
import GHC.Show (Show(show))
import GHC.Event.Internal (Timeout(..))
import System.Posix.Internals (c_close)
import System.Posix.Types (Fd(..))
import qualified GHC.Event.Array as A

#if defined(netbsd_HOST_OS)
import Data.Int (Int64)
#endif

#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>

-- Handle brokenness on some BSD variants, notably OS X up to at least
-- 10.6.  If NOTE_EOF isn't available, we have no way to receive a
-- notification from the kernel when we reach EOF on a plain file.
#ifndef NOTE_EOF
# define NOTE_EOF 0
#endif

available :: Bool
available = True
{-# INLINE available #-}

------------------------------------------------------------------------
-- Exported interface

data KQueue = KQueue {
      kqueueFd     :: {-# UNPACK #-} !KQueueFd
    , kqueueEvents :: {-# UNPACK #-} !(A.Array Event)
    }

new :: IO E.Backend
new = do
  kqfd <- kqueue
  events <- A.new 64
  let !be = E.backend poll modifyFd modifyFdOnce delete (KQueue kqfd events)
  return be

delete :: KQueue -> IO ()
delete kq = do
  _ <- c_close . fromKQueueFd . kqueueFd $ kq
  return ()

modifyFd :: KQueue -> Fd -> E.Event -> E.Event -> IO ()
modifyFd kq fd oevt nevt
  | nevt == mempty = do
      let !ev = event fd (toFilter oevt) flagDelete noteEOF
      kqueueControl (kqueueFd kq) ev
  | otherwise      = do
      let !ev = event fd (toFilter nevt) flagAdd noteEOF
      kqueueControl (kqueueFd kq) ev

toFilter :: E.Event -> Filter
toFilter evt
  | evt `E.eventIs` E.evtRead = filterRead
  | otherwise                 = filterWrite

modifyFdOnce :: KQueue -> Fd -> E.Event -> IO ()
modifyFdOnce kq fd evt = do
    let !ev = event fd (toFilter evt) (flagAdd .|. flagOneshot) noteEOF
    kqueueControl (kqueueFd kq) ev

poll :: KQueue
     -> Maybe Timeout
     -> (Fd -> E.Event -> IO ())
     -> IO Int
poll kq mtimeout f = do
    let events = kqueueEvents kq
        fd = kqueueFd kq

    n <- A.unsafeLoad events $ \es cap -> case mtimeout of
      Just timeout -> kqueueWait fd es cap $ fromTimeout timeout
      Nothing      -> kqueueWaitNonBlock fd es cap

    when (n > 0) $ do
        A.forM_ events $ \e -> f (fromIntegral (ident e)) (toEvent (filter e))
        cap <- A.capacity events
        when (n == cap) $ A.ensureCapacity events (2 * cap)
    return n
------------------------------------------------------------------------
-- FFI binding

newtype KQueueFd = KQueueFd {
      fromKQueueFd :: CInt
    } deriving (Eq, Show)

data Event = KEvent {
      ident  :: {-# UNPACK #-} !CUIntPtr
    , filter :: {-# UNPACK #-} !Filter
    , flags  :: {-# UNPACK #-} !Flag
    , fflags :: {-# UNPACK #-} !FFlag
#ifdef netbsd_HOST_OS
    , data_  :: {-# UNPACK #-} !Int64
#else
    , data_  :: {-# UNPACK #-} !CIntPtr
#endif
    , udata  :: {-# UNPACK #-} !(Ptr ())
    } deriving Show

event :: Fd -> Filter -> Flag -> FFlag -> Event
event fd filt flag fflag = KEvent (fromIntegral fd) filt flag fflag 0 nullPtr

instance Storable Event where
    sizeOf _ = #size struct kevent
    alignment _ = alignment (undefined :: CInt)

    peek ptr = do
        ident'  <- #{peek struct kevent, ident} ptr
        filter' <- #{peek struct kevent, filter} ptr
        flags'  <- #{peek struct kevent, flags} ptr
        fflags' <- #{peek struct kevent, fflags} ptr
        data'   <- #{peek struct kevent, data} ptr
        udata'  <- #{peek struct kevent, udata} ptr
        let !ev = KEvent ident' (Filter filter') (Flag flags') fflags' data'
                         udata'
        return ev

    poke ptr ev = do
        #{poke struct kevent, ident} ptr (ident ev)
        #{poke struct kevent, filter} ptr (filter ev)
        #{poke struct kevent, flags} ptr (flags ev)
        #{poke struct kevent, fflags} ptr (fflags ev)
        #{poke struct kevent, data} ptr (data_ ev)
        #{poke struct kevent, udata} ptr (udata ev)

newtype FFlag = FFlag Word32
    deriving (Eq, Show, Storable)

#{enum FFlag, FFlag
 , noteEOF = NOTE_EOF
 }

#if SIZEOF_KEV_FLAGS == 4 /* kevent.flag: uint32_t or uint16_t. */
newtype Flag = Flag Word32
#else
newtype Flag = Flag Word16
#endif
    deriving (Bits, Eq, Num, Show, Storable)

#{enum Flag, Flag
 , flagAdd     = EV_ADD
 , flagDelete  = EV_DELETE
 , flagOneshot = EV_ONESHOT
 }

#if SIZEOF_KEV_FILTER == 4 /*kevent.filter: uint32_t or uint16_t. */
newtype Filter = Filter Word32
#else
newtype Filter = Filter Word16
#endif
    deriving (Bits, Eq, Num, Show, Storable)

#{enum Filter, Filter
 , filterRead   = EVFILT_READ
 , filterWrite  = EVFILT_WRITE
 }

data TimeSpec = TimeSpec {
      tv_sec  :: {-# UNPACK #-} !CTime
    , tv_nsec :: {-# UNPACK #-} !CLong
    }

instance Storable TimeSpec where
    sizeOf _ = #size struct timespec
    alignment _ = alignment (undefined :: CInt)

    peek ptr = do
        tv_sec'  <- #{peek struct timespec, tv_sec} ptr
        tv_nsec' <- #{peek struct timespec, tv_nsec} ptr
        let !ts = TimeSpec tv_sec' tv_nsec'
        return ts

    poke ptr ts = do
        #{poke struct timespec, tv_sec} ptr (tv_sec ts)
        #{poke struct timespec, tv_nsec} ptr (tv_nsec ts)

kqueue :: IO KQueueFd
kqueue = KQueueFd `fmap` throwErrnoIfMinus1 "kqueue" c_kqueue

kqueueControl :: KQueueFd -> Event -> IO ()
kqueueControl kfd ev = void $
    withTimeSpec (TimeSpec 0 0) $ \tp ->
        withEvent ev $ \evp -> kevent False kfd evp 1 nullPtr 0 tp

kqueueWait :: KQueueFd -> Ptr Event -> Int -> TimeSpec -> IO Int
kqueueWait fd es cap tm =
    withTimeSpec tm $ kevent True fd nullPtr 0 es cap

kqueueWaitNonBlock :: KQueueFd -> Ptr Event -> Int -> IO Int
kqueueWaitNonBlock fd es cap =
    withTimeSpec (TimeSpec 0 0) $ kevent False fd nullPtr 0 es cap

-- TODO: We cannot retry on EINTR as the timeout would be wrong.
-- Perhaps we should just return without calling any callbacks.
kevent :: Bool -> KQueueFd -> Ptr Event -> Int -> Ptr Event -> Int -> Ptr TimeSpec
       -> IO Int
kevent safe k chs chlen evs evlen ts
    = fmap fromIntegral $ E.throwErrnoIfMinus1NoRetry "kevent" $
      if safe 
      then c_kevent k chs (fromIntegral chlen) evs (fromIntegral evlen) ts
      else c_kevent_unsafe k chs (fromIntegral chlen) evs (fromIntegral evlen) ts

withEvent :: Event -> (Ptr Event -> IO a) -> IO a
withEvent ev f = alloca $ \ptr -> poke ptr ev >> f ptr

withTimeSpec :: TimeSpec -> (Ptr TimeSpec -> IO a) -> IO a
withTimeSpec ts f
  | tv_sec ts < 0 = f nullPtr
  | otherwise     = alloca $ \ptr -> poke ptr ts >> f ptr

fromTimeout :: Timeout -> TimeSpec
fromTimeout Forever     = TimeSpec (-1) (-1)
fromTimeout (Timeout s) = TimeSpec (toEnum sec) (toEnum nanosec)
  where
    sec :: Int
    sec     = floor s

    nanosec :: Int
    nanosec = ceiling $ (s - fromIntegral sec) * 1000000000

toEvent :: Filter -> E.Event
toEvent (Filter f)
  | f == (#const EVFILT_READ) = E.evtRead
  | f == (#const EVFILT_WRITE) = E.evtWrite
  | otherwise = error $ "toEvent: unknown filter " ++ show f

foreign import ccall unsafe "kqueue"
    c_kqueue :: IO CInt

#if defined(HAVE_KEVENT)
foreign import capi safe "sys/event.h kevent"
    c_kevent :: KQueueFd -> Ptr Event -> CInt -> Ptr Event -> CInt
             -> Ptr TimeSpec -> IO CInt

foreign import ccall unsafe "kevent"
    c_kevent_unsafe :: KQueueFd -> Ptr Event -> CInt -> Ptr Event -> CInt
                    -> Ptr TimeSpec -> IO CInt
#else
#error no kevent system call available!?
#endif

#endif /* defined(HAVE_KQUEUE) */
