{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CApiFFI                    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE Trustworthy                #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module GHC.Internal.Event.KQueue
    (
      new
    , available
    ) where

import qualified GHC.Internal.Event.Internal as E

#include "EventConfig.h"
#if !defined(HAVE_KQUEUE)
import GHC.Internal.Base

new :: IO E.Backend
new = errorWithoutStackTrace "KQueue back end not implemented for this platform"

available :: Bool
available = False
{-# INLINE available #-}
#else

import GHC.Internal.Data.Bits (Bits(..), FiniteBits(..))
import GHC.Internal.Int
import GHC.Internal.Data.Maybe ( catMaybes )
import GHC.Internal.Word (Word16, Word32)
import GHC.Internal.Foreign.C.Error (throwErrnoIfMinus1, eINTR, eINVAL,
                        eNOTSUP, getErrno, throwErrno)
import GHC.Internal.Foreign.C.Types
import GHC.Internal.Foreign.Marshal.Alloc (alloca)
import GHC.Internal.Foreign.Marshal.Array (withArrayLen)
import GHC.Internal.Foreign.Ptr (Ptr, nullPtr)
import GHC.Internal.Foreign.Storable (Storable(..))
import GHC.Internal.Base
import GHC.Internal.Enum (toEnum)
import GHC.Internal.Num (Num(..))
import GHC.Internal.Real (quotRem, fromIntegral)
import GHC.Internal.Show (Show(show))
import GHC.Internal.Event.Internal (Timeout(..))
import GHC.Internal.System.Posix.Internals (c_close,c_getpid)
import GHC.Internal.System.Posix.Types (Fd(..), CPid)
import qualified GHC.Internal.Event.Array as A

#if defined(netbsd_HOST_OS)
import GHC.Internal.Int (Int64)
#endif

#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>

-- Handle brokenness on some BSD variants, notably OS X up to at least
-- 10.6.  If NOTE_EOF isn't available, we have no way to receive a
-- notification from the kernel when we reach EOF on a plain file.
#if !defined(NOTE_EOF)
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
    , kqueuePid    :: {-# UNPACK #-} !CPid -- ^ pid, used to detect forks
    }

new :: IO E.Backend
new = do
  kqfd <- kqueue
  events <- A.new 64
  pid <- c_getpid
  let !be = E.backend poll modifyFd modifyFdOnce delete (KQueue kqfd events pid)
  return be

delete :: KQueue -> IO ()
delete kq = do
  -- detect forks: the queue isn't inherited by a child process created with
  -- fork. Hence we mustn't try to close the old fd or we might close a random
  -- one (e.g. the one used by timerfd, cf #24672).
  pid <- c_getpid
  when (pid == kqueuePid kq) $ do
    _ <- c_close . fromKQueueFd . kqueueFd $ kq
    return ()

modifyFd :: KQueue -> Fd -> E.Event -> E.Event -> IO Bool
modifyFd kq fd oevt nevt = do
  kqueueControl (kqueueFd kq) evs
  where
    evs = toEvents fd (toFilter oevt) flagDelete noteEOF
       <> toEvents fd (toFilter nevt) flagAdd noteEOF

toFilter :: E.Event -> [Filter]
toFilter e = catMaybes [ check E.evtRead filterRead, check E.evtWrite filterWrite ]
  where
    check e' f = if e `E.eventIs` e' then Just f else Nothing

modifyFdOnce :: KQueue -> Fd -> E.Event -> IO Bool
modifyFdOnce kq fd evt =
    kqueueControl (kqueueFd kq) (toEvents fd (toFilter evt) (flagAdd .|. flagOneshot) noteEOF)

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
    } deriving ( Eq   -- ^ @since base-4.4.0.0
               , Show -- ^ @since base-4.4.0.0
               )

data Event = KEvent {
      ident  :: {-# UNPACK #-} !CUIntPtr
    , filter :: {-# UNPACK #-} !Filter
    , flags  :: {-# UNPACK #-} !Flag
    , fflags :: {-# UNPACK #-} !FFlag
#if defined(netbsd_HOST_OS)
    , data_  :: {-# UNPACK #-} !Int64
#else
    , data_  :: {-# UNPACK #-} !CIntPtr
#endif
    , udata  :: {-# UNPACK #-} !(Ptr ())
    } deriving Show -- ^ @since base-4.4.0.0

toEvents :: Fd -> [Filter] -> Flag -> FFlag -> [Event]
toEvents fd flts flag fflag = map (\filt -> KEvent (fromIntegral fd) filt flag fflag 0 nullPtr) flts

-- | @since base-4.3.1.0
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
    deriving ( Eq       -- ^ @since base-4.4.0.0
             , Show     -- ^ @since base-4.4.0.0
             , Storable -- ^ @since base-4.4.0.0
             )

#{enum FFlag, FFlag
 , noteEOF = NOTE_EOF
 }

#if SIZEOF_KEV_FLAGS == 4 /* kevent.flag: uint32_t or uint16_t. */
newtype Flag = Flag Word32
#else
newtype Flag = Flag Word16
#endif
    deriving ( Bits       -- ^ @since base-4.7.0.0
             , FiniteBits -- ^ @since base-4.7.0.0
             , Eq         -- ^ @since base-4.4.0.0
             , Num        -- ^ @since base-4.7.0.0
             , Show       -- ^ @since base-4.4.0.0
             , Storable   -- ^ @since base-4.4.0.0
             )

#{enum Flag, Flag
 , flagAdd     = EV_ADD
 , flagDelete  = EV_DELETE
 , flagOneshot = EV_ONESHOT
 }

#if SIZEOF_KEV_FILTER == 4 /*kevent.filter: int32_t or int16_t. */
newtype Filter = Filter Int32
#else
newtype Filter = Filter Int16
#endif
    deriving ( Eq       -- ^ @since base-4.4.0.0
             , Num      -- ^ @since base-4.4.0.0
             , Show     -- ^ @since base-4.4.0.0
             , Storable -- ^ @since base-4.4.0.0
             )

filterRead :: Filter
filterRead = Filter (#const EVFILT_READ)
filterWrite :: Filter
filterWrite  = Filter (#const EVFILT_WRITE)

data TimeSpec = TimeSpec {
      tv_sec  :: {-# UNPACK #-} !CTime
    , tv_nsec :: {-# UNPACK #-} !CLong
    }

-- | @since base-4.3.1.0
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

kqueueControl :: KQueueFd -> [Event] -> IO Bool
kqueueControl kfd evts =
    withTimeSpec (TimeSpec 0 0) $ \tp ->
        withArrayLen evts $ \evlen evp -> do
            res <- kevent False kfd evp evlen nullPtr 0 tp
            if res == -1
              then do
               err <- getErrno
               case err of
                 _ | err == eINTR  -> return True
                 _ | err == eINVAL -> return False
                 _ | err == eNOTSUP -> return False
                 _                 -> throwErrno "kevent"
              else return True

kqueueWait :: KQueueFd -> Ptr Event -> Int -> TimeSpec -> IO Int
kqueueWait fd es cap tm =
    fmap fromIntegral $ E.throwErrnoIfMinus1NoRetry "kevent" $
    withTimeSpec tm $ kevent True fd nullPtr 0 es cap

kqueueWaitNonBlock :: KQueueFd -> Ptr Event -> Int -> IO Int
kqueueWaitNonBlock fd es cap =
    fmap fromIntegral $ E.throwErrnoIfMinus1NoRetry "kevent" $
    withTimeSpec (TimeSpec 0 0) $ kevent False fd nullPtr 0 es cap

-- TODO: We cannot retry on EINTR as the timeout would be wrong.
-- Perhaps we should just return without calling any callbacks.
kevent :: Bool -> KQueueFd -> Ptr Event -> Int -> Ptr Event -> Int -> Ptr TimeSpec
       -> IO CInt
kevent safe k chs chlen evs evlen ts
  | safe      = c_kevent k chs (fromIntegral chlen) evs (fromIntegral evlen) ts
  | otherwise = c_kevent_unsafe k chs (fromIntegral chlen) evs (fromIntegral evlen) ts

withTimeSpec :: TimeSpec -> (Ptr TimeSpec -> IO a) -> IO a
withTimeSpec ts f
  | tv_sec ts < 0 = f nullPtr
  | otherwise     = alloca $ \ptr -> poke ptr ts >> f ptr

fromTimeout :: Timeout -> TimeSpec
fromTimeout Forever     = TimeSpec (-1) (-1)
fromTimeout (Timeout s) = TimeSpec (toEnum sec') (toEnum nanosec')
  where
    (sec, nanosec) = s `quotRem` 1000000000

    nanosec', sec' :: Int
    sec' = fromIntegral sec
    nanosec' = fromIntegral nanosec

toEvent :: Filter -> E.Event
toEvent (Filter f)
  | f == (#const EVFILT_READ) = E.evtRead
  | f == (#const EVFILT_WRITE) = E.evtWrite
  | otherwise = errorWithoutStackTrace $ "toEvent: unknown filter " ++ show f

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
