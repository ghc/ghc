{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

-----------------------------------------------------------------------------
-- |
-- A binding to the epoll I/O event notification facility
--
-- epoll is a variant of poll that can be used either as an edge-triggered or
-- a level-triggered interface and scales well to large numbers of watched file
-- descriptors.
--
-- epoll decouples monitor an fd from the process of registering it.
--
-----------------------------------------------------------------------------

#include "EventConfig.h"

module GHC.Event.EPoll
    (
#if defined(HAVE_EPOLL)
      BackendState
    , new
    , poll
    , modifyFd
    , modifyFdOnce
    , delete
#endif
    ) where


#if defined(HAVE_EPOLL)

#include <sys/epoll.h>

import qualified GHC.Event.Internal as E
import Data.Bits (Bits, FiniteBits, (.|.), (.&.))
import Data.Word (Word32)
import Foreign.C.Error (eNOENT, getErrno, throwErrno,
                        throwErrnoIfMinus1, throwErrnoIfMinus1_)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import GHC.Base
import GHC.Num (Num(..))
import GHC.Real (fromIntegral, div)
import GHC.Show (Show)
import System.Posix.Internals (c_close)
import System.Posix.Internals (setCloseOnExec)
import System.Posix.Types (Fd(..))
import GHC.Primitive.PrimArray (MutablePrimArray(..))
import GHC.Primitive.PrimArray (newPrimArray,writePrimArray)
import GHC.Primitive.Monad (Prim(..))

import qualified GHC.Event.Array     as A
import qualified GHC.Primitive.Monad as P
import           GHC.Event.Internal (Timeout(..))

data BackendState = BackendState {
      epollFd     :: {-# UNPACK #-} !EPollFd
    , epollEvents :: {-# UNPACK #-} !(A.Array EPollEvent)
    }

-- | Create a new epoll backend.
new :: IO BackendState
new = liftA2 BackendState epollCreate (A.new 64)

delete :: BackendState -> IO ()
delete !be = do
  _ <- c_close . fromEPollFd . epollFd $ be
  return ()

-- | Change the set of events we are interested in for a given file
-- descriptor.
modifyFd :: BackendState -> Fd -> E.Event -> E.Event -> IO Bool
modifyFd !ep !fd !oevt !nevt = do
  -- No need to pin the array since c_epoll_ctl uses unsafe FFI. 
  evArr <- newPrimArray 1 
  writePrimArray evArr 0 (EPollEvent (fromEvent nevt) fd)
  epollControl (epollFd ep) op fd evArr
  return True
  where op | oevt == mempty = controlOpAdd
           | nevt == mempty = controlOpDelete
           | otherwise      = controlOpModify

modifyFdOnce :: BackendState -> Fd -> E.Event -> IO Bool
modifyFdOnce !ep !fd !evt =
  do let !ev = fromEvent evt .|. epollOneShot
     -- No need to pin the array since c_epoll_ctl uses unsafe FFI. 
     evArr <- newPrimArray 1 
     writePrimArray evArr 0 (EPollEvent ev fd)
     res <- epollControl_ (epollFd ep) controlOpModify fd evArr
     if res == 0
       then return True
       else do
         err <- getErrno
         if err == eNOENT
           then do
             writePrimArray evArr 0 (EPollEvent ev fd)
             epollControl (epollFd ep) controlOpAdd fd evArr
             return True
           else throwErrno "modifyFdOnce"

-- | Select a set of file descriptors which are ready for I/O
-- operations and call @f@ for all ready file descriptors, passing the
-- events that are ready.
poll :: BackendState              -- ^ state
     -> Maybe Timeout             -- ^ timeout in milliseconds
     -> (Fd -> E.Event -> IO ())  -- ^ I/O callback
     -> IO Int
poll !ep mtimeout f = do
  let events = epollEvents ep
      fd = epollFd ep

  -- Will return zero if the system call was interrupted, in which case
  -- we just return (and try again later.)
  n <- A.unsafeLoad events $ \es cap -> case mtimeout of
    Just timeout -> epollWait fd es cap $ fromTimeout timeout
    Nothing      -> epollWaitNonBlock fd es cap

  -- If we received the greatest number of events that our buffer allows,
  -- double the size of the buffer so that future calls to epoll_wait
  -- are more likely to have enough space to put every epoll_event.
  when (n > 0) $ do
    A.forM_ events $ \e -> f (eventFd e) (toEvent (eventTypes e))
    cap <- A.capacity events
    when (cap == n) $ A.ensureCapacity events (2 * cap)
  return n

newtype EPollFd = EPollFd { fromEPollFd :: CInt }
  deriving stock (Eq, Show)
  deriving newtype (Prim)

data EPollEvent = EPollEvent {
      eventTypes :: {-# UNPACK #-} !EventType
    , eventFd    :: {-# UNPACK #-} !Fd
    } deriving (Show)

unI :: Int -> Int##
unI (I## i) = i

-- | @since 4.14.0.0
instance Prim EPollEvent where
  sizeOf## _ = unI #{size struct epoll_event}
  alignment## _ = alignment## (undefined :: CInt)
  indexByteArray## arr i = EPollEvent
    (#{indexByteArrayHash struct epoll_event, events} arr i)
    (#{indexByteArrayHash struct epoll_event, data.fd} arr i)
  writeByteArray## arr i p s0 = case #{writeByteArrayHash struct epoll_event, events} arr i (eventTypes p) s0 of
    s1 -> #{writeByteArrayHash struct epoll_event, data.fd} arr i (eventFd p) s1
  readByteArray## arr i s0 = case #{readByteArrayHash struct epoll_event, events} arr i s0 of
    (## s1, eventsVal ##) -> case #{readByteArrayHash struct epoll_event, data.fd} arr i s1 of
      (## s2, dataFdVal ##) -> (## s2, EPollEvent eventsVal dataFdVal ##)
  setByteArray## = P.defaultSetByteArray##
  indexOffAddr## arr i = EPollEvent
    (#{indexOffAddrHash struct epoll_event, events} arr i)
    (#{indexOffAddrHash struct epoll_event, data.fd} arr i)
  writeOffAddr## arr i p s0 = case #{writeOffAddrHash struct epoll_event, events} arr i (eventTypes p) s0 of
    s1 -> #{writeOffAddrHash struct epoll_event, data.fd} arr i (eventFd p) s1
  readOffAddr## arr i s0 = case #{readOffAddrHash struct epoll_event, data.fd} arr i s0 of
    (## s1, eventsVal ##) -> case #{readOffAddrHash struct epoll_event, events} arr i s1 of
      (## s2, dataFdVal ##) -> (## s2, EPollEvent eventsVal dataFdVal ##)
  setOffAddr## = P.defaultSetOffAddr##
  
-- | @since 4.3.1.0
instance Storable EPollEvent where
    sizeOf    _ = #size struct epoll_event
    alignment _ = alignment (undefined :: CInt)

    peek ptr = do
        ets <- #{peek struct epoll_event, events} ptr
        ed  <- #{peek struct epoll_event, data.fd}   ptr
        let !ev = EPollEvent (EventType ets) ed
        return ev

    poke ptr e = do
        #{poke struct epoll_event, events} ptr (unEventType $ eventTypes e)
        #{poke struct epoll_event, data.fd}   ptr (eventFd e)

newtype ControlOp = ControlOp CInt

#{enum ControlOp, ControlOp
 , controlOpAdd    = EPOLL_CTL_ADD
 , controlOpModify = EPOLL_CTL_MOD
 , controlOpDelete = EPOLL_CTL_DEL
 }

newtype EventType = EventType { unEventType :: Word32 }
  deriving stock
    ( Show       -- ^ @since 4.4.0.0
    , Eq         -- ^ @since 4.4.0.0
    )
  deriving newtype
    ( Num        -- ^ @since 4.4.0.0
    , Bits       -- ^ @since 4.4.0.0
    , FiniteBits -- ^ @since 4.7.0.0
    , Prim       -- ^ @since 4.14.0.0
    )

#{enum EventType, EventType
 , epollIn  = EPOLLIN
 , epollOut = EPOLLOUT
 , epollErr = EPOLLERR
 , epollHup = EPOLLHUP
 , epollOneShot = EPOLLONESHOT
 }

-- | Create a new epoll context, returning a file descriptor associated with the context.
-- The fd may be used for subsequent calls to this epoll context.
--
-- The size parameter to epoll_create is a hint about the expected number of handles.
--
-- The file descriptor returned from epoll_create() should be destroyed via
-- a call to close() after polling is finished
--
epollCreate :: IO EPollFd
epollCreate = do
  fd <- throwErrnoIfMinus1 "epollCreate" $
        c_epoll_create 256 -- argument is ignored
  setCloseOnExec fd
  let !epollFd' = EPollFd fd
  return epollFd'

epollControl ::
     EPollFd
  -> ControlOp
  -> Fd
  -> MutablePrimArray RealWorld EPollEvent
  -> IO ()
epollControl !epfd op !fd !event =
    throwErrnoIfMinus1_ "epollControl" $ epollControl_ epfd op fd event

epollControl_ ::
     EPollFd
  -> ControlOp
  -> Fd
  -> MutablePrimArray RealWorld EPollEvent
  -> IO CInt
epollControl_ (EPollFd epfd) (ControlOp op) (Fd fd) (MutablePrimArray eventBytes) =
    c_epoll_ctl epfd op fd eventBytes

epollWait :: EPollFd -> Ptr EPollEvent -> Int -> Int -> IO Int
epollWait (EPollFd !epfd) !events !numEvents !timeout =
    fmap fromIntegral .
    E.throwErrnoIfMinus1NoRetry "epollWait" $
    c_epoll_wait epfd events (fromIntegral numEvents) (fromIntegral timeout)

epollWaitNonBlock :: EPollFd -> Ptr EPollEvent -> Int -> IO Int
epollWaitNonBlock (EPollFd !epfd) !events !numEvents =
  fmap fromIntegral .
  E.throwErrnoIfMinus1NoRetry "epollWaitNonBlock" $
  c_epoll_wait_unsafe epfd events (fromIntegral numEvents) 0

fromEvent :: E.Event -> EventType
fromEvent !e = remap E.evtRead  epollIn .|.
               remap E.evtWrite epollOut
  where remap evt to
            | e `E.eventIs` evt = to
            | otherwise         = 0

toEvent :: EventType -> E.Event
toEvent !e = remap (epollIn  .|. epollErr .|. epollHup) E.evtRead `mappend`
             remap (epollOut .|. epollErr .|. epollHup) E.evtWrite
  where remap evt to
            | e .&. evt /= 0 = to
            | otherwise      = mempty

fromTimeout :: Timeout -> Int
fromTimeout Forever     = -1
fromTimeout (Timeout s) = fromIntegral $ s `divRoundUp` 1000000
  where
    divRoundUp num denom = (num + denom - 1) `div` denom

foreign import ccall unsafe "sys/epoll.h epoll_create"
    c_epoll_create :: CInt -> IO CInt

foreign import ccall unsafe "sys/epoll.h epoll_ctl"
    c_epoll_ctl :: CInt -> CInt -> CInt 
                -> MutableByteArray## RealWorld
                   -- this is an array of epoll_event
                -> IO CInt

foreign import ccall safe "sys/epoll.h epoll_wait"
    c_epoll_wait :: CInt -> Ptr EPollEvent -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "sys/epoll.h epoll_wait"
    c_epoll_wait_unsafe :: CInt -> Ptr EPollEvent -> CInt -> CInt -> IO CInt

#endif
