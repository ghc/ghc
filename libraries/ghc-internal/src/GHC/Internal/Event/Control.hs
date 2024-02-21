{-# LANGUAGE Unsafe #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , ScopedTypeVariables
           , BangPatterns
  #-}

module GHC.Internal.Event.Control
    (
    -- * Managing the IO manager
      Signal
    , ControlMessage(..)
    , Control
    , newControl
    , closeControl
    -- ** Control message reception
    , readControlMessage
    -- *** File descriptors
    , controlReadFd
    , controlWriteFd
    , wakeupReadFd
    -- ** Control message sending
    , sendWakeup
    , sendDie
    -- * Utilities
    , setNonBlockingFD
    ) where

#include <ghcplatform.h>
#include "EventConfig.h"

import GHC.Internal.Base
import GHC.Internal.IORef
import GHC.Internal.Conc.Signal (Signal)
import GHC.Internal.Real (fromIntegral)
import GHC.Internal.Show (Show)
import GHC.Internal.Word (Word8)
import GHC.Internal.Foreign.C.Error (throwErrnoIfMinus1_, throwErrno, getErrno)
import GHC.Internal.Foreign.C.Types (CInt(..), CSize(..))
import GHC.Internal.Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import GHC.Internal.Foreign.Marshal.Alloc (alloca, allocaBytes)
import GHC.Internal.Foreign.Marshal.Array (allocaArray)
import GHC.Internal.Foreign.Ptr (castPtr)
import GHC.Internal.Foreign.Storable (peek, peekElemOff, poke)
import GHC.Internal.System.Posix.Internals (c_close, c_pipe, c_read, c_write,
                               setCloseOnExec, setNonBlockingFD)
import GHC.Internal.System.Posix.Types (Fd)

#if defined(HAVE_EVENTFD)
import GHC.Internal.Foreign.C.Error (throwErrnoIfMinus1, eBADF)
import GHC.Internal.Foreign.C.Types (CULLong(..))
#else
import GHC.Internal.Foreign.C.Error (eAGAIN, eWOULDBLOCK, eBADF)
#endif

data ControlMessage = CMsgWakeup
                    | CMsgDie
                    | CMsgSignal {-# UNPACK #-} !(ForeignPtr Word8)
                                 {-# UNPACK #-} !Signal
    deriving ( Eq   -- ^ @since base-4.4.0.0
             , Show -- ^ @since base-4.4.0.0
             )

-- | The structure used to tell the IO manager thread what to do.
data Control = W {
      controlReadFd  :: {-# UNPACK #-} !Fd
    , controlWriteFd :: {-# UNPACK #-} !Fd
#if defined(HAVE_EVENTFD)
    , controlEventFd :: {-# UNPACK #-} !Fd
#else
    , wakeupReadFd   :: {-# UNPACK #-} !Fd
    , wakeupWriteFd  :: {-# UNPACK #-} !Fd
#endif
    , didRegisterWakeupFd :: !Bool
      -- | Have this Control's fds been cleaned up?
    , controlIsDead  :: !(IORef Bool)
    }

#if defined(HAVE_EVENTFD)
wakeupReadFd :: Control -> Fd
wakeupReadFd = controlEventFd
{-# INLINE wakeupReadFd #-}
#endif

-- | Create the structure (usually a pipe) used for waking up the IO
-- manager thread from another thread.
newControl :: Bool -> IO Control
newControl shouldRegister = allocaArray 2 $ \fds -> do
  let createPipe = do
        throwErrnoIfMinus1_ "pipe" $ c_pipe fds
        rd <- peekElemOff fds 0
        wr <- peekElemOff fds 1
        -- The write end must be non-blocking, since we may need to
        -- poke the event manager from a signal handler.
        setNonBlockingFD wr True
        setCloseOnExec rd
        setCloseOnExec wr
        return (rd, wr)
  (ctrl_rd, ctrl_wr) <- createPipe
#if defined(HAVE_EVENTFD)
  ev <- throwErrnoIfMinus1 "eventfd" $ c_eventfd 0 0
  setNonBlockingFD ev True
  setCloseOnExec ev
  when shouldRegister $ c_setIOManagerWakeupFd ev
#else
  (wake_rd, wake_wr) <- createPipe
  when shouldRegister $ c_setIOManagerWakeupFd wake_wr
#endif
  isDead <- newIORef False
  return W { controlReadFd  = fromIntegral ctrl_rd
           , controlWriteFd = fromIntegral ctrl_wr
#if defined(HAVE_EVENTFD)
           , controlEventFd = fromIntegral ev
#else
           , wakeupReadFd   = fromIntegral wake_rd
           , wakeupWriteFd  = fromIntegral wake_wr
#endif
           , didRegisterWakeupFd = shouldRegister
           , controlIsDead  = isDead
           }

-- | Close the control structure used by the IO manager thread.
-- N.B. If this Control is the Control whose wakeup file was registered with
-- the RTS, then *BEFORE* the wakeup file is closed, we must call
-- c_setIOManagerWakeupFd (-1), so that the RTS does not try to use the wakeup
-- file after it has been closed.
--
-- Note, however, that even if we do the above, this function is still racy
-- since we do not synchronize between here and ioManagerWakeup.
-- ioManagerWakeup ignores failures that arise from this case.
closeControl :: Control -> IO ()
closeControl w = do
  _ <- atomicSwapIORef (controlIsDead w) True
  _ <- c_close . fromIntegral . controlReadFd $ w
  _ <- c_close . fromIntegral . controlWriteFd $ w
  when (didRegisterWakeupFd w) $ c_setIOManagerWakeupFd (-1)
#if defined(HAVE_EVENTFD)
  _ <- c_close . fromIntegral . controlEventFd $ w
#else
  _ <- c_close . fromIntegral . wakeupReadFd $ w
  _ <- c_close . fromIntegral . wakeupWriteFd $ w
#endif
  return ()

io_MANAGER_WAKEUP, io_MANAGER_DIE :: Word8
io_MANAGER_WAKEUP = 0xff
io_MANAGER_DIE    = 0xfe

#if !defined(HAVE_SIGNAL_H)
readControlMessage :: Control -> Fd -> IO ControlMessage
readControlMessage _ _ = errorWithoutStackTrace "readControlMessage"
#else
foreign import ccall "__hscore_sizeof_siginfo_t"
    sizeof_siginfo_t :: CSize

readControlMessage :: Control -> Fd -> IO ControlMessage
readControlMessage ctrl fd
    | fd == wakeupReadFd ctrl = allocaBytes wakeupBufferSize $ \p -> do
                    throwErrnoIfMinus1_ "readWakeupMessage" $
                      c_read (fromIntegral fd) p (fromIntegral wakeupBufferSize)
                    return CMsgWakeup
    | otherwise =
        alloca $ \p -> do
            throwErrnoIfMinus1_ "readControlMessage" $
                c_read (fromIntegral fd) p 1
            s <- peek p
            case s of
                -- Wakeup messages shouldn't be sent on the control
                -- file descriptor but we handle them anyway.
                _ | s == io_MANAGER_WAKEUP -> return CMsgWakeup
                _ | s == io_MANAGER_DIE    -> return CMsgDie
                _ -> do  -- Signal
                    fp <- mallocForeignPtrBytes (fromIntegral sizeof_siginfo_t)
                    withForeignPtr fp $ \p_siginfo -> do
                        r <- c_read (fromIntegral fd) (castPtr p_siginfo)
                             sizeof_siginfo_t
                        when (r /= fromIntegral sizeof_siginfo_t) $
                            errorWithoutStackTrace "failed to read siginfo_t"
                        let !s' = fromIntegral s
                        return $ CMsgSignal fp s'

  where wakeupBufferSize =
#if defined(HAVE_EVENTFD)
            8
#else
            4096
#endif
#endif

sendWakeup :: Control -> IO ()
#if defined(HAVE_EVENTFD)
sendWakeup c = do
  n <- c_eventfd_write (fromIntegral (controlEventFd c)) 1
  case n of
    0     -> return ()
    _     -> do errno <- getErrno
                -- Check that Control is still alive if we failed, since it's
                -- possible that someone cleaned up the fds behind our backs and
                -- consequently eventfd_write failed with EBADF. If it is dead
                -- then just swallow the error since we are shutting down
                -- anyways. Otherwise we will see failures during shutdown from
                -- setnumcapabilities001 (#12038)
                isDead <- readIORef (controlIsDead c)
                if isDead && errno == eBADF
                  then return ()
                  else throwErrno "sendWakeup"
#else
sendWakeup c = do
  n <- sendMessage (wakeupWriteFd c) CMsgWakeup
  case n of
    _ | n /= -1   -> return ()
      | otherwise -> do
                   errno <- getErrno
                   isDead <- readIORef (controlIsDead c)
                   case () of
                     _   -- Someone else has beat us to waking it up
                       | errno == eAGAIN          -> return ()
                       | errno == eWOULDBLOCK     -> return ()
                         -- we are shutting down
                       | errno == eBADF && isDead -> return ()
                         -- something bad happened
                       | otherwise                -> throwErrno "sendWakeup"
#endif

sendDie :: Control -> IO ()
sendDie c = throwErrnoIfMinus1_ "sendDie" $
            sendMessage (controlWriteFd c) CMsgDie

sendMessage :: Fd -> ControlMessage -> IO Int
sendMessage fd msg = alloca $ \p -> do
  case msg of
    CMsgWakeup        -> poke p io_MANAGER_WAKEUP
    CMsgDie           -> poke p io_MANAGER_DIE
    CMsgSignal _fp _s -> errorWithoutStackTrace "Signals can only be sent from within the RTS"
  fromIntegral `fmap` c_write (fromIntegral fd) p 1

#if defined(HAVE_EVENTFD)
foreign import ccall unsafe "sys/eventfd.h eventfd"
   c_eventfd :: CInt -> CInt -> IO CInt

foreign import ccall unsafe "sys/eventfd.h eventfd_write"
   c_eventfd_write :: CInt -> CULLong -> IO CInt
#endif

#if defined(wasm32_HOST_ARCH)
c_setIOManagerWakeupFd :: CInt -> IO ()
c_setIOManagerWakeupFd _ = pure ()
#else
foreign import ccall unsafe "setIOManagerWakeupFd"
   c_setIOManagerWakeupFd :: CInt -> IO ()
#endif
