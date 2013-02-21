{-# LANGUAGE Unsafe #-}
{-# LANGUAGE CPP
           , ForeignFunctionInterface
           , NoImplicitPrelude
           , ScopedTypeVariables
           , BangPatterns
  #-}

module GHC.Event.Control
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
    , wakeupReadFd
    -- ** Control message sending
    , sendWakeup
    , sendDie
    -- * Utilities
    , setNonBlockingFD
    ) where

#include "EventConfig.h"

import Control.Monad (when)
import Foreign.ForeignPtr (ForeignPtr)
import GHC.Base
import GHC.Conc.Signal (Signal)
import GHC.Real (fromIntegral)
import GHC.Show (Show)
import GHC.Word (Word8)
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal (alloca, allocaBytes)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (castPtr)
import Foreign.Storable (peek, peekElemOff, poke)
import System.Posix.Internals (c_close, c_pipe, c_read, c_write,
                               setCloseOnExec, setNonBlockingFD)
import System.Posix.Types (Fd)

#if defined(HAVE_EVENTFD)
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.C.Types (CULLong(..))
#else
import Foreign.C.Error (eAGAIN, eWOULDBLOCK, getErrno, throwErrno)
#endif

data ControlMessage = CMsgWakeup
                    | CMsgDie
                    | CMsgSignal {-# UNPACK #-} !(ForeignPtr Word8)
                                 {-# UNPACK #-} !Signal
    deriving (Eq, Show)

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
    } deriving (Show)

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
  when shouldRegister $ c_setIOManagerControlFd ctrl_wr
#if defined(HAVE_EVENTFD)
  ev <- throwErrnoIfMinus1 "eventfd" $ c_eventfd 0 0
  setNonBlockingFD ev True
  setCloseOnExec ev
  when shouldRegister $ c_setIOManagerWakeupFd ev
#else
  (wake_rd, wake_wr) <- createPipe
  when shouldRegister $ c_setIOManagerWakeupFd wake_wr
#endif
  return W { controlReadFd  = fromIntegral ctrl_rd
           , controlWriteFd = fromIntegral ctrl_wr
#if defined(HAVE_EVENTFD)
           , controlEventFd = fromIntegral ev
#else
           , wakeupReadFd   = fromIntegral wake_rd
           , wakeupWriteFd  = fromIntegral wake_wr
#endif
           }

-- | Close the control structure used by the IO manager thread.
closeControl :: Control -> IO ()
closeControl w = do
  _ <- c_close . fromIntegral . controlReadFd $ w
  _ <- c_close . fromIntegral . controlWriteFd $ w
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
                            error "failed to read siginfo_t"
                        let !s' = fromIntegral s
                        return $ CMsgSignal fp s'

  where wakeupBufferSize =
#if defined(HAVE_EVENTFD)
            8
#else
            4096
#endif

sendWakeup :: Control -> IO ()
#if defined(HAVE_EVENTFD)
sendWakeup c =
  throwErrnoIfMinus1_ "sendWakeup" $
  c_eventfd_write (fromIntegral (controlEventFd c)) 1
#else
sendWakeup c = do
  n <- sendMessage (wakeupWriteFd c) CMsgWakeup
  case n of
    _ | n /= -1   -> return ()
      | otherwise -> do
                   errno <- getErrno
                   when (errno /= eAGAIN && errno /= eWOULDBLOCK) $
                     throwErrno "sendWakeup"
#endif

sendDie :: Control -> IO ()
sendDie c = throwErrnoIfMinus1_ "sendDie" $
            sendMessage (controlWriteFd c) CMsgDie

sendMessage :: Fd -> ControlMessage -> IO Int
sendMessage fd msg = alloca $ \p -> do
  case msg of
    CMsgWakeup        -> poke p io_MANAGER_WAKEUP
    CMsgDie           -> poke p io_MANAGER_DIE
    CMsgSignal _fp _s -> error "Signals can only be sent from within the RTS"
  fromIntegral `fmap` c_write (fromIntegral fd) p 1

#if defined(HAVE_EVENTFD)
foreign import ccall unsafe "sys/eventfd.h eventfd"
   c_eventfd :: CInt -> CInt -> IO CInt

foreign import ccall unsafe "sys/eventfd.h eventfd_write"
   c_eventfd_write :: CInt -> CULLong -> IO CInt
#endif

-- Used to tell the RTS how it can send messages to the I/O manager.
foreign import ccall "setIOManagerControlFd"
   c_setIOManagerControlFd :: CInt -> IO ()

foreign import ccall "setIOManagerWakeupFd"
   c_setIOManagerWakeupFd :: CInt -> IO ()
