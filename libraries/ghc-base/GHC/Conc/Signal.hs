{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Conc.Signal
        ( Signal
        , HandlerFun
        , setHandler
        , runHandlers
        , runHandlersPtr
        ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Data.Dynamic (Dynamic)
import Foreign.C.Types (CInt)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr)
import Foreign.StablePtr (castPtrToStablePtr, castStablePtrToPtr,
                          deRefStablePtr, freeStablePtr, newStablePtr)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Marshal.Alloc (finalizerFree)
import GHC.Arr (inRange)
import GHC.Base
import GHC.Conc.Sync (forkIO)
import GHC.IO (mask_, unsafePerformIO)
import GHC.IOArray (IOArray, boundsIOArray, newIOArray,
                    unsafeReadIOArray, unsafeWriteIOArray)
import GHC.Real (fromIntegral)
import GHC.Word (Word8)

------------------------------------------------------------------------
-- Signal handling

type Signal = CInt

maxSig :: Int
maxSig = 64

type HandlerFun = ForeignPtr Word8 -> IO ()

-- Lock used to protect concurrent access to signal_handlers.  Symptom
-- of this race condition is GHC bug #1922, although that bug was on
-- Windows a similar bug also exists on Unix.
signal_handlers :: MVar (IOArray Int (Maybe (HandlerFun,Dynamic)))
signal_handlers = unsafePerformIO $ do
  arr <- newIOArray (0, maxSig) Nothing
  m <- newMVar arr
  sharedCAF m getOrSetGHCConcSignalSignalHandlerStore
{-# NOINLINE signal_handlers #-}

foreign import ccall unsafe "getOrSetGHCConcSignalSignalHandlerStore"
  getOrSetGHCConcSignalSignalHandlerStore :: Ptr a -> IO (Ptr a)

setHandler :: Signal -> Maybe (HandlerFun, Dynamic)
           -> IO (Maybe (HandlerFun, Dynamic))
setHandler sig handler = do
  let int = fromIntegral sig
  withMVar signal_handlers $ \arr ->
    if not (inRange (boundsIOArray arr) int)
      then errorWithoutStackTrace "GHC.Conc.setHandler: signal out of range"
      else do old <- unsafeReadIOArray arr int
              unsafeWriteIOArray arr int handler
              return old

runHandlers :: ForeignPtr Word8 -> Signal -> IO ()
runHandlers p_info sig = do
  let int = fromIntegral sig
  withMVar signal_handlers $ \arr ->
    if not (inRange (boundsIOArray arr) int)
      then return ()
      else do handler <- unsafeReadIOArray arr int
              case handler of
                Nothing -> return ()
                Just (f,_)  -> do _ <- forkIO (f p_info)
                                  return ()

-- It is our responsibility to free the memory buffer, so we create a
-- foreignPtr.
runHandlersPtr :: Ptr Word8 -> Signal -> IO ()
runHandlersPtr p s = do
  fp <- newForeignPtr finalizerFree p
  runHandlers fp s

-- Machinery needed to ensure that we only have one copy of certain
-- CAFs in this module even when the base package is present twice, as
-- it is when base is dynamically loaded into GHCi.  The RTS keeps
-- track of the single true value of the CAF, so even when the CAFs in
-- the dynamically-loaded base package are reverted, nothing bad
-- happens.
--
sharedCAF :: a -> (Ptr a -> IO (Ptr a)) -> IO a
sharedCAF a get_or_set =
  mask_ $ do
    stable_ref <- newStablePtr a
    let ref = castPtr (castStablePtrToPtr stable_ref)
    ref2 <- get_or_set ref
    if ref == ref2
      then return a
      else do freeStablePtr stable_ref
              deRefStablePtr (castPtrToStablePtr (castPtr ref2))

